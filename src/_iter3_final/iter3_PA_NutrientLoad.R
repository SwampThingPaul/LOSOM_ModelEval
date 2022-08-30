## 
## LOSOM - Preferred Alternative nutrient load evaluation
##
## Iteration 3 final array
## NOTES: C10A renamed to S271
##
## Code was compiled by Paul Julian
## contact info: pjulian@sccf.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(openxlsx)
library(plyr)
library(reshape2)
library(dssrip)
library(zoo)
library(classInt)
#
library(magrittr)
library(flextable)
library(ggplot2)

## Paths
wd="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

# Load models -------------------------------------------------------------
load(paste0(data.path,"WQ_models/CRE_mods.RData"))
CRE.mod.TN.all=mod.TN.all
CRE.mod.TP.all=mod.TP.all

load(paste0(data.path,"WQ_models/SLE_mods.RData"))
SLE.mod.TN.all=mod.TN.all
SLE.mod.TP.all=mod.TP.all

# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_3/Model_Output/"))
alts=alts[!alts%in%c("Northern_Estuaries","Lake_Okeechobee")]
n.alts=length(alts)
alts.sort=c("ECB19","NA22f","NA25f","PA22","PA25")

cols.alts=c(grey.colors(3),wesanderson::wes_palette("Zissou1",2,"continuous"))

cols.alts2=c("#4D4D4D", "#AEAEAE", "#3B9AB2", "#E6E6E6", "#F21A00")
alts.sort2=c("ECB19","NA22f","PA22","NA25f","PA25")


# Lake Stage --------------------------------------------------------------

n.alts=length(alts)
lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/RSMBN_output.dss"))
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
unique(lakeO.stage$Alt)
range(lakeO.stage$Date)

lakeO.stage$WY=WY(lakeO.stage$Date)
RSM.lakeO.stg=ddply(lakeO.stage,c("Alt","WY"),summarise,
                    mean.stg=mean(STAGE,na.rm=T))
head(RSM.lakeO.stg)

# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S78","S79","S80","S308")

q.dat=data.frame()
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    q.dat=rbind(tmp,q.dat)
    print(i)
  }
}
unique(q.dat$Alt)

head(q.dat,20)
q.dat=q.dat[order(q.dat$Alt,q.dat$SITE,q.dat$Date),]
q.dat$WY=WY(q.dat$Date)

RSM.q.dat.xtab=reshape2::dcast(q.dat,Alt+Date~SITE,value.var="FLOW",mean)
RSM.q.dat.xtab$WY=WY(RSM.q.dat.xtab$Date)
summary(RSM.q.dat.xtab)

RSM.q.dat.xtab.WY=ddply(RSM.q.dat.xtab,c("Alt","WY"),summarise,
                        Q.S77=sum(cfs.to.acftd(S77),na.rm=T),
                        Q.S79=sum(cfs.to.acftd(S79),na.rm=T),
                        Q.S308=sum(cfs.to.acftd(S308),na.rm=T),
                        Q.S80=sum(cfs.to.acftd(S80),na.rm=T))
RSM.q.dat.xtab.WY$Q.C43=with(RSM.q.dat.xtab.WY,ifelse(Q.S79<Q.S77,0,Q.S79-Q.S77))
RSM.q.dat.xtab.WY$Q.C44=with(RSM.q.dat.xtab.WY,ifelse(Q.S80<Q.S308,0,Q.S80-Q.S308))

RSM.hydro=merge(RSM.q.dat.xtab.WY,RSM.lakeO.stg,c("Alt","WY"))
RSM.hydro=subset(RSM.hydro,WY%in%seq(1966,2016,1))
RSM.hydro$Alt=factor(RSM.hydro$Alt,levels=alts.sort)


# Nutrient Load -----------------------------------------------------------
cre.nut.mod=RSM.hydro[,c("Alt","WY","Q.S77","Q.S79","Q.C43","mean.stg")]
cre.nut.mod$TPLoad.kg.fit=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,1])
cre.nut.mod$TPLoad.kg.95LCI=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,2])
cre.nut.mod$TPLoad.kg.95UCI=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,3])

cre.nut.mod$TNLoad.kg.fit=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,1])
cre.nut.mod$TNLoad.kg.95LCI=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,2])
cre.nut.mod$TNLoad.kg.95UCI=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,3])

cre.nut.mod$S79.TNFWM=with(cre.nut.mod,(TNLoad.kg.fit/(Q.S79*1.233e6))*1e6)
cre.nut.mod$S79.TPFWM=with(cre.nut.mod,(TPLoad.kg.fit/(Q.S79*1.233e6))*1e9)
cre.nut.mod$Alt=factor(cre.nut.mod$Alt,levels=alts.sort)


tmp=reshape2::dcast(cre.nut.mod,WY~Alt,value.var = "TNLoad.kg.fit",mean)
tmp$perdiff.NA25=with(tmp,((PA25-NA25f)/NA25f)*100)
tmp$diff.NA25=with(tmp,NA25f-PA25)
plot(diff.NA25~WY,tmp,type="l");abline(h=0)

##
vars=c("Alt","WY","TPLoad.kg.fit","TNLoad.kg.fit","S79.TPFWM","S79.TNFWM")
cre.nut.mod.melt=reshape2::melt(cre.nut.mod[,vars],id.vars=vars[1:2])

cre.nut.mod.sum1=reshape2::dcast(cre.nut.mod.melt,variable~Alt,value.var="value",mean)
cre.nut.mod.sum1$perdiff.PA25=with(cre.nut.mod.sum1,((PA25-NA25f)/NA25f)*100)
cre.nut.mod.sum1$perdiff.PA22=with(cre.nut.mod.sum1,((PA22-NA22f)/NA22f)*100)

barplot(t(subset(cre.nut.mod.sum1,variable=="TPLoad.kg.fit")[,c("perdiff.PA22","perdiff.PA25")]),
        beside=T,ylim=c(-5,0))


barplot(t(subset(cre.nut.mod.sum1,variable=="TPLoad.kg.fit")[,alts.sort2]),
        beside=T)


alts.sort2
cre.nut.mod$Alt=factor(cre.nut.mod$Alt,levels=alts.sort2)

# png(filename=paste0(plot.path,"Iteration3_Final/S79_Load_bxp.png"),width=6,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.25,1),oma=c(2,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(9e4,42e4);by.y=10e4;ymaj=seq(max(c(ylim.val[1],10e4)),ylim.val[2],by.y);ymin=seq(max(c(ylim.val[1],10e4)),ylim.val[2],by.y/2)
boxplot(TPLoad.kg.fit~Alt,cre.nut.mod,col=adjustcolor(cols.alts2,0.5),outline=F,ann=F,axes=F,ylim=ylim.val)
axis_fun(1,1:5,1:5,NA)
axis_fun(2,ymaj,ymin,ymaj/1e4);box(lwd=1)
mtext(side=2,line=2,"TP Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=3,adj=0,"CRE (S-79)")
mtext(side=3,adj=1,"FLWY 1966 - 2016")

ylim.val=c(7e5,60e5);by.y=20e5;ymaj=seq(max(c(ylim.val[1],10e5)),ylim.val[2],by.y);ymin=seq(max(c(ylim.val[1],10e5)),ylim.val[2],by.y/2)
boxplot(TNLoad.kg.fit~Alt,cre.nut.mod,col=adjustcolor(cols.alts2,0.5),outline=F,ann=F,axes=F,ylim=ylim.val)
axis_fun(1,1:5,1:5,alts.sort2,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/1e5);box(lwd=1)
mtext(side=2,line=2,"TN Load (x10\u2075 kg WY\u207B\u00B9)")
mtext(side=1,line=1.5,"Alternatives")
# abline(h=5900*1000,lty=2,col="red")
# text(0.5,5900*1000,"TMDL",pos=1,cex=0.75,col="red",offset=0.1)

dev.off()



xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,75*10e4);by.y=25*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[1])$TNLoad.kg.fit),lines(1-proportion,value,col=cols.alts2[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[2])$TNLoad.kg.fit),lines(1-proportion,value,col=cols.alts2[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[4])$TNLoad.kg.fit),lines(1-proportion,value,col=adjustcolor(cols.alts2[4],0.5),lwd=2))

  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[1])$TNLoad.kg.fit),lines(1-proportion,value,col=cols.alts2[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[3])$TNLoad.kg.fit),lines(1-proportion,value,col=cols.alts2[3],lty=2,lwd=1.5))
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[5])$TNLoad.kg.fit),lines(1-proportion,value,col=adjustcolor(cols.alts2[5],0.5),lwd=2))
  
## 
# cre.nut.mod.sum=ddply(cre.nut.mod,"Alt",summarise,
#                       mean.TP.load=mean(TPLoad.kg.fit),mean.TN.load=mean(TNLoad.kg.fit),
#                       mean.TP.FWM=mean(S79.TPFWM),mean.TN.FWM=mean(S79.TNFWM))
# cre.nut.mod.sum$Alt=factor(cre.nut.mod.sum$Alt,levels=alts.sort)
# 
# cre.nut.mod.sum$TP.FWO.diff=with(cre.nut.mod.sum,(mean.TP.load-mean.TP.load[3])/mean.TP.load[3])*100
# cre.nut.mod.sum$TN.FWO.diff=with(cre.nut.mod.sum,(mean.TN.load-mean.TN.load[3])/mean.TN.load[3])*100
# 
# cre.nut.mod.sum$TP.FWM.FWO.diff=with(cre.nut.mod.sum,(mean.TP.FWM-mean.TP.FWM[1])/mean.TP.FWM[1])*100
# cre.nut.mod.sum$TN.FWM.FWO.diff=with(cre.nut.mod.sum,(mean.TN.FWM-mean.TN.FWM[1])/mean.TN.FWM[1])*100
