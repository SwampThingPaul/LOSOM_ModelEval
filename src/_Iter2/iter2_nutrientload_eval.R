## 
## LOSOM - WQ evaluation
##
## Iteration 2 alternative evaluation
##
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
library(reshape)
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

##GGPLOT theme defaults
theme_set(theme_minimal(base_size = 16))


# Load models -------------------------------------------------------------
load(paste0(data.path,"WQ_models/CRE_mods.RData"))
CRE.mod.TN.all=mod.TN.all
CRE.mod.TP.all=mod.TP.all

load(paste0(data.path,"WQ_models/SLE_mods.RData"))
SLE.mod.TN.all=mod.TN.all
SLE.mod.TP.all=mod.TP.all

# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
alts=alts[!alts%in%c("_Batch_Results","Northern_Estuaries","Iteration2_STL_Flows_13Jul2021.xlsx")]
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2","SR3.5")
cols=c("grey50","grey80",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-3,"continuous")),"deeppink")

# Lake Stage --------------------------------------------------------------
lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/RSMBN_output.dss"))  
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
lakeO.stage$WY=WY(lakeO.stage$Date)
RSM.lakeO.stg=ddply(lakeO.stage,c("Alt","WY"),summarise,
                    mean.stg=mean(STAGE,na.rm=T))
head(RSM.lakeO.stg)
# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S78","S79","S80","S308")
q.dat=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
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

ylab.cex=0.8
# png(filename=paste0(plot.path,"Iteration_2/RSM_hydro_ENLM.png"),width=7.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,5,0.25,1),oma=c(3.25,1,1,0.25));
layout(matrix(1:6,3,2,byrow=F))

ylim.val=c(0,180e4);by.y=90e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(Q.C43~Alt,RSM.hydro,ylim=ylim.val,ann=F,axes=F,outline=F,col=adjustcolor(cols,0.5))
axis_fun(2,ymaj,ymin,ymaj/1000)
axis_fun(1,1:n.alts,1:n.alts,NA)
box(lwd=1)
mtext(side=2,line=2.75,"C43 Basin Q\n(kAc-Ft WY\u207B\u00B9)",cex=ylab.cex)

ylim.val=c(0,200e4);by.y=100e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(Q.S77~Alt,RSM.hydro,ylim=ylim.val,ann=F,axes=F,outline=F,col=adjustcolor(cols,0.5))
axis_fun(2,ymaj,ymin,ymaj/1000)
axis_fun(1,1:n.alts,1:n.alts,NA)
box(lwd=1)
mtext(side=2,line=2.75,"S77 Q\n(kAc-Ft WY\u207B\u00B9)",cex=ylab.cex)

ylim.val=c(9,17);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(mean.stg~Alt,RSM.hydro,ylim=ylim.val,ann=F,axes=F,outline=F,col=adjustcolor(cols,0.5))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,cex=0.95,las=2)
box(lwd=1)
mtext(side=2,line=2.75,"Mean Annual\nStage (Ft, NGVD29)",cex=ylab.cex)

ylim.val=c(0,32e4);by.y=15.0e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(Q.C44~Alt,RSM.hydro,ylim=ylim.val,ann=F,axes=F,outline=F,col=adjustcolor(cols,0.5))
axis_fun(2,ymaj,ymin,ymaj/1000)
axis_fun(1,1:n.alts,1:n.alts,NA)
box(lwd=1)
mtext(side=2,line=2.75,"C44 Basin Q\n(kAc-Ft WY\u207B\u00B9)",cex=ylab.cex)

ylim.val=c(0,100e4);by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(Q.S308~Alt,RSM.hydro,ylim=ylim.val,ann=F,axes=F,outline=F,col=adjustcolor(cols,0.5))
axis_fun(2,ymaj,ymin,ymaj/1000)
axis_fun(1,1:n.alts,1:n.alts,NA)
box(lwd=1)
mtext(side=2,line=2.75,"S308 Q\n(kAc-Ft WY\u207B\u00B9)",cex=ylab.cex)

ylim.val=c(100,100e4);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");#by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(Q.S80~Alt,subset(RSM.hydro,Q.S80>0),ylim=ylim.val,ann=F,axes=F,outline=F,col=adjustcolor(cols,0.5),log="y")
axis_fun(2,ymaj,ymin,ymaj/1000)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,cex=0.95,las=2)
box(lwd=1)
mtext(side=2,line=2.75,"S80 Q\n(kAc-Ft WY\u207B\u00B9)",cex=ylab.cex)
mtext(side=1,adj=0,line=-1.25,cex=0.45," Zero flow years removed",font=3)
mtext(side=1,outer=T,line=2,"Alternatives")
dev.off()

cre.nut.mod=RSM.hydro[,c("Alt","WY","Q.S77","Q.S79","Q.C43","mean.stg")]
cre.nut.mod$TPLoad.kg.fit=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,1])
cre.nut.mod$TPLoad.kg.95LCI=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,2])
cre.nut.mod$TPLoad.kg.95UCI=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,3])
# ggplot(cre.nut.mod, aes(x = WY, y = TPLoad.kg.fit))+geom_point(colour="indianred1") + geom_line() + facet_wrap(~ Alt)

cre.nut.mod$TNLoad.kg.fit=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,1])
cre.nut.mod$TNLoad.kg.95LCI=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,2])
cre.nut.mod$TNLoad.kg.95UCI=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,3])
# ggplot(cre.nut.mod, aes(x = WY, y = TNLoad.kg.fit)) +geom_point(colour="indianred1")+ geom_line()+ facet_wrap(~ Alt)

# write.csv(cre.nut.mod,paste0(export.path,"RSMBN_iter2_load_CRE.csv"),row.names=F)
cre.nut.mod$S79.TNFWM=with(cre.nut.mod,(TNLoad.kg.fit/(Q.S79*1.233e6))*1e6)
cre.nut.mod$S79.TPFWM=with(cre.nut.mod,(TPLoad.kg.fit/(Q.S79*1.233e6))*1e9)
# ggplot(cre.nut.mod, aes(x = WY, y = S79.TPFWM))+geom_point(colour="indianred1") + geom_line() + facet_wrap(~ Alt)
# ggplot(cre.nut.mod, aes(x = WY, y = S79.TNFWM)) +geom_point(colour="indianred1")+ geom_line() + facet_wrap(~ Alt)
cre.nut.mod$Alt=factor(cre.nut.mod$Alt,levels=alts.sort)

sle.nut.mod=RSM.hydro[,c("Alt","WY","Q.S80","Q.S308","Q.C44","mean.stg")]
sle.nut.mod$TPLoad.kg.fit=exp(predict(SLE.mod.TP.all,sle.nut.mod,interval="confidence")[,1])
sle.nut.mod$TPLoad.kg.95LCI=exp(predict(SLE.mod.TP.all,sle.nut.mod,interval="confidence")[,2])
sle.nut.mod$TPLoad.kg.95UCI=exp(predict(SLE.mod.TP.all,sle.nut.mod,interval="confidence")[,3])
sle.nut.mod$TNLoad.kg.fit=exp(predict(SLE.mod.TN.all,sle.nut.mod,interval="confidence")[,1])
sle.nut.mod$TNLoad.kg.95LCI=exp(predict(SLE.mod.TN.all,sle.nut.mod,interval="confidence")[,2])
sle.nut.mod$TNLoad.kg.95UCI=exp(predict(SLE.mod.TN.all,sle.nut.mod,interval="confidence")[,3])

# write.csv(sle.nut.mod,paste0(export.path,"RSMBN_iter2_load_SLE.csv"),row.names=F)
sle.nut.mod$S80.TNFWM=with(sle.nut.mod,ifelse(Q.S80<80,NA,(TNLoad.kg.fit/(Q.S80*1.233e6))*1e6))
sle.nut.mod$S80.TPFWM=with(sle.nut.mod,ifelse(Q.S80<80,NA,(TPLoad.kg.fit/(Q.S80*1.233e6))*1e9))
# ggplot(sle.nut.mod, aes(x = WY, y = S80.TPFWM)) +geom_point(colour="indianred1")+ geom_line() + facet_wrap(~ Alt)
# ggplot(sle.nut.mod, aes(x = WY, y = S80.TNFWM)) +geom_point(colour="indianred1") +geom_line() + facet_wrap(~ Alt)
sle.nut.mod$Alt=factor(sle.nut.mod$Alt,levels=alts.sort)
# subset(sle.nut.mod,Q.S80<200&is.na(S80.TPFWM)==F)
# subset(sle.nut.mod,TPLoad.kg.fit<5&is.na(S80.TPFWM)==F)
# -------------------------------------------------------------------------
cols=c("grey50","grey80",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-3,"continuous")),"deeppink")

# Load duration curves
# png(filename=paste0(plot.path,"Iteration_2/S79_TPLoadDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,550000);by.y=250000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[1])$TPLoad.kg.fit),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[2])$TPLoad.kg.fit),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[i])$TPLoad.kg.fit),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  #abline(h=c(750,2100,2600),lty=2,col=adjustcolor("black",0.5))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:5)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"CRE (S-79)")}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj/10e3)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2,outer=T,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Load")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S79_TNLoadDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,75*10e4);by.y=25*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[1])$TNLoad.kg.fit),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[2])$TNLoad.kg.fit),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(cre.nut.mod,Alt==alts.sort[i])$TNLoad.kg.fit),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  #abline(h=c(750,2100,2600),lty=2,col=adjustcolor("black",0.5))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:5)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"CRE (S-79)")}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj/10e4)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2,outer=T,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Load")
dev.off()


cre.nut.mod.sum=ddply(cre.nut.mod,"Alt",summarise,
                      mean.TP.load=mean(TPLoad.kg.fit),mean.TN.load=mean(TNLoad.kg.fit),
                      mean.TP.FWM=mean(S79.TPFWM),mean.TN.FWM=mean(S79.TNFWM))
cre.nut.mod.sum$Alt=factor(cre.nut.mod.sum$Alt,levels=alts.sort)

cre.nut.mod.sum$TP.FWO.diff=with(cre.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
cre.nut.mod.sum$TN.FWO.diff=with(cre.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100

cre.nut.mod.sum$TP.FWM.FWO.diff=with(cre.nut.mod.sum,(mean.TP.FWM-mean.TP.FWM[1])/mean.TP.FWM[1])*100
cre.nut.mod.sum$TN.FWM.FWO.diff=with(cre.nut.mod.sum,(mean.TN.FWM-mean.TN.FWM[1])/mean.TN.FWM[1])*100

# MCDA type analysis
cre.nut.mod.sum2=subset(cre.nut.mod.sum,Alt!="SR3.5")
cre.nut.mod.sum2$TP.load.RS=with(cre.nut.mod.sum2,(mean.TP.load/max(mean.TP.load,na.rm=T)))
cre.nut.mod.sum2$TP.load.RS=1-cre.nut.mod.sum2$TP.load.RS; # lower the better
cre.nut.mod.sum2$TN.load.RS=with(cre.nut.mod.sum2,(mean.TN.load/max(mean.TN.load,na.rm=T)))
cre.nut.mod.sum2$TN.load.RS=1-cre.nut.mod.sum2$TN.load.RS
wts=c(0.5,0.5); # weights do not affect final score.
vars=c("TP.load","TN.load")
cre.nut.mod.sum2$cre.nut.load.score=apply(cre.nut.mod.sum2[,paste0(vars,".RS")],1,FUN=function(x) Hmisc::wtd.mean(x,wts,na.rm=T))
cre.nut.mod.sum2$cre.nut.load.score=round(cre.nut.mod.sum2$cre.nut.load.score/max(cre.nut.mod.sum2$cre.nut.load.score,na.rm=T),2)
cre.nut.mod.sum2[order(-cre.nut.mod.sum2$cre.nut.load.score),]

cre.nut.mod.sum2$TP.FWM.RS=with(cre.nut.mod.sum2,(mean.TP.FWM/max(mean.TP.FWM,na.rm=T)))
cre.nut.mod.sum2$TP.FWM.RS=1-cre.nut.mod.sum2$TP.FWM.RS
cre.nut.mod.sum2$TN.FWM.RS=with(cre.nut.mod.sum2,(mean.TN.FWM/max(mean.TN.FWM,na.rm=T)))
cre.nut.mod.sum2$TN.FWM.RS=1-cre.nut.mod.sum2$TN.FWM.RS
wts=c(0.5,0.5); # weights do not affect final score.
vars=c("TP.FWM","TN.FWM")
cre.nut.mod.sum2$cre.nut.FWM.score=apply(cre.nut.mod.sum2[,paste0(vars,".RS")],1,FUN=function(x) Hmisc::wtd.mean(x,wts,na.rm=T))
cre.nut.mod.sum2$cre.nut.FWM.score=round(cre.nut.mod.sum2$cre.nut.FWM.score/max(cre.nut.mod.sum2$cre.nut.FWM.score,na.rm=T),2)
cre.nut.mod.sum2[order(-cre.nut.mod.sum2$cre.nut.FWM.score),]

wt1=seq(0,1,0.2)
wt2=1-wt1
vars=paste(c("TP","TN"),"FWM",sep=".")
rslt=data.frame()
for(i in 1:length(wt1)){
  wts=c(wt1[i],wt2[i])
  score=apply(cre.nut.mod.sum2[,paste0(vars,".RS")],1,FUN=function(x) Hmisc::wtd.mean(x,wts,na.rm=T))
  score=round(score/max(score,na.rm=T),2)
  tmp=data.frame(Alt=cre.nut.mod.sum2$Alt,score=score)
  tmp$i.val=i
  tmp$TP.wt=wt1[i]
  tmp$TN.wt=wt2[i]
  rslt=rbind(tmp,rslt)
  
}
unique(rslt$Alt)
# png(filename=paste0(plot.path,"Iteration_2/S79_FWM_MCDASense.png"),width=7.5,height=4.5,units="in",res=200,type="windows",bg="white")
xlim.val=c(0,1);by.x=0.2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
par(family="serif",mar=c(1,0.75,1,1),oma=c(3,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))
for(i in 1:8){
plot(score~TP.wt,rslt,type="n",axes=F,ann=F,ylim=xlim.val,xlim=xlim.val)
  abline(h=xmaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
with(subset(rslt,Alt==alts.sort[i]),lines(score~TP.wt,col=cols[i],lwd=2))
with(subset(rslt,Alt==alts.sort[i]),lines(score~TN.wt,col=cols[i],lwd=2,lty=2))
if(i%in%c(1:4)){axis_fun(1,xmaj,xmin,NA,line=-0.5)}else{axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)}
if(i%in%c(1,5)){axis_fun(2,xmaj,xmin,format(xmaj))}else{axis_fun(2,xmaj,xmin,NA)}
  box(lwd=1)
mtext(side=3,adj=0,alts.sort[i])
if(i==4){mtext(side=3,adj=1,"CRE (S79) - FWM")}
if(i==1){
  legend("topleft",legend=c("TP Weights","TN Weights"),
       lty=c(1,2),lwd=c(2),col="grey",
       ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.)
  }
}
mtext(side=2,outer=T,"MCDA Score",line=2)
mtext(side=1,outer=T,"Variable Weights",line=1.5)
dev.off()

wt1=seq(0,1,0.2)
wt2=1-wt1
vars=paste(c("TP","TN"),"load",sep=".")
rslt=data.frame()
for(i in 1:length(wt1)){
  wts=c(wt1[i],wt2[i])
  score=apply(cre.nut.mod.sum2[,paste0(vars,".RS")],1,FUN=function(x) Hmisc::wtd.mean(x,wts,na.rm=T))
  score=round(score/max(score,na.rm=T),2)
  tmp=data.frame(Alt=cre.nut.mod.sum2$Alt,score=score)
  tmp$i.val=i
  tmp$TP.wt=wt1[i]
  tmp$TN.wt=wt2[i]
  rslt=rbind(tmp,rslt)
  
}

# png(filename=paste0(plot.path,"Iteration_2/S79_Load_MCDASense.png"),width=7.5,height=4.5,units="in",res=200,type="windows",bg="white")
xlim.val=c(0,1);by.x=0.2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
par(family="serif",mar=c(1,0.75,1,1),oma=c(3,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))
for(i in 1:8){
  plot(score~TP.wt,rslt,type="n",axes=F,ann=F,ylim=xlim.val,xlim=xlim.val)
  abline(h=xmaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
  with(subset(rslt,Alt==alts.sort[i]),lines(score~TP.wt,col=cols[i],lwd=2))
  with(subset(rslt,Alt==alts.sort[i]),lines(score~TN.wt,col=cols[i],lwd=2,lty=2))
  if(i%in%c(1:4)){axis_fun(1,xmaj,xmin,NA,line=-0.5)}else{axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)}
  if(i%in%c(1,5)){axis_fun(2,xmaj,xmin,format(xmaj))}else{axis_fun(2,xmaj,xmin,NA)}
  box(lwd=1)
  mtext(side=3,adj=0,alts.sort[i])
  if(i==4){mtext(side=3,adj=1,"CRE (S79) - Load")}
  if(i==1){
    legend("topleft",legend=c("TP Weights","TN Weights"),
           lty=c(1,2),lwd=c(2),col="grey",
           ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.)
  }
}
mtext(side=2,outer=T,"MCDA Score",line=2)
mtext(side=1,outer=T,"Variable Weights",line=1.5)
dev.off()

# cre.nut.mod.sum$TP.load.RS2=with(cre.nut.mod.sum,(TP.load.RS/max(TP.load.RS,na.rm=T)))
# cre.nut.mod.sum[order(-cre.nut.mod.sum$TP.load.RS2),]

# png(filename=paste0(plot.path,"Iteration_2/S79_Load_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,500000);by.y=250000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TPLoad.kg.fit~Alt,cre.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(cre.nut.mod,Alt==alts.sort[1])$TPLoad.kg.fit),lty=2)
abline(h=subset(cre.nut.mod.sum,Alt==alts.sort[1])$mean.TP.load,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=3,adj=0,"CRE (S-79)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,60*10e4);by.y=20*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TNLoad.kg.fit~Alt,cre.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.nut.mod.sum$mean.TN.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(cre.nut.mod,Alt==alts.sort[1])$TNLoad.kg.fit),lty=2)
abline(h=subset(cre.nut.mod.sum,Alt==alts.sort[1])$mean.TN.load,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e4)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S79_FWM_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(100,230);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(S79.TPFWM~Alt,cre.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.nut.mod.sum$mean.TP.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(cre.nut.mod.sum,Alt==alts.sort[1])$mean.TP.FWM,lty=2,col="springgreen")
abline(h=median(subset(cre.nut.mod,Alt==alts.sort[1])$S79.TPFWM),lty=2,col="black")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"TP FWM (\u03BCg L\u207B\u00B9)")

ylim.val=c(1.3,1.8);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(S79.TNFWM~Alt,cre.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.nut.mod.sum$mean.TN.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(cre.nut.mod.sum,Alt==alts.sort[1])$mean.TN.FWM,lty=2,col="springgreen")
abline(h=median(subset(cre.nut.mod,Alt==alts.sort[1])$S79.TNFWM),lty=2,col="black")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN FWM (mg L\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()



# png(filename=paste0(plot.path,"Iteration_2/S79_Load_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1.75,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-20,20);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(cre.nut.mod.sum$TP.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(cre.nut.mod.sum,segments(TP.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(cre.nut.mod.sum,points(TP.FWO.diff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(cre.nut.mod.sum,text(TP.FWO.diff,1:n.alts,format(round(TP.FWO.diff,1)),pos=ifelse(TP.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,cre.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP Load",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"CRE (S-79)",line=0.8)

xlim.val=c(-20,20);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(cre.nut.mod.sum$TN.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(cre.nut.mod.sum,segments(TN.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(cre.nut.mod.sum,points(TN.FWO.diff,1:n.alts,pch=21,bg="indianred1",lwd=0.1))
with(cre.nut.mod.sum,text(TN.FWO.diff,1:n.alts,format(round(TN.FWO.diff,1)),pos=ifelse(TN.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN Load",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=0.8)
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S79_FWM_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1.75,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-10,10);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(cre.nut.mod.sum$TP.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(cre.nut.mod.sum,segments(TP.FWM.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(cre.nut.mod.sum,points(TP.FWM.FWO.diff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(cre.nut.mod.sum,text(TP.FWM.FWO.diff,1:n.alts,format(round(TP.FWM.FWO.diff,1)),pos=ifelse(TP.FWM.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,cre.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP FWM",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"CRE (S-79)",line=0.8)

xlim.val=c(-2,3);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(cre.nut.mod.sum$TN.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(cre.nut.mod.sum,segments(TN.FWM.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(cre.nut.mod.sum,points(TN.FWM.FWO.diff,1:n.alts,pch=21,bg="indianred1",lwd=0.1))
with(cre.nut.mod.sum,text(TN.FWM.FWO.diff,1:n.alts,format(round(TN.FWM.FWO.diff,1)),pos=ifelse(TN.FWM.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,NA)
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN FWM",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=0.8)
dev.off()


# SLE ---------------------------------------------------------------------

# Load duration curves
# png(filename=paste0(plot.path,"Iteration_2/S80_TPLoadDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,360000);by.y=150000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(sle.nut.mod,Alt==alts.sort[1])$TPLoad.kg.fit),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(sle.nut.mod,Alt==alts.sort[2])$TPLoad.kg.fit),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(sle.nut.mod,Alt==alts.sort[i])$TPLoad.kg.fit),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  #abline(h=c(750,2100,2600),lty=2,col=adjustcolor("black",0.5))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:5)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"SLE (S-80)")}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj/10e3)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2,outer=T,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Load")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S80_TNLoadDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,5*10e5);by.y=25*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(sle.nut.mod,Alt==alts.sort[1])$TNLoad.kg.fit),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(sle.nut.mod,Alt==alts.sort[2])$TNLoad.kg.fit),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(sle.nut.mod,Alt==alts.sort[i])$TNLoad.kg.fit),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  #abline(h=c(750,2100,2600),lty=2,col=adjustcolor("black",0.5))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:5)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"SLE (S-80)")}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj/10e3)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2,outer=T,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Load")
dev.off()

sle.nut.mod.sum=ddply(sle.nut.mod,"Alt",summarise,
                      mean.TP.load=mean(TPLoad.kg.fit),mean.TN.load=mean(TNLoad.kg.fit),
                      mean.TP.FWM=mean(S80.TPFWM,na.rm=T),mean.TN.FWM=mean(S80.TNFWM,na.rm=T))
sle.nut.mod.sum$Alt=factor(sle.nut.mod.sum$Alt,levels=alts.sort)

sle.nut.mod.sum$TP.FWO.diff=with(sle.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
sle.nut.mod.sum$TN.FWO.diff=with(sle.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100

sle.nut.mod.sum$TP.FWM.FWO.diff=with(sle.nut.mod.sum,(mean.TP.FWM-mean.TP.FWM[1])/mean.TP.FWM[1])*100
sle.nut.mod.sum$TN.FWM.FWO.diff=with(sle.nut.mod.sum,(mean.TN.FWM-mean.TN.FWM[1])/mean.TN.FWM[1])*100

# png(filename=paste0(plot.path,"Iteration_2/S80_Load_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,24e4);by.y=10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TPLoad.kg.fit~Alt,sle.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(sle.nut.mod,Alt==alts.sort[1])$TPLoad.kg.fit),lty=2)
abline(h=subset(sle.nut.mod.sum,Alt==alts.sort[1])$mean.TP.load,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=3,adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,20e5);by.y=10e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TNLoad.kg.fit~Alt,sle.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.nut.mod.sum$mean.TN.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(sle.nut.mod,Alt==alts.sort[1])$TNLoad.kg.fit),lty=2)
abline(h=subset(sle.nut.mod.sum,Alt==alts.sort[1])$mean.TN.load,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e4)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S80_FWM_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(75,240);by.y=100;ymaj=seq(100,ylim.val[2],by.y);ymin=seq(100,ylim.val[2],by.y/2)
boxplot(S80.TPFWM~Alt,sle.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.nut.mod.sum$mean.TP.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(sle.nut.mod.sum,Alt==alts.sort[1])$mean.TP.FWM,lty=2,col="springgreen")
abline(h=median(subset(sle.nut.mod,Alt==alts.sort[1])$S80.TPFWM,na.rm=T),lty=2,col="black")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=2,line=2.5,"TP FWM (\u03BCg L\u207B\u00B9)")

ylim.val=c(1,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(S80.TNFWM~Alt,sle.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.nut.mod.sum$mean.TN.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(sle.nut.mod.sum,Alt==alts.sort[1])$mean.TN.FWM,lty=2,col="springgreen")
abline(h=median(subset(sle.nut.mod,Alt==alts.sort[1])$S80.TNFWM),lty=2,col="black")
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN FWM (mg L\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()



# png(filename=paste0(plot.path,"Iteration_2/S80_Load_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1.75,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-115,100);by.x=50;xmaj=seq(max(c(xlim.val[1],-100)),xlim.val[2],by.x);xmin=seq(max(c(xlim.val[1],-100)),xlim.val[2],by.x/2)
plot(sle.nut.mod.sum$TP.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(sle.nut.mod.sum,segments(TP.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(sle.nut.mod.sum,points(TP.FWO.diff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(sle.nut.mod.sum,text(TP.FWO.diff,1:n.alts,format(round(TP.FWO.diff,1)),pos=ifelse(TP.FWO.diff<0,2,4),cex=0.75))
axis_fun(2,1:n.alts,1:n.alts,sle.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP Load",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"SLE (S-80)",line=0.8)

# xlim.val=c(-110,100);by.x=50;xmaj=seq(max(c(xlim.val[1],-100)),xlim.val[2],by.x);xmin=seq(max(c(xlim.val[1],-100)),xlim.val[2],by.x/2)
plot(sle.nut.mod.sum$TN.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(sle.nut.mod.sum,segments(TN.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(sle.nut.mod.sum,points(TN.FWO.diff,1:n.alts,pch=21,bg="indianred1",lwd=0.1))
with(sle.nut.mod.sum,text(TN.FWO.diff,1:n.alts,format(round(TN.FWO.diff,1)),pos=ifelse(TN.FWO.diff<0,2,4),cex=0.75))
axis_fun(2,1:n.alts,1:n.alts,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN Load",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=0.8)
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S80_FWM_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1.75,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-50,25);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(sle.nut.mod.sum$TP.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(sle.nut.mod.sum,segments(TP.FWM.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(sle.nut.mod.sum,points(TP.FWM.FWO.diff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(sle.nut.mod.sum,text(TP.FWM.FWO.diff,1:n.alts,format(round(TP.FWM.FWO.diff,1)),pos=ifelse(TP.FWM.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,sle.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP FWM",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"SLE (S-80)",line=0.8)

#xlim.val=c(-5,5);by.x=2.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(sle.nut.mod.sum$TN.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(sle.nut.mod.sum,segments(TN.FWM.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(sle.nut.mod.sum,points(TN.FWM.FWO.diff,1:n.alts,pch=21,bg="indianred1",lwd=0.1))
with(sle.nut.mod.sum,text(TN.FWM.FWO.diff,1:n.alts,format(round(TN.FWM.FWO.diff,1)),pos=ifelse(TN.FWM.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,NA)
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN FWM",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=0.8)
dev.off()



# MCDA type analysis
sle.nut.mod.sum2=subset(sle.nut.mod.sum,Alt!="SR3.5")
sle.nut.mod.sum2$TP.load.RS=with(sle.nut.mod.sum2,(mean.TP.load/max(mean.TP.load,na.rm=T)))
sle.nut.mod.sum2$TP.load.RS=1-sle.nut.mod.sum2$TP.load.RS; # lower the better
sle.nut.mod.sum2$TN.load.RS=with(sle.nut.mod.sum2,(mean.TN.load/max(mean.TN.load,na.rm=T)))
sle.nut.mod.sum2$TN.load.RS=1-sle.nut.mod.sum2$TN.load.RS
wts=c(0.5,0.5); # weights do not affect final score.
vars=c("TP.load","TN.load")
sle.nut.mod.sum2$sle.nut.load.score=apply(sle.nut.mod.sum2[,paste0(vars,".RS")],1,FUN=function(x) Hmisc::wtd.mean(x,wts,na.rm=T))
sle.nut.mod.sum2$sle.nut.load.score=round(sle.nut.mod.sum2$sle.nut.load.score/max(sle.nut.mod.sum2$sle.nut.load.score,na.rm=T),2)
sle.nut.mod.sum2[order(-sle.nut.mod.sum2$sle.nut.load.score),]

sle.nut.mod.sum2$TP.FWM.RS=with(sle.nut.mod.sum2,(mean.TP.FWM/max(mean.TP.FWM,na.rm=T)))
sle.nut.mod.sum2$TP.FWM.RS=1-sle.nut.mod.sum2$TP.FWM.RS
sle.nut.mod.sum2$TN.FWM.RS=with(sle.nut.mod.sum2,(mean.TN.FWM/max(mean.TN.FWM,na.rm=T)))
sle.nut.mod.sum2$TN.FWM.RS=1-sle.nut.mod.sum2$TN.FWM.RS
wts=c(0.5,0.5); # weights do not affect final score.
vars=c("TP.FWM","TN.FWM")
sle.nut.mod.sum2$sle.nut.FWM.score=apply(sle.nut.mod.sum2[,paste0(vars,".RS")],1,FUN=function(x) Hmisc::wtd.mean(x,wts,na.rm=T))
sle.nut.mod.sum2$sle.nut.FWM.score=round(sle.nut.mod.sum2$sle.nut.FWM.score/max(sle.nut.mod.sum2$sle.nut.FWM.score,na.rm=T),2)
sle.nut.mod.sum2[order(-sle.nut.mod.sum2$sle.nut.FWM.score),]

wt1=seq(0,1,0.2)
wt2=1-wt1
vars=paste(c("TP","TN"),"FWM",sep=".")
rslt=data.frame()
for(i in 1:length(wt1)){
  wts=c(wt1[i],wt2[i])
  score=apply(sle.nut.mod.sum2[,paste0(vars,".RS")],1,FUN=function(x) Hmisc::wtd.mean(x,wts,na.rm=T))
  score=round(score/max(score,na.rm=T),2)
  tmp=data.frame(Alt=sle.nut.mod.sum2$Alt,score=score)
  tmp$i.val=i
  tmp$TP.wt=wt1[i]
  tmp$TN.wt=wt2[i]
  rslt=rbind(tmp,rslt)
  
}

# png(filename=paste0(plot.path,"Iteration_2/S80_FWM_MCDASense.png"),width=7.5,height=4.5,units="in",res=200,type="windows",bg="white")
xlim.val=c(0,1);by.x=0.2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
par(family="serif",mar=c(1,0.75,1,1),oma=c(3,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))
for(i in 1:8){
  plot(score~TP.wt,rslt,type="n",axes=F,ann=F,ylim=xlim.val,xlim=xlim.val)
  abline(h=xmaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
  with(subset(rslt,Alt==alts.sort[i]),lines(score~TP.wt,col=cols[i],lwd=2))
  with(subset(rslt,Alt==alts.sort[i]),lines(score~TN.wt,col=cols[i],lwd=2,lty=2))
  if(i%in%c(1:4)){axis_fun(1,xmaj,xmin,NA,line=-0.5)}else{axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)}
  if(i%in%c(1,5)){axis_fun(2,xmaj,xmin,format(xmaj))}else{axis_fun(2,xmaj,xmin,NA)}
  box(lwd=1)
  mtext(side=3,adj=0,alts.sort[i])
  if(i==4){mtext(side=3,adj=1,"SLE (S80) - FWM")}
  if(i==1){
    legend("topleft",legend=c("TP Weights","TN Weights"),
           lty=c(1,2),lwd=c(2),col="grey",
           ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.)
  }
}
mtext(side=2,outer=T,"MCDA Score",line=2)
mtext(side=1,outer=T,"Variable Weights",line=1.5)
dev.off()

wt1=seq(0,1,0.2)
wt2=1-wt1
vars=paste(c("TP","TN"),"load",sep=".")
rslt=data.frame()
for(i in 1:length(wt1)){
  wts=c(wt1[i],wt2[i])
  score=apply(sle.nut.mod.sum2[,paste0(vars,".RS")],1,FUN=function(x) Hmisc::wtd.mean(x,wts,na.rm=T))
  score=round(score/max(score,na.rm=T),2)
  tmp=data.frame(Alt=sle.nut.mod.sum2$Alt,score=score)
  tmp$i.val=i
  tmp$TP.wt=wt1[i]
  tmp$TN.wt=wt2[i]
  rslt=rbind(tmp,rslt)
  
}

# png(filename=paste0(plot.path,"Iteration_2/S80_Load_MCDASense.png"),width=7.5,height=4.5,units="in",res=200,type="windows",bg="white")
xlim.val=c(0,1);by.x=0.2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
par(family="serif",mar=c(1,0.75,1,1),oma=c(3,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))
for(i in 1:8){
  plot(score~TP.wt,rslt,type="n",axes=F,ann=F,ylim=xlim.val,xlim=xlim.val)
  abline(h=xmaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
  with(subset(rslt,Alt==alts.sort[i]),lines(score~TP.wt,col=cols[i],lwd=2))
  with(subset(rslt,Alt==alts.sort[i]),lines(score~TN.wt,col=cols[i],lwd=2,lty=2))
  if(i%in%c(1:4)){axis_fun(1,xmaj,xmin,NA,line=-0.5)}else{axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)}
  if(i%in%c(1,5)){axis_fun(2,xmaj,xmin,format(xmaj))}else{axis_fun(2,xmaj,xmin,NA)}
  box(lwd=1)
  mtext(side=3,adj=0,alts.sort[i])
  if(i==4){mtext(side=3,adj=1,"SLE (S80) - Load")}
  if(i==1){
    legend("topleft",legend=c("TP Weights","TN Weights"),
           lty=c(1,2),lwd=c(2),col="grey",
           ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.)
  }
}
mtext(side=2,outer=T,"MCDA Score",line=2)
mtext(side=1,outer=T,"Variable Weights",line=1.5)
dev.off()
