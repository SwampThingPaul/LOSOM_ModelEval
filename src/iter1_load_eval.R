## 
## LOSOM - WQ evaluation
##
## Iteration 1 alternative evaluation
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
alts=list.files(paste0(data.path,"Iteration_1/Model_Output/"))
# alts=alts[!(alts%in%c("SPEF","SPAS","SPLC"))];# removed the Everglades Foundation, Audubon and Lake Commnuities alteratives
n.alts=length(alts)
alts.sort=c("LSM25B", "LSMECB","ABNE", "ECRE", "ELOK","ESFL", "ESLE", "WAS","WRDC","WRDS","NAV","REC","SPAS","SPEF","SPLC")

# Lake Stage --------------------------------------------------------------
lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_1/Model_Output/",alts[i],"/RSMBN/RSMBN_output.dss"))  
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
  dss_out=opendss(paste0(data.path,"Iteration_1/Model_Output/",alts[j],"/RSMBN/RSMBN_output.dss"))  
  
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

RSM.q.dat.xtab=cast(q.dat,Alt+Date~SITE,value="FLOW",mean)
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

cre.nut.mod=RSM.hydro[,c("Alt","WY","Q.S77","Q.S79","Q.C43","mean.stg")]
cre.nut.mod$TPLoad.kg.fit=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,1])
cre.nut.mod$TPLoad.kg.95LCI=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,2])
cre.nut.mod$TPLoad.kg.95UCI=(predict(CRE.mod.TP.all,cre.nut.mod,interval="confidence")[,3])
# ggplot(cre.nut.mod, aes(x = WY, y = TPLoad.kg.fit))+geom_point(colour="indianred1") + geom_line() + facet_wrap(~ Alt)

cre.nut.mod$TNLoad.kg.fit=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,1])
cre.nut.mod$TNLoad.kg.95LCI=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,2])
cre.nut.mod$TNLoad.kg.95UCI=(predict(CRE.mod.TN.all,cre.nut.mod,interval="confidence")[,3])
# ggplot(cre.nut.mod, aes(x = WY, y = TNLoad.kg.fit)) +geom_point(colour="indianred1")+ geom_line()++ facet_wrap(~ Alt)

#write.csv(cre.nut.mod,paste0(export.path,"RSMBN_iter1_load_CRE.csv"),row.names=F)
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

# write.csv(sle.nut.mod,paste0(export.path,"RSMBN_iter1_load_SLE.csv"),row.names=F)
sle.nut.mod$S80.TNFWM=with(sle.nut.mod,ifelse(Q.S80<80,NA,(TNLoad.kg.fit/(Q.S80*1.233e6))*1e6))
sle.nut.mod$S80.TPFWM=with(sle.nut.mod,ifelse(Q.S80<80,NA,(TPLoad.kg.fit/(Q.S80*1.233e6))*1e9))
# ggplot(sle.nut.mod, aes(x = WY, y = S80.TPFWM)) +geom_point(colour="indianred1")+ geom_line() + facet_wrap(~ Alt)
# ggplot(sle.nut.mod, aes(x = WY, y = S80.TNFWM)) +geom_point(colour="indianred1") +geom_line() + facet_wrap(~ Alt)
sle.nut.mod$Alt=factor(sle.nut.mod$Alt,levels=alts.sort)
# subset(sle.nut.mod,Q.S80<200&is.na(S80.TPFWM)==F)
# subset(sle.nut.mod,TPLoad.kg.fit<5&is.na(S80.TPFWM)==F)
# -------------------------------------------------------------------------
# alts.sort=c("LSM25B", "LSMECB","ABNE", "ECRE", "ELOK","ESFL", "ESLE", "WAS","WRDC","WRDS","NAV","REC","SPAS","SPEF","SPLC")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

# Load duration curves
# png(filename=paste0(plot.path,"Iteration_1/S79_TPLoadDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:16,4,4,byrow=T))

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
  if(i%in%c(3:11)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"CRE (S-79)")}
  if(i%in%c(3,7,11,15)){axis_fun(2,ymaj,ymin,ymaj/10e3)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2,outer=T,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Load")
dev.off()

# png(filename=paste0(plot.path,"Iteration_1/S79_TNLoadDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:16,4,4,byrow=T))

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
  if(i%in%c(3:11)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"CRE (S-79)")}
  if(i%in%c(3,7,11,15)){axis_fun(2,ymaj,ymin,ymaj/10e4)}else{axis_fun(2,ymaj,ymin,NA)}
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
# png(filename=paste0(plot.path,"Iteration_1/S79_TPLoad_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,550000);by.y=250000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TPLoad.kg.fit~Alt,cre.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(cre.nut.mod,Alt==alts.sort[1])$TPLoad.kg.fit),lty=2)
abline(h=subset(cre.nut.mod.sum,Alt==alts.sort[1])$mean.TP.load,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:15,1:15,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
mtext(side=3,adj=0,"CRE (S-79)")
dev.off()

###
# obs.dat=data.frame(WY=1991:2020,S79.TPFWM=c(194.172942475321, 174.627760344702, 249.686473330221, 144.979319706607, 
#   93.6865705865878, 77.6342149200262, 113.323629146915, 85.3890149735639, 
#   139.332785552204, 165.676457610166, 162.95173681696, 209.993140195582, 
#   158.634036625188, 104.019301608094, 110.907638976708, 125.021126563642, 
#   182.212020773742, 233.384689852762, 229.005998451357, 151.468667148604, 
#   118.869186137397, 150.55523249488, 127.3288944946, 116.608555081983, 
#   113.649896145235, 113.195471033476, 116.461396384246, 184.658695289428, 
#   159.005231193847, 145.794670077943),Alt="Obs")
# 
# test=rbind(obs.dat,cre.nut.mod[,c("WY","S79.TPFWM","Alt")])
# 
# alts.sort2=c("Obs",alts.sort)
# cols2=c("white",cols)
# 
# test$Alt=factor(test$Alt,levels=alts.sort2)
# 
# ylim.val=c(0,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# x=boxplot(S79.TPFWM~Alt,test,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols2)
# # points(1:n.alts,cre.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
# abline(h=median(subset(test,Alt==alts.sort[2])$S79.TPFWM),lty=2)
# # abline(h=subset(cre.nut.mod.sum,Alt==alts.sort[1])$mean.TP.load,lty=2,col="springgreen")
# axis_fun(2,ymaj,ymin,ymaj)
# axis_fun(1,1:16,1:16,alts.sort2,las=2)
# abline(v=3.5)
# box(lwd=1)
# mtext(side=2,line=2.5,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
# mtext(side=1,line=3.5,"Model Alternative")
# mtext(side=3,adj=0,"CRE (S-79)")
# dev.off()

## TMDL "limits"
test=ddply(cre.nut.mod,"Alt",summarise,N.exceed=sum(ifelse(TNLoad.kg.fit>(9086094*0.453592),1,0)),N.total=N.obs(TNLoad.kg.fit),percent=N.exceed/N.total*100)
plot(1:15,test$percent)
abline(h=12)

# png(filename=paste0(plot.path,"Iteration_1/S79_TNLoad_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,70*10e4);by.y=20*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TNLoad.kg.fit~Alt,cre.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.nut.mod.sum$mean.TN,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(cre.nut.mod,Alt==alts.sort[1])$TNLoad.kg.fit),lty=2)
abline(h=subset(cre.nut.mod.sum,Alt==alts.sort[1])$mean.TN.load,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e4)
axis_fun(1,1:15,1:15,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
mtext(side=3,adj=0,"CRE (S-79)")
dev.off()

# png(filename=paste0(plot.path,"Iteration_1/S79_FWM_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(100,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(S79.TPFWM~Alt,cre.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.nut.mod.sum$mean.TP.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(cre.nut.mod.sum,Alt==alts.sort[1])$mean.TP.FWM,lty=2,col="springgreen")
abline(h=median(subset(cre.nut.mod,Alt==alts.sort[1])$S79.TPFWM),lty=2,col="black")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=2,line=2.5,"TP FWM (\u03BCg L\u207B\u00B9)")

ylim.val=c(1.3,1.9);by.y=0.3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
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



# png(filename=paste0(plot.path,"Iteration_1/S79_Load_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-20,20);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(cre.nut.mod.sum$TP.FWO.diff,1:15,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(cre.nut.mod.sum,segments(TP.FWO.diff,1:15,rep(0,15),1:15))
with(cre.nut.mod.sum,points(TP.FWO.diff,1:15,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:15,1:15,cre.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP Load")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')

xlim.val=c(-20,20);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(cre.nut.mod.sum$TN.FWO.diff,1:15,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(cre.nut.mod.sum,segments(TN.FWO.diff,1:15,rep(0,15),1:15))
with(cre.nut.mod.sum,points(TN.FWO.diff,1:15,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:15,1:15,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN Load")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
dev.off()

# png(filename=paste0(plot.path,"Iteration_1/S79_FWM_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-10,10);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(cre.nut.mod.sum$TP.FWO.diff,1:15,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(cre.nut.mod.sum,segments(TP.FWM.FWO.diff,1:15,rep(0,15),1:15))
with(cre.nut.mod.sum,points(TP.FWM.FWO.diff,1:15,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:15,1:15,cre.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP FWM")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')

#xlim.val=c(-5,5);by.x=2.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(cre.nut.mod.sum$TN.FWO.diff,1:15,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(cre.nut.mod.sum,segments(TN.FWM.FWO.diff,1:15,rep(0,15),1:15))
with(cre.nut.mod.sum,points(TN.FWM.FWO.diff,1:15,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:15,1:15,NA)
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN FWM")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
dev.off()


# SLE ---------------------------------------------------------------------

# Load duration curves
# png(filename=paste0(plot.path,"Iteration_1/S80_TPLoadDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:16,4,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,700000);by.y=250000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

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
  if(i%in%c(3:11)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"SLE (S-80)")}
  if(i%in%c(3,7,11,15)){axis_fun(2,ymaj,ymin,ymaj/10e3)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2,outer=T,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Load")
dev.off()

# png(filename=paste0(plot.path,"Iteration_1/S80_TNLoadDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:16,4,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,10*10e5);by.y=25*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

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
  if(i%in%c(3:11)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"SLE (S-80)")}
  if(i%in%c(3,7,11,15)){axis_fun(2,ymaj,ymin,ymaj/10e4)}else{axis_fun(2,ymaj,ymin,NA)}
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
# png(filename=paste0(plot.path,"Iteration_1/S80_TPLoad_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,400000);by.y=200000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TPLoad.kg.fit~Alt,sle.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(sle.nut.mod,Alt==alts.sort[1])$TPLoad.kg.fit),lty=2)
abline(h=subset(sle.nut.mod.sum,Alt==alts.sort[1])$mean.TP.load,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:15,1:15,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
mtext(side=3,adj=0,"SLE (S-80)")
dev.off()

# png(filename=paste0(plot.path,"Iteration_1/S80_TNLoad_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,30*10e4);by.y=10*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TNLoad.kg.fit~Alt,sle.nut.mod,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.nut.mod.sum$mean.TN.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(sle.nut.mod,Alt==alts.sort[1])$TNLoad.kg.fit),lty=2)
abline(h=subset(sle.nut.mod.sum,Alt==alts.sort[1])$mean.TN.load,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e4)
axis_fun(1,1:15,1:15,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
mtext(side=3,adj=0,"SLE (S-80)")
dev.off()

# png(filename=paste0(plot.path,"Iteration_1/S80_FWM_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(50,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
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



# png(filename=paste0(plot.path,"Iteration_1/S80_Load_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-100,100);by.x=50;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(sle.nut.mod.sum$TP.FWO.diff,1:15,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(sle.nut.mod.sum,segments(TP.FWO.diff,1:15,rep(0,15),1:15))
with(sle.nut.mod.sum,points(TP.FWO.diff,1:15,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:15,1:15,sle.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP Load")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')

xlim.val=c(-100,100);by.x=50;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(sle.nut.mod.sum$TN.FWO.diff,1:15,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(sle.nut.mod.sum,segments(TN.FWO.diff,1:15,rep(0,15),1:15))
with(sle.nut.mod.sum,points(TN.FWO.diff,1:15,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:15,1:15,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN Load")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
dev.off()

# png(filename=paste0(plot.path,"Iteration_1/S80_FWM_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-25,25);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(sle.nut.mod.sum$TP.FWO.diff,1:15,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(sle.nut.mod.sum,segments(TP.FWM.FWO.diff,1:15,rep(0,15),1:15))
with(sle.nut.mod.sum,points(TP.FWM.FWO.diff,1:15,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:15,1:15,sle.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP FWM")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')

#xlim.val=c(-5,5);by.x=2.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(sle.nut.mod.sum$TN.FWO.diff,1:15,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(sle.nut.mod.sum,segments(TN.FWM.FWO.diff,1:15,rep(0,15),1:15))
with(sle.nut.mod.sum,points(TN.FWM.FWO.diff,1:15,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:15,1:15,NA)
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN FWM")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
dev.off()
