## 
## LOSOM
##
## Iteration 2 S77/S308 Loads
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


# Simulated WQ ------------------------------------------------------------
dates=as.Date(c("2000-05-01","2020-04-30"))

params=data.frame(Test.Number=c(18,21,80,20,25,23,61,179,7,16),param=c("NOx","TKN","TN","NH4","TP","SRP","Chla","Chla","Temp","TSS"))
params=subset(params,param%in%c("TP","TN","NOx","TKN","NH4","SRP"))
wq.sites=c("S77","S308C")
wq.dat=DBHYDRO_WQ(dates[1],dates[2],wq.sites,params$Test.Number)
wq.dat=merge(wq.dat,params,"Test.Number")
unique(wq.dat$Collection.Method)
wq.dat=subset(wq.dat,Collection.Method=="G")

# plot(HalfMDL~Date.EST,subset(wq.dat,Station.ID=="S79"& param=="TP"))
# plot(HalfMDL~Date.EST,subset(wq.dat,Station.ID=="S77"& param=="TP"))

wq.dat.xtab=dcast(wq.dat,Station.ID+Date.EST~param,value.var="HalfMDL",mean)
wq.dat.xtab$DIN=with(wq.dat.xtab,NH4+NOx)
wq.dat.xtab$TN=with(wq.dat.xtab, TN_Combine(NOx,TKN,TN))
wq.dat.xtab$TON=with(wq.dat.xtab,TN-DIN)
wq.dat.xtab$WY=WY(wq.dat.xtab$Date.EST)
wq.dat.xtab$month=as.numeric(format(wq.dat.xtab$Date.EST,"%m"))
wq.dat.xtab$CY=as.numeric(format(wq.dat.xtab$Date.EST,"%Y"))
wq.dat.xtab$monCY=with(wq.dat.xtab,date.fun(paste(CY,month,"01",sep="-")))

# Reversal Evaluation
wq.dat.xtab$TPReversal=with(wq.dat.xtab,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
wq.dat.xtab$TNReversal=with(wq.dat.xtab,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));
subset(wq.dat.xtab,TN<0.05)
sum(wq.dat.xtab$TNReversal,na.rm=T)
sum(wq.dat.xtab$TPReversal,na.rm=T)

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,wq.dat.xtab,ylab="TN (mg L\u207B\u00b9)",xlab="DIN (mg L\u207B\u00b9)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP~SRP,wq.dat.xtab,ylab="TP (mg L\u207B\u00b9)",xlab="SRP (mg L\u207B\u00b9)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

dev.off()


# Trend Analysis -----------------------------------------------------------

wq.dat.xtab.sea=ddply(wq.dat.xtab,c("Station.ID","month","CY"),summarise,mean.TP=mean(TP,na.rm=T),mean.TN=mean(TN,na.rm=T))
wq.dat.xtab.sea$Date.MonCY=with(wq.dat.xtab.sea,date.fun(paste(CY,month,"01",sep="-")))

library(EnvStats)
S77.TP.trend=kendallSeasonalTrendTest(mean.TP~month+CY,data=subset(wq.dat.xtab.sea,Station.ID=="S77"))
print(S77.TP.trend)
S77.TN.trend=kendallSeasonalTrendTest(mean.TN~month+CY,data=subset(wq.dat.xtab.sea,Station.ID=="S77"))
print(S77.TN.trend)
plot(mean.TN~Date.MonCY,subset(wq.dat.xtab.sea,Station.ID=="S77"))

S308.TP.trend=kendallSeasonalTrendTest(mean.TP~month+CY,data=subset(wq.dat.xtab.sea,Station.ID=="S308C"))
print(S308.TP.trend)
S308.TN.trend=kendallSeasonalTrendTest(mean.TN~month+CY,data=subset(wq.dat.xtab.sea,Station.ID=="S308C"))
print(S308.TN.trend)


# POS WQ data --------------------------------------------------------------
month.mean=ddply(wq.dat.xtab,c("Station.ID","month"),summarise,
                 mean.TP=mean(TP,na.rm=T),SD.TP=sd(TP,na.rm=T),N.TP=N.obs(TP),
                 mean.TN=mean(TN,na.rm=T),SD.TN=sd(TN,na.rm=T),N.TN=N.obs(TN))
# write.csv(month.mean,paste0(export.path,"S77S308_monthlymean.csv"),row.names = F)
S77.WQ.sim=data.frame(Date.EST=seq(date.fun("1965-01-15"),date.fun("2020-12-31"),"1 month"))
S77.WQ.sim$WY=WY(S77.WQ.sim$Date.EST)
S77.WQ.sim$month=as.numeric(format(S77.WQ.sim$Date.EST,'%m'))
S77.WQ.sim=merge(S77.WQ.sim,subset(month.mean,Station.ID=="S77"),"month")
S77.WQ.sim=S77.WQ.sim[order(S77.WQ.sim$Date.EST),]
head(S77.WQ.sim)

#test
set.seed(123)
exp(rnorm(1,3,1))
set.seed(123)
rlnorm(1,3,1)

set.seed(1)
m=2
s=1
mean(rnorm(100,m,s))
set.seed(1)
mean(log(rlnorm(100,m,s)))
# https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/
loc=log(m^2/sqrt(s^2+m^2))
shp=sqrt(log(1+(s^2/m^2)))
set.seed(1)
rlnorm(100,loc,shp)

## 

S77.WQ.sim$sim.TP=NA
S77.WQ.sim$sim.TN=NA
set.seed(123)
for(i in 1:nrow(S77.WQ.sim)){
  S77.WQ.sim$sim.TP[i]=rnorm(1,S77.WQ.sim$mean.TP[i],S77.WQ.sim$SD.TP[i])
}
S77.WQ.sim$sim.TP=with(S77.WQ.sim,ifelse(sim.TP<0,mean.TP,sim.TP))

set.seed(123)
for(i in 1:nrow(S77.WQ.sim)){
  S77.WQ.sim$sim.TN[i]=rnorm(1,S77.WQ.sim$mean.TN[i],S77.WQ.sim$SD.TN[i])
}
S77.WQ.sim$sim.TN=with(S77.WQ.sim,ifelse(sim.TN<0,mean.TN,sim.TN))

plot(sim.TN~Date.EST,S77.WQ.sim)
plot(sim.TP~Date.EST,S77.WQ.sim)

mean(S77.WQ.sim$sim.TP)
sd(S77.WQ.sim$sim.TP)
mean(subset(wq.dat.xtab,Station.ID=="S77")$TP,na.rm=T)
sd(subset(wq.dat.xtab,Station.ID=="S77")$TP,na.rm=T)
mean(S77.WQ.sim$sim.TN)
sd(S77.WQ.sim$sim.TN)
mean(subset(wq.dat.xtab,Station.ID=="S77")$TN,na.rm=T)
sd(subset(wq.dat.xtab,Station.ID=="S77")$TN,na.rm=T)

S308.WQ.sim=data.frame(Date.EST=seq(date.fun("1965-01-15"),date.fun("2020-12-31"),"1 month"))
S308.WQ.sim$WY=WY(S308.WQ.sim$Date.EST)
S308.WQ.sim$month=as.numeric(format(S308.WQ.sim$Date.EST,'%m'))
S308.WQ.sim=merge(S308.WQ.sim,subset(month.mean,Station.ID=="S308C"),"month")
S308.WQ.sim=S308.WQ.sim[order(S308.WQ.sim$Date.EST),]
head(S308.WQ.sim)

S308.WQ.sim$sim.TP=NA
S308.WQ.sim$sim.TN=NA
set.seed(123)
for(i in 1:nrow(S308.WQ.sim)){
  S308.WQ.sim$sim.TP[i]=rnorm(1,S308.WQ.sim$mean.TP[i],S308.WQ.sim$SD.TP[i])
}
S308.WQ.sim$sim.TP=with(S308.WQ.sim,ifelse(sim.TP<0,mean.TP,sim.TP))

set.seed(123)
for(i in 1:nrow(S308.WQ.sim)){
  S308.WQ.sim$sim.TN[i]=rnorm(1,S308.WQ.sim$mean.TN[i],S308.WQ.sim$SD.TN[i])
}
S308.WQ.sim$sim.TN=with(S308.WQ.sim,ifelse(sim.TN<0,mean.TN,sim.TN))

plot(sim.TN~Date.EST,S308.WQ.sim)
plot(sim.TP~Date.EST,S308.WQ.sim)

mean(S308.WQ.sim$sim.TP)
sd(S308.WQ.sim$sim.TP)
mean(subset(wq.dat.xtab,Station.ID=="S308C")$TP,na.rm=T)
sd(subset(wq.dat.xtab,Station.ID=="S308C")$TP,na.rm=T)
mean(S308.WQ.sim$sim.TN)
sd(S308.WQ.sim$sim.TN)
mean(subset(wq.dat.xtab,Station.ID=="S308C")$TN,na.rm=T)
sd(subset(wq.dat.xtab,Station.ID=="S308C")$TN,na.rm=T)


# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
alts=alts[!alts%in%c("_Batch_Results","Northern_Estuaries")]
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S308","S77_QFC","S308_QFC","S308BF")
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

head(q.dat,20)
q.dat=q.dat[order(q.dat$Alt,q.dat$SITE,q.dat$Date),]
q.dat$WY=WY(q.dat$Date)
q.dat$CY=as.numeric(format(q.dat$Date,"%Y"))

q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+WY~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))


# S77 ---------------------------------------------------------------------
vars=c("Alt","Date",'WY',"S77","S77_QFC")
S77.q.dat.xtab=q.dat.xtab[,vars]
S77.q.dat.xtab$preReg=with(S77.q.dat.xtab,ifelse(S77==0,0,round((S77_QFC/S77)*100,2)))
range(S77.q.dat.xtab$preReg,na.rm=T)

head(S77.WQ.sim)
vars=c('Date.EST',"sim.TP","sim.TN")
S77.q.dat.xtab=merge(S77.q.dat.xtab,S77.WQ.sim[,vars],by.x="Date",by.y="Date.EST",all.x=T)
S77.q.dat.xtab=S77.q.dat.xtab[order(S77.q.dat.xtab$Alt,S77.q.dat.xtab$Date),]

S77.q.dat.xtab$sim.TP.inter=with(S77.q.dat.xtab,ave(sim.TP,Alt,FUN=function(x) dat.interp(x)))
S77.q.dat.xtab$sim.TN.inter=with(S77.q.dat.xtab,ave(sim.TN,Alt,FUN=function(x) dat.interp(x)))
# plot(sim.TP~Date,subset(S77.q.dat.xtab,Alt=="AA"))
# with(subset(S77.q.dat.xtab,Alt=="AA"),lines(Date,sim.TP.inter))
S77.q.dat.xtab$TP.load=with(S77.q.dat.xtab,Load.Calc.kg(S77,sim.TP.inter))
S77.q.dat.xtab$TN.load=with(S77.q.dat.xtab,Load.Calc.kg(S77,sim.TN.inter))

## CQ
S77.q.dat.xtab$log_S77=log(S77.q.dat.xtab$S77)
S77.q.dat.xtab$log_sim.TP=log(S77.q.dat.xtab$sim.TP)
S77.q.dat.xtab$log_sim.TN=log(S77.q.dat.xtab$sim.TN)
library(smatr)
ann.beta=ddply(subset(S77.q.dat.xtab,S77>0),c("Alt","WY"),summarise,
               TP.beta=as.numeric(coef(sma(log_sim.TP~log_S77))[2]),
               TN.beta=as.numeric(coef(sma(log_sim.TN~log_S77))[2]))

par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))
for(i in 1:n.alts){
  plot(TP.beta~WY,subset(ann.beta,Alt==alts.sort[i]),ylim=c(-3,3))
  abline(h=0)
  mtext(side=3,line=-1.25,alts.sort[i])
}
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))
for(i in 1:n.alts){
  plot(TN.beta~WY,subset(ann.beta,Alt==alts.sort[i]),ylim=c(-3,3))
  abline(h=0)
  mtext(side=3,line=-1.25,alts.sort[i])
}

## 
S77.Load.WY=ddply(subset(S77.q.dat.xtab,WY%in%WYs),c("Alt","WY"),summarise,
      TFlow=sum(cfs.to.acftd(S77),na.rm=T),
      TPLoad=sum(TP.load,na.rm=T),
      TNLoad=sum(TN.load,na.rm=T))
S77.Load.WY$S77.TNFWM=with(S77.Load.WY,(TNLoad/(TFlow*1.233e6))*1e6)
S77.Load.WY$S77.TPFWM=with(S77.Load.WY,(TPLoad/(TFlow*1.233e6))*1e9)
S77.Load.WY$Alt=factor(S77.Load.WY$Alt,levels=alts.sort)

boxplot(TPLoad~Alt,S77.Load.WY)
boxplot(TNLoad~Alt,S77.Load.WY)

boxplot(S77.TNFWM~Alt,S77.Load.WY)
boxplot(S77.TPFWM~Alt,S77.Load.WY)


S77.nut.mod.sum=ddply(S77.Load.WY,"Alt",summarise,
                      mean.Q=mean(TFlow/1000,na.rm=T),
                      mean.TP.load=mean(TPLoad),mean.TN.load=mean(TNLoad),
                      mean.TP.FWM=mean(S77.TPFWM),mean.TN.FWM=mean(S77.TNFWM))
# write.csv(S77.nut.mod.sum,paste0(export.path,"S77_load.csv"),row.names = F)
S77.nut.mod.sum$Alt=factor(S77.nut.mod.sum$Alt,levels=alts.sort)

S77.nut.mod.sum$TP.FWO.diff=with(S77.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
S77.nut.mod.sum$TN.FWO.diff=with(S77.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100

S77.nut.mod.sum$TP.FWM.FWO.diff=with(S77.nut.mod.sum,(mean.TP.FWM-mean.TP.FWM[1])/mean.TP.FWM[1])*100
S77.nut.mod.sum$TN.FWM.FWO.diff=with(S77.nut.mod.sum,(mean.TN.FWM-mean.TN.FWM[1])/mean.TN.FWM[1])*100

# png(filename=paste0(plot.path,"Iteration_2/S77_TPLoad_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,25e4);by.y=10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TPLoad~Alt,S77.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S77.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S77.Load.WY,Alt==alts.sort[1])$TPLoad),lty=2)
abline(h=mean(subset(S77.Load.WY,Alt==alts.sort[1])$TPLoad),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:8,1:8,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
mtext(side=3,adj=0,"S-77")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S77_TNLoad_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,40*10e4);by.y=20*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TNLoad~Alt,S77.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S77.nut.mod.sum$mean.TN.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S77.Load.WY,Alt==alts.sort[1])$TNLoad),lty=2)
abline(h=mean(subset(S77.Load.WY,Alt==alts.sort[1])$TNLoad),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e4)
axis_fun(1,1:8,1:8,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
mtext(side=3,adj=0,"S-77")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S77_Load_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,25e4);by.y=10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TPLoad~Alt,S77.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S77.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S77.Load.WY,Alt==alts.sort[1])$TPLoad),lty=2)
abline(h=mean(subset(S77.Load.WY,Alt==alts.sort[1])$TPLoad),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:8,1:8,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=3,adj=0,"S-77")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,40*10e4);by.y=20*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TNLoad~Alt,S77.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S77.nut.mod.sum$mean.TN.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S77.Load.WY,Alt==alts.sort[1])$TNLoad),lty=2)
abline(h=mean(subset(S77.Load.WY,Alt==alts.sort[1])$TNLoad),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e4)
axis_fun(1,1:8,1:8,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S77_FWM_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(50,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(S77.TPFWM~Alt,S77.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S77.nut.mod.sum$mean.TP.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S77.Load.WY,Alt==alts.sort[1])$S77.TPFWM),lty=2)
abline(h=mean(subset(S77.Load.WY,Alt==alts.sort[1])$S77.TPFWM),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"S-77")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"TP FWM (\u03BCg L\u207B\u00B9)")

ylim.val=c(1.3,1.9);by.y=0.3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(S77.TNFWM~Alt,S77.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S77.nut.mod.sum$mean.TN.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S77.Load.WY,Alt==alts.sort[1])$S77.TNFWM),lty=2)
abline(h=mean(subset(S77.Load.WY,Alt==alts.sort[1])$S77.TNFWM),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN FWM (mg L\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S77_Load_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1.75,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-40,32);by.x=20;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S77.nut.mod.sum$TP.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(S77.nut.mod.sum,segments(TP.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(S77.nut.mod.sum,points(TP.FWO.diff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(S77.nut.mod.sum,text(TP.FWO.diff,1:n.alts,format(round(TP.FWO.diff,1)),pos=ifelse(TP.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,S77.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP Load",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"S-77",line=0.8)

#xlim.val=c(-40,35);by.x=20;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S77.nut.mod.sum$TN.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(S77.nut.mod.sum,segments(TN.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(S77.nut.mod.sum,points(TN.FWO.diff,1:n.alts,pch=21,bg="indianred1",lwd=0.1))
with(S77.nut.mod.sum,text(TN.FWO.diff,1:n.alts,format(round(TN.FWO.diff,1)),pos=ifelse(TN.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN Load",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=0.8)
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S77_FWM_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1.75,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-10,10);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S77.nut.mod.sum$TP.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(S77.nut.mod.sum,segments(TP.FWM.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(S77.nut.mod.sum,points(TP.FWM.FWO.diff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(S77.nut.mod.sum,text(TP.FWM.FWO.diff,1:n.alts,format(round(TP.FWM.FWO.diff,1)),pos=ifelse(TP.FWM.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,S77.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP FWM",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"S-77",line=0.8)

xlim.val=c(-10,10);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S77.nut.mod.sum$TN.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(S77.nut.mod.sum,segments(TN.FWM.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(S77.nut.mod.sum,points(TN.FWM.FWO.diff,1:n.alts,pch=21,bg="indianred1",lwd=0.1))
with(S77.nut.mod.sum,text(TN.FWM.FWO.diff,1:n.alts,format(round(TN.FWM.FWO.diff,1)),pos=ifelse(TN.FWM.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN FWM",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=0.8)
dev.off()


# S308 ---------------------------------------------------------------------
vars=c("Alt","Date",'WY',"S308","S308_QFC","S308BF")
S308.q.dat.xtab=q.dat.xtab[,vars]
S308.q.dat.xtab$preReg=with(S308.q.dat.xtab,ifelse(S308==0,0,round((S308_QFC/S308)*100,2)))
range(S308.q.dat.xtab$preReg,na.rm=T)

head(S308.WQ.sim)
vars=c('Date.EST',"sim.TP","sim.TN")
S308.q.dat.xtab=merge(S308.q.dat.xtab,S308.WQ.sim[,vars],by.x="Date",by.y="Date.EST",all.x=T)
S308.q.dat.xtab=S308.q.dat.xtab[order(S308.q.dat.xtab$Alt,S308.q.dat.xtab$Date),]

S308.q.dat.xtab$sim.TP.inter=with(S308.q.dat.xtab,ave(sim.TP,Alt,FUN=function(x) dat.interp(x)))
S308.q.dat.xtab$sim.TN.inter=with(S308.q.dat.xtab,ave(sim.TN,Alt,FUN=function(x) dat.interp(x)))
# plot(sim.TP~Date,subset(S308.q.dat.xtab,Alt=="AA"))
# with(subset(S308.q.dat.xtab,Alt=="AA"),lines(Date,sim.TP.inter))
S308.q.dat.xtab$TP.load=with(S308.q.dat.xtab,Load.Calc.kg(S308,sim.TP.inter))
S308.q.dat.xtab$TN.load=with(S308.q.dat.xtab,Load.Calc.kg(S308,sim.TN.inter))
S308.q.dat.xtab$TP.load.BF=with(S308.q.dat.xtab,Load.Calc.kg(S308BF,sim.TP.inter))
S308.q.dat.xtab$TN.load.BF=with(S308.q.dat.xtab,Load.Calc.kg(S308BF,sim.TN.inter))
head(S308.q.dat.xtab)

S308.Load.WY=ddply(subset(S308.q.dat.xtab,WY%in%WYs),c("Alt","WY"),summarise,
                  TFlow=sum(cfs.to.acftd(S308),na.rm=T),
                  TFlow.BF=sum(cfs.to.acftd(S308BF),na.rm=T),
                  TPLoad=sum(TP.load,na.rm=T),
                  TNLoad=sum(TN.load,na.rm=T),
                  TPLoad.BF=sum(TP.load.BF,na.rm=T),
                  TNLoad.BF=sum(TN.load.BF,na.rm=T))

S308.Load.WY$S308.TNFWM=with(S308.Load.WY,(TNLoad/(TFlow*1.233e6))*1e6)
S308.Load.WY$S308.TPFWM=with(S308.Load.WY,(TPLoad/(TFlow*1.233e6))*1e9)
S308.Load.WY$Alt=factor(S308.Load.WY$Alt,levels=alts.sort)

S308.WY.sum=ddply(S308.Load.WY,"Alt",summarise,
      mean.Q=mean(TFlow/1000),mean.BF.Q=mean(TFlow.BF/1000),
      mean.TPLoad=mean(TPLoad,na.rm=T),mean.TNLoad=mean(TNLoad,na.rm=T),
      mean.TPFWM=mean(S308.TPFWM,na.rm=T),mean.TNFWM=mean(S308.TNFWM,na.rm=T),
      mean.BF.TPLoad=mean(TPLoad.BF,na.rm=T),mean.BF.TNLoad=mean(TNLoad.BF,na.rm=T))
with(S308.WY.sum,mean.BF.TPLoad/mean.TPLoad)
with(S308.WY.sum,mean.BF.TNLoad/mean.TNLoad)
# write.csv(S308.WY.sum,paste0(export.path,"S308_load.csv"),row.names = F)

boxplot(TPLoad~Alt,S308.Load.WY)
boxplot(TNLoad~Alt,S308.Load.WY)

boxplot(S308.TNFWM~Alt,S308.Load.WY)
boxplot(S308.TPFWM~Alt,S308.Load.WY)


S308.nut.mod.sum=ddply(S308.Load.WY,"Alt",summarise,
                      mean.TP.load=mean(TPLoad),mean.TN.load=mean(TNLoad),
                      mean.TP.FWM=mean(S308.TPFWM,na.rm=T),mean.TN.FWM=mean(S308.TNFWM,na.rm=T))
S308.nut.mod.sum$Alt=factor(S308.nut.mod.sum$Alt,levels=alts.sort)

S308.nut.mod.sum$TP.FWO.diff=with(S308.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
S308.nut.mod.sum$TN.FWO.diff=with(S308.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100

S308.nut.mod.sum$TP.FWM.FWO.diff=with(S308.nut.mod.sum,(mean.TP.FWM-mean.TP.FWM[1])/mean.TP.FWM[1])*100
S308.nut.mod.sum$TN.FWM.FWO.diff=with(S308.nut.mod.sum,(mean.TN.FWM-mean.TN.FWM[1])/mean.TN.FWM[1])*100

# png(filename=paste0(plot.path,"Iteration_2/S308_Load_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,25e4);by.y=10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TPLoad~Alt,S308.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S308.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S308.Load.WY,Alt==alts.sort[1])$TPLoad),lty=2)
abline(h=mean(subset(S308.Load.WY,Alt==alts.sort[1])$TPLoad),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:8,1:8,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TP Load (x10\u00B3 kg WY\u207B\u00B9)")
mtext(side=3,adj=0,"S-308")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,20*10e4);by.y=10*10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TNLoad~Alt,S308.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S308.nut.mod.sum$mean.TN.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S308.Load.WY,Alt==alts.sort[1])$TNLoad),lty=2)
abline(h=mean(subset(S308.Load.WY,Alt==alts.sort[1])$TNLoad),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e4)
axis_fun(1,1:8,1:8,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN Load (x10\u2074 kg WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Model Alternative")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S308_FWM_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(50,350);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(S308.TPFWM~Alt,S308.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S308.nut.mod.sum$mean.TP.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S308.Load.WY,Alt==alts.sort[1])$S308.TPFWM),lty=2)
abline(h=mean(subset(S308.Load.WY,Alt==alts.sort[1])$S308.TPFWM),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"S-308")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"TP FWM (\u03BCg L\u207B\u00B9)")

ylim.val=c(1.0,3.0);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(S308.TNFWM~Alt,S308.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S308.nut.mod.sum$mean.TN.FWM,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S308.Load.WY,Alt==alts.sort[1])$S308.TNFWM),lty=2)
abline(h=mean(subset(S308.Load.WY,Alt==alts.sort[1])$S308.TNFWM),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"TN FWM (mg L\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S308_Load_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1.75,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-120,70);by.x=50;xmaj=seq(max(c(100,xlim.val[1])),xlim.val[2],by.x);xmin=seq(max(c(100,xlim.val[1])),xlim.val[2],by.x/2)
plot(S308.nut.mod.sum$TP.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(S308.nut.mod.sum,segments(TP.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(S308.nut.mod.sum,points(TP.FWO.diff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(S308.nut.mod.sum,text(TP.FWO.diff,1:n.alts,format(round(TP.FWO.diff,1)),pos=ifelse(TP.FWO.diff<0,2,4),cex=0.75,offset=0.25))
axis_fun(2,1:n.alts,1:n.alts,S308.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP Load",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"S-308",line=0.8)

#xlim.val=c(-40,35);by.x=20;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S308.nut.mod.sum$TN.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(S308.nut.mod.sum,segments(TN.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(S308.nut.mod.sum,points(TN.FWO.diff,1:n.alts,pch=21,bg="indianred1",lwd=0.1))
with(S308.nut.mod.sum,text(TN.FWO.diff,1:n.alts,format(round(TN.FWO.diff,1)),pos=ifelse(TN.FWO.diff<0,2,4),cex=0.75,offset=0.25))
axis_fun(2,1:n.alts,1:n.alts,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN Load",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=0.8)
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S308_FWM_PerDiff.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1.75,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-10,10);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S308.nut.mod.sum$TP.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
# abline(v=c(10),lty=2)
with(S308.nut.mod.sum,segments(TP.FWM.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(S308.nut.mod.sum,points(TP.FWM.FWO.diff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(S308.nut.mod.sum,text(TP.FWM.FWO.diff,1:n.alts,format(round(TP.FWM.FWO.diff,1)),pos=ifelse(TP.FWM.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,S308.nut.mod.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TP FWM",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"S-308",line=0.8)

xlim.val=c(-10,10);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S308.nut.mod.sum$TN.FWO.diff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(S308.nut.mod.sum,segments(TN.FWM.FWO.diff,1:n.alts,rep(0,n.alts),1:n.alts))
with(S308.nut.mod.sum,points(TN.FWM.FWO.diff,1:n.alts,pch=21,bg="indianred1",lwd=0.1))
with(S308.nut.mod.sum,text(TN.FWM.FWO.diff,1:n.alts,format(round(TN.FWM.FWO.diff,1)),pos=ifelse(TN.FWM.FWO.diff<0,2,4),cex=0.8))
axis_fun(2,1:n.alts,1:n.alts,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"TN FWM",font=2,cex=0.75)
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=0.8)
dev.off()


