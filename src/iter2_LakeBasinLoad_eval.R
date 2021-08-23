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


# Simulated WQ ------------------------------------------------------------
dates=as.Date(c("1999-05-01","2020-04-30"))

params=data.frame(Test.Number=c(18,21,80,20,25,23,61,179,7,16),param=c("NOx","TKN","TN","NH4","TP","SRP","Chla","Chla","Temp","TSS"))
params=subset(params,param%in%c("TP","TN","NOx","TKN","NH4","SRP"))
wq.sites=c("S77","S308C","S65E","S2","S3","S4")
wq.dat=DBHYDRO_WQ(dates[1],dates[2],wq.sites,params$Test.Number)
wq.dat=merge(wq.dat,params,"Test.Number")
unique(wq.dat$Collection.Method)
wq.dat=subset(wq.dat,Collection.Method=="G")

# plot(HalfMDL~Date.EST,subset(wq.dat,Station.ID=="S79"& param=="TP"))
# plot(HalfMDL~Date.EST,subset(wq.dat,Station.ID=="S77"& param=="TP"))
# plot(HalfMDL~Date.EST,subset(wq.dat,Station.ID=="S4"& param=="TP"))

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


##
WY.mean=ddply(wq.dat.xtab,c("Station.ID","WY"),summarise,
                 TP.GM=exp(mean(log(TP),na.rm=T)),mean.TP=mean(TP,na.rm=T),SD.TP=sd(TP,na.rm=T),N.TP=N.obs(TP),
                 TN.GM=exp(mean(log(TN),na.rm=T)),mean.TN=mean(TN,na.rm=T),SD.TN=sd(TN,na.rm=T),N.TN=N.obs(TN))
# png(filename=paste0(plot.path,"S77_S308_S65E_WQgrab.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

cols=c("dodgerblue1","indianred1","forestgreen")
ylim.val=c(0,0.30);by.y=0.10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2000,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(mean.TP~WY,WY.mean,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
for(i in 1:3){
  with(subset(WY.mean,Station.ID==wq.sites[i]),pt_line(WY,TP.GM,2,cols[i],2,21,cols[i]))
}
axis_fun(2,ymaj,ymin,ymaj*1000)
axis_fun(1,xmaj,xmin,NA);box(lwd=1)
mtext(side=2,line=2.5,"TP GM (\u03BCg L\u207B\u00B9)")
legend("topleft",legend=c("S77","S308","S65E"),
       lty=c(2),lwd=c(2),col=cols,
       pch=NA,pt.bg=cols,pt.cex = 1.25,
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white"
       )
legend("topleft",legend=c("S77","S308","S65E"),
       lty=NA,lwd=c(0.1),col="black",
       pch=21,pt.bg=cols,pt.cex = 1.25,
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="black"
)

ylim.val=c(0,2.5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(mean.TP~WY,WY.mean,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
for(i in 1:3){
  with(subset(WY.mean,Station.ID==wq.sites[i]),pt_line(WY,TN.GM,2,cols[i],2,21,cols[i]))
}
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
mtext(side=2,line=2.5,"TN GM (mg L\u207B\u00B9)")
mtext(side=1,line=2,"Florida Water Year")
dev.off()


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

set.seed(123)
mean.val<-0.1168
sd.val<-0.0826

sim.TP<-rnorm(1,
              mean=mean.val,
              sd=sd.val)
sim.TP

x <- seq(-4,4,length=100)*sd.val + mean.val
hx <- dnorm(x,mean.val,sd.val)
# png(filename=paste0(plot.path,"normdist_ex.png"),width=5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1.5,0.5,0.5),oma=c(2,1,1,1));
ylim.val=c(-0.1,5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(-0.300,0.500);by.x=0.100;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
par(family="serif",mar=c(2,1.5,0.5,0.5),oma=c(2,1,1,1));
plot(hx~x,xaxs="i",yaxs="i",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
lines(hx~x,lwd=2,col=adjustcolor("dodgerblue1",0.5))
segments(sim.TP,-1,sim.TP,dnorm(sim.TP,mean.val,sd.val),col="red",lwd=2)
segments(mean.val,-1,mean.val,dnorm(mean.val,mean.val,sd.val),lty=2,col=adjustcolor("dodgerblue1",0.5))
segments(mean.val-sd.val,-1,mean.val-sd.val,dnorm(mean.val-sd.val,mean.val,sd.val),lty=3,col=adjustcolor("dodgerblue1",0.5))
segments(mean.val+sd.val,-1,mean.val+sd.val,dnorm(mean.val+sd.val,mean.val,sd.val),lty=3,col=adjustcolor("dodgerblue1",0.5))
points(sim.TP,dnorm(sim.TP,mean.val,sd.val),pch=21,bg="red",cex=2)
axis_fun(1,round(xmaj,1),round(xmin,1),round(xmaj,1)*1000,line=-0.5)
mtext(side=1,line=2,"Simulated TP (\u03BCg L\u207B\u00B9)")
dev.off()
###

# S77
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

# S308
S308.WQ.sim=data.frame(Date.EST=seq(date.fun("1965-01-15"),date.fun("2016-12-31"),"1 month"))
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

# S2
S2.WQ.sim=data.frame(Date.EST=seq(date.fun("1965-01-15"),date.fun("2016-12-31"),"1 month"))
S2.WQ.sim$WY=WY(S2.WQ.sim$Date.EST)
S2.WQ.sim$month=as.numeric(format(S2.WQ.sim$Date.EST,'%m'))
S2.WQ.sim=merge(S2.WQ.sim,subset(month.mean,Station.ID=="S2"),"month")
S2.WQ.sim=S2.WQ.sim[order(S2.WQ.sim$Date.EST),]
head(S2.WQ.sim)

S2.WQ.sim$sim.TP=NA
S2.WQ.sim$sim.TN=NA
set.seed(123)
for(i in 1:nrow(S2.WQ.sim)){
  S2.WQ.sim$sim.TP[i]=rnorm(1,S2.WQ.sim$mean.TP[i],S2.WQ.sim$SD.TP[i])
}
S2.WQ.sim$sim.TP=with(S2.WQ.sim,ifelse(sim.TP<0,mean.TP,sim.TP))

set.seed(123)
for(i in 1:nrow(S2.WQ.sim)){
  S2.WQ.sim$sim.TN[i]=rnorm(1,S2.WQ.sim$mean.TN[i],S2.WQ.sim$SD.TN[i])
}
S2.WQ.sim$sim.TN=with(S2.WQ.sim,ifelse(sim.TN<0,mean.TN,sim.TN))

plot(sim.TN~Date.EST,S2.WQ.sim)
plot(sim.TP~Date.EST,S2.WQ.sim)

# S3
S3.WQ.sim=data.frame(Date.EST=seq(date.fun("1965-01-15"),date.fun("2016-12-31"),"1 month"))
S3.WQ.sim$WY=WY(S3.WQ.sim$Date.EST)
S3.WQ.sim$month=as.numeric(format(S3.WQ.sim$Date.EST,'%m'))
S3.WQ.sim=merge(S3.WQ.sim,subset(month.mean,Station.ID=="S3"),"month")
S3.WQ.sim=S3.WQ.sim[order(S3.WQ.sim$Date.EST),]
head(S3.WQ.sim)

S3.WQ.sim$sim.TP=NA
S3.WQ.sim$sim.TN=NA
set.seed(123)
for(i in 1:nrow(S3.WQ.sim)){
  S3.WQ.sim$sim.TP[i]=rnorm(1,S3.WQ.sim$mean.TP[i],S3.WQ.sim$SD.TP[i])
}
S3.WQ.sim$sim.TP=with(S3.WQ.sim,ifelse(sim.TP<0,mean.TP,sim.TP))

set.seed(123)
for(i in 1:nrow(S3.WQ.sim)){
  S3.WQ.sim$sim.TN[i]=rnorm(1,S3.WQ.sim$mean.TN[i],S3.WQ.sim$SD.TN[i])
}
S3.WQ.sim$sim.TN=with(S3.WQ.sim,ifelse(sim.TN<0,mean.TN,sim.TN))

plot(sim.TN~Date.EST,S3.WQ.sim)
plot(sim.TP~Date.EST,S3.WQ.sim)

# S4
S4.WQ.sim=data.frame(Date.EST=seq(date.fun("1965-01-15"),date.fun("2016-12-31"),"1 month"))
S4.WQ.sim$WY=WY(S4.WQ.sim$Date.EST)
S4.WQ.sim$month=as.numeric(format(S4.WQ.sim$Date.EST,'%m'))
S4.WQ.sim=merge(S4.WQ.sim,subset(month.mean,Station.ID=="S4"),"month")
S4.WQ.sim=S4.WQ.sim[order(S4.WQ.sim$Date.EST),]
head(S4.WQ.sim)

S4.WQ.sim$sim.TP=NA
S4.WQ.sim$sim.TN=NA
set.seed(123)
for(i in 1:nrow(S4.WQ.sim)){
  S4.WQ.sim$sim.TP[i]=rnorm(1,S4.WQ.sim$mean.TP[i],S4.WQ.sim$SD.TP[i])
}
S4.WQ.sim$sim.TP=with(S4.WQ.sim,ifelse(sim.TP<0,mean.TP,sim.TP))

set.seed(123)
for(i in 1:nrow(S4.WQ.sim)){
  S4.WQ.sim$sim.TN[i]=rnorm(1,S4.WQ.sim$mean.TN[i],S4.WQ.sim$SD.TN[i])
}
S4.WQ.sim$sim.TN=with(S4.WQ.sim,ifelse(sim.TN<0,mean.TN,sim.TN))

plot(sim.TN~Date.EST,S4.WQ.sim)
plot(sim.TP~Date.EST,S4.WQ.sim)
# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
alts=alts[!alts%in%c("_Batch_Results","Northern_Estuaries","Iteration2_STL_Flows_13Jul2021.xlsx")]
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2","SR3.5")
cols=c("grey50","grey80",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-3,"continuous")),"deeppink")

alts=c("NA25","ECBr","CC")
cols=cols[alts.sort%in%alts]
alts.sort=alts
n.alts=length(alts)
# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S77BF","S308","S77_QFC","S308_QFC","S308BF","S2","S3","S4BP")
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
vars=c("Alt","Date",'WY',"S77","S77_QFC","S77BF")
S77.q.dat.xtab=q.dat.xtab[,vars]
S77.q.dat.xtab$preReg=with(S77.q.dat.xtab,ifelse(S77==0,0,round((S77_QFC/S77)*100,2)))
range(S77.q.dat.xtab$preReg,na.rm=T)

head(S77.WQ.sim)
vars=c('Date.EST',"sim.TP","sim.TN")
S77.q.dat.xtab=merge(S77.q.dat.xtab,S77.WQ.sim[,vars],by.x="Date",by.y="Date.EST",all.x=T)
S77.q.dat.xtab=S77.q.dat.xtab[order(S77.q.dat.xtab$Alt,S77.q.dat.xtab$Date),]

S77.q.dat.xtab=S77.q.dat.xtab[order(S77.q.dat.xtab$Alt,S77.q.dat.xtab$Date),]
S77.q.dat.xtab$sim.TP.inter=with(S77.q.dat.xtab,ave(sim.TP,Alt,FUN=function(x) dat.interp(x)))
S77.q.dat.xtab$sim.TN.inter=with(S77.q.dat.xtab,ave(sim.TN,Alt,FUN=function(x) dat.interp(x)))
# plot(sim.TP~Date,subset(S77.q.dat.xtab,Alt=="AA"))
# with(subset(S77.q.dat.xtab,Alt=="AA"),lines(Date,sim.TP.inter))
S77.q.dat.xtab$TP.load=with(S77.q.dat.xtab,Load.Calc.kg(S77,sim.TP.inter))
S77.q.dat.xtab$TN.load=with(S77.q.dat.xtab,Load.Calc.kg(S77,sim.TN.inter))
S77.q.dat.xtab$S77BF.TP.load=with(S77.q.dat.xtab,Load.Calc.kg(S77BF,sim.TP.inter))
S77.q.dat.xtab$S77BF.TN.load=with(S77.q.dat.xtab,Load.Calc.kg(S77BF,sim.TN.inter))

## CQ
S77.q.dat.xtab$log_S77=log(S77.q.dat.xtab$S77)
S77.q.dat.xtab$log_sim.TP=log(S77.q.dat.xtab$sim.TP)
S77.q.dat.xtab$log_sim.TN=log(S77.q.dat.xtab$sim.TN)
library(smatr)
ann.beta=ddply(subset(S77.q.dat.xtab,S77>0),c("Alt","WY"),summarise,
               TP.beta=as.numeric(coef(sma(log_sim.TP~log_S77))[2]),
               TN.beta=as.numeric(coef(sma(log_sim.TN~log_S77))[2]))

par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:10,2,5,byrow=T))
for(i in 1:n.alts){
  plot(TP.beta~WY,subset(ann.beta,Alt==alts.sort[i]),ylim=c(-3,3))
  abline(h=0)
  mtext(side=3,line=-1.25,alts.sort[i])
}
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:10,2,5,byrow=T))
for(i in 1:n.alts){
  plot(TN.beta~WY,subset(ann.beta,Alt==alts.sort[i]),ylim=c(-3,3))
  abline(h=0)
  mtext(side=3,line=-1.25,alts.sort[i])
}

## 
S77.Load.WY=ddply(subset(S77.q.dat.xtab,WY%in%WYs),c("Alt","WY"),summarise,
      TFlow=sum(cfs.to.acftd(S77),na.rm=T),
      TPLoad=sum(TP.load,na.rm=T),
      TNLoad=sum(TN.load,na.rm=T),
      TFlow.BF=sum(cfs.to.acftd(S77BF),na.rm=T),
      TPLoad.S77BF=sum(S77BF.TP.load,na.rm=T),
      TNLoad.S77BF=sum(S77BF.TN.load,na.rm=T))
S77.Load.WY$S77.TNFWM=with(S77.Load.WY,(TNLoad/(TFlow*1.233e6))*1e6)
S77.Load.WY$S77.TPFWM=with(S77.Load.WY,(TPLoad/(TFlow*1.233e6))*1e9)
S77.Load.WY$Alt=factor(S77.Load.WY$Alt,levels=alts.sort)

plot(TFlow.BF~WY,S77.Load.WY)
boxplot(TPLoad~Alt,S77.Load.WY)
boxplot(TNLoad~Alt,S77.Load.WY)

boxplot(TPLoad.S77BF~Alt,S77.Load.WY,outline=F)
boxplot(TNLoad.S77BF~Alt,S77.Load.WY,outline=F)

boxplot(S77.TNFWM~Alt,S77.Load.WY)
boxplot(S77.TPFWM~Alt,S77.Load.WY)

S77.nut.mod.sum=ddply(S77.Load.WY,"Alt",summarise,
                      mean.Q=mean(TFlow/1000,na.rm=T),
                      mean.Q.BF=mean(TFlow.BF/1000,na.rm=T),
                      mean.TP.load=mean(TPLoad),mean.TN.load=mean(TNLoad),
                      mean.TP.FWM=mean(S77.TPFWM),mean.TN.FWM=mean(S77.TNFWM),
                      mean.TP.load.BF=mean(TPLoad.S77BF),mean.TN.load.BF=mean(TNLoad.S77BF))
# write.csv(S77.nut.mod.sum,paste0(export.path,"S77_load.csv"),row.names = F)
S77.nut.mod.sum$Alt=factor(S77.nut.mod.sum$Alt,levels=alts.sort)

S77.nut.mod.sum$Q.BF.FWO.diff=with(S77.nut.mod.sum,(mean.Q.BF-mean.Q.BF[1])/mean.Q.BF[1])*100
S77.nut.mod.sum$TP.FWO.diff=with(S77.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
S77.nut.mod.sum$TN.FWO.diff=with(S77.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100
S77.nut.mod.sum$TP.FWO.BF.diff=with(S77.nut.mod.sum,(mean.TP.load.BF-mean.TP.load.BF[1])/mean.TP.load.BF[1])*100
S77.nut.mod.sum$TN.FWO.BF.diff=with(S77.nut.mod.sum,(mean.TN.load.BF-mean.TN.load.BF[1])/mean.TN.load.BF[1])*100

# S77.nut.mod.sum$TP.FWM.FWO.diff=with(S77.nut.mod.sum,(mean.TP.FWM-mean.TP.FWM[1])/mean.TP.FWM[1])*100
# S77.nut.mod.sum$TN.FWM.FWO.diff=with(S77.nut.mod.sum,(mean.TN.FWM-mean.TN.FWM[1])/mean.TN.FWM[1])*100

# png(filename=paste0(plot.path,"Iteration_2/S77_TPLoad_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,25e4);by.y=10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(TPLoad~Alt,S77.Load.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S77.nut.mod.sum$mean.TP.load,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S77.Load.WY,Alt==alts.sort[1])$TPLoad),lty=2)
abline(h=mean(subset(S77.Load.WY,Alt==alts.sort[1])$TPLoad),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
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
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
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
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
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
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
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
                       mean.Q=mean(TFlow/1000),mean.Q.BF=mean(TFlow.BF/1000),
                      mean.TP.load=mean(TPLoad),mean.TN.load=mean(TNLoad),
                      mean.TP.FWM=mean(S308.TPFWM,na.rm=T),mean.TN.FWM=mean(S308.TNFWM,na.rm=T),
                      mean.TP.load.BF=mean(TPLoad.BF),mean.TN.load.BF=mean(TNLoad.BF))
S308.nut.mod.sum$Alt=factor(S308.nut.mod.sum$Alt,levels=alts.sort)

S308.nut.mod.sum$Q.BF.FWO.diff=with(S308.nut.mod.sum,(mean.Q.BF-mean.Q.BF[1])/mean.Q.BF[1])*100
S308.nut.mod.sum$TP.FWO.diff=with(S308.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
S308.nut.mod.sum$TN.FWO.diff=with(S308.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100
S308.nut.mod.sum$TP.FWO.BF.diff=with(S308.nut.mod.sum,(mean.TP.load.BF-mean.TP.load.BF[1])/mean.TP.load.BF[1])*100
S308.nut.mod.sum$TN.FWO.BF.diff=with(S308.nut.mod.sum,(mean.TN.load.BF-mean.TN.load.BF[1])/mean.TN.load.BF[1])*100

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
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
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
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
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

xlim.val=c(-120,70);by.x=50;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
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


# S2 ----------------------------------------------------------------------
vars=c("Alt","Date",'WY',"S2")
S2.q.dat.xtab=q.dat.xtab[,vars]

head(S2.WQ.sim)
vars=c('Date.EST',"sim.TP","sim.TN")
S2.q.dat.xtab=merge(S2.q.dat.xtab,S2.WQ.sim[,vars],by.x="Date",by.y="Date.EST",all.x=T)
S2.q.dat.xtab=S2.q.dat.xtab[order(S2.q.dat.xtab$Alt,S2.q.dat.xtab$Date),]

S2.q.dat.xtab=S2.q.dat.xtab[order(S2.q.dat.xtab$Alt,S2.q.dat.xtab$Date),]
S2.q.dat.xtab$sim.TP.inter=with(S2.q.dat.xtab,ave(sim.TP,Alt,FUN=function(x) dat.interp(x)))
S2.q.dat.xtab$sim.TN.inter=with(S2.q.dat.xtab,ave(sim.TN,Alt,FUN=function(x) dat.interp(x)))
S2.q.dat.xtab$TP.load=with(S2.q.dat.xtab,Load.Calc.kg(S2,sim.TP.inter))
S2.q.dat.xtab$TN.load=with(S2.q.dat.xtab,Load.Calc.kg(S2,sim.TN.inter))

S2.Load.WY=ddply(subset(S2.q.dat.xtab,WY%in%WYs),c("Alt","WY"),summarise,
                   TFlow=sum(cfs.to.acftd(S2),na.rm=T),
                   TPLoad=sum(TP.load,na.rm=T),
                   TNLoad=sum(TN.load,na.rm=T))

# S3 ----------------------------------------------------------------------
vars=c("Alt","Date",'WY',"S3")
S3.q.dat.xtab=q.dat.xtab[,vars]

head(S3.WQ.sim)
vars=c('Date.EST',"sim.TP","sim.TN")
S3.q.dat.xtab=merge(S3.q.dat.xtab,S3.WQ.sim[,vars],by.x="Date",by.y="Date.EST",all.x=T)
S3.q.dat.xtab=S3.q.dat.xtab[order(S3.q.dat.xtab$Alt,S3.q.dat.xtab$Date),]

S3.q.dat.xtab=S3.q.dat.xtab[order(S3.q.dat.xtab$Alt,S3.q.dat.xtab$Date),]
S3.q.dat.xtab$sim.TP.inter=with(S3.q.dat.xtab,ave(sim.TP,Alt,FUN=function(x) dat.interp(x)))
S3.q.dat.xtab$sim.TN.inter=with(S3.q.dat.xtab,ave(sim.TN,Alt,FUN=function(x) dat.interp(x)))
S3.q.dat.xtab$TP.load=with(S3.q.dat.xtab,Load.Calc.kg(S3,sim.TP.inter))
S3.q.dat.xtab$TN.load=with(S3.q.dat.xtab,Load.Calc.kg(S3,sim.TN.inter))

S3.Load.WY=ddply(subset(S3.q.dat.xtab,WY%in%WYs),c("Alt","WY"),summarise,
                 TFlow=sum(cfs.to.acftd(S3),na.rm=T),
                 TPLoad=sum(TP.load,na.rm=T),
                 TNLoad=sum(TN.load,na.rm=T))

# S4 ----------------------------------------------------------------------
vars=c("Alt","Date",'WY',"S4BP")
S4.q.dat.xtab=q.dat.xtab[,vars]

head(S4.WQ.sim)
vars=c('Date.EST',"sim.TP","sim.TN")
S4.q.dat.xtab=merge(S4.q.dat.xtab,S4.WQ.sim[,vars],by.x="Date",by.y="Date.EST",all.x=T)
S4.q.dat.xtab=S4.q.dat.xtab[order(S4.q.dat.xtab$Alt,S4.q.dat.xtab$Date),]

S4.q.dat.xtab=S4.q.dat.xtab[order(S4.q.dat.xtab$Alt,S4.q.dat.xtab$Date),]
S4.q.dat.xtab$sim.TP.inter=with(S4.q.dat.xtab,ave(sim.TP,Alt,FUN=function(x) dat.interp(x)))
S4.q.dat.xtab$sim.TN.inter=with(S4.q.dat.xtab,ave(sim.TN,Alt,FUN=function(x) dat.interp(x)))
S4.q.dat.xtab$TP.load=with(S4.q.dat.xtab,Load.Calc.kg(S4BP,sim.TP.inter))
S4.q.dat.xtab$TN.load=with(S4.q.dat.xtab,Load.Calc.kg(S4BP,sim.TN.inter))

S4.Load.WY=ddply(subset(S4.q.dat.xtab,WY%in%WYs),c("Alt","WY"),summarise,
                 TFlow=sum(cfs.to.acftd(S4BP),na.rm=T),
                 TPLoad=sum(TP.load,na.rm=T),
                 TNLoad=sum(TN.load,na.rm=T))

EAA.BF.Load.WY=ddply(rbind(S2.Load.WY,S3.Load.WY,S4.Load.WY),
                     c("Alt","WY"),summarise,
                     TFlow.all=sum(TFlow)/1000,
                     TPLoad.all=sum(TPLoad),
                     TNLoad.all=sum(TNLoad))
head(EAA.BF.Load.WY)
EAA.nut.mod.sum=ddply(EAA.BF.Load.WY,"Alt",summarise,
                      mean.flow=mean(TFlow.all),
                      mean.TP.load=mean(TPLoad.all),
                      mean.TN.load=mean(TNLoad.all))
EAA.nut.mod.sum$Alt=factor(EAA.nut.mod.sum$Alt,levels=alts.sort)
EAA.nut.mod.sum=EAA.nut.mod.sum[match(EAA.nut.mod.sum$Alt,alts.sort),]

EAA.nut.mod.sum$Q.BF.FWO.diff=with(EAA.nut.mod.sum,(mean.flow-mean.flow[1])/mean.flow[1])*100
EAA.nut.mod.sum$TP.FWO.diff=with(EAA.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
EAA.nut.mod.sum$TN.FWO.diff=with(EAA.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100
EAA.nut.mod.sum


### 
vars=c("Area","Alt","mean.Q.BF","mean.TP.load.BF", "mean.TN.load.BF","Q.BF.FWO.diff","TP.FWO.BF.diff", "TN.FWO.BF.diff")
S77.nut.mod.sum
S77.nut.mod.sum$Area="S77"
S77.nut.mod.sum=S77.nut.mod.sum[,vars]

S308.nut.mod.sum
S308.nut.mod.sum$Area="S308"
S308.nut.mod.sum=S308.nut.mod.sum[,vars]

vars=c("Area","Alt","mean.flow","mean.TP.load", "mean.TN.load","Q.BF.FWO.diff","TP.FWO.diff", "TN.FWO.diff")
EAA.nut.mod.sum
EAA.nut.mod.sum$Area="EAA"
EAA.nut.mod.sum=EAA.nut.mod.sum[,vars]

colnames(S77.nut.mod.sum)=vars
colnames(S308.nut.mod.sum)=vars
colnames(EAA.nut.mod.sum)=vars

all.nut.mod.sum=rbind(S77.nut.mod.sum,S308.nut.mod.sum,EAA.nut.mod.sum)
all.nut.mod.sum[all.nut.mod.sum$Alt=="NA25","Q.BF.FWO.diff"]=NA
all.nut.mod.sum[all.nut.mod.sum$Alt=="NA25","TP.FWO.diff"]=NA
all.nut.mod.sum[all.nut.mod.sum$Alt=="NA25","TN.FWO.diff"]=NA

# -------------------------------------------------------------------------
## Daily Water Budget Summary files provided on FTP site.
head.val=c("year","month","day","Rainfall", "ET", "HpmDelta", "C10ABK", 
           "LKTFPL", "MDS", "NELKSH_WS", "NLKSH_WS", "ISTOK_WS", "S77", 
           "S4_WS", "C10A", "C12A", "C12", "C10", "S352", "S351", "C4A", 
           "C3", "S354", "brighton_WS", "S308", "S65E", "fec_wm", "TOTAL_ISTOK", 
           "S77bf", "S4bp", "S3", "S2", "C12Abp", "C12bp", "C10bp", "C4Abp", 
           "S236", "p5wps", "S308BF", "tcnsq", "S154", "S135", "Residual", 
           "WBDelta", "WBError")
lok.wb=data.frame()
for(i in 1:length(alts.sort)){
  tmp=read.csv(paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/lok_WB.csv"),skip=1,col.names = head.val)
  tmp$Alt=alts[i]
  lok.wb=rbind(lok.wb,tmp)
  print(alts[i])
}
lok.wb$Date.EST=with(lok.wb,date.fun(paste(year,month,day,sep="-")))
lok.wb$WY=WY(lok.wb$Date.EST)

summary(lok.wb)

inflows=c("S65E", "fec_wm", "TOTAL_ISTOK", 
               "S77bf", "S4bp", "S3", "S2", "C12Abp", "C12bp", "C10bp", "C4Abp", 
               "S236", "p5wps", "S308BF", "tcnsq", "S154", "S135")
lok.wb$TInflow=rowSums(lok.wb[,inflows],na.rm=T)
lok.wb$TEAA=rowSums(lok.wb[,c("S4bp", "S3", "S2")],na.rm=T)

lok.wb.WY=ddply(subset(lok.wb,WY%in%WYs),c("Alt","WY"),summarise,
                Inflow=sum(TInflow,na.rm=T),
                EAA.BF=sum(TEAA),
                S77.BF=sum(S77bf),
                S308.BF=sum(S308BF))
lok.wb.WY$S77_per=with(lok.wb.WY,(S77.BF/Inflow)*100)
lok.wb.WY$S308_per=with(lok.wb.WY,(S308.BF/Inflow)*100)
lok.wb.WY$EAA_per=with(lok.wb.WY,(EAA.BF/Inflow)*100)

mean.LakeWB=ddply(lok.wb.WY,"Alt",summarise,
      mean.Inflow=mean(Inflow),
      mean.EAA.BF=mean(EAA.BF),
      mean.S308.BF=mean(S308.BF),
      mean.S77.BF=mean(S77.BF))
      # mean.S77_per=mean(S77_per),
      # mean.S308_per=mean(S308_per),
      # mean.EAA_per=mean(EAA_per))
      
mean.LakeWB$S77_per=with(mean.LakeWB,(mean.S77.BF/mean.Inflow)*100)
mean.LakeWB$S308_per=with(mean.LakeWB,(mean.S308.BF/mean.Inflow)*100)
mean.LakeWB$EAA_per=with(mean.LakeWB,(mean.EAA.BF/mean.Inflow)*100)
mean.LakeWB=mean.LakeWB[match(mean.LakeWB$Alt,alts.sort),]

sum.WB=rbind(data.frame(Area="S77",Alt=mean.LakeWB$Alt,PerWB=mean.LakeWB$S77_per),
data.frame(Area="S308",Alt=mean.LakeWB$Alt,PerWB=mean.LakeWB$S308_per),
data.frame(Area="EAA",Alt=mean.LakeWB$Alt,PerWB=mean.LakeWB$EAA_per))

sort.vals=c("S77_NA25", "S77_ECBr", "S77_CC", "S308_NA25", "S308_ECBr", 
            "S308_CC", "EAA_NA25", "EAA_ECBr", "EAA_CC")
all.nut.mod.sum=merge(all.nut.mod.sum,sum.WB,c("Area","Alt"),all.x=T)

vars=c("Area", "Alt", "PerWB", "mean.flow", "mean.TP.load", "mean.TN.load", 
  "Q.BF.FWO.diff", "TP.FWO.diff", "TN.FWO.diff")
all.nut.mod.sum=all.nut.mod.sum[,vars]
all.nut.mod.sum=all.nut.mod.sum[match(paste(all.nut.mod.sum$Area,all.nut.mod.sum$Alt,sep="_"),sort.vals),]

all.nut.mod.sum%>%
  flextable()%>%
  colformat_double(j=3,digits=1,big.mark="",suffix="%")%>%
  colformat_double(j=4,digits=1,big.mark="")%>%
  colformat_double(j=5:6,digits=0,big.mark="")%>%
  colformat_double(j=7:9,digits=1,big.mark="",na_str="---")%>%
  merge_v(j=1)%>%
  fix_border_issues()%>%
  valign(j=1,valign="top")%>%
  vline(j=c(2,6))%>%
  hline(i=c(3,6))%>%
  set_header_labels(
    "Area"="Area",
    "PerWB"="Percent Total Inflow\nWater Budget",
    "mean.flow"="Discharge\n(kAcf-Ft WY\u207B\u00B9)",
    "mean.TP.load"="TP Load (kg WY\u207B\u00B9)",
    "mean.TN.load"="TN Load (kg WY\u207B\u00B9)",
    "Q.BF.FWO.diff"="Discharge",
    "TP.FWO.diff"="TP Load",
    "TN.FWO.diff"="TN Load")%>%
  add_header("PerWB"="Average Annual",
             "mean.flow"="Average Annual",
             "mean.TP.load"="Average Annual",
             "mean.TN.load"="Average Annual",
             "Q.BF.FWO.diff"="% Change\nCompare to FWO",
             "TP.FWO.diff"="% Change\nCompare to FWO",
             "TN.FWO.diff"="% Change\nCompare to FWO")%>%
  merge_h(part="header")%>%
  align(j=3:7,align="center",part="header")%>%
  padding(padding=1.5,part="all")%>%
  align(j=3:9,align="center",part="all")%>%
  bg(i=~Q.BF.FWO.diff<0,j=7,bg="lightgreen")%>%bg(i=~Q.BF.FWO.diff>0,j=7,bg="tomato")%>%bg(i=~is.na(Q.BF.FWO.diff)==T,j=7,bg="lightgrey")%>%
  bg(i=~TP.FWO.diff<0,j=8,bg="lightgreen")%>%bg(i=~TP.FWO.diff>0,j=8,bg="tomato")%>%bg(i=~is.na(TP.FWO.diff)==T,j=8,bg="lightgrey")%>%
  bg(i=~TN.FWO.diff<0,j=9,bg="lightgreen")%>%bg(i=~TN.FWO.diff>0,j=9,bg="tomato")%>%bg(i=~is.na(TN.FWO.diff)==T,j=9,bg="lightgrey")%>%
  footnote(j=2:4,part="header",value=as_paragraph("Simulation period of record between Florida Water Year 1966 - 2016 (May 1965 - April 2016)"))%>%
  set_caption(caption="Average annual load and average percent change relative to FWO (NA25) over the simulation period or record between May 1965 and April 2016 for back flow/pumping from S77, S308 and EAA (S2, S3 and S4) to Lake Okeechobee.")%>%print("docx")
