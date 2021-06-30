## 
## LOSOM
##
## Iteration 2 alternative evaluation
## Climate perspective
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

# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
# alts=alts[!(alts%in%c("SPEF","SPAS","SPLC"))];# removed the Everglades Foundation, Audubon and Lake Commnuities alteratives
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))


# Climate - AMO -----------------------------------------------------------
# AMO data https://psl.noaa.gov/data/timeseries/AMO/
vars=c('year',month.abb)
row.count=length(seq(1856,2021,1))
noaa.amo.path="https://psl.noaa.gov/data/correlation/amon.us.long.data"

# AMO.dat=read.table("https://psl.noaa.gov/data/correlation/amon.us.long.data",header=F,skip=1,col.names=vars,nrows=row.count,na.string="-99.990")
AMO.dat=read.table("https://psl.noaa.gov/data/correlation/amon.sm.long.data",header=F,skip=1,col.names=vars,nrows=row.count,na.string="-99.990")
AMO.dat.melt=melt(AMO.dat,id.vars="year")
AMO.dat.melt=merge(AMO.dat.melt,data.frame(variable=month.abb,month=1:12))
AMO.dat.melt$Date.mon=with(AMO.dat.melt,date.fun(paste(year,month,"01",sep="-")))
AMO.dat.melt=AMO.dat.melt[order(AMO.dat.melt$Date.mon),c("Date.mon","value")]
AMO.dat.melt$warm=with(AMO.dat.melt,ifelse(value>0,value,0))
AMO.dat.melt$dry=with(AMO.dat.melt,ifelse(value<0,value,0))
AMO.dat.melt$ma=with(AMO.dat.melt,c(rep(NA,120),zoo::rollapply(value,width=121,FUN=function(x)mean(x,na.rm=T))))
head(AMO.dat.melt)
tail(AMO.dat.melt)

# png(filename=paste0(plot.path,"Kaplan_AMO.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(1,2,1,0.25));

ylim.val=c(-0.3,0.2);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1850-01-01","2020-12-01"));xmaj=seq(xlim.val[1],xlim.val[2],"20 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ymaj[4]=0

plot(value~Date.mon,AMO.dat.melt,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F)
abline(v=xmaj,h=ymaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(AMO.dat.melt,is.na(value)==F),shaded.range(Date.mon,rep(0,length(Date.mon)),ifelse(value>0,value,0),"indianred1",lty=1))
with(subset(AMO.dat.melt,is.na(value)==F),shaded.range(Date.mon,ifelse(value<0,value,0),rep(0,length(Date.mon)),"dodgerblue1",lty=1))
abline(h=0)
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
abline(v=date.fun(c("1965-01-01","2016-12-31")))
text(date.fun(date.fun("1965-01-01")+diff(date.fun(c("1965-01-01","2016-12-31")))/2),ylim.val[2],"RSM P.O.S.",font=2,cex=0.75)

mtext(side=2,line=2.5,"Observed AMO Index")
mtext(side=1,line=2,"Date (Year)")
mtext(side=1,outer=T,adj=1,"Kaplan SST dataset\nN Atlantic temp (0 to 70N)",cex=0.75)
dev.off()

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
lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,"%Y"))
lakeO.stage$AMO_period=with(lakeO.stage,ifelse(CY%in%c(1965:1994),"cool_dry","warm_wet"))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))

lakeO.stage$H_GT17=lakeO.stage$vHigh.stg
lakeO.stage$H_16_17=with(lakeO.stage,ifelse(STAGE>=16&STAGE<17,1,0))
lakeO.stage$H_LT16=with(lakeO.stage,ifelse(STAGE<16,1,0))
lakeO.stage$H_16_10=with(lakeO.stage,ifelse(STAGE>=10&STAGE<16,1,0))
lakeO.stage$H_LT10=lakeO.stage$vlow.stg
ddply(lakeO.stage,c("Alt"),summarise,
      N.val=N.obs(Alt),
      N.H_GT17=sum(H_GT17,na.rm=T),
      N.H_16_17=sum(H_16_17,na.rm=T),
      N.H_16_10=sum(H_16_10,na.rm=T),
      N.H_LT10=sum(H_LT10,na.rm=T))
Lake.Stg.per.sum=ddply(lakeO.stage,c("AMO_period","Alt"),summarise,
      N.val=N.obs(Alt),
      per.H_GT17=sum(H_GT17,na.rm=T)/18993,
      per.H_16_17=sum(H_16_17,na.rm=T)/18993,
      per.H_16_10=sum(H_16_10,na.rm=T)/18993,
      per.H_LT10=sum(H_LT10,na.rm=T)/18993)
Lake.Stg.per.sum[,4:7]=Lake.Stg.per.sum[,4:7]*100
Lake.Stg.per.sum
Lake.Stg.per.sum$Alt=factor(Lake.Stg.per.sum$Alt,levels=alts.sort)

# png(filename=paste0(plot.path,"Iteration_2/LakeH_AMO_cat.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);
layout(matrix(c(1:5,rep(5,3)),4,2,byrow=F),widths=c(1,0.25))

ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=reshape2::dcast(Lake.Stg.per.sum,Alt~AMO_period,value.var="per.H_GT17",sum)
x=barplot(t(tmp[,2:3]),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp[,2]),format(round(t(tmp[,2]),1),nsmall=1),cex=0.7,pos=3)
text(x[2,],t(tmp[,3]),format(round(t(tmp[,3]),1),nsmall=1),cex=0.7,pos=3)#col="white")
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,NA)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,"\u2265 17 Ft NGVD")

ylim.val=c(0,10);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=reshape2::dcast(Lake.Stg.per.sum,Alt~AMO_period,value.var="per.H_16_17",sum)
x=barplot(t(tmp[,2:3]),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp[,2]),format(round(t(tmp[,2]),1),nsmall=1),cex=0.7,pos=1)
text(x[2,],t(tmp[,3]),format(round(t(tmp[,3]),1),nsmall=1),cex=0.7,pos=1,col="white")
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,NA)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,"\u2265 16 & < 17 Ft NGVD")

ylim.val=c(0,60);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=reshape2::dcast(Lake.Stg.per.sum,Alt~AMO_period,value.var="per.H_16_10",sum)
x=barplot(t(tmp[,2:3]),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp[,2]),format(round(t(tmp[,2]),1),nsmall=1),cex=0.7,pos=1)
text(x[2,],t(tmp[,3]),format(round(t(tmp[,3]),1),nsmall=1),cex=0.7,pos=1,col="white")
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,NA)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,"\u2265 10 & < 16 Ft NGVD")

ylim.val=c(0,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=reshape2::dcast(Lake.Stg.per.sum,Alt~AMO_period,value.var="per.H_LT10",sum)
x=barplot(t(tmp[,2:3]),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp[,2]),format(round(t(tmp[,2]),1),nsmall=1),cex=0.7,pos=1)
text(x[2,],t(tmp[,3]),format(round(t(tmp[,3]),1),nsmall=1),cex=0.7,pos=1,col="white")
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,alts.sort)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,"< 10 Ft NGVD")
mtext(side=2,outer=T,line=1,"% of Time")
mtext(side=1,line=2.5,"Alternative")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0.5,legend=c("Dry Phase\n(1965 - 1994)","Wet Phase\n(1995 - 2016)"),
       pch=22,pt.cex=2,lty=0,pt.bg=c("khaki","dodgerblue1"),
       bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       ncol=1,title="AMO Period",title.adj = 0)
text(1,0,"Lake Okeechobee\nDaily Stage\n \nPeriod of Simulation\n CY1965 - 2016.",adj=1,cex=0.75,xpd=NA)
dev.off()

days.stage.POS=ddply(lakeO.stage,c("Alt"),summarise,
                     N.val=N.obs(STAGE),
                     vlow.N=sum(vlow.stg),vlow.per=vlow.N/N.val*100,
                     vhigh.N=sum(vHigh.stg),vhigh.per=vhigh.N/N.val*100)
# png(filename=paste0(plot.path,"Iteration_2/LOperDays.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(1,2,1,0.25));
layout(matrix(1:2,1,2),widths=c(1,0.25))
ylim.val=c(0,10);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,6);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(vlow.per~vhigh.per,days.stage.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col="grey")
with(days.stage.POS,points(vhigh.per,vlow.per,pch=21,bg=adjustcolor("grey",0.5),lwd=0.1))
with(days.stage.POS,text(vhigh.per,vlow.per,Alt,pos=3,cex=0.75,col="black",font=2))

axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.25,"% of time \u2265 17 ft NGVD29")
mtext(side=2,line=2,"% of time \u2264 10 ft NGVD29")

plot(0:1,0:1,type="n",ann=F,axes=F)
dev.off()


days.stage.POS=ddply(lakeO.stage,c("AMO_period","Alt"),summarise,
                     N.val=N.obs(STAGE),
                     vlow.N=sum(vlow.stg),vlow.per=vlow.N/N.val*100,
                     vhigh.N=sum(vHigh.stg),vhigh.per=vhigh.N/N.val*100)
# png(filename=paste0(plot.path,"Iteration_2/LO_AMO_perDays.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(1,2,1,0.25));
layout(matrix(1:2,1,2),widths=c(1,0.25))
ylim.val=c(0,10);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,6);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(vlow.per~vhigh.per,days.stage.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col="grey")
with(subset(days.stage.POS,AMO_period=="cool_dry"),points(vhigh.per,vlow.per,
                                                          pch=21,bg=adjustcolor("indianred1",0.5),lwd=0.1))
with(subset(days.stage.POS,AMO_period=="cool_dry"),text(vhigh.per,vlow.per,Alt,pos=3,cex=0.75,col="indianred1",font=2))
with(subset(days.stage.POS,AMO_period=="warm_wet"),points(vhigh.per,vlow.per,
                                                          pch=21,bg=adjustcolor("dodgerblue1",0.5),lwd=0.1))
with(subset(days.stage.POS,AMO_period=="warm_wet"),text(vhigh.per,vlow.per,Alt,pos=3,cex=0.75,col="dodgerblue1",font=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.25,"% of time \u2265 17 ft NGVD29")
mtext(side=2,line=2,"% of time \u2264 10 ft NGVD29")

plot(0:1,0:1,type="n",ann=F,axes=F)
legend(0.5,0.5,legend=c("Dry Phase\n(1965 - 1994)","Wet Phase\n(1995 - 2016)"),
       pch=21,pt.cex=2,lty=0,pt.bg=adjustcolor(c("indianred1","dodgerblue1"),0.5),
       bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,cex=0.8,
       ncol=1,title="AMO Period",title.adj = 0)
dev.off

# png(filename=paste0(plot.path,"Iteration_2/LO_StageDuration_AMO.png"),width=8.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,1,8,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(8,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 1:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(lakeO.stage,AMO_period=="cool_dry"&Alt==alts.sort[i])$STAGE),lines(1-proportion,value,col=cols[i],lty=1,lwd=1.5))
  with(ecdf_fun(subset(lakeO.stage,AMO_period=="warm_wet"&Alt==alts.sort[i])$STAGE),lines(1-proportion,value,col=cols[i],lty=2,lwd=1.5))
  axis_fun(1,xmaj,xmin,format(xmaj))
  mtext(side=3, adj=0,alts.sort[i])
  if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  if(i==1){
    legend("bottomleft",legend=c("Cool/Dry","Warm/Wet"),
         lty=c(1,2),lwd=c(1.5,1.5),col="grey",
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)}
  if(i==1){mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")}
}
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/LO_StageDuration_Diff_AMO.png"),width=8.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,1,8,byrow=T))

xlim.val=c(8,18);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(-0.1,0.2);by.y=0.05;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 1:n.alts){
  x.val=ecdf_fun(subset(lakeO.stage,AMO_period=="cool_dry"&Alt==alts.sort[i])$STAGE)
  x.val$value=round(x.val$value,4)
  y.val=ecdf_fun(subset(lakeO.stage,AMO_period=="warm_wet"&Alt==alts.sort[i])$STAGE)
  y.val$value=round(y.val$value,4)
  tmp=merge(x.val,y.val,"value")
  tmp$diff.val=with(tmp,proportion.x-proportion.y)
  
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  with(tmp,lines(diff.val~value,col=cols[1],lwd=2))
  axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
  if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  if(i==1){mtext(side=2,line=3,"Difference in SDC")}
  mtext(side=3, adj=0,alts.sort[i])
}
# mtext(side=2,line=1.75,outer=T,"Difference in SDC")
mtext(side=1,line=1,outer=T,"Stage Elevation (Ft, NGVD29)")
dev.off()


# RECOVER Stage -----------------------------------------------------------
stg.env=lakeO.stage

AprSep=seq(date.fun("1965-04-15"),date.fun("1965-09-15"),"1 days")
MayAug=seq(date.fun("1965-05-01"),date.fun("1965-09-01"),"1 days")

# stg.env$STAGE=round(stg.env$STAGE,2)
stg.env$CY=as.numeric(format(stg.env$Date,"%Y"))
stg.env$month_day=as.character(format(stg.env$Date,"%m_%d"))
stg.env$AugDec.stg=with(stg.env,ifelse(as.numeric(format(Date,"%m"))%in%c(8:12),STAGE,NA))
stg.env$JuneJuly.stg=with(stg.env,ifelse(as.numeric(format(Date,"%m"))%in%c(6,7),STAGE,NA))
stg.env$AprSep.stg=with(stg.env,ifelse(month_day%in%as.character(format(AprSep,"%m_%d")),STAGE,NA))
stg.env$MayAug.stg=with(stg.env,ifelse(month_day%in%as.character(format(MayAug,"%m_%d")),STAGE,NA))

stg.env.CY=ddply(stg.env,c("Alt","CY"),summarise,
                 max.stg=max(STAGE,na.rm=T),
                 AugDec.max=max(AugDec.stg,na.rm=T),
                 JuneJuly_13=sum(JuneJuly.stg>13,na.rm=T),
                 MayAug_11.5=sum(MayAug.stg<11.5,na.rm=T),
                 AprSep_12=sum(AprSep.stg<12,na.rm=T))
stg.env.CY$env=1
env.rslt=data.frame()
for(i in 1:length(alts.sort)){
  tmp=subset(stg.env.CY,Alt==alts.sort[i])
  for(j in 2:nrow(tmp)){
    tmp$env[j]=with(tmp,
                    ifelse(max.stg[j-1]>17|JuneJuly_13[j-1]>=30,2,
                           ifelse(AugDec.max[j-1]<=16&(MayAug_11.5[j-1]>=60|AprSep_12[j-1]>=90),1,tmp$env[j-1])))
    
  }
  env.rslt=rbind(env.rslt,tmp)
}
env.rslt$AMO_period=with(env.rslt,ifelse(CY%in%seq(1965,1994,1),"dry_cold","wet_warm"))

env.rslt.amo=reshape2::dcast(env.rslt,Alt+env~AMO_period,value.var="env",fun.aggregate = function(x)N.obs(x))
subset(env.rslt.amo,Alt=="AA")

# png(filename=paste0(plot.path,"Iteration_2/LakeO_Env_AMO.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,50);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(3,3.75,1,0.25));
layout(matrix(1:9,3,3,byrow=T))
for(i in 1:8){
  tmp=t(t((subset(env.rslt.amo,Alt==alts.sort[i])[,3:4]/52)*100))
x=barplot(tmp,beside=T,ann=F,axes=F,ylim=ylim.val,names.arg = rep(NA,2))
text(x[,1],tmp[,1],round(tmp[,1],1),pos=1,col=c("white","black"),font=2)
text(x[,2],tmp[,2],round(tmp[,2],1),pos=1,col=c("white","black"),font=2)
if(i%in%c(7,8,6)){axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,c("Cool/Dry","Warm/Wet"))}else{axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,NA)}
if(i%in%c(1,4,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
mtext(side=3,adj=0,alts.sort[i])
}
plot(0:1,0:1,type="n",ann=F,axes=F)
legend(0.5,0.5,legend=c("Normal","Recovery"),
       pch=22,pt.cex=2,lty=0,pt.bg=c("black","grey"),
       bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,cex=1,
       ncol=1,title="Stage Envelope",title.adj = 0)
mtext(side=2,line=2,outer=T,"Percent of Time")
mtext(side=1,line=1.5,outer=T,"AMO Phases")
dev.off()
# Discharge ---------------------------------------------------------------
RSM.sites="S79"# c("S77","S78","S79","S80","S308","S351","S352","S354")
q.dat=data.frame()

i=1
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  #for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    q.dat=rbind(tmp,q.dat)
    print(i)
  #}
}
q.dat$CY=as.numeric(format(q.dat$Date,"%Y"))
q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+CY~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
q.dat.xtab$AMO_period=with(q.dat.xtab,ifelse(CY%in%seq(1965,1994,1),"dry_cold","wet_warm"))
q.dat.xtab$S79.AF=cfs.to.acftd(q.dat.xtab$S79)
q.dat.xtab$Alt=factor(q.dat.xtab$Alt,levels=alts.sort)

q.dat.xtab$QLT457=with(q.dat.xtab,ifelse(S79<457,1,0))
q.dat.xtab$Q457_750=with(q.dat.xtab,ifelse(S79>=457&S79<750,1,0))
q.dat.xtab$Q_Opt=with(q.dat.xtab,ifelse(S79>=750&S79<2100,1,0))
q.dat.xtab$Q_Stress=with(q.dat.xtab,ifelse(S79>=2100&S79<2600,1,0))
q.dat.xtab$Q2600_4500=with(q.dat.xtab,ifelse(S79>=2600&S79<4500,1,0))
q.dat.xtab$Q4500_6500=with(q.dat.xtab,ifelse(S79>=4500&S79<6500,1,0))
q.dat.xtab$QGT6500=with(q.dat.xtab,ifelse(S79>6500,1,0))
q.dat.xtab$month=as.numeric(format(q.dat.xtab$Date,"%m"))

amo.mean=reshape2::dcast(q.dat.xtab,Alt~AMO_period,value.var = "S79.AF",mean)

# png(filename=paste0(plot.path,"Iteration_2/S79Q_AMO_TS.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,50000);by.y=25000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1965-01-01","2016-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

par(family="serif",mar=c(2,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);
layout(matrix(c(1:8),8,1,byrow=F))

for(i in 1:8){
  plot(S79.AF~Date,q.dat.xtab,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n",xaxs="i",yaxs="i")
  abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
  with(subset(q.dat.xtab,Alt==alts.sort[i]),shaded.range(Date,rep(1,length(Date)),S79.AF,bg="black",col="grey",lty=1))
  # with(subset(q.dat1.xtab,Alt==alts.sort[i]),lines(Date,S79,col=cols[i]))
  lines(date.fun(c("1965-01-01","1994-12-31")),rep(subset(amo.mean,Alt==alts.sort[i])$dry_cold,2),col="indianred1",lwd=2)
  lines(date.fun(c("1995-01-01","2016-12-31")),rep(subset(amo.mean,Alt==alts.sort[i])$wet_warm,2),col="dodgerblue1",lwd=2)
  axis_fun(2,ymaj,ymin,ymaj/1000)
  if(i==8){axis_fun(1,xmaj,xmin,format(xmaj,"%Y"));}else{axis_fun(1,xmaj,xmin,NA);}
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"S-79")}
  mtext(side=3,adj=1,line=-1.25,paste0(alts.sort[i]," "),cex=0.75)
}
mtext(side=2,outer=T,line=1,"Discharge (x1000 Ac-Ft d\u207B\u00B9)")
mtext(side=1,line=2,"Date (Year)")
dev.off()

ylim.val=c(0,15000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration_2/S79Q_AMO_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);
layout(matrix(c(1:8),2,4,byrow=T))

for(i in 1:8){
  boxplot(S79.AF~AMO_period,subset(q.dat.xtab,Alt==alts.sort[i]),col=adjustcolor(c("indianred1","dodgerblue1"),0.5),outline=F,ann=F,axes=F,ylim=ylim.val)
  axis_fun(1,1:2,1:2,c("Dry", "Wet"))
  axis_fun(2,ymaj,ymin,ymaj/1000)
  box(lwd=1)
  mtext(side=3,adj=0,line=-1,paste(" ",alts.sort[i]),cex=0.75,font=2)
  if(i==1){mtext(side=3,adj=0,"S-79")}
}
mtext(side=1,outer=T,"AMO Phase")
mtext(side=2,outer=T,line=0.5,"Daily Discharge (x1000 Ac-Ft d\u207B\u00B9)")
dev.off()

CRE.QCat.POS.sum=ddply(q.dat.xtab,c("Alt","AMO_period"),summarise,
                       N.total=N.obs(Alt),
                       N.LT457=sum(QLT457,na.rm=T),
                       N.Q457_750=sum(Q457_750,na.rm=T),
                       N.Q_Opt=sum(Q_Opt,na.rm=T),
                       N.Q_Stress=sum(Q_Stress,na.rm=T),
                       N.Q2600_4500=sum(Q2600_4500,na.rm=T),
                       N.Q4500_6500=sum(Q4500_6500,na.rm = T),
                       N.QGT6500=sum(QGT6500,na.rm=T))
# Within each phase
# CRE.QCat.POS.per.sum=ddply(q.dat.xtab,c("Alt","AMO_period"),summarise,
#                        N.total=N.obs(Alt),
#                        per.LT457=sum(QLT457,na.rm=T)/N.total,
#                        per.Q457_750=sum(Q457_750,na.rm=T)/N.total,
#                        per.Q_Opt=sum(Q_Opt,na.rm=T)/N.total,
#                        per.Q_Stress=sum(Q_Stress,na.rm=T)/N.total,
#                        per.Q2600_4500=sum(Q2600_4500,na.rm=T)/N.total,
#                        per.Q4500_6500=sum(Q4500_6500,na.rm = T)/N.total,
#                        per.QGT6500=sum(QGT6500,na.rm=T)/N.total)
CRE.QCat.POS.per.sum=ddply(q.dat.xtab,c("Alt","AMO_period"),summarise,
                           N.total=N.obs(Alt),
                           per.LT457=sum(QLT457,na.rm=T)/18993,
                           per.Q457_750=sum(Q457_750,na.rm=T)/18993,
                           per.Q_Opt=sum(Q_Opt,na.rm=T)/18993,
                           per.Q_Stress=sum(Q_Stress,na.rm=T)/18993,
                           per.Q2600_4500=sum(Q2600_4500,na.rm=T)/18993,
                           per.Q4500_6500=sum(Q4500_6500,na.rm = T)/18993,
                           per.QGT6500=sum(QGT6500,na.rm=T)/18993)

CRE.QCat.POS.per.sum[,4:10]=round(CRE.QCat.POS.per.sum[,4:10]*100,1)
CRE.QCat.POS.per.sum
# png(filename=paste0(plot.path,"Iteration_2/S79Q_AMO_cat_total.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);
layout(matrix(c(1:4,4,4),3,2,byrow=F),widths=c(1,0.25))

ylim.val=c(0,5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=reshape2::dcast(CRE.QCat.POS.per.sum,Alt~AMO_period,value.var="per.Q_Stress",sum)
x=barplot(t(tmp[,2:3]),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp[,2]),format(t(tmp[,2]),nsmall=1),cex=0.7,pos=1)
text(x[2,],t(tmp[,3]),format(t(tmp[,3]),nsmall=1),cex=0.7,col="white",pos=1)
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,NA)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,"2100 - 2600 cfs")

ylim.val=c(0,10);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=reshape2::dcast(CRE.QCat.POS.per.sum,Alt~AMO_period,value.var="per.Q2600_4500",sum)
x=barplot(t(tmp[,2:3]),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp[,2]),format(t(tmp[,2]),nsmall=1),cex=0.7,pos=1)
text(x[2,],t(tmp[,3]),format(t(tmp[,3]),nsmall=1),cex=0.7,col="white",pos=1)
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,NA)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,"2600 - 4500 cfs")

ylim.val=c(0,5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=reshape2::dcast(CRE.QCat.POS.per.sum,Alt~AMO_period,value.var="per.QGT6500",sum)
x=barplot(t(tmp[,2:3]),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp[,2]),format(t(tmp[,2]),nsmall=1),cex=0.7,pos=1)
text(x[2,],t(tmp[,3]),format(t(tmp[,3]),nsmall=1),cex=0.7,col="white",pos=1)
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,alts.sort)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,">6500 cfs")
mtext(side=2,outer=T,line=1,"% of Time")
mtext(side=1,line=2.5,"Alternative")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0.5,legend=c("Dry Phase\n(1965 - 1994)","Wet Phase\n(1995 - 2016)"),
       pch=22,pt.cex=2,lty=0,pt.bg=c("khaki","dodgerblue1"),
       bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       ncol=1,title="AMO Period",title.adj = 0)
text(1,0,"S79 Daily Discharge\n \nPeriod of Simulation\n CY1965 - 2016.",adj=1,cex=0.5,xpd=NA)

dev.off()
