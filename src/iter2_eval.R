## 
## LOSOM
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

# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
# alts=alts[!(alts%in%c("SPEF","SPAS","SPLC"))];# removed the Everglades Foundation, Audubon and Lake Commnuities alteratives
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

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

# # Sanity Check
# plot(STAGE~Date,lakeO.stage,type="n")
# with(subset(lakeO.stage,Alt==alts[1]),lines(Date,STAGE,col="red"))
# with(subset(lakeO.stage,Alt==alts[2]),lines(Date,STAGE,col="green"))
# with(subset(lakeO.stage,Alt==alts[3]),lines(Date,STAGE,col="blue"))
# 
# plot(value~proportion,ecdf_fun(subset(lakeO.stage,Alt=="LSM25B")$STAGE),type="n")
# with(ecdf_fun(subset(lakeO.stage,Alt=="LSM25B")$STAGE),lines(1-proportion,value,col="purple"))
# with(ecdf_fun(subset(lakeO.stage,Alt=="ECRE")$STAGE),lines(1-proportion,value,col="red"))
# with(ecdf_fun(subset(lakeO.stage,Alt=="ESLE")$STAGE),lines(1-proportion,value,col="lightblue"))
# with(ecdf_fun(subset(lakeO.stage,Alt=="ABNE")$STAGE),lines(1-proportion,value,col="green"))

lakeO.stage$WY=WY(lakeO.stage$Date)
lakeO.stage$low.stg=with(lakeO.stage,ifelse(STAGE<=11,1,0))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$High.stg=with(lakeO.stage,ifelse(STAGE>16,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))

# png(filename=paste0(plot.path,"Iteration_2/LO_totalDays.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,0.25,1),oma=c(1,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,4000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,5000);by.x=1000;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
days.POS=ddply(lakeO.stage,"Alt",summarise,sum.low=sum(low.stg),sum.vlow=sum(vlow.stg),sum.High=sum(High.stg),sum.vHigh=sum(vHigh.stg))
days.POS$Alt=factor(days.POS$Alt,level=alts.sort)
plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(days.POS,points(sum.High,sum.low,pch=19,col=adjustcolor("dodgerblue1",0.5),lwd=0.1))
text(days.POS$sum.High,days.POS$sum.low,days.POS$Alt,font=2,col=adjustcolor("black",0.75),cex=0.75)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u003E 16 ft NGVD29")
mtext(side=2,line=2.75,"Days \u2264 11 ft NGVD29")
mtext(side=3,adj=0, "Cumulative days over the enitre period of simulation")

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,1500);by.x=500;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

days.POS=ddply(lakeO.stage,"Alt",summarise,sum.low=sum(low.stg),sum.vlow=sum(vlow.stg),sum.High=sum(High.stg),sum.vHigh=sum(vHigh.stg))
days.POS$Alt=factor(days.POS$Alt,level=alts.sort)
plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(days.POS,points(sum.vHigh,sum.vlow,pch=19,col=adjustcolor("dodgerblue1",0.5),lwd=0.1))
text(days.POS$sum.vHigh,days.POS$sum.vlow,days.POS$Alt,font=2,col=adjustcolor("black",0.75),cex=0.75)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u2265 17 ft NGVD29")
mtext(side=2,line=2.75,"Days \u2264 10 ft NGVD29")
dev.off()


lakeO.stage.low=ddply(subset(lakeO.stage,WY%in%WYs),c("WY","Alt"),summarise,freq=sum(vlow.stg,na.rm=T),N.val=N.obs(vlow.stg))
lakeO.stage.low$Alt=factor(lakeO.stage.low$Alt,level=alts.sort)
lakeO.stage.low.sum=ddply(lakeO.stage.low,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
lakeO.stage.low.sum=lakeO.stage.low.sum[match(alts.sort,lakeO.stage.low.sum$Alt),]

lakeO.stage.high=ddply(subset(lakeO.stage,WY%in%WYs),c("WY","Alt"),summarise,freq=sum(vHigh.stg,na.rm=T),N.val=N.obs(vHigh.stg))
lakeO.stage.high$Alt=factor(lakeO.stage.high$Alt,level=alts.sort)
lakeO.stage.high.sum=ddply(lakeO.stage.high,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
lakeO.stage.high.sum=lakeO.stage.high.sum[match(alts.sort,lakeO.stage.high.sum$Alt),]

lakeO.stage.freq.sum=merge(lakeO.stage.low.sum[,c("Alt","mean.freq")],
lakeO.stage.high.sum[,c("Alt","mean.freq")],"Alt")
colnames(lakeO.stage.freq.sum)<-c("Alt","freqLow","freqHigh")
lakeO.stage.freq.sum=lakeO.stage.freq.sum[match(alts.sort,lakeO.stage.freq.sum$Alt),]
# write.csv(lakeO.stage.freq.sum,paste0(export.path,"highlow_meanfreq_rslt.csv"),row.names = F)

# png(filename=paste0(plot.path,"Iteration_2/LO_iter1_freq_bxp.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.25,1),oma=c(4,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,lakeO.stage.high,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,lakeO.stage.high.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(lakeO.stage.high,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(lakeO.stage.high.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(High Stage)\n(Days Yr\u207B\u00B9)")
mtext(side=3,adj=0,"Lake Okeechobee")

ylim.val=c(0,25);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,lakeO.stage.low,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,lakeO.stage.low.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(lakeO.stage.low,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(lakeO.stage.low.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Low Stage)\n(Days Yr\u207B\u00B9)")
mtext(side=1,line=4,'Model Alternative')
dev.off()

# Stage duration curves

# png(filename=paste0(plot.path,"Iteration_2/LO_StageDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(8,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[1])$STAGE),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[2])$STAGE),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  # with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-lwr.CI,value,col=adjustcolor(cols[i],0.5),lwd=1,lty=2))
  # with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-upr.CI,value,col=adjustcolor(cols[i],0.5),lwd=1,lty=2))
  legend("bottomleft",legend=c(alts.sort[c(1,2,i)]),
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"Lake Okeechobee")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()


highlow.ecdf=data.frame()
for(i in 1:n.alts){
  tmp.ecdf=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE)
  tmp.ecdf$proportion=1-tmp.ecdf$proportion
  
  # plot(value~proportion,tmp.ecdf)
  # lines(c(-1,max(subset(tmp.ecdf,value>=17)$proportion)),y=c(17,17),col="Red")
  # lines(c(max(subset(tmp.ecdf,value>=17)$proportion),max(subset(tmp.ecdf,value>=17)$proportion)),
  #       y=c(0,17),col="red")
  # lines(c(-1,max(subset(tmp.ecdf,value>=10)$proportion)),y=c(10,10),col="Red")
  # lines(c(max(subset(tmp.ecdf,value>=10)$proportion),max(subset(tmp.ecdf,value>=10)$proportion)),
  #       y=c(0,10),col="red")

  high.val=max(subset(tmp.ecdf,value>=17)$proportion)
  low.val=max(subset(tmp.ecdf,value>=10)$proportion)
  tmp=data.frame(Alt=alts.sort[i],high.prop=high.val,low.prop=low.val)
  highlow.ecdf=rbind(tmp,highlow.ecdf)
}
highlow.ecdf

highlow.ecdf=highlow.ecdf[match(alts.sort,highlow.ecdf$Alt),]
highlow.ecdf$high.prop[is.infinite(highlow.ecdf$high.prop)==T]<-NA
# write.csv(highlow.ecdf,paste0(export.path,"iter2_highlow_ecdf_rslt.csv"),row.names = F)

# RECOVER Stage Envelope --------------------------------------------------
## 
library(LORECOVER)

head(lakeO.stage)
lakeO.stage$Data.Value=lakeO.stage$STAGE

norm.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts[i])
  rslt=norm_env(tmp)
  rslt$Alt=alts[i]
  norm.lakeO.stage.scr=rbind(norm.lakeO.stage.scr,rslt)
  print(i)
}
norm.lakeO.stage.scr=rename(norm.lakeO.stage.scr,c("score"="norm.score"))
norm.lakeO.stage.scr$WY=WY(norm.lakeO.stage.scr$Date)
norm.lakeO.stage.scr=subset(norm.lakeO.stage.scr,WY%in%seq(1966,2016,1));# Full Florida WY (MAy - April) 

head(norm.lakeO.stage.scr)
norm.lakeO.stage.scr.WY=ddply(norm.lakeO.stage.scr,c("Alt","WY"),summarise,TScore=sum(norm.score,na.rm=T))
norm.lakeO.stage.scr.WY$Alt=factor(norm.lakeO.stage.scr.WY$Alt,levels=alts.sort)
norm.lakeO.stage.scr.WY.sum=ddply(norm.lakeO.stage.scr.WY,"Alt",summarise,mean.val=mean(TScore))
norm.lakeO.stage.scr.WY.sum$Alt=factor(norm.lakeO.stage.scr.WY.sum$Alt,levels=alts.sort)
norm.lakeO.stage.scr.WY.sum$FWO.diff=with(norm.lakeO.stage.scr.WY.sum,(mean.val-mean.val[1])/mean.val[1])*100

boxplot(TScore~Alt,norm.lakeO.stage.scr.WY,outline=F)


rec.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts[i])
  rslt=rec_env(tmp)
  rslt$Alt=alts[i]
  rec.lakeO.stage.scr=rbind(rec.lakeO.stage.scr,rslt)
  print(i)
}
rec.lakeO.stage.scr=rename(rec.lakeO.stage.scr,c("score"="rec.score"))
rec.lakeO.stage.scr$WY=WY(rec.lakeO.stage.scr$Date)
rec.lakeO.stage.scr=subset(rec.lakeO.stage.scr,WY%in%seq(1966,2016,1));# Full Florida WY (MAy - April) 

rec.lakeO.stage.scr.WY=ddply(rec.lakeO.stage.scr,c("Alt","WY"),summarise,TScore=sum(rec.score,na.rm=T))
rec.lakeO.stage.scr.WY$Alt=factor(rec.lakeO.stage.scr.WY$Alt,levels=alts.sort)
rec.lakeO.stage.scr.WY.sum=ddply(rec.lakeO.stage.scr.WY,"Alt",summarise,mean.val=mean(TScore))
rec.lakeO.stage.scr.WY.sum$Alt=factor(rec.lakeO.stage.scr.WY.sum$Alt,levels=alts.sort)
rec.lakeO.stage.scr.WY.sum$FWO.diff=with(rec.lakeO.stage.scr.WY.sum,(mean.val-mean.val[1])/mean.val[1])*100

# lakeO.stage.scr=merge(norm.lakeO.stage.scr,rec.lakeO.stage.scr[,c("Date","Alt","rec.score")],c("Date","Alt"))
# head(lakeO.stage.scr)
# lakeO.stage.scr$CY=as.numeric(format(lakeO.stage.scr$Date,"%Y"))
# lakeO.stage.scr$WY=WY(lakeO.stage.scr$Date)
# lakeO.stage.scr=lakeO.stage.scr[order(lakeO.stage.scr$Alt,lakeO.stage.scr$Date),]
# lakeO.stage.scr$JJ.period=with(lakeO.stage.scr,ifelse(as.numeric(format(Date,"%m"))%in%c(6,7),1,0))
# # lakeO.stage.scr$MinStage.30d=with(lakeO.stage.scr,ave(Data.Value,Alt,FUN=function(x) c(rep(NA,29),rollapply(x,30,FUN=function(x)min(x,na.rm=T)))))
# 
# norm.to.rec=ddply(lakeO.stage.scr,c("CY","Alt"),summarise,max.stg=max(Data.Value,na.rm=T),summer.N=sum(ifelse(JJ.period==1&Data.Value>13,1,0)))
# norm.to.rec$trans=with(norm.to.rec,ifelse(max.stg>17|summer.N<30,1,0))


# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_bxp.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
# par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,4,1,0.25));
par(family="serif",mar=c(1,3,0.25,1),oma=c(4,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TScore~Alt,norm.lakeO.stage.scr.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,norm.lakeO.stage.scr.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(norm.lakeO.stage.scr.WY,Alt==alts.sort[1])$TScore),lty=2,col="black")
abline(h=subset(norm.lakeO.stage.scr.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
# axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"Lake Okeechobee")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Normal Lake Envelope\nAnnual Score (unitless)")

ylim.val=c(0,2500);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TScore~Alt,rec.lakeO.stage.scr.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,rec.lakeO.stage.scr.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(rec.lakeO.stage.scr.WY,Alt==alts.sort[1])$TScore),lty=2,col="black")
abline(h=subset(rec.lakeO.stage.scr.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"Recovery Lake Envelope\nAnnual Score (unitless)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_sum.png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-25,50);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(norm.lakeO.stage.scr.WY.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(norm.lakeO.stage.scr.WY.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(norm.lakeO.stage.scr.WY.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,norm.lakeO.stage.scr.WY.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Normal Envelope")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')

xlim.val=c(-25,50);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(rec.lakeO.stage.scr.WY.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(rec.lakeO.stage.scr.WY.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(rec.lakeO.stage.scr.WY.sum,points(FWO.diff,1:8,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:8,1:8,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Recovery Envelope")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
dev.off()

# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S78","S79","S80","S308","S351","S352","S354")
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
q.dat=subset(q.dat,WY%in%WYs);# Full Florida WY (MAy - April) 

q.dat$Alt_SITE=paste(q.dat$Alt,q.dat$SITE,sep="_")
q.dat$Q.14=with(q.dat,ave(FLOW,Alt_SITE,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))

q.dat$CRE.low=with(q.dat,ifelse(SITE=="S79"&Q.14<750,1,0))
q.dat$CRE.dam=with(q.dat,ifelse(SITE=="S79"&Q.14>2600,1,0))
q.dat$CRE.opt=with(q.dat,ifelse(SITE=="S79"&Q.14>=750&Q.14<2100,1,0))
q.dat$CRE.stress=with(q.dat,ifelse(SITE=="S79"&Q.14>=2100&Q.14<=2600,1,0))

q.dat$SLE.low=with(q.dat,ifelse(SITE=="S80"&Q.14<150,1,0))
q.dat$SLE.dam=with(q.dat,ifelse(SITE=="S80"&Q.14>1700,1,0))
q.dat$SLE.opt=with(q.dat,ifelse(SITE=="S80"&Q.14>=150&Q.14<1400,1,0))
q.dat$SLE.stress=with(q.dat,ifelse(SITE=="S80"&Q.14>=1400&Q.14<1700,1,0))

q.dat$hydro.season=with(q.dat,FL.Hydroseason(q.dat$Date))
q.dat$bloom.period=with(q.dat,ifelse(SITE%in%c("S77","S78","S79"),
                                     ifelse(as.numeric(format(Date,"%m"))%in%c(6:9),"bloom","no.bloom"),
                                     ifelse(as.numeric(format(Date,"%m"))%in%c(5:9),"bloom","no.bloom")))

# CRE ---------------------------------------------------------------------
# Flow duration curves
# png(filename=paste0(plot.path,"Iteration_2/S79_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,20000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S79"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S79"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S79"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  #abline(h=c(750,2100,2600),lty=2,col=adjustcolor("black",0.5))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"CRE (S-79)")}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S77_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,8000);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S77"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S77"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S77"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"C-43 (S-77)")}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

## 
q.dat.cre.low2=ddply(subset(q.dat,SITE=="S79"),c("WY","SITE","Alt"),summarise,freq=sum(CRE.low,na.rm=T),N.val=N.obs(Q.14))
q.dat.cre.low2$Alt=factor(q.dat.cre.low2$Alt,levels=alts.sort)
q.dat.cre.low2.sum=ddply(q.dat.cre.low2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm = T))
q.dat.cre.low2.sum=q.dat.cre.low2.sum[match(alts.sort,q.dat.cre.low2.sum$Alt),]

q.dat.cre.dam2=ddply(subset(q.dat,SITE=="S79"),c("WY","SITE","Alt"),summarise,freq=sum(CRE.dam,na.rm=T),N.val=N.obs(Q.14))
q.dat.cre.dam2$Alt=factor(q.dat.cre.dam2$Alt,levels=alts.sort)
q.dat.cre.dam2.sum=ddply(q.dat.cre.dam2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
q.dat.cre.dam2.sum=q.dat.cre.dam2.sum[match(alts.sort,q.dat.cre.dam2.sum$Alt),]

q.dat.cre.opt2=ddply(subset(q.dat,SITE=="S79"),c("WY","SITE","Alt"),summarise,freq=sum(CRE.opt,na.rm=T),N.val=N.obs(Q.14))
q.dat.cre.opt2$Alt=factor(q.dat.cre.opt2$Alt,levels=alts.sort)
q.dat.cre.opt2.sum=ddply(q.dat.cre.opt2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
q.dat.cre.opt2.sum=q.dat.cre.opt2.sum[match(alts.sort,q.dat.cre.opt2.sum$Alt),]


q.dat.cre.flowcats=ddply(subset(q.dat,SITE=="S79"),c("WY","SITE","Alt"),summarise,
                         opt.freq=sum(CRE.opt,na.rm=T),
                         dam.freq=sum(CRE.dam,na.rm=T),
                         low.freq=sum(CRE.low,na.rm=T),N.val=N.obs(Q.14))
#q.dat.cre.flowcats=ddply(q.dat.cre.flowcats,"Alt",summarise,opt.freq=mean(opt.freq),dam.freq=mean(opt.freq),low.freq=mean(low.freq))

q.dat.cre.flowcats$Alt=factor(q.dat.cre.flowcats$Alt,levels=alts.sort)

cols=c("grey","grey",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

library(ggtern)
CRE.flow.tern=ggtern(q.dat.cre.flowcats,aes(x=dam.freq,y=low.freq,z=opt.freq,label=Alt))+
  geom_point(aes(colour=factor(Alt)),size=2)+
  scale_color_manual(values=cols)+
  geom_confidence_tern(colour="dodgerblue1",alpha=0.5,breaks=c(0.5,0.95),linetype=2)+
  #geom_text(hjust=0, vjust=0,alpha=0.5)+
  labs(title="Frequency of RECOVER Flow catergories (CRE)",
       subtitle=" FLWY 1966 - 2016",
       x="",xarrow="Damaging",
       y="",yarrow="Low",
       z="",zarrow="Optimum",color="Alternatives")+
  theme_bw(base_size=10)+theme_arrowcustomlength(0.1,0.85)+
  theme(panel.spacing=unit(0.5, "lines"),
        text=element_text(family="serif"),
        plot.margin=margin(0.25,1,0.25,1),
        tern.axis.arrow = element_line(color = "red",size=1.75))+
  facet_wrap(~ Alt, ncol = 4)
CRE.flow.tern  

# ggsave(CRE.flow.tern,filename=paste0(plot.path,"Iteration_2/CRE_FlowCat.png"),width=8,height=4,unit="in",device="png")

# png(filename=paste0(plot.path,"Iteration_2/CRE_iter2_freq_bxp.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.25,1),oma=c(4,3,1,0.25));
layout(matrix(1:3,3,1,byrow=T))

ylim.val=c(0,365);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,q.dat.cre.dam2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.cre.dam2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.cre.dam2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.cre.dam2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Damaging Flow)\n(Days Yr\u207B\u00B9)")
mtext(side=3,adj=0,"CRE (S-79)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

boxplot(freq~Alt,q.dat.cre.opt2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.cre.opt2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.cre.opt2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.cre.opt2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Optimum Flow)\n(Days Yr\u207B\u00B9)")

boxplot(freq~Alt,q.dat.cre.low2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.cre.low2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.cre.low2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.cre.low2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Low Flow)\n(Days Yr\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Percent Dry season Q ----------------------------------------------------
cre.q.season=cast(subset(q.dat,SITE=="S79"),WY+Alt~hydro.season,value="FLOW",sum,na.rm=T)
cre.q.season$TFlow=rowSums(cre.q.season[,c("A_Wet","B_Dry")],na.rm=T)
cre.q.season$PerDry=with(cre.q.season,(B_Dry/TFlow)*100)
cre.q.season$Alt=factor(cre.q.season$Alt,levels=alts.sort)

cre.q.season.sum=ddply(cre.q.season,"Alt",summarise,mean.val=mean(PerDry,na.rm=T))
cre.q.season.sum=cre.q.season.sum[match(alts.sort,cre.q.season.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/S79_PercentDryQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerDry~Alt,cre.q.season,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.q.season.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(cre.q.season,Alt==alts.sort[1])$PerDry),lty=2,col="black")
abline(h=subset(cre.q.season.sum,Alt==alts.sort[1])$mean,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Percent Dry Season Discharge\n(May - October)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Basin Contribution ------------------------------------------------------
q.dat$month=as.numeric(format(q.dat$Date,"%m"))
basin.q.season=cast(subset(q.dat,SITE%in%c("S79","S77")),WY+month+Alt~SITE,value="FLOW",sum,na.rm=T)
basin.q.season$Q.C43=with(basin.q.season,ifelse(S79<S77,0,S79-S77))
basin.q.season$Alt=factor(basin.q.season$Alt,levels=alts.sort)

basin.q.season.WY=ddply(basin.q.season,c("WY","Alt"),summarise,TFlow.S79=sum(S79,na.rm=T),TFlow.S77=sum(S77,na.rm=T),TFlow.basin=sum(Q.C43,na.rm=T))
basin.q.season.WY$PerBasin=with(basin.q.season.WY,(TFlow.basin/TFlow.S79)*100)
basin.q.season.WY$PerLake=with(basin.q.season.WY,(TFlow.S77/TFlow.S79)*100)

basin.q.season.WY.sum=ddply(basin.q.season.WY,"Alt",summarise,mean=mean(PerBasin,na.rm=T),mean.lake=mean(PerLake,na.rm=T))
basin.q.season.WY.sum=basin.q.season.WY.sum[match(alts.sort,basin.q.season.WY.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/S79_PercentBasinQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerBasin~Alt,basin.q.season.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,basin.q.season.WY.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(basin.q.season.WY,Alt==alts.sort[1])$PerBasin),lty=2,col="black")
abline(h=subset(basin.q.season.WY.sum,Alt==alts.sort[1])$mean,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Percent S-79 Discharge\nfrom C-43 Basin")

boxplot(PerLake~Alt,basin.q.season.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,basin.q.season.WY.sum$mean.lake,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(basin.q.season.WY,Alt==alts.sort[1])$PerLake),lty=2,col="black")
abline(h=subset(basin.q.season.WY.sum,Alt==alts.sort[1])$mean.lake,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.25,"Percent S-79 Discharge\nfrom Lake")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Bloom period ------------------------------------------------------------
bloom.q.season=cast(subset(q.dat,SITE%in%c("S79","S77")),SITE+WY+Alt~bloom.period,value="FLOW",function(x) sum(cfs.to.acftd(x),na.rm=T))
bloom.q.season$TFlow=rowSums(bloom.q.season[,c("bloom","no.bloom")],na.rm=T)
bloom.q.season$per_bloom=with(bloom.q.season,(bloom/TFlow)*100)
bloom.q.season$Alt=factor(bloom.q.season$Alt,levels=alts.sort)

# bloom.q.season.WY.sum=ddply(bloom.q.season,c("Alt"),summarise,mean_bloom=mean(per_bloom,na.rm=T))
bloom.q.season.WY.sum=cast(bloom.q.season,Alt~SITE,value="per_bloom",function(x)mean(x,na.rm=T))
bloom.q.season.WY.sum=bloom.q.season.WY.sum[match(alts.sort,bloom.q.season.WY.sum$Alt),]

bloom.q.season.WY.sum2=cast(bloom.q.season,Alt~SITE,value="bloom",function(x)mean(x,na.rm=T))
bloom.q.season.WY.sum2=bloom.q.season.WY.sum2[match(alts.sort,bloom.q.season.WY.sum2$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/CRE_PercentBloomQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(per_bloom~Alt,subset(bloom.q.season,SITE=="S77"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum$S77,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S77")$per_bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum,Alt==alts.sort[1])$S77,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"C-43 (S-77)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(per_bloom~Alt,subset(bloom.q.season,SITE=="S79"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum$S79,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S79")$per_bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum,Alt==alts.sort[1])$S79,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=2,line=1.5,"Percent Discharge\nDuring June - Aug",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/CRE_BloomQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,350e3);by.y=100e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(bloom~Alt,subset(bloom.q.season,SITE=="S77"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum2$S77,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S77")$bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum2,Alt==alts.sort[1])$S77,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"C-43 (S-77)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,125e4);by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(bloom~Alt,subset(bloom.q.season,SITE=="S79"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum2$S79,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S79")$bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum2,Alt==alts.sort[1])$S79,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=2,line=1.5,"Total Discharge Volume During June - Aug\n(x10\u00B3 AcFt d\u207B\u00B9)",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()

# total Annual Q ---------------------------------------------------------
S79.q.WY=ddply(subset(q.dat,SITE%in%c("S79")),c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(FLOW)))
S79.q.WY$Alt=factor(S79.q.WY$Alt,levels=alts.sort)
S79.q.WY.sum=ddply(S79.q.WY,"Alt",summarise,mean.val=mean(TFlow.acft),med.val=median(TFlow.acft))
S79.q.WY.sum$Alt=factor(S79.q.WY.sum$Alt,levels=alts.sort)

boxplot(TFlow.acft~Alt,S79.q.WY)

# png(filename=paste0(plot.path,"Iteration_2/S79_WYQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,4e6);by.y=1e6;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,S79.q.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S79.q.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S79.q.WY.sum,Alt==alts.sort[1])$med.val),lty=2,col="black")
abline(h=subset(S79.q.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=2,line=2.5,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()


CRE.Q.cat=ddply(subset(q.dat,SITE=="S79"),c("Alt","WY"),summarise,N.low=sum(CRE.low,na.rm=T),N.opt=sum(CRE.opt,na.rm=T),N.dam=sum(CRE.dam,na.rm=T))
CRE.Q.cat.mean=ddply(CRE.Q.cat,"Alt",summarise,low=mean(N.low,na.rm=T),opt=mean(N.opt,na.rm=T),dam=mean(N.dam,na.rm=T))
CRE.Q.cat.mean=CRE.Q.cat.mean[match(alts.sort,CRE.Q.cat.mean$Alt),]
CRE.Q.cat.mean$low.perdiff=with(CRE.Q.cat.mean,((low-low[1])/low[1])*100)
CRE.Q.cat.mean$opt.perdiff=with(CRE.Q.cat.mean,((opt-opt[1])/opt[1])*100)
CRE.Q.cat.mean$dam.perdiff=with(CRE.Q.cat.mean,((dam-dam[1])/dam[1])*100)

CRE.Q.cat.mean.melt=reshape::melt(CRE.Q.cat.mean[c("Alt",paste0(c("low","opt","dam"),".perdiff"))],id.vars = "Alt")
layout(matrix(1:8,2,4,byrow=T))
for(i in 3:8){
  barplot(subset(CRE.Q.cat.mean.melt,Alt==alts.sort[i])$value)
}




# SLE ---------------------------------------------------------------------
# Flow duration curves
# png(filename=paste0(plot.path,"Iteration_2/S80_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25),xpd=F);
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,7200);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S80"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S80"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S80"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"SLE (S-80)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S308_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,7200);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S308"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S308"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S308"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"C-44 (S-308)")}
  box(lwd=1)
  abline(h=250,lty=2)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

## 
q.dat.sle.low2=ddply(subset(q.dat,SITE=="S80"),c("WY","SITE","Alt"),summarise,freq=sum(SLE.low,na.rm=T),N.val=N.obs(Q.14))
q.dat.sle.low2$Alt=factor(q.dat.sle.low2$Alt,levels=alts.sort)
q.dat.sle.low2.sum=ddply(q.dat.sle.low2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm = T))
q.dat.sle.low2.sum=q.dat.sle.low2.sum[match(alts.sort,q.dat.sle.low2.sum$Alt),]

q.dat.sle.dam2=ddply(subset(q.dat,SITE=="S80"),c("WY","SITE","Alt"),summarise,freq=sum(SLE.dam,na.rm=T),N.val=N.obs(Q.14))
q.dat.sle.dam2$Alt=factor(q.dat.sle.dam2$Alt,levels=alts.sort)
q.dat.sle.dam2.sum=ddply(q.dat.sle.dam2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
q.dat.sle.dam2.sum=q.dat.sle.dam2.sum[match(alts.sort,q.dat.sle.dam2.sum$Alt),]

q.dat.sle.opt2=ddply(subset(q.dat,SITE=="S80"),c("WY","SITE","Alt"),summarise,freq=sum(SLE.opt,na.rm=T),N.val=N.obs(Q.14))
q.dat.sle.opt2$Alt=factor(q.dat.sle.opt2$Alt,levels=alts.sort)
q.dat.sle.opt2.sum=ddply(q.dat.sle.opt2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
q.dat.sle.opt2.sum=q.dat.sle.opt2.sum[match(alts.sort,q.dat.sle.opt2.sum$Alt),]

q.dat.sle.flowcats=ddply(subset(q.dat,SITE=="S80"),c("WY","SITE","Alt"),summarise,
                         opt.freq=sum(SLE.opt,na.rm=T),
                         dam.freq=sum(SLE.dam,na.rm=T),
                         low.freq=sum(SLE.low,na.rm=T),N.val=N.obs(Q.14))
#q.dat.cre.flowcats=ddply(q.dat.cre.flowcats,"Alt",summarise,opt.freq=mean(opt.freq),dam.freq=mean(opt.freq),low.freq=mean(low.freq))

q.dat.sle.flowcats$Alt=factor(q.dat.sle.flowcats$Alt,levels=alts.sort)

cols=c("grey","grey",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

SLE.flow.tern=ggtern(q.dat.sle.flowcats,aes(x=dam.freq,y=low.freq,z=opt.freq,label=Alt))+
  geom_point(aes(colour=factor(Alt)),size=2)+
  scale_color_manual(values=cols)+
  geom_confidence_tern(colour="dodgerblue1",alpha=0.5,breaks=c(0.5,0.95),linetype=2)+
  #geom_text(hjust=0, vjust=0,alpha=0.5)+
  labs(title="Frequency of RECOVER Flow catergories (SLE)",
       subtitle="FL WY 1966 - 2016",
       x="",xarrow="Damaging",
       y="",yarrow="Low",
       z="",zarrow="Optimum",color="Alternatives")+
  theme_bw(base_size=10)+theme_arrowcustomlength(0.1,0.85)+
  theme(panel.spacing=unit(0.5, "lines"),
        text=element_text(family="serif"),
        plot.margin=margin(0.25,1,0.25,1),
        tern.axis.arrow = element_line(color = "red",size=1.75))+
  facet_wrap(~ Alt, ncol = 4)
SLE.flow.tern  

# ggsave(SLE.flow.tern,filename=paste0(plot.path,"Iteration_2/SLE_FlowCat.png"), width=8,height=4,unit="in",device="png")


# png(filename=paste0(plot.path,"Iteration_2/SLE_iter2_freq_bxp.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.25,1),oma=c(4,3,1,0.25));
layout(matrix(1:3,3,1,byrow=T))

ylim.val=c(0,130);by.y=60;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,q.dat.sle.dam2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.sle.dam2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.sle.dam2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.sle.dam2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Damaging Flow)\n(Days Yr\u207B\u00B9)")
mtext(side=3,adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,365);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,q.dat.sle.opt2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.sle.opt2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.sle.opt2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.sle.opt2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Optimum Flow)\n(Days Yr\u207B\u00B9)")

boxplot(freq~Alt,q.dat.sle.low2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.sle.low2.sum$mean.freq,pch=21,bg="olivedrab2",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.sle.low2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.sle.low2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Low Flow)\n(Days Yr\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Percent Dry season Q ----------------------------------------------------
sle.q.season=cast(subset(q.dat,SITE=="S80"),WY+Alt~hydro.season,value="FLOW",sum,na.rm=T)
sle.q.season$TFlow=rowSums(sle.q.season[,c("A_Wet","B_Dry")],na.rm=T)
sle.q.season$PerDry=with(sle.q.season,(B_Dry/TFlow)*100)
sle.q.season$PerWet=with(sle.q.season,(A_Wet/TFlow)*100)
sle.q.season$Alt=factor(sle.q.season$Alt,levels=alts.sort)

sle.q.season.sum=ddply(sle.q.season,"Alt",summarise,mean.val=mean(PerDry,na.rm=T),mean.wet.val=mean(PerWet,na.rm=T))
sle.q.season.sum=sle.q.season.sum[match(alts.sort,sle.q.season.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/S80_PercentDryQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerDry~Alt,sle.q.season,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.q.season.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(sle.q.season,Alt==alts.sort[1])$PerDry,na.rm=T),lty=2,col="black")
abline(h=subset(sle.q.season.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Percent Dry Season Discharge\n(May - October)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S80_PercentWetQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerWet~Alt,sle.q.season,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.q.season.sum$mean.wet.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(sle.q.season,Alt==alts.sort[1])$PerWet,na.rm=T),lty=2,col="black")
abline(h=subset(sle.q.season.sum,Alt==alts.sort[1])$mean.wet.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Percent Wet Season Discharge\n(November - April)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()
# Basin Contribution ------------------------------------------------------
basin.q.season=cast(subset(q.dat,SITE%in%c("S80","S308")),WY+month+Alt~SITE,value="FLOW",sum,na.rm=T)
basin.q.season$Q.C44=with(basin.q.season,ifelse(S80<S308,0,S80-S308))
basin.q.season$Alt=factor(basin.q.season$Alt,levels=alts.sort)

basin.q.season.WY=ddply(basin.q.season,c("WY","Alt"),summarise,TFlow.S80=sum(S80,na.rm=T),TFlow.S308=sum(S308,na.rm=T),TFlow.basin=sum(Q.C44,na.rm=T))
basin.q.season.WY$PerBasin=with(basin.q.season.WY,(TFlow.basin/TFlow.S80)*100)
basin.q.season.WY$PerLake=with(basin.q.season.WY,(TFlow.S308/TFlow.S80)*100)
basin.q.season.WY$PerLake[is.infinite(basin.q.season.WY$PerLake)==T]<-NA
range(basin.q.season.WY$PerLake,na.rm=T)

basin.q.season.WY.sum=ddply(basin.q.season.WY,"Alt",summarise,mean=mean(PerBasin,na.rm=T),mean.lake=mean(PerLake,na.rm=T))
basin.q.season.WY.sum=basin.q.season.WY.sum[match(alts.sort,basin.q.season.WY.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/S80_PercentBasinQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerBasin~Alt,basin.q.season.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,basin.q.season.WY.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(basin.q.season.WY,Alt==alts.sort[1])$PerBasin),lty=2,col="black")
abline(h=subset(basin.q.season.WY.sum,Alt==alts.sort[1])$mean,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Percent S-80 Discharge\nfrom C-44 Basin")

boxplot(PerLake~Alt,basin.q.season.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,basin.q.season.WY.sum$mean.lake,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(basin.q.season.WY,Alt==alts.sort[1])$PerLake),lty=2,col="black")
abline(h=subset(basin.q.season.WY.sum,Alt==alts.sort[1])$mean.lake,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.25,"Percent S-80 Discharge\nfrom Lake")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Bloom period ------------------------------------------------------------
bloom.q.season=cast(subset(q.dat,SITE%in%c("S80","S308")),SITE+WY+Alt~bloom.period,value="FLOW",function(x) sum(cfs.to.acftd(x),na.rm=T))
bloom.q.season$TFlow=rowSums(bloom.q.season[,c("bloom","no.bloom")],na.rm=T)
bloom.q.season$per_bloom=with(bloom.q.season,(bloom/TFlow)*100)
bloom.q.season$Alt=factor(bloom.q.season$Alt,levels=alts.sort)

# bloom.q.season.WY.sum=ddply(bloom.q.season,c("Alt"),summarise,mean_bloom=mean(per_bloom,na.rm=T))
bloom.q.season.WY.sum=cast(bloom.q.season,Alt~SITE,value="per_bloom",function(x)mean(x,na.rm=T))
bloom.q.season.WY.sum=bloom.q.season.WY.sum[match(alts.sort,bloom.q.season.WY.sum$Alt),]

bloom.q.season.WY.sum2=cast(bloom.q.season,Alt~SITE,value="bloom",function(x)mean(x,na.rm=T))
bloom.q.season.WY.sum2=bloom.q.season.WY.sum2[match(alts.sort,bloom.q.season.WY.sum2$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/SLE_PercentBloomQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(per_bloom~Alt,subset(bloom.q.season,SITE=="S308"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum$S308,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S308")$per_bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum,Alt==alts.sort[1])$S308,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"C-44 (S-308)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(per_bloom~Alt,subset(bloom.q.season,SITE=="S80"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum$S80,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S80")$per_bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum,Alt==alts.sort[1])$S80,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=2,line=1.5,"Percent Discharge\nDuring May - Aug",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/SLE_BloomQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,200e3);by.y=50e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(bloom~Alt,subset(bloom.q.season,SITE=="S308"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum2$S308,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S308")$bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum2,Alt==alts.sort[1])$S308,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"C-44 (S-308)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,40e4);by.y=10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(bloom~Alt,subset(bloom.q.season,SITE=="S80"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum2$S80,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S80")$bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum2,Alt==alts.sort[1])$S80,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=2,line=1.5,"Total Discharge Volume During May - Aug\n(x10\u00B3 AcFt d\u207B\u00B9)",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()

# total Annual Q ---------------------------------------------------------
S80.q.WY=ddply(subset(q.dat,SITE%in%c("S80")),c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(FLOW)))
S80.q.WY$Alt=factor(S80.q.WY$Alt,levels=alts.sort)
S80.q.WY.sum=ddply(S80.q.WY,"Alt",summarise,mean.val=mean(TFlow.acft),med.val=median(TFlow.acft))
S80.q.WY.sum$Alt=factor(S80.q.WY.sum$Alt,levels=alts.sort)

boxplot(TFlow.acft~Alt,S80.q.WY)

# png(filename=paste0(plot.path,"Iteration_2/S80_WYQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,1e6);by.y=0.25e6;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,S80.q.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S80.q.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S80.q.WY.sum,Alt==alts.sort[1])$med.val),lty=2,col="black")
abline(h=subset(S80.q.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()


SLE.Q.cat=ddply(subset(q.dat,SITE=="S80"),c("Alt","WY"),summarise,N.low=sum(SLE.low,na.rm=T),N.opt=sum(SLE.opt,na.rm=T),N.dam=sum(SLE.dam,na.rm=T))
SLE.Q.cat.mean=ddply(SLE.Q.cat,"Alt",summarise,low=mean(N.low,na.rm=T),opt=mean(N.opt,na.rm=T),dam=mean(N.dam,na.rm=T))
SLE.Q.cat.mean=SLE.Q.cat.mean[match(alts.sort,SLE.Q.cat.mean$Alt),]
SLE.Q.cat.mean$low.perdiff=with(SLE.Q.cat.mean,((low-low[1])/low[1])*100)
SLE.Q.cat.mean$opt.perdiff=with(SLE.Q.cat.mean,((opt-opt[1])/opt[1])*100)
SLE.Q.cat.mean$dam.perdiff=with(SLE.Q.cat.mean,((dam-dam[1])/dam[1])*100)

SLE.Q.cat.mean.melt=reshape::melt(SLE.Q.cat.mean[,c("Alt",paste0(c("low","opt","dam"),".perdiff"))],id.vars = "Alt")
layout(matrix(1:8,2,4,byrow=T))
for(i in 3:8){
  barplot(subset(SLE.Q.cat.mean.melt,Alt==alts.sort[i])$value)
}

## 
CRE.Q.cat.mean.melt$Est="CRE"
SLE.Q.cat.mean.melt$Est="SLE"

Q.cat.mean=rbind(CRE.Q.cat.mean.melt,SLE.Q.cat.mean.melt)
Q.cat.mean$variable=factor(Q.cat.mean$variable,levels = c("low.perdiff", "opt.perdiff", "dam.perdiff"))
Q.cat.mean$Alt=factor(Q.cat.mean$Alt,levels=alts.sort)

Q.cat.mean2=reshape2::dcast(Q.cat.mean,Alt+variable~Est,value.var="value",mean)


ylim.val=c(-75,210);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration_2/Estuary_FWOCompare.png"),width=8,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:7,1,7,byrow=T),widths=c(1,1,1,1,1,1,0.5))

for(i in 3:8){
pln=alts.sort[i]
x=barplot(t(subset(Q.cat.mean2,Alt==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(subset(Q.cat.mean2,Alt==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("dodgerblue1","indianred1"),add=T,xaxt="n")
x.val=x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,c("Low","Optimal","High"),line=-0.5,cex=0.8)
if(i==3){
  axis_fun(2,ymaj,ymin,ymaj)
  mtext(side=2,line=2,"Percent Difference from FWO",cex=0.8)}else{axis_fun(2,ymaj,ymin,NA)};
box(lwd=1)
mtext(side=3,adj=0,paste0("Alternative ",pln),cex=0.5)

}
mtext(side=1,line=0.75,outer=T,"Discharge Category")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("CRE","SLE"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c("dodgerblue1","indianred1"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Estuary")
dev.off()

# Flow South --------------------------------------------------------------

# Flow duration curves
# png(filename=paste0(plot.path,"Iteration_2/S351_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,2400);by.y=800;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S351"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S351"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S351"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"EAA - NNR & Hillsboro Canal (S-351)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S352_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1250);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S352"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S352"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S352"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"EAA - WPB Canal (S-352)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S354_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S354"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S354"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S354"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"EAA - Miami Canal (S-354)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

EAA.q=ddply(subset(q.dat,SITE%in%c("S354","S352","S351")),c("Alt","Date","WY"),summarise,TFlow.cfs=sum(FLOW,na.rm=T))

# png(filename=paste0(plot.path,"Iteration_2/EAA_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,6000);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(EAA.q,Alt==alts.sort[1])$TFlow.cfs),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(EAA.q,Alt==alts.sort[2])$TFlow.cfs),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(EAA.q,Alt==alts.sort[i])$TFlow.cfs),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"EAA (S-354 + S-351 + S-352)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()


EAA.q.WY=ddply(EAA.q,c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(TFlow.cfs)))
EAA.q.WY$Alt=factor(EAA.q.WY$Alt,levels=alts.sort)
EAA.q.WY.sum=ddply(EAA.q.WY,"Alt",summarise,mean.val=mean(TFlow.acft))
EAA.q.WY.sum=EAA.q.WY.sum[match(alts.sort,EAA.q.WY.sum$Alt),]
EAA.q.WY.sum$FWO.diff=with(EAA.q.WY.sum,(mean.val-mean.val[1])/mean.val[1])*100
# png(filename=paste0(plot.path,"Iteration_2/EAA_discharge_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,10e5);by.y=2.5e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,EAA.q.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,EAA.q.WY.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(EAA.q.WY,Alt==alts.sort[1])$TFlow.acft),lty=2,col="black")
abline(h=subset(EAA.q.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"EAA (S-354 + S-351 + S-352)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/EAA_discharge_sum.png"),width=3,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));

xlim.val=c(-25,50);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(EAA.q.WY.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(EAA.q.WY.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(EAA.q.WY.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,EAA.q.WY.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"EAA Inflow")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()



# Southern Everglades -----------------------------------------------------
RSM.stg.sites=c("WCA3A_3A-3","WCA3A_3A-2","WCA3A_3A-4","WCA3A_3A-28","S333_US")
RSM.stg.sites=data.frame(SITE=RSM.stg.sites,Station=c("CA3_63","CA3_62","CA3_64","CA3_65","S333_HW"))
wca.stg.dat=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMGL_output.dss"))  
  
  for(i in 1:length(RSM.stg.sites$SITE)){
    paths=paste0("/RSMGL/",RSM.stg.sites$SITE[i],"/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.stg.sites$SITE[i]
    tmp$Alt=alts[j]
    wca.stg.dat=rbind(tmp,wca.stg.dat)
    print(i)
  }
}
wca.stg.dat=merge(wca.stg.dat,RSM.stg.sites,"SITE")

unique(wca.stg.dat$Alt)

wca.stg.dat.xtab=data.frame(cast(wca.stg.dat,Date+Alt~Station,value="STAGE",mean))
wca.stg.dat.xtab$mean_6263=rowMeans(wca.stg.dat.xtab[,c("CA3_62","CA3_63")],na.rm=T)
wca.stg.dat.xtab$mean_636465=rowMeans(wca.stg.dat.xtab[,c("CA3_63","CA3_64","CA3_65")],na.rm=T)
wca.stg.dat.xtab$S333_HW.LT9.2=with(wca.stg.dat.xtab,ifelse(S333_HW<9.2,1,0))

wca.stg.dat.xtab$low.close=with(wca.stg.dat.xtab,ifelse(mean_6263<9.3,1,0))
wca.stg.dat.xtab$high.close=with(wca.stg.dat.xtab,ifelse(mean_6263>11.6,1,0))
wca.stg.dat.xtab$LT9.5=with(wca.stg.dat.xtab,ifelse(mean_636465<9.5,1,0))
wca.stg.dat.xtab$Alt=factor(wca.stg.dat.xtab$Alt,levels=alts.sort)


S333HW.sum=ddply(wca.stg.dat.xtab,c("Alt"),summarise,N.LT9.2=sum(S333_HW.LT9.2))
S333HW.sum=S333HW.sum[match(alts.sort,S333HW.sum$Alt),]
S333HW.sum$FWO.diff=with(S333HW.sum,(N.LT9.2-N.LT9.2[1])/N.LT9.2[1])*100

Low.wca3.sum=ddply(wca.stg.dat.xtab,c("Alt"),summarise,N.LT9.5=sum(LT9.5))
Low.wca3.sum=Low.wca3.sum[match(alts.sort,Low.wca3.sum$Alt),]
Low.wca3.sum$FWO.diff=with(Low.wca3.sum,(N.LT9.5-N.LT9.5[1])/N.LT9.5[1])*100

HiLo.close.sum=ddply(wca.stg.dat.xtab,c("Alt"),summarise,N.low=sum(low.close),N.high=sum(high.close))
HiLo.close.sum=HiLo.close.sum[match(alts.sort,HiLo.close.sum$Alt),]
HiLo.close.sum$FWO.Lo.diff=with(HiLo.close.sum,(N.low-N.low[1])/N.low[1])*100
HiLo.close.sum$ECB.Lo.diff=with(HiLo.close.sum,(N.low-N.low[2])/N.low[2])*100
HiLo.close.sum$FWO.Hi.diff=with(HiLo.close.sum,(N.high-N.high[1])/N.high[1])*100
HiLo.close.sum$ECB.Hi.diff=with(HiLo.close.sum,(N.high-N.high[2])/N.high[2])*100

boxplot(mean_6263~Alt,wca.stg.dat.xtab,outline=F)
# Stage duration curves

# png(filename=paste0(plot.path,"Iteration_2/WCA3_6263_StageDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(8,14);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[1])$mean_6263),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[2])$mean_6263),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[i])$mean_6263),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("bottomleft",legend=c(alts.sort[c(1,2,i)]),
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"WCA-3 (3-62 & 3-63 Gauge Avg.)")}
  box(lwd=1)
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/WCA3_636465_StageDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(7,13);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[1])$mean_636465),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[2])$mean_636465),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[i])$mean_636465),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("bottomleft",legend=c(alts.sort[c(1,2,i)]),
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"WCA-3 (3-63, 3-64 & 3-65 Gauge Avg.)")}
  box(lwd=1)
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/WCA_6263_perdiff.png"),width=5,height=5.25,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,1,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-20,10);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(HiLo.close.sum$FWO.Lo.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(HiLo.close.sum,segments(FWO.Lo.diff,1:8,rep(0,8),1:8))
with(HiLo.close.sum,points(FWO.Lo.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,HiLo.close.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"WCA-3 (3-62 & 3-63 Gauge)",line=1)
mtext(side=3,adj=0,"Freq. Low Water Closure",cex=0.8,col="grey40")
mtext(side=2,line=4,'Model Alternative')

xlim.val=c(-10,20);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(HiLo.close.sum$FWO.Hi.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(HiLo.close.sum,segments(FWO.Hi.diff,1:8,rep(0,8),1:8))
with(HiLo.close.sum,points(FWO.Hi.diff,1:8,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:8,1:8,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=1)
mtext(side=3,adj=0,"Freq. High Water Closure",cex=0.8,col="grey40")
mtext(side=1,outer=T,"Percent Difference to FWO")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/WCA_636465_perdiff.png"),width=3,height=5.25,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,1,1),oma=c(1.5,5,1,0.25));

xlim.val=c(-20,2);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(Low.wca3.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(Low.wca3.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(Low.wca3.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,Low.wca3.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"WCA-3 (3-Gauge Avg.)",line=1)
mtext(side=3,adj=0,"Freq. < 9.5 Ft",cex=0.8,col="grey40")
mtext(side=1,line=1.5,"Percent Difference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/WCA_S333HW_perdiff.png"),width=3,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,0.5,1),oma=c(1.5,5,1,0.25));

xlim.val=c(-30,0);by.x=10;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S333HW.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(S333HW.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(S333HW.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,S333HW.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Freq. S333HW < 9.2 Ft",cex=1,col="black")
mtext(side=1,line=1.5,"Percent Difference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()

# SRS Appendix A ----------------------------------------------------------
wca.stg.dat.xtab=wca.stg.dat.xtab[order(wca.stg.dat.xtab$Alt,wca.stg.dat.xtab$Date),]
wca.stg.dat.xtab$Stg.inc=with(wca.stg.dat.xtab,ave(mean_636465,Alt,FUN=function(x)c(rep(NA,29),diff(x,lag=29))))
wca.stg.dat.xtab$Stg.Gradient=with(wca.stg.dat.xtab,CA3_63-CA3_65)
wca.stg.dat.xtab$Stage.Anteced.90freq=with(wca.stg.dat.xtab,ave(LT9.5,Alt,FUN=function(x)c(rep(NA,89),rollsum(x,90)/90)))
wca.stg.dat.xtab$decimal.yr=lubridate::decimal_date(wca.stg.dat.xtab$Date)

## Equations from COP
walker.coef=data.frame(Trend=-0.027286178,Stg_Gradient=0.043006751,Freq_90Day=0.303630589,Stage2=0.098476045,Stage_Inc=0.12746041,Stage=-2.375222139,Intercept=70.80455867)

S12ABC.TP.FWM=with(wca.stg.dat.xtab,data.frame(Date=Date,Alt=Alt,
                                             FWM=exp((mean_636465*walker.coef$Stage)+(Stg.Gradient*walker.coef$Stg_Gradient)+((mean_636465^2)*walker.coef$Stage2)+(Stage.Anteced.90freq*walker.coef$Freq_90Day)+(Stg.inc*walker.coef$Stage_Inc)+(2017.75*walker.coef$Trend)+walker.coef$Intercept),WQ.Site="S12ABC"))

walker.coef=data.frame(Trend=-0.015901212,Stg_Gradient=0.336072873,Freq_90Day=0.410513468,Stage2=0.055665333,Stage_Inc=0.07686639,Stage=-1.473865744,Intercept=43.16477753)

S12DS333.TP.FWM=with(wca.stg.dat.xtab,data.frame(Date=Date,Alt=Alt,
                                               FWM=exp((mean_636465*walker.coef$Stage)+(Stg.Gradient*walker.coef$Stg_Gradient)+((mean_636465^2)*walker.coef$Stage2)+(Stage.Anteced.90freq*walker.coef$Freq_90Day)+(Stg.inc*walker.coef$Stage_Inc)+(2017.75*walker.coef$Trend)+walker.coef$Intercept),WQ.Site="S12DS333"))


SRS.WQ.mod=rbind(S12ABC.TP.FWM,S12DS333.TP.FWM)

##Discharge
RSM.sites=c(paste0("S12",c("A","B","C","D")),"S333","S333N","S334","S355A","S355B","S356")
alt.srs=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMGL_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMGL/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    alt.srs=rbind(tmp,alt.srs)
    print(i)
  }
}

alt.srs$Q=cfs.to.acftd(alt.srs$FLOW)
alt.srs$Alt=factor(alt.srs$Alt,levels=alts.sort)
####
acft.to.L=function(x)x*1.233e6
ug.to.kg=function(x)x*1e-9
kg.to.ug=function(x)x/1e-9

flow.xtab=reshape2::dcast(alt.srs,Alt+Date~SITE,value.var="Q",mean)
flow.xtab$FedWY=WY(flow.xtab$Date,"Fed")
flow.xtab=subset(flow.xtab,FedWY%in%seq(1966,2016,1))

flow.xtab$S12ABC=rowSums(flow.xtab[,c("S12A","S12B","S12C")],na.rm=T)
flow.xtab$S12DS333=rowSums(flow.xtab[,c("S12D","S333")],na.rm=T)
flow.xtab$S355AB=rowSums(flow.xtab[,c("S355A","S355B")],na.rm=T)

#pseduo method #2
flow.xtab$S333.adj=with(flow.xtab,S333*(1-ifelse((S333+S333N+S355AB+S356)>0,S334/(S333+S333N+S355AB+S356),0)))
flow.xtab$S333N.adj=with(flow.xtab,S333N*(1-ifelse((S333+S333N+S355AB+S356)>0,S334/(S333+S333N+S355AB+S356),0)))
flow.xtab$S355AB.adj=with(flow.xtab,S355AB*(1-ifelse((S333+S333N+S355AB+S356)>0,S334/(S333+S333N+S355AB+S356),0)))
flow.xtab$S356.adj=with(flow.xtab,S356*(1-ifelse((S333+S333N+S355AB+S356)>0,S334/(S333+S333N+S355AB+S356),0)))

flow.xtab$TFlow.acftd=rowSums(flow.xtab[,c("S12ABC","S12D","S333.adj","S333N.adj","S355AB.adj","S356.adj")],na.rm=T)

S12ABC.TP.FWM$S12ABC.FWM=S12ABC.TP.FWM$FWM
S12DS333.TP.FWM$S12DS333.FWM=S12DS333.TP.FWM$FWM

flow.xtab=merge(flow.xtab,S12ABC.TP.FWM[,c("Date","Alt","S12ABC.FWM")],c("Date","Alt"))
flow.xtab=merge(flow.xtab,S12DS333.TP.FWM[,c("Date","Alt","S12DS333.FWM")],c("Date","Alt"))
flow.xtab$S12A.Load.kg=with(flow.xtab,ug.to.kg(S12ABC.FWM*acft.to.L(S12A)))
flow.xtab$S12B.Load.kg=with(flow.xtab,ug.to.kg(S12ABC.FWM*acft.to.L(S12B)))
flow.xtab$S12C.Load.kg=with(flow.xtab,ug.to.kg(S12ABC.FWM*acft.to.L(S12C)))
flow.xtab$S12D.Load.kg=with(flow.xtab,ug.to.kg(S12DS333.FWM*acft.to.L(S12D)))
flow.xtab$S333.Load.kg=with(flow.xtab,ug.to.kg(S12DS333.FWM*acft.to.L(S333.adj)))
flow.xtab$S333N.Load.kg=with(flow.xtab,ug.to.kg(S12DS333.FWM*acft.to.L(S333N.adj)));
flow.xtab$S355AB.Load.kg=with(flow.xtab,ug.to.kg(8*acft.to.L(S355AB.adj)));#based on walkers analysis
flow.xtab$S356.Load.kg=with(flow.xtab,ug.to.kg(6.1*acft.to.L(S356.adj)));#based on walkers analysis

flow.xtab$TLoad.kg=rowSums(flow.xtab[,c("S12A.Load.kg","S12B.Load.kg","S12C.Load.kg","S12D.Load.kg","S333.Load.kg","S333N.Load.kg","S355AB.Load.kg","S356.Load.kg")],na.rm = T)
flow.xtab$FWM=with(flow.xtab,kg.to.ug(TLoad.kg)/acft.to.L(TFlow.acftd))

flow.xtab$week.num=format(flow.xtab$Date,"%V")
# biweekly.WQ=subset(flow.xtab,week.num%in%seq(1,53,2)&format(Date,"%A")=="Tuesday")

WY.FWM=ddply(flow.xtab,c("Alt","FedWY"),summarise,TFlow=sum(TFlow.acftd,na.rm=T),TLoad=sum(TLoad.kg,na.rm=T))
WY.FWM$FWM=with(WY.FWM,kg.to.ug(TLoad)/acft.to.L(TFlow))
WY.FWM$Alt=factor(WY.FWM$Alt,levels=alts.sort)
# WY.FWM=merge(WY.FWM,ddply(WY.FWM,"Alt",summarise,MeanFWM=mean(FWM,na.rm=T)),"Alt")
# WY.FWM$RF_FWM=with(WY.FWM,9.7/MeanFWM);#Rescale to Fed WY 2017 SRS FWM (method 2)
# WY.FWM$ResFWM=with(WY.FWM,RF_FWM*FWM)

plot(FWM~FedWY,subset(WY.FWM,Alt=="CC"))
with(subset(WY.FWM,Alt=="NA25"),points(FedWY,FWM,pch=21,bg="red"))


WY.FWM.sum=ddply(WY.FWM,"Alt",summarise,mean.val=mean(FWM),median.val=median(FWM))
WY.FWM.sum=WY.FWM.sum[match(alts.sort,WY.FWM.sum$Alt),]
WY.FWM.sum$FWO.diff=with(WY.FWM.sum,(mean.val-mean.val[1])/mean.val[1])*100

# png(filename=paste0(plot.path,"Iteration_2/SRS_FWM_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(FWM~Alt,WY.FWM,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,WY.FWM.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(WY.FWM.sum,Alt==alts.sort[1])$median.val,lty=2,col="black")
abline(h=subset(WY.FWM.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SRS (S12s+[(S333s+S355AB+S356)-S334])")
mtext(side=2,line=2.25,"TP FWM (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/SRS_FWM_perdiff.png"),width=3,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,1,1),oma=c(1.5,5,1,0.25));

xlim.val=c(-15,0);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(WY.FWM.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(WY.FWM.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(WY.FWM.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,WY.FWM.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"SRS FWM",cex=1,col="black",line=1)
mtext(side=1,line=2,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"FLWY 1966 - 2016",cex=0.75)
dev.off()



# Discharge to WCA3 -------------------------------------------------------
##Discharge
RSM.sites=c("S8","S150")
wca3.q=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    wca3.q=rbind(tmp,wca3.q)
    print(i)
  }
}

RSM.sites=c(paste0("S11",LETTERS[1:3]))
S11.q=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMGL_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMGL/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    S11.q=rbind(tmp,S11.q)
    print(i)
  }
}

wca3.q=rbind(wca3.q,S11.q)

head(wca3.q)
wca3.q.total=ddply(wca3.q,c("Date","Alt"),summarise,TFlow=sum(FLOW))
wca3.q.total=wca3.q.total[order(wca3.q.total$Alt,wca3.q.total$Date),]
wca3.q.total$WY=WY(wca3.q.total$Date)
wca3.q.total=subset(wca3.q.total,WY%in%WYs);# Full Florida WY (MAy - April) 
wca3.q.total$area="WCA3Inflow"

wca3.inQ.WY=ddply(wca3.q.total,c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(TFlow)))
wca3.inQ.WY$Alt=factor(wca3.inQ.WY$Alt,levels=alts.sort)
wca3.inQ.WY.sum=ddply(wca3.inQ.WY,"Alt",summarise,mean.val=mean(TFlow.acft),median.val=median(TFlow.acft))
wca3.inQ.WY.sum=wca3.inQ.WY.sum[match(alts.sort,wca3.inQ.WY.sum$Alt),]

srs.sites=c(paste0("S12",c("A","B","C","D")),"S333","S333N")
alt.srs$WY=WY(alt.srs$Date)
SRS.inQ.WY=ddply(subset(alt.srs,SITE%in%srs.sites),c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(FLOW)))
SRS.inQ.WY.sum=ddply(SRS.inQ.WY,"Alt",summarise,mean.val=mean(TFlow.acft),median.val=median(TFlow.acft))
SRS.inQ.WY.sum=SRS.inQ.WY.sum[match(alts.sort,SRS.inQ.WY.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/WCA3_WYQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.25,0.75,0.5,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,210e4);by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,wca3.inQ.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,wca3.inQ.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(wca3.inQ.WY.sum,Alt==alts.sort[1])$median.val,lty=2,col="black")
abline(h=subset(wca3.inQ.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"WCA-3A Inflow (S-8 + S-150 + S-11s)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,205e4);by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,SRS.inQ.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,SRS.inQ.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(SRS.inQ.WY.sum,Alt==alts.sort[1])$median.val,lty=2,col="black")
abline(h=subset(SRS.inQ.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"WCA-3A Outflow (S-12s+S-333+S-333N)")

mtext(side=2,outer=T,line=2,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()