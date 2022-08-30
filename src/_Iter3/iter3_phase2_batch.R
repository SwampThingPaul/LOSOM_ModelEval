## 
## LOSOM
##
## Iteration 3 - Phase 2
## Batch Results
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

## Paths
wd="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]


# -------------------------------------------------------------------------
dat1=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch1_13Oct2021.csv"),skip=22)[,1:185]
dat2=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch2_13Oct2021.csv"),skip=22)[,1:185]
dat3=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch3_13Oct2021.csv"),skip=22)[,1:185]

baselines=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch1_13Oct2021.csv"),skip=17,header=F)[1:3,1:185]
colnames(baselines)<-names(dat1)

baselines[,3:ncol(baselines)]=sapply(baselines[,3:ncol(baselines)],FUN=function(x) as.numeric(sub("%","",x)))

dat1$batch=1
dat2$batch=2
dat3$batch=3

# names(dat1)
# names(dat2)
# names(dat3)

dat=rbind(
  subset(dat1,is.na(Index)==F),
  subset(dat2,is.na(Index)==F),
  subset(dat3,is.na(Index)==F)
  )
unique(dat$Model)
## Dealing with character values as percent
# test=dat[1,]
# test[,c(7:11,14:16,121:126,130:144)]=as.numeric(sub("%","",test[,c(7:11,14:16,121:126,130:144)]))
# test2=dat[2,]
# sapply(test2[,3:ncol(dat)],FUN=function(x) as.numeric(sub("%","",x)))

dat[,3:ncol(dat)]=sapply(dat[,3:ncol(dat)],FUN=function(x) as.numeric(sub("%","",x)))


# Explore plots -----------------------------------------------------------
# CRE Number of 14-day moving average events >= 750 and < 2100 cfs.
plot(PM31~Index,dat)
abline(h=subset(baselines,Model=="CCTSP")$PM31,col="red")
abline(h=subset(baselines,Model=="NA25")$PM31,col="blue")

# CRE LOK triggered stress flow events >= 2100 and < 2600 cfs.
plot(PM37~Index,dat)
abline(h=subset(baselines,Model=="CCTSP")$PM37,col="red")
abline(h=subset(baselines,Model=="NA25")$PM37,col="blue")

# CRE LOK triggered damaging flow events > 2600 cfs.
plot(PM38~Index,dat,ylim=c(0,160))
abline(h=subset(baselines,Model=="CCTSP")$PM38,col="red")
abline(h=subset(baselines,Model=="NA25")$PM38,col="blue")

# CRE Number of 14-day moving average events >= 6500 cfs.
plot(PM36~Index,dat,ylim=c(0,120))
abline(h=subset(baselines,Model=="CCTSP")$PM36,col="red")
abline(h=subset(baselines,Model=="NA25")$PM36,col="blue")

# CRE Average annual flow for S77 regulatory discharge.
plot(PM40~Index,dat)
abline(h=subset(baselines,Model=="CCTSP")$PM40,col="red")
abline(h=subset(baselines,Model=="NA25")$PM40,col="blue")

# CRE Max number of consecutive months at S79 in > 2100 cfs category.
plot(PM41~Index,dat)
abline(h=subset(baselines,Model=="CCTSP")$PM41,col="red")
abline(h=subset(baselines,Model=="NA25")$PM41,col="blue")

# CRE Mean number of consecutive months at S79 in > 2100 cfs category.
plot(PM42~Index,dat)
abline(h=subset(baselines,Model=="CCTSP")$PM42,col="red")
abline(h=subset(baselines,Model=="NA25")$PM42,col="blue")

# Number of times > 3 consecutive months at S79 in > 2100 cfs category.
plot(PM43~Index,dat)
abline(h=subset(baselines,Model=="CCTSP")$PM43,col="red")
abline(h=subset(baselines,Model=="NA25")$PM43,col="blue")

# LOK Percent of POR when LOK stage > 17 ft, NGVD.
plot(PM5~Index,dat)
abline(h=subset(baselines,Model=="CCTSP")$PM5,col="red")
abline(h=subset(baselines,Model=="NA25")$PM5,col="blue")

# LOK Count of weighted exceedances (penalty) above LOK stage envelope. 
## Criteria based on previous year's stage to trigger shifting envelopes 
## on 1-Jan each year.
plot(PM11~Index,dat)
abline(h=subset(baselines,Model=="CCTSP")$PM11,col="red")
abline(h=subset(baselines,Model=="NA25")$PM11,col="blue")



# General Premise for screening...we want things better than CC 

cols=c(rgb(0/255,132/255,80/255),rgb(239/255,183/255,0/255),rgb(184/255,29/255,19/255))

# png(filename=paste0(plot.path,"Iteration3_Batch/CRE_screen.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(4,3.5,2,1),lwd=0.5);
layout(matrix(1:5,5,1,byrow=T))
xlim.val=c(0,279800);by.x=100000;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

# CRE Number of 14-day moving average events >= 750 and < 2100 cfs.
ylim.val=c(300,1000);by.y=300;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM31~Index,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM31
FWO.val=subset(baselines,Model=="NA25")$PM31
with(subset(dat,PM31>=CC.val),points(Index,PM31,pch=21,bg=adjustcolor(cols[1],0.1),col=adjustcolor(cols[1],0.1)))
with(subset(dat,PM31<CC.val&PM31>=FWO.val),points(Index,PM31,pch=21,bg=adjustcolor(cols[2],0.1),col=adjustcolor(cols[2],0.1)))
with(subset(dat,PM31<FWO.val),points(Index,PM31,pch=21,bg=adjustcolor(cols[3],0.1),col=adjustcolor(cols[3],0.1)))
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="black",lwd=1.5)
# axis_fun(1,xmaj,xmin,xmaj/1000,line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=1,line=2,"Model Index")
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=0.8)
mtext(side=3,adj=0,"Optimum (750 - 2100 cfs)",cex=0.8)
legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
       pch=NA,lwd=2,lty=1,
       pt.bg=NA,col=c("black","indianred1"),
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)


# CRE LOK triggered stress flow events >= 2100 and < 2600 cfs.
ylim.val=c(0,500);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM37~Index,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM37
FWO.val=subset(baselines,Model=="NA25")$PM37
with(subset(dat,PM37<FWO.val),points(Index,PM37,pch=21,bg=adjustcolor(cols[1],0.1),col=adjustcolor(cols[1],0.1)))
with(subset(dat,PM37>=FWO.val&PM37<CC.val),points(Index,PM37,pch=21,bg=adjustcolor(cols[2],0.1),col=adjustcolor(cols[2],0.1)))
with(subset(dat,PM37>CC.val),points(Index,PM37,pch=21,bg=adjustcolor(cols[3],0.1),col=adjustcolor(cols[3],0.1)))
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="black",lwd=1.5)
# axis_fun(1,xmaj,xmin,xmaj/1000,line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=1,line=2,"Model Index")
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=0.8)
mtext(side=3,adj=0,"Stress from LOK (2100 - 2600 cfs)",cex=0.8)

# CRE LOK triggered damaging flow events > 2600 cfs.
ylim.val=c(0,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM38~Index,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
# with(dat,points(Index,PM38))
CC.val=subset(baselines,Model=="CCTSP")$PM38
FWO.val=subset(baselines,Model=="NA25")$PM38
with(subset(dat,PM38<FWO.val),points(Index,PM38,pch=21,bg=adjustcolor(cols[1],0.1),col=adjustcolor(cols[1],0.1)))
with(subset(dat,PM38>=FWO.val&PM38<CC.val),points(Index,PM38,pch=21,bg=adjustcolor(cols[2],0.1),col=adjustcolor(cols[2],0.1)))
with(subset(dat,PM38>CC.val),points(Index,PM38,pch=21,bg=adjustcolor(cols[3],0.1),col=adjustcolor(cols[3],0.1)))
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="black",lwd=1.5)
# axis_fun(1,xmaj,xmin,xmaj/1000,line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=1,line=2,"Model Index")
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=0.8)
mtext(side=3,adj=0,"Damaging from LOK (> 2600 cfs)",cex=0.8)

# CRE Number of 14-day moving average events >= 6500 cfs.
ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM36~Index,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
# with(dat,points(Index,PM36))
CC.val=subset(baselines,Model=="CCTSP")$PM36
FWO.val=subset(baselines,Model=="NA25")$PM36
with(subset(dat,PM36<FWO.val),points(Index,PM36,pch=21,bg=adjustcolor(cols[1],0.1),col=adjustcolor(cols[1],0.1)))
with(subset(dat,PM36>=FWO.val&PM36<CC.val),points(Index,PM36,pch=21,bg=adjustcolor(cols[2],0.1),col=adjustcolor(cols[2],0.1)))
with(subset(dat,PM36>CC.val),points(Index,PM36,pch=21,bg=adjustcolor(cols[3],0.1),col=adjustcolor(cols[3],0.1)))
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="black",lwd=1.5)
# axis_fun(1,xmaj,xmin,xmaj/1000,line=-0.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=1,line=2,"Model Index")
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=0.8)
mtext(side=3,adj=0,"Extreme (> 6500 cfs)",cex=0.8)

# LOK Percent of POR when LOK stage > 17 ft, NGVD.
ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM5~Index,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
# with(dat,points(Index,PM5))
CC.val=subset(baselines,Model=="CCTSP")$PM5
FWO.val=subset(baselines,Model=="NA25")$PM5
with(subset(dat,PM5<FWO.val),points(Index,PM5,pch=21,bg=adjustcolor(cols[1],0.1),col=adjustcolor(cols[1],0.1)))
with(subset(dat,PM5>=FWO.val&PM5<CC.val),points(Index,PM5,pch=21,bg=adjustcolor(cols[2],0.1),col=adjustcolor(cols[2],0.1)))
with(subset(dat,PM5>CC.val),points(Index,PM5,pch=21,bg=adjustcolor(cols[3],0.1),col=adjustcolor(cols[3],0.1)))
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="black",lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj/1000,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Model Index (x1000)")
mtext(side=2,line=2.5,"Percent of\nTime",cex=0.8)
mtext(side=3,adj=0,"% LOK Stage >17 Ft NGVD29",cex=0.8)
dev.off()



# Screening ---------------------------------------------------------------
CC.val=subset(baselines,Model=="CCTSP")
FWO.val=subset(baselines,Model=="NA25")

b1.screen=subset(dat,
                 PM31>CC.val$PM31*1.05&
                   PM37<CC.val$PM37*0.95&
                   PM38<CC.val$PM38*0.95&
                   PM36<CC.val$PM36*0.95&
                   PM5<CC.val$PM5*0.95&
                   batch==1)
b1.screen=b1.screen[order(-b1.screen$PM31,b1.screen$PM37,b1.screen$PM38,b1.screen$PM36,b1.screen$PM5),]
b1.screen=b1.screen[1:8,]
b1.screen$Index

b2.screen=subset(dat,
                 PM31>CC.val$PM31*1.05&
                   PM37<CC.val$PM37*0.95&
                   PM38<CC.val$PM38*0.95&
                   PM36<CC.val$PM36*0.95&
                   PM5<CC.val$PM5*0.95&
                   batch==2)
b2.screen=b2.screen[order(-b2.screen$PM31,b2.screen$PM37,b2.screen$PM38,b2.screen$PM36,b2.screen$PM5),]
b2.screen=b2.screen[1:8,]
b2.screen$Index

b3.screen=subset(dat,
                 PM31>CC.val$PM31*1.05&
                   PM37<CC.val$PM37*0.95&
                   PM38<CC.val$PM38*0.95&
                   PM36<CC.val$PM36*0.95&
                   PM5<CC.val$PM5*0.95&
                   batch==3)
b3.screen=b3.screen[order(-b3.screen$PM31,b3.screen$PM37,b3.screen$PM38,b3.screen$PM36,b3.screen$PM5),]
b3.screen=b3.screen[1:8,]
b3.screen$Index

nrow(subset(dat,
       PM31>CC.val$PM31*1.05&
         PM37<CC.val$PM37*0.95&
         PM38<CC.val$PM38*0.95&
         PM36<CC.val$PM36*0.95&
         PM5<CC.val$PM5*0.95))

dat.screen=rbind(b1.screen,b2.screen,b3.screen)
# write.csv(dat.screen,paste0(export.path,"Iteration3/Iter3_P2_screen.csv"),row.names = F)
dat.screen$plot.ID=1:24


# png(filename=paste0(plot.path,"Iteration3_Batch/PMScreen_dist.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1),oma=c(1,3.5,1,1),lwd=0.5);
layout(matrix(1:5,5,1,byrow=T))

text.cex=0.7
range(dat$PM31)
xlim.val=c(300,1000);by.x=200;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,2.5e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM31,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen,points(PM31,rep(max(x$counts)+max(x$counts)*0.05,length(PM31)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM31,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"CRE - Optimum (750 - 2100 cfs)",cex=text.cex)
# mtext(side=3,adj=0,line=1.25,"Caloosahatchee")
legend("topleft",legend=c("All Data","Screened Models","NA25 (FWO)","CC (TSP)"),
       pch=c(22,21,NA,NA),lwd=0.1,lty=c(0,0,1,1),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"red"),col=c("black","black","grey","red"),
       pt.cex=1.5,ncol=3,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

range(dat$PM37)
xlim.val=c(0,500);by.x=250;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,8.5e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM37,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen,points(PM37,rep(max(x$counts)+max(x$counts)*0.05,length(PM37)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM37,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"CRE - Stress from LOK (2100 - 2600 cfs)",cex=text.cex)

range(dat$PM38)
xlim.val=c(0,200);by.x=50;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,2.5e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM38,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen,points(PM38,rep(max(x$counts)+max(x$counts)*0.05,length(PM38)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM38,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"CRE - Damaging from LOK (2100 - 2600 cfs)",cex=text.cex)

range(dat$PM36)
xlim.val=c(0,100);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1.75e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM36,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen,points(PM36,rep(max(x$counts)+max(x$counts)*0.05,length(PM36)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM36,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"CRE - Extreme (> 6500 cfs)",cex=text.cex)
mtext(side=1,line=1.5,"Count of 14-Day Periods",cex=text.cex)

range(dat$PM5)
xlim.val=c(0,20);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,10e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM5,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen,points(PM5,rep(max(x$counts)+max(x$counts)*0.075,length(PM36)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM5,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,scientific = F));box(lwd=1)
mtext(side=3,adj=0,"% LOK Stage >17 Ft NGVD29",cex=text.cex)
mtext(side=1,line=1.75,"% of Time",cex=text.cex)

mtext(side=2,outer=T,line=2,"Frequency")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/CRE_screen_subset.png"),width=4,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(5,3.5,2,1),lwd=0.5);
layout(matrix(1:6,6,1,byrow=T))
xlim.val=c(1,24);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
text.cex=0.7

# CRE Number of 14-day moving average events >= 750 and < 2100 cfs.
ylim.val=c(400,1000);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM31~plot.ID,dat.screen,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM31
FWO.val=subset(baselines,Model=="NA25")$PM31
with(dat.screen,points(plot.ID,PM31,pch=21,bg="dodgerblue1"))
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=CC.val+CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=text.cex)
mtext(side=3,adj=0,"Optimum (750 - 2100 cfs)",cex=text.cex)
abline(v=c(8.5,16.5))
legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)","CC (TSP) \u00B1 5%"),
       pch=NA,lwd=2,lty=c(1,1,2),
       pt.bg=NA,col=c("grey","indianred1","indianred1"),
       pt.cex=1.5,ncol=3,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

# CRE LOK triggered stress flow events >= 2100 and < 2600 cfs.
ylim.val=c(0,500);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM37~plot.ID,dat.screen,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM37
FWO.val=subset(baselines,Model=="NA25")$PM37
with(dat.screen,points(plot.ID,PM37,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
# axis_fun(1,xmaj,xmin,xmaj/1000,line=-0.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=1,line=2,"Model Index")
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=text.cex)
mtext(side=3,adj=0,"Stress from LOK (2100 - 2600 cfs)",cex=text.cex)
abline(v=c(8.5,16.5))

# CRE LOK triggered damaging flow events > 2600 cfs.
ylim.val=c(0,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM38~plot.ID,dat.screen,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM38
FWO.val=subset(baselines,Model=="NA25")$PM38
with(dat.screen,points(plot.ID,PM38,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=text.cex)
mtext(side=3,adj=0,"Damaging from LOK (> 2600 cfs)",cex=text.cex)
abline(v=c(8.5,16.5))

# CRE Number of 14-day moving average events >= 6500 cfs.
ylim.val=c(25,75);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM36~plot.ID,dat.screen,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM36
FWO.val=subset(baselines,Model=="NA25")$PM36
with(dat.screen,points(plot.ID,PM36,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=text.cex)
mtext(side=3,adj=0,"Extreme (> 6500 cfs)",cex=text.cex)
abline(v=c(8.5,16.5))

# S77 Regulatory Flow
ylim.val=c(300,600);by.y=150;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM40~plot.ID,dat.screen,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM40
FWO.val=subset(baselines,Model=="NA25")$PM40
with(dat.screen,points(plot.ID,PM40,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Discharge\n(k Ac-Ft Yr\u207B\u00B9)",cex=text.cex)
mtext(side=3,adj=0,"S-77 Regulatory Discharge",cex=text.cex)
abline(v=c(8.5,16.5))

# LOK Percent of POR when LOK stage > 17 ft, NGVD.
ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM5~plot.ID,dat.screen,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM5
FWO.val=subset(baselines,Model=="NA25")$PM5
with(dat.screen,points(plot.ID,PM5,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.5,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,dat.screen$Index,las=2)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=4,"Model Index")
mtext(side=2,line=2.5,"Percent of\nTime",cex=text.cex)
mtext(side=3,adj=0,"% LOK Stage >17 Ft NGVD29",cex=text.cex)
abline(v=c(8.5,16.5))
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/SalEnv_subset.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:10,5,2,byrow=F))
xlim.val=c(1,24);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(600,1000,500,200,75)
ylim.min=c(200,400,0,0,25)
by.y.val=c(200,250,250,100,25)
vars=paste0("PM",c(30,31,37,38,36))
labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress from LOK (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Extreme (>6500 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen$plot.ID,dat.screen[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,dat.screen$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Count of 14-Day Periods",outer=T)

ylim.max=c(200,1000,350,300,200)
ylim.min=c(100,600,0,0,100)
by.y.val=c(50,200,100,150,50)
vars=paste0("PM",c(80,81,86,87,85))
labs=c("Low (<150 cfs)","Optimum (150 - 1400 cfs)","Stress from LOK (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Extreme (>4000 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen$plot.ID,dat.screen[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,dat.screen$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
}

mtext(side=1,line=2,"Model Index",outer=T)
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Batch/South_subset.png"),width=4,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:4,4,1,byrow=F))
xlim.val=c(1,24);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(250,100,100,100)
ylim.min=c(0,0,0,0)
by.y.val=c(125,50,50,50)
vars=paste0("PM",c(21,22,23,24))
labs=c("Flow South","Flow south - Wet Season","Flow south - Early Dry Season","Flow south - Late Dry Season")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen$plot.ID,dat.screen[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,dat.screen$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Everglades")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Avg Annual Discharge (k Ac-Ft Yr\u207B\u00B9)",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_subset.png"),width=4,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:4,4,1,byrow=F))
xlim.val=c(1,24);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(1.05,20,10,30)
ylim.min=c(0,0,0,10)
by.y.val=c(0.5,10,5,10)
vars=paste0("PM",c(5,6,7,13))
labs=c("% LOK Stage >17 Ft NGVD29","% LOK Stage >16 Ft NGVD29","% LOK Stage <10 Ft NGVD29","% within Stage Envelope")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen$plot.ID,dat.screen[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,dat.screen$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"LOK")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/WS_subset.png"),width=4.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:6,6,1,byrow=F))
xlim.val=c(1,24);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.min=c(0,0,0,0,0,0)
ylim.max=c(35,100,50,12,5,5)
by.y.val=ylim.max/2
vars=paste0("PM",c(124,126,127,130,131,132))
labs=c("LOSA Cutbacks","LOSA Duration","LOSA Severity","LOSA Sup. Demand not met EAA & Non-EAA","STOF - BCYP Demand Not Met", "SOTF - BR Demand Not Met")
xlabs=c("Weighted Avg %","Count","Score","Percent","Percent","Percent")
for(i in 1:6){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen$plot.ID,dat.screen[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==6){axis_fun(1,xmaj,xmaj,dat.screen$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  mtext(side=2,line=2.5,xlabs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Water Supply")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

cols=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))
vars=paste0("PM",c(21,40,88,118))
ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/AvgFloodControl.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],dat.screen[,c("Index",vars)])

tmp2=tmp[,vars]

x=barplot(t(tmp2),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,nrow(tmp)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp2),beside=F,col=cols,ylim=ylim.val,axes=F,ann=F,names.arg =rep(NA,nrow(tmp)),add=T)
with(tmp,text(x,PM21/2,round(PM21,0),cex=0.7,col="white"))
with(tmp,text(x,PM21+(((PM40+PM21)-PM21)/2),round(PM40,0),cex=0.7))
with(tmp,text(x,(PM21+PM40)+(((PM40+PM21+PM88)-(PM21+PM40))/2),round(PM88,0),cex=0.7))
with(tmp,text(x,PM40+PM21+PM88+PM118,round(PM118,0),pos=3,offset=0.1,cex=0.7))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,tmp$Index,line=-0.25,las=2,cex=0.8);box(lwd=1)
mtext(side=2,line=2,"Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=2.75,"Model Index")

par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0,0,legend=c("Water Conservation Areas (PM21)","Caloosahatchee River (PM40)","St. Lucie River (PM88)","Lake Worth Lagoon (PM118)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols,
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=0.5)
text(1,0,"Iteration 3 screened batch results. Mean annual flood control\nreleases from Lake Okeechobee for the 52 year (1965 - 2016)\nsimulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()



# Screening#2 -------------------------------------------------------------
CC.val=subset(baselines,Model=="CCTSP")
FWO.val=subset(baselines,Model=="NA25")

b1.screen2=subset(dat,
                   PM37<CC.val$PM37&
                   PM38<CC.val$PM38&
                   PM36<CC.val$PM36&
                   PM85<=CC.val$PM85&
                   PM5<CC.val$PM5&
                   batch==1)
b1.screen2=b1.screen2[order(b1.screen2$PM37,b1.screen2$PM38,b1.screen2$PM36,b1.screen2$PM85,b1.screen2$PM5),]
b1.screen2=b1.screen2[1:8,]
b1.screen2$Index

b2.screen2=subset(dat,
                  PM37<CC.val$PM37&
                    PM38<CC.val$PM38&
                    PM36<CC.val$PM36&
                    PM85<=CC.val$PM85&
                    PM5<CC.val$PM5&
                   batch==2)
b2.screen=b2.screen[order(b2.screen2$PM37,b2.screen2$PM38,b2.screen2$PM36,b2.screen2$PM85,b2.screen2$PM5),]
b2.screen=b2.screen[1:8,]
b2.screen2$Index

b3.screen2=subset(dat,
                  PM37<CC.val$PM37&
                    PM38<CC.val$PM38&
                    PM36<CC.val$PM36&
                    PM85<=CC.val$PM85&
                    PM5<CC.val$PM5&
                   batch==3)
b3.screen2=b3.screen2[order(b3.screen2$PM37,b3.screen2$PM38,b3.screen2$PM36,b3.screen2$PM85,b3.screen2$PM5),]
b3.screen2=b3.screen2[1:8,]
b3.screen2$Index

nrow(subset(dat,
            PM37<CC.val$PM37&
              PM38<CC.val$PM38&
              PM36<CC.val$PM36&
              PM85<=CC.val$PM85&
              PM5<CC.val$PM5))

dat.screen2=rbind(b1.screen2,b2.screen2,b3.screen2)
dat.screen2=subset(dat.screen2,is.na(Index)==F)
# write.csv(dat.screen2,paste0(export.path,"Iteration3/Iter3_P2_screen2.csv"),row.names = F)
dat.screen2$plot.ID=1:nrow(dat.screen2)


# png(filename=paste0(plot.path,"Iteration3_Batch/PMScreen_dist_screen2.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1),oma=c(1,3.5,1,1),lwd=0.5);
layout(matrix(1:5,5,1,byrow=T))

text.cex=0.7
# range(dat$PM31)
# xlim.val=c(300,1000);by.x=200;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
# ylim.val=c(0,2.5e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# x=hist(dat$PM31,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
# with(dat.screen2,points(PM31,rep(max(x$counts)+max(x$counts)*0.05,length(PM31)),pch=21,bg="red"))
# abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM31,col=c("grey","red"),lwd=1.5)
# axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
# axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=3,adj=0,"CRE - Optimum (750 - 2100 cfs)",cex=text.cex)
# # mtext(side=3,adj=0,line=1.25,"Caloosahatchee")


range(dat$PM37)
xlim.val=c(0,500);by.x=250;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,8.5e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM37,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen2,points(PM37,rep(max(x$counts)+max(x$counts)*0.05,length(PM37)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM37,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"CRE - Stress from LOK (2100 - 2600 cfs)",cex=text.cex)
legend("topright",legend=c("All Data","Screened Models","NA25 (FWO)","CC (TSP)"),
       pch=c(22,21,NA,NA),lwd=0.1,lty=c(0,0,1,1),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"red"),col=c("black","black","grey","red"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

range(dat$PM38)
xlim.val=c(0,200);by.x=50;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,2.5e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM38,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen2,points(PM38,rep(max(x$counts)+max(x$counts)*0.05,length(PM38)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM38,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"CRE - Damaging from LOK (2100 - 2600 cfs)",cex=text.cex)

range(dat$PM36)
xlim.val=c(0,100);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1.75e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM36,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen2,points(PM36,rep(max(x$counts)+max(x$counts)*0.05,length(PM36)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM36,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"CRE - Extreme (> 6500 cfs)",cex=text.cex)

range(dat$PM85)
xlim.val=c(0,300);by.x=100;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1.8e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM85,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen2,points(PM85,rep(max(x$counts)+max(x$counts)*0.05,length(PM85)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM85,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"SLE - Extreme (> 4000 cfs)",cex=text.cex)
mtext(side=1,line=1.5,"Count of 14-Day Periods",cex=text.cex)

range(dat$PM5)
xlim.val=c(0,20);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,10e4);by.y=ylim.val[2]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=hist(dat$PM5,yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F,breaks=50,col=adjustcolor("dodgerblue1",0.5))
with(dat.screen2,points(PM5,rep(max(x$counts)+max(x$counts)*0.075,length(PM36)),pch=21,bg="red"))
abline(v=subset(baselines,Model%in%c("NA25","CCTSP"))$PM5,col=c("grey","red"),lwd=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,scientific = F));box(lwd=1)
mtext(side=3,adj=0,"% LOK Stage >17 Ft NGVD29",cex=text.cex)
mtext(side=1,line=1.75,"% of Time",cex=text.cex)

mtext(side=2,outer=T,line=2,"Frequency")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/CRE_screen_subset2.png"),width=4,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(5,3.5,2,1),lwd=0.5);
layout(matrix(1:6,6,1,byrow=T))
xlim.val=c(1,nrow(dat.screen2));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
text.cex=0.7

# CRE Number of 14-day moving average events >= 750 and < 2100 cfs.
ylim.val=c(400,1000);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM31~plot.ID,dat.screen2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM31
FWO.val=subset(baselines,Model=="NA25")$PM31
with(dat.screen2,points(plot.ID,PM31,pch=21,bg="dodgerblue1"))
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=CC.val+CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=text.cex)
mtext(side=3,adj=0,"Optimum (750 - 2100 cfs)",cex=text.cex)
legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)","CC (TSP) \u00B1 5%"),
       pch=NA,lwd=2,lty=c(1,1,2),
       pt.bg=NA,col=c("grey","indianred1","indianred1"),
       pt.cex=1.5,ncol=3,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

# CRE LOK triggered stress flow events >= 2100 and < 2600 cfs.
ylim.val=c(0,500);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM37~plot.ID,dat.screen2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM37
FWO.val=subset(baselines,Model=="NA25")$PM37
with(dat.screen2,points(plot.ID,PM37,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
# axis_fun(1,xmaj,xmin,xmaj/1000,line=-0.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=1,line=2,"Model Index")
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=text.cex)
mtext(side=3,adj=0,"Stress from LOK (2100 - 2600 cfs)",cex=text.cex)

# CRE LOK triggered damaging flow events > 2600 cfs.
ylim.val=c(0,200);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM38~plot.ID,dat.screen2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM38
FWO.val=subset(baselines,Model=="NA25")$PM38
with(dat.screen2,points(plot.ID,PM38,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=text.cex)
mtext(side=3,adj=0,"Damaging from LOK (> 2600 cfs)",cex=text.cex)

# CRE Number of 14-day moving average events >= 6500 cfs.
ylim.val=c(25,75);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM36~plot.ID,dat.screen2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM36
FWO.val=subset(baselines,Model=="NA25")$PM36
with(dat.screen2,points(plot.ID,PM36,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=text.cex)
mtext(side=3,adj=0,"Extreme (> 6500 cfs)",cex=text.cex)

# S77 Regulatory Flow
ylim.val=c(300,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM40~plot.ID,dat.screen2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM40
FWO.val=subset(baselines,Model=="NA25")$PM40
with(dat.screen2,points(plot.ID,PM40,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.05,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Discharge\n(k Ac-Ft Yr\u207B\u00B9)",cex=text.cex)
mtext(side=3,adj=0,"S-77 Regulatory Discharge",cex=text.cex)

# LOK Percent of POR when LOK stage > 17 ft, NGVD.
ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM5~plot.ID,dat.screen2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
CC.val=subset(baselines,Model=="CCTSP")$PM5
FWO.val=subset(baselines,Model=="NA25")$PM5
with(dat.screen2,points(plot.ID,PM5,pch=21,bg="dodgerblue1"))
abline(h=CC.val-CC.val*0.5,col="indianred1",lwd=1.5,lty=2)
abline(h=CC.val,col="indianred1",lwd=1.5)
abline(h=FWO.val,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,dat.screen2$Index,las=2)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=4,"Model Index")
mtext(side=2,line=2.5,"Percent of\nTime",cex=text.cex)
mtext(side=3,adj=0,"% LOK Stage >17 Ft NGVD29",cex=text.cex)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/SalEnv_subset2.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:10,5,2,byrow=F))
xlim.val=c(1,nrow(dat.screen2));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(600,1000,500,200,75)
ylim.min=c(200,400,0,0,25)
by.y.val=c(200,250,250,100,25)
vars=paste0("PM",c(30,31,37,38,36))
labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress from LOK (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Extreme (>6500 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen2[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen2$plot.ID,dat.screen2[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,dat.screen2$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Count of 14-Day Periods",outer=T)

ylim.max=c(200,1000,350,300,200)
ylim.min=c(100,600,0,0,100)
by.y.val=c(50,200,100,150,50)
vars=paste0("PM",c(80,81,86,87,85))
labs=c("Low (<150 cfs)","Optimum (150 - 1400 cfs)","Stress from LOK (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Extreme (>4000 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen2[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen2$plot.ID,dat.screen2[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,dat.screen2$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
}

mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_subset2.png"),width=4,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:4,4,1,byrow=F))
xlim.val=c(1,nrow(dat.screen2));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(1.05,20,10,30)
ylim.min=c(0,0,0,10)
by.y.val=c(0.5,10,5,10)
vars=paste0("PM",c(5,6,7,13))
labs=c("% LOK Stage >17 Ft NGVD29","% LOK Stage >16 Ft NGVD29","% LOK Stage <10 Ft NGVD29","% within Stage Envelope")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen2[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen2$plot.ID,dat.screen2[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,dat.screen2$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"LOK")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/WS_subset2.png"),width=4.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:6,6,1,byrow=F))
xlim.val=c(1,nrow(dat.screen2));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.min=c(0,0,0,0,0,0)
ylim.max=c(35,100,50,12,5,5)
by.y.val=ylim.max/2
vars=paste0("PM",c(124,126,127,130,131,132))
labs=c("LOSA Cutbacks","LOSA Duration","LOSA Severity","LOSA Sup. Demand not met EAA & Non-EAA","STOF - BCYP Demand Not Met", "SOTF - BR Demand Not Met")
xlabs=c("Weighted Avg %","Count","Score","Percent","Percent","Percent")
for(i in 1:6){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(dat.screen2[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(dat.screen2$plot.ID,dat.screen2[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==6){axis_fun(1,xmaj,xmaj,dat.screen2$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  mtext(side=2,line=2.5,xlabs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Water Supply")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

cols=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))
vars=paste0("PM",c(21,40,88,118))
ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/AvgFloodControl_subset2.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],dat.screen2[,c("Index",vars)])

tmp2=tmp[,vars]

x=barplot(t(tmp2),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,nrow(tmp)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp2),beside=F,col=cols,ylim=ylim.val,axes=F,ann=F,names.arg =rep(NA,nrow(tmp)),add=T)
with(tmp,text(x,PM21/2,round(PM21,0),cex=0.7,col="white"))
with(tmp,text(x,PM21+(((PM40+PM21)-PM21)/2),round(PM40,0),cex=0.7))
with(tmp,text(x,(PM21+PM40)+(((PM40+PM21+PM88)-(PM21+PM40))/2),round(PM88,0),cex=0.7))
with(tmp,text(x,PM40+PM21+PM88+PM118,round(PM118,0),pos=3,offset=0.1,cex=0.7))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,tmp$Index,line=-0.25,las=2,cex=0.8);box(lwd=1)
mtext(side=2,line=2,"Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=2.75,"Model Index")

par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0,0,legend=c("Water Conservation Areas (PM21)","Caloosahatchee River (PM40)","St. Lucie River (PM88)","Lake Worth Lagoon (PM118)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols,
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=0.5)
text(1,0,"Iteration 3 screened batch results. Mean annual flood control\nreleases from Lake Okeechobee for the 52 year (1965 - 2016)\nsimulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()



# USACE presentation - 2021-10-26 -----------------------------------------

dat$StressDam_lake.cre=rowSums(dat[,c("PM37","PM38")])
dat$StressDam_lake.sle=rowSums(dat[,c("PM86","PM87")])

dat$Stress_Basin.cre=with(dat,PM32-PM37)
dat$Dam_Basin.cre=with(dat,PM33-PM38)
dat$Stress_Basin.sle=with(dat,PM82-PM86)
dat$Dam_Basin.sle=with(dat,PM83-PM87)

dat$total.pen=rowSums(dat[,c("PM10","PM11")])

baselines$StressDam_lake.cre=rowSums(baselines[,c("PM37","PM38")])
baselines$StressDam_lake.sle=rowSums(baselines[,c("PM86","PM87")])
baselines$Stress_Basin.cre=with(baselines,PM32-PM37)
baselines$Dam_Basin.cre=with(baselines,PM33-PM38)
baselines$Stress_Basin.sle=with(baselines,PM82-PM86)
baselines$Dam_Basin.sle=with(baselines,PM83-PM87)
baselines$total.pen=rowSums(baselines[,c("PM10","PM11")])

usace.mods=c(173186,261768,262200,125463,138722,279349,168491,260467)
usace.dat=subset(dat,Index%in%usace.mods)

usace.dat$plot.ID=1:nrow(usace.dat)

## relative to FWO
range(((usace.dat$PM88-subset(baselines,Model=="NA25")$PM88)/subset(baselines,Model=="NA25")$PM88)*100)
range(((usace.dat$PM40-subset(baselines,Model=="NA25")$PM40)/subset(baselines,Model=="NA25")$PM40)*100)
((usace.dat$PM88-subset(baselines,Model=="CCTSP")$PM88)/subset(baselines,Model=="CCTSP")$PM88)*100

##
vars=c(paste0("PM",c(30,31,37,38,36,80,81,86,87,85,88,40,39,20)),"total.pen")
baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
# write.csv(baseline2,paste0(export.path,"Iteration3/Iter3P2_baselines.csv"),row.names = F)
tmp.dat=rbind(subset(baseline2,Model%in%c("CCTSP"))[,c("Index",vars)],usace.dat[,c("Index",vars)])
# write.csv(tmp.dat,paste0(export.path,"Iteration3/Iter3P2_USACEselect.csv"),row.names = F)

FWO.compare=data.frame(Index=tmp.dat$Index)
for(i in 1:length(vars)){
FWO.val=subset(baseline2,Model=="NA25")[,vars[i]]
val=((tmp.dat[,vars[i]]-FWO.val)/FWO.val)*100
tmp=data.frame(val=val)
colnames(tmp)=paste0(vars[i],".FWO")
FWO.compare=cbind(FWO.compare,tmp)
}
FWO.compare

PM.xwalk=data.frame(PM=vars,
                    Descript=c(paste("CRE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               paste("SLE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               "S308 Regulatory Flow","S77 Regulatory Flow","CRE MFL","LOK MFL","LOK - Total Stage Envelope Penalty"))
PM.xwalk$variable=paste0(PM.xwalk$PM,".FWO")
FWO.compare2=reshape2::melt(FWO.compare,id.vars="Index")
FWO.compare2=merge(FWO.compare2,PM.xwalk[,c("variable","Descript")])
FWO.compare2.xtab=reshape2::dcast(FWO.compare2,Descript~Index,value.var = "value",fun.aggregate=function(x) round(mean(x),1))
FWO.compare2.xtab=FWO.compare2.xtab[,c("Descript",tmp.dat$Index)]
FWO.compare2.xtab=FWO.compare2.xtab[match(PM.xwalk$Descript,FWO.compare2.xtab$Descript),]

cols=adjustcolor(c("lightgreen",rep("white",6),"indianred1"),0.5)
rank(FWO.compare2.xtab[1,3:10])

#cols=adjustcolor(colorRampPalette(c("lightgreen","seashell","indianred"))(8),0.5)


FWO.compare2.xtab%>%
  flextable()%>%
  padding(padding=1,part="all")%>%
  fontsize(size=8,part="body")%>%
  fontsize(size=9,part="header")%>%
  font(fontname="Times New Roman",part="all")%>%
  autofit()%>%
  add_header("CC"="Model Index",
             "125463"="Model Index",
             "138722"="Model Index",
             "168491"="Model Index",
             "173186"="Model Index",
             "260467"="Model Index", 
             "261768"="Model Index",
             "262200"="Model Index",
             "279349"="Model Index")%>%
  merge_h(part="header")%>%
  align(align="center",part="header")%>%
  align(align="center",part="all")%>%
  align(j=1,align="left",part="all")%>%
  bg(j=3:10,i=1,bg=cols[rank(FWO.compare2.xtab[1,3:10])])%>%
  bg(j=3:10,i=2,bg=cols[rank(-FWO.compare2.xtab[2,3:10])])%>%
  bg(j=3:10,i=3,bg=cols[rank(FWO.compare2.xtab[3,3:10])])%>%
  bg(j=3:10,i=4,bg=cols[rank(FWO.compare2.xtab[4,3:10])])%>%
  bg(j=3:10,i=5,bg=cols[rank(FWO.compare2.xtab[5,3:10])])%>%
  bg(j=3:10,i=6,bg=cols[rank(FWO.compare2.xtab[6,3:10])])%>%
  bg(j=3:10,i=7,bg=cols[rank(-FWO.compare2.xtab[7,3:10])])%>%
  bg(j=3:10,i=8,bg=cols[rank(FWO.compare2.xtab[8,3:10])])%>%
  bg(j=3:10,i=9,bg=cols[rank(FWO.compare2.xtab[9,3:10])])%>%
  bg(j=3:10,i=10,bg=cols[rank(FWO.compare2.xtab[10,3:10])])%>%
  bg(j=3:10,i=11,bg=cols[rank(FWO.compare2.xtab[11,3:10])])%>%
  bg(j=3:10,i=12,bg=cols[rank(FWO.compare2.xtab[12,3:10])])%>%
  bg(j=3:10,i=13,bg=cols[rank(FWO.compare2.xtab[13,3:10])])%>%
  bg(j=3:10,i=14,bg=cols[rank(FWO.compare2.xtab[14,3:10])])%>%
  bg(j=3:10,i=15,bg=cols[rank(FWO.compare2.xtab[15,3:10])])%>%
  footnote(j=1,part="header",ref_symbols = "",value=as_paragraph("Percent Difference relative to FWO (NA25)"))%>%
  footnote(j=1,part="header",ref_symbols = "",value=as_paragraph("Green value indicates best and Red indicates worst for each metric across selected models"))%>%
  font(fontname="Times New Roman",part="all")%>%print("docx")

  






text.cex=0.7
# png(filename=paste0(plot.path,"Iteration3_Batch/SalEnv_USACEsubset.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:10,5,2,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(600,1000,500,200,100)
ylim.min=c(200,400,0,0,0)
by.y.val=c(200,250,250,100,25)
vars=paste0("PM",c(30,31,37,38,36))
labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress from LOK (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Extreme (>6500 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Count of 14-Day Periods",outer=T)

ylim.max=c(200,1000,150,150,200)
ylim.min=c(100,800,0,0,100)
by.y.val=c(50,100,50,50,50)
vars=paste0("PM",c(80,81,86,87,85))
labs=c("Low (<150 cfs)","Optimum (150 - 1400 cfs)","Stress from LOK (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Extreme (>4000 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
}

mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

text.cex=0.7
# png(filename=paste0(plot.path,"Iteration3_Batch/SalEnv2_USACEsubset.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:8,4,2,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(300,200,200,210)
ylim.min=c(0,0,0,0)
by.y.val=c(150,100,100,100)
vars=c("PM37","Stress_Basin.cre","PM38","Dam_Basin.cre")# paste0("PM",c(30,31,37,38,36))
labs=c("Stress from LOK (2100 - 2600 cfs)","Stress from Basin (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Damaging from Basin (>2600 cfs)")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}


ylim.max=c(150,310,150,500)
ylim.min=c(0,0,0,400)
by.y.val=c(50,150,50,50)
vars=c("PM86","Stress_Basin.sle","PM87","Dam_Basin.sle")# paste0("PM",c(30,31,37,38,36))
labs=c("Stress from LOK (1400 - 1700 cfs)","Stress from Basin (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Damaging from Basin (>1700 cfs)")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
}
mtext(side=2,line=0.25,"Count of 14-Day Periods",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()



cols.val2=c(rgb(255/255,255/255,0),rgb(143/255,188/255,143/255),rgb(100/255,149/255,237/255))
vars=paste0("PM",c(12,13,14))
ylim.val=c(0,60);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_EnvScore_BWA_USACE.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.5,1),oma=c(1.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),widths=c(1,0.25))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],usace.dat[,c("Index",vars)])
tmp2=tmp[,vars]

x=barplot(t(tmp[,vars]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,v=x[2,],lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(tmp[,vars]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=cols.val2,add=T,xaxt="n")
with(tmp,text(x[1,],PM12/2,paste0(round(PM12,0),"%"),cex=0.7,srt=90,font=2))
with(tmp,text(x[2,],PM13/2,paste0(round(PM13,0),"%"),cex=0.7,srt=90,font=2))
with(tmp,text(x[3,],PM14/2,paste0(round(PM14,0),"%"),cex=0.7,srt=90,font=2))
x.val=x[2,] #x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,tmp$Index,line=-0.25,las=2,cex=0.8)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=3,"Model Index")
mtext(side=2,line=2,"Percent",cex=1)

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("% time Below\n(PM12)","% time Within\n(PM13)","% time Above\n(PM14)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols.val2,
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Stage Envelope")
dev.off()

cols.IMC=c(rgb(238,232,170,maxColorValue = 255),rgb(143,188,143,maxColorValue = 255))
vars=paste0("PM",c(10,11))
ylim.val=c(0,50000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_EnvScore_AllYrs_USACE.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.5,0.5),oma=c(1.5,2,0.5,0.5),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),widths=c(1,0.25))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],usace.dat[,c("Index",vars)])
tmp$total.pen=rowSums(tmp[,vars])
tmp2=tmp[,vars]

x=barplot(t(tmp2[,vars]),beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(tmp2)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(t(tmp2[,vars]),beside=F,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,nrow(tmp2)),add=T)
with(tmp,text(x,PM10/2,round(PM10,0),cex=0.7,srt=90,font=2))
with(tmp,text(x,PM10+abs((PM10-total.pen)/2),round(PM11,0),cex=0.7,srt=90,font=2))
with(tmp,text(x,total.pen,round(total.pen,0),pos=3,cex=0.7))
axis_fun(1,x,x,tmp$Index,line=-0.25,las=2,cex=0.8)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Stage Envelope Penalty Score")
mtext(side=1,line=3,"Model Index")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
mtext(side=3,line=-2,"Lake Okeechobee\nEnvelope Penalty Score\nAll Years",cex=0.7)
legend(0.5,0.5,legend=c("Lower Penalty\n(Below Envelope;\nPM10)","Upper Penalty\n(Above Envelope;\nPM11)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c(cols.IMC),
       pt.cex=2,ncol=1,cex=0.7,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Total Score")
mtext(side=1,line=-1,adj=1,"Iteration 3 - Batch Analysis\nSimulation Period of Record\n(1965-2016). Includes Normal\nand Recovery envelope.",cex=0.5)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_USACEsubset.png"),width=4,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:4,4,1,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(2,20,10,30)
ylim.min=c(0,0,0,10)
by.y.val=c(1,10,5,10)
vars=paste0("PM",c(5,6,7,13))
labs=c("% LOK Stage >17 Ft NGVD29","% LOK Stage >16 Ft NGVD29","% LOK Stage <10 Ft NGVD29","% within Stage Envelope")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"LOK")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/WS_USACEsubset.png"),width=4.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:6,6,1,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.min=c(0,0,0,0,0,0)
ylim.max=c(35,100,50,12,5,5)
by.y.val=ylim.max/2
vars=paste0("PM",c(124,126,127,130,131,132))
labs=c("LOSA Cutbacks","LOSA Duration","LOSA Severity","LOSA Sup. Demand not met EAA & Non-EAA","STOF - BCYP Demand Not Met", "SOTF - BR Demand Not Met")
xlabs=c("Weighted Avg %","Count","Score","Percent","Percent","Percent")
for(i in 1:6){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==6){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  mtext(side=2,line=2.5,xlabs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Water Supply")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

cols=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))
vars=paste0("PM",c(21,40,88,118))
ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/AvgFloodControl_USACEsubset.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],usace.dat[,c("Index",vars)])

tmp2=tmp[,vars]

x=barplot(t(tmp2),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,nrow(tmp)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp2),beside=F,col=cols,ylim=ylim.val,axes=F,ann=F,names.arg =rep(NA,nrow(tmp)),add=T)
with(tmp,text(x,PM21/2,round(PM21,0),cex=0.7,col="white"))
with(tmp,text(x,PM21+(((PM40+PM21)-PM21)/2),round(PM40,0),cex=0.7))
with(tmp,text(x,(PM21+PM40)+(((PM40+PM21+PM88)-(PM21+PM40))/2),round(PM88,0),cex=0.7))
with(tmp,text(x,PM40+PM21+PM88+PM118,round(PM118,0),pos=3,offset=0.1,cex=0.7))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,tmp$Index,line=-0.25,las=2,cex=0.8);box(lwd=1)
mtext(side=2,line=2,"Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=2.75,"Model Index")

par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0,0,legend=c("Water Conservation Areas (PM21)","Caloosahatchee River (PM40)","St. Lucie River (PM88)","Lake Worth Lagoon (PM118)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols,
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=0.5)
text(1,0,"Iteration 3 screened batch results. Mean annual flood control\nreleases from Lake Okeechobee for the 52 year (1965 - 2016)\nsimulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Batch/Est_StressDam_USACEsubset.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
# layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

xlim.val=c(0,600);by.x=100;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,600);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(StressDam_lake.cre~StressDam_lake.sle,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(StressDam_lake.cre~StressDam_lake.sle,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(usace.dat,points(StressDam_lake.cre~StressDam_lake.sle,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(StressDam_lake.cre~StressDam_lake.sle,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(StressDam_lake.cre~StressDam_lake.sle,pch=16,col="black",cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2.5,"SLE Stress and Damaging From LOK\n(PM86+PM87)")
mtext(side=2,line=2.25,"CRE Stress and Damaging From LOK\n(PM37+PM38)")

# plot(0:1,0:1,axes=F,ann=F,type="n")
legend("topright",legend=c("Round 3 Batch Runs","CC","NA25","USACE Selected Runs","1:1 Line"),
       pch=c(19,16,15,21,NA),lwd=c(0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),NA),col=c("dodgerblue1","black","black","indianred1","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off

xlim.val=c(0,20);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,11);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_StageExt_USACEsubset.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
# layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

xlim.val=c(0,20);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,11);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM7~PM5,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM7~PM5,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(usace.dat,points(PM7~PM5,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(PM7~PM5,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM7~PM5,pch=16,col="black",cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"% of Time >17 Ft NGVD29 (PM5)")
mtext(side=2,line=2,"% of Time <10 Ft NGVD29 (PM7)")

legend("topright",legend=c("Round 3 Batch Runs","CC","NA25","USACE Selected Runs","1:1 Line"),
       pch=c(19,16,15,21,NA),lwd=c(0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),NA),col=c("dodgerblue1","black","black","indianred1","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

# plot(0:1,0:1,axes=F,ann=F,type="n")
legend("topright",legend=c("Round 3 Batch Runs","CC","NA25","USACE Selected Runs","1:1 Line"),
       pch=c(19,16,15,21,NA),lwd=c(0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),NA),col=c("dodgerblue1","black","black","indianred1","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off


# png(filename=paste0(plot.path,"Iteration3_Batch/Est_mo_extreme_cnt.png"),width=4.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,0.5,1),lwd=0.5);
layout(matrix(1:2,2,1,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.val=c(0,35);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM79~plot.ID,usace.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
# abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(usace.dat,points(PM79~plot.ID,pch=21,bg=adjustcolor("dodgerblue1",0.5)))
abline(h=subset(baselines,Model=="CCTSP")$PM79,col="indianred1",lwd=1.5)
abline(h=subset(baselines,Model=="NA25")$PM79,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"CRE - Extreme (>6500 cfs; PM79)")
legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
       pch=NA,lwd=2,lty=c(1,1),
       pt.bg=NA,col=c("grey","indianred1"),
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

ylim.val=c(40,50);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM117~plot.ID,usace.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
# abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(usace.dat,points(PM117~plot.ID,pch=21,bg=adjustcolor("dodgerblue1",0.5)))
abline(h=subset(baselines,Model=="CCTSP")$PM117,col="indianred1",lwd=1.5)
abline(h=subset(baselines,Model=="NA25")$PM117,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"SLE - Extreme (>4000 cfs; PM117)")

mtext(side=2,line=0,"Count of Months\nAvg Discharge > Extreme Threshold",outer=T)
mtext(side=1,line=3,"Model Index",outer=F)
dev.off()


