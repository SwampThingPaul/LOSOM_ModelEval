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
                   batch==3)[1:8,]
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