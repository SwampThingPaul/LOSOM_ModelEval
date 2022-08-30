## 
## LOSOM
##
## Iteration 3 - Phase 2 
## Batch Results - TSP
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

## Functions
consec.startend=function(var){
  runs=rle(var)
  myruns = which(runs$values == TRUE)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  rslt=list(starts=starts,ends=ends)
  return(rslt)
}
# -------------------------------------------------------------------------
dat1=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch1_13Oct2021.csv"),skip=22)[,1:185]
# dat2=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch2_13Oct2021.csv"),skip=22)[,1:185]
dat3=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch3_13Oct2021.csv"),skip=22)[,1:185]

baselines=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch1_13Oct2021.csv"),skip=17,header=F)[1:3,1:185]
colnames(baselines)<-names(dat1)

baselines[,3:ncol(baselines)]=sapply(baselines[,3:ncol(baselines)],FUN=function(x) as.numeric(sub("%","",x)))

# dat1$batch=1
# dat2$batch=2
dat3$batch=3

# names(dat1)
# names(dat2)
# names(dat3)
# dat=rbind(
#   subset(dat1,is.na(Index)==F),
#   subset(dat2,is.na(Index)==F),
#   subset(dat3,is.na(Index)==F)
# )
# unique(dat$Model)
dat=dat3

# subset(dat,Index==260467)

dat[,3:ncol(dat)]=sapply(dat[,3:ncol(dat)],FUN=function(x) as.numeric(sub("%","",x)))

# USACE presentation - 2021-11-16 -----------------------------------------

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

### 
tsp.mod=260467
tsp.dat=subset(dat,Index%in%tsp.mod)
tsp.dat$PM5=as.numeric(sub("%","",tsp.dat$PM5))
tsp.dat$PM6=as.numeric(sub("%","",tsp.dat$PM6))

tsp.dat[,3:ncol(tsp.dat)]=sapply(dat[,3:ncol(tsp.dat)],FUN=function(x) as.numeric(sub("%","",x)))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))

vars=c(paste0("PM",c(30,31,37,38,36,80,81,86,87,85,79,117,88,40,39,20,5,6)),"total.pen")

base.tsp=rbind(baseline2[,c("Index",vars)],tsp.dat[,c("Index",vars)])
base.tsp$PM5=as.numeric(sub("%","",base.tsp$PM5))
base.tsp$PM6=as.numeric(sub("%","",base.tsp$PM6))

base.tsp[,c("Index","PM85")]


base.compare=data.frame(base=c("NA25","ECBr"),Index=tsp.dat$Index)
for(i in 1:length(vars)){
  FWO.val=subset(baseline2,Model=="NA25")[,vars[i]]
  val=((tsp.dat[,vars[i]]-FWO.val)/FWO.val)*100
  
  ECB.val=subset(baseline2,Model=="ECBr")[,vars[i]]
  val2=((tsp.dat[,vars[i]]-ECB.val)/ECB.val)*100
  
  tmp=data.frame(val=c(val,val2))
  colnames(tmp)=paste0(vars[i],".PerDif")
  base.compare=cbind(base.compare,tmp)
}
base.compare


PM.xwalk=data.frame(PM=vars,
                    Descript=c(paste("CRE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               paste("SLE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),"CRE - Months >6500 cfs","SLE - Months >4000 cfs",
                               "S308 Regulatory Flow","S77 Regulatory Flow","CRE MFL","LOK MFL","LOK >17 Ft","LOK >16 Ft","LOK - Total Stage Envelope Penalty"))
base.tsp[,vars[1:5]]
# write.csv(base.tsp,paste0(export.path,"Iteration3/Iter3P2_tsp.csv"),row.names = F)

barplot(base.tsp$PM30)
cols=c("grey80","grey50","#E6C019","indianred1")
text.cex=0.7
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/SalEnv_BaselineTSP.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:10,5,2,byrow=F))
# xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(700,1000,500,225,100)
ylim.min=c(200,400,0,0,0)
by.y.val=c(200,250,250,100,25)
vars=paste0("PM",c(30,31,37,38,36))
labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress from LOK (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Extreme (>6500 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  x=barplot(base.tsp[,vars[i]],ylim=ylim.val,ann=F,axes=F,xpd=F,space=0,col=adjustcolor(cols,0.5))
  if(i==5){axis_fun(1,x,x,base.tsp$Index,las=2,cex=0.75)}else{axis_fun(1,x,x,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
}
mtext(side=2,line=0.5,"Count of 14-Day Periods",outer=T)

ylim.max=c(200,1000,175,175,200)
ylim.min=c(100,800,0,0,100)
by.y.val=c(50,100,50,50,50)
vars=paste0("PM",c(80,81,86,87,85))
labs=c("Low (<150 cfs)","Optimum (150 - 1400 cfs)","Stress from LOK (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Extreme (>4000 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  x=barplot(base.tsp[,vars[i]],ylim=ylim.val,ann=F,axes=F,xpd=F,space=0,col=adjustcolor(cols,0.5))
  if(i==5){axis_fun(1,x,x,base.tsp$Index,las=2,cex=0.75)}else{axis_fun(1,x,x,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
}

mtext(side=1,line=2,"Model Index",outer=T)
dev.off()


cols2=c("khaki","dodgerblue1")
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/SalEnv_BaselinePerDiff.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:10,5,2,byrow=F))
# xlim.val=c(1,nrow(base.tsp));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(0,70,0,0,30)
ylim.min=c(-40,0,-100,-70,0)
by.y.val=(pmax(ylim.max,ylim.min)-pmin(ylim.max,ylim.min))/2# c(20,30,50,35,15)
vars=paste0("PM",c(30,31,37,38,36),".PerDif")
labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress from LOK (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Extreme (>6500 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    x=barplot(base.compare[,vars[i]],ylim=ylim.val,ann=F,axes=F,xpd=F,space=0,col=adjustcolor(cols2,0.5))
    if(i==5){axis_fun(1,x,x,base.compare$base,las=2,cex=0.75)}else{axis_fun(1,x,x,NA,line=-0.5)}
    axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
    mtext(side=3,adj=0,labs[i],cex=text.cex)
    text(x,base.compare[,vars[i]],round(base.compare[,vars[i]],0),pos=ifelse(base.compare[,vars[i]]<0,3,1),offset=0.25)
    if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
  }

ylim.max=c(80,20,0,0,10)
ylim.min=c(0,0,-100,-100,-10)
by.y.val=(pmax(ylim.max,ylim.min)-pmin(ylim.max,ylim.min))/2# c(40,30,50,30,15)

vars=paste0("PM",c(80,81,86,87,85),".PerDif")
labs=c("Low (<150 cfs)","Optimum (150 - 1400 cfs)","Stress from LOK (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Extreme (>4000 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  x=barplot(base.compare[,vars[i]],ylim=ylim.val,ann=F,axes=F,xpd=F,space=0,col=adjustcolor(cols2,0.5))
  if(i==5){axis_fun(1,x,x,base.compare$base,las=2,cex=0.75)}else{axis_fun(1,x,x,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  text(x,base.compare[,vars[i]],round(base.compare[,vars[i]],0),pos=ifelse(base.compare[,vars[i]]<0,3,1),offset=0.25)
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
  if(pmax(ylim.max,ylim.min)[i]!=0|pmin(ylim.max,ylim.min)[i]!=0){abline(h=0)}
}

mtext(side=2,line=0.5,"TSP % Difference relative to Baseline",outer=T)
mtext(side=1,line=2,"Baslines",outer=T)
dev.off()

base.tsp
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/Est_mo_extreme_cnt.png"),width=4.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,0.5,1),lwd=0.5);
layout(matrix(1:2,2,1,byrow=F))
xlim.val=c(1,nrow(base.tsp));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.val=c(0,35);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(base.tsp$PM79,ylim=ylim.val,ann=F,axes=F,xpd=F,space=0,col=adjustcolor(cols,0.5))
axis_fun(1,x,x,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
text(x,base.tsp$PM79,base.tsp$PM79,pos=3,offset=0.25)
mtext(side=3,adj=0,"CRE - Extreme (>6500 cfs; PM79)")

ylim.val=c(40,55);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(base.tsp$PM117,ylim=ylim.val,ann=F,axes=F,xpd=F,space=0,col=adjustcolor(cols,0.5))
axis_fun(1,x,x,base.tsp$Index,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
text(x,base.tsp$PM117,base.tsp$PM117,pos=3,offset=0.25)
mtext(side=3,adj=0,"SLE - Extreme (>4000 cfs; PM117)")
mtext(side=2,line=0,"Count of Months\nAvg Discharge > Extreme Threshold",outer=T)
mtext(side=1,line=3,"Model Index",outer=F)
dev.off()

cols3=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))
vars=paste0("PM",c(21,40,88,118))
ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/AvgFloodControl_BaselineTSP.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

tmp=rbind(baseline2[,c("Index",vars)],tsp.dat[,c("Index",vars)])

tmp2=tmp[,vars]

x=barplot(t(tmp2),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,nrow(tmp)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp2),beside=F,col=cols3,ylim=ylim.val,axes=F,ann=F,names.arg =rep(NA,nrow(tmp)),add=T)
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
       pt.bg=cols3,
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=0.5)
text(1,0,"Iteration 3 screened batch results with TSP.\nMean annual flood control releases\nfrom Lake Okeechobee for the 52 year (1965 - 2016)\nsimulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/LOK_BaselineTSP.png"),width=4,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:4,4,1,byrow=F))
# xlim.val=c(1,nrow(base.tsp));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(2,25,10,30)
ylim.min=c(0,0,0,10)
by.y.val=c(1,10,5,10)
vars=paste0("PM",c(5,6,7,13))
labs=c("% LOK Stage >17 Ft NGVD29","% LOK Stage >16 Ft NGVD29","% LOK Stage <10 Ft NGVD29","% within Stage Envelope")
tmp=rbind(baseline2[,c("Index",vars)],tsp.dat[,c("Index",vars)])
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  x=barplot(tmp[,vars[i]],ylim=ylim.val,ann=F,axes=F,xpd=F,space=0,col=adjustcolor(cols,0.5))
  if(i==4){axis_fun(1,x,x,base.tsp$Index,las=2,cex=0.75)}else{axis_fun(1,x,x,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  text(x,tmp[,vars[i]],format(round(tmp[,vars[i]],1)),pos=3,offset=0.25)
  if(i==1){mtext(side=3,adj=0,line=1.25,"LOK")}
}
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

# RSMBN -------------------------------------------------------------------
# dput(list.files("C:/Julian_LaCie/_GitHub/LOSOM_ModelEval/Data/Iteration_3_Batch/20211027/RSMBN"))
alts.iter2=c("ECBr","NA25","CC")
alts.sort=c(alts.iter2,tsp.mod)
RSM.sites=c("S79","S80","S80_QPFCSOURCE_LAKE","S79_QPFCSOURCE_LAKE",
            "S308","S77_QFC","S308_QFC","S308BF",
            "TMC2EST","S48","S49","NSF2EST")

q.dat=data.frame()
n.alts=length(alts.iter2)
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts.iter2[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts.iter2[j]
    q.dat=rbind(tmp,q.dat)
    print(i)
  }
}

n.alts=1
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3_Batch/20211027/RSMBN/model_",tsp.mod[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=tsp.mod[j]
    q.dat=rbind(tmp,q.dat)
    print(i)
  }
}

q.dat$CY=as.numeric(format(q.dat$Date,"%Y"))
q.dat$month=as.numeric(format(q.dat$Date,"%m"))

q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+CY+month~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
head(q.dat.xtab)
q.dat.xtab$S79.14d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80.14d=with(q.dat.xtab,ave(S80,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))

# RECOVER Salinity Envelope -----------------------------------------------
## CRE
q.dat.xtab$S79_QPFCSOURCE_LAKE.14d=with(q.dat.xtab,ave(S79_QPFCSOURCE_LAKE,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$CRE.low=with(q.dat.xtab,ifelse(S79.14d<750,1,0)) # RECOVER Low
q.dat.xtab$CRE.low1=with(q.dat.xtab,ifelse(S79.14d<457,1,0))
q.dat.xtab$CRE.low2=with(q.dat.xtab,ifelse(S79.14d>=457&S79.14d<750,1,0))
q.dat.xtab$CRE.opt=with(q.dat.xtab,ifelse(S79.14d>=750&S79.14d<2100,1,0)) # RECOVER Optimum
q.dat.xtab$CRE.high=with(q.dat.xtab,ifelse(S79.14d>=2100&S79.14d<2600,1,0)) # RECOVER Stress
q.dat.xtab$CRE.high_2100=with(q.dat.xtab,ifelse(S79.14d>=2100,1,0))
q.dat.xtab$CRE.high1=with(q.dat.xtab,ifelse(S79.14d>=2600&S79.14d<4500,1,0))
q.dat.xtab$CRE.high2=with(q.dat.xtab,ifelse(S79.14d>=4500&S79.14d<6500,1,0))
q.dat.xtab$CRE.high3=with(q.dat.xtab,ifelse(S79.14d>=6500,1,0))
q.dat.xtab$CRE.dam=with(q.dat.xtab,ifelse(S79.14d>=2600,1,0)) # RECOVER Damaging

q.dat.xtab$da.CRE.high3=with(q.dat.xtab,ifelse(S79>=6500,1,0))
q.dat.xtab$da.CRE.high3.LOK=with(q.dat.xtab,ifelse(da.CRE.high3==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=6500,1,0),0))
q.dat.xtab$da.CRE.high3.Basin=with(q.dat.xtab,da.CRE.high3-da.CRE.high3.LOK)
## SLE
sle.gw=openxlsx::read.xlsx(paste0(data.path,"Iteration_2/Model_Output/Iteration2_STL_Flows_13Jul2021.xlsx"))
sle.gw$Date=with(sle.gw,date.fun(paste(year,month,day,sep="-")))
sle.gw=sle.gw[,c("Date","sle_gw")]
q.dat.xtab=merge(q.dat.xtab,sle.gw,"Date")
q.dat.xtab=q.dat.xtab[order(q.dat.xtab$Alt,q.dat.xtab$Date),]

q.dat.xtab$SLE.S80trib=rowSums(q.dat.xtab[,c("S80","TMC2EST","S48","S49","NSF2EST","sle_gw")],na.rm=T)
q.dat.xtab$SLE.S80trib.14d=with(q.dat.xtab,ave(SLE.S80trib,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80_QPFCSOURCE_LAKE.14d=with(q.dat.xtab,ave(S80_QPFCSOURCE_LAKE,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$SLE.low=with(q.dat.xtab,ifelse(SLE.S80trib.14d<150,1,0)) # RECOVER Low
q.dat.xtab$SLE.opt=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=150&SLE.S80trib.14d<1400,1,0)) # RECOVER Optimum
q.dat.xtab$SLE.high=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1400&SLE.S80trib.14d<1700,1,0)) # RECOVER stress
q.dat.xtab$SLE.high_1400=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1400,1,0)) 
q.dat.xtab$SLE.dam=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1700,1,0)) # RECOVER damaging
q.dat.xtab$SLE.high1=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1700&SLE.S80trib.14d<4000,1,0))
q.dat.xtab$SLE.high2=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=4000,1,0))

q.dat.xtab$da.SLE.high2=with(q.dat.xtab,ifelse(SLE.S80trib>=4000,1,0))
q.dat.xtab$da.SLE.high2.LOK=with(q.dat.xtab,ifelse(da.SLE.high2==1,ifelse((SLE.S80trib-S80_QPFCSOURCE_LAKE)<=4000,1,0),0))
q.dat.xtab$da.SLE.high2.Basin=with(q.dat.xtab,da.SLE.high2-da.SLE.high2.LOK)

##
q.dat.xtab$CRE.low.count=0
q.dat.xtab$CRE.low.LOK.count=0
q.dat.xtab$CRE.low.basin.count=0
q.dat.xtab$CRE.low1.count=0
q.dat.xtab$CRE.low2.count=0
q.dat.xtab$CRE.opt.count=0
q.dat.xtab$CRE.opt.LOK.count=0
q.dat.xtab$CRE.opt.basin.count=0
q.dat.xtab$CRE.high.count=0
q.dat.xtab$CRE.high_2100.count=0
q.dat.xtab$CRE.high1.count=0
q.dat.xtab$CRE.high2.count=0
q.dat.xtab$CRE.high3.count=0
q.dat.xtab$CRE.high.LOK.count=0
q.dat.xtab$CRE.high.basin.count=0
q.dat.xtab$CRE.dam.count=0
q.dat.xtab$CRE.dam.LOK.count=0
q.dat.xtab$CRE.dam.basin.count=0
q.dat.xtab$CRE.high3.LOK.count=0
q.dat.xtab$CRE.high3.basin.count=0

q.dat.xtab$SLE.low.count=0
q.dat.xtab$SLE.low.LOK.count=0
q.dat.xtab$SLE.low.basin.count=0
q.dat.xtab$SLE.opt.count=0
q.dat.xtab$SLE.opt.LOK.count=0
q.dat.xtab$SLE.opt.basin.count=0
q.dat.xtab$SLE.high.count=0
q.dat.xtab$SLE.high_1400.count=0
# q.dat.xtab$SLE.S80.high.count=0
# q.dat.xtab$SLE.S80.high.LOK.count=0
# q.dat.xtab$SLE.S80.high.count2=0
# q.dat.xtab$SLE.S80.high.LOK.count2=0

q.dat.xtab$SLE.high1.count=0
q.dat.xtab$SLE.high2.count=0
q.dat.xtab$SLE.high.LOK.count=0
q.dat.xtab$SLE.high.basin.count=0
q.dat.xtab$SLE.dam.count=0
q.dat.xtab$SLE.dam.LOK.count=0
q.dat.xtab$SLE.dam.basin.count=0
q.dat.xtab$SLE.high2.LOK.count=0
q.dat.xtab$SLE.high2.basin.count=0

###
alts.sort=c(alts.iter2,tsp.mod)
q.dat.xtab2=data.frame()
for(j in 1:length(alts.sort)){
  tmp=subset(q.dat.xtab,Alt==alts.sort[j])
  for(i in 14:nrow(tmp)){
    ## CRE
    tmp$CRE.low.count[i]=with(tmp,ifelse(CRE.low[i]==1&sum(CRE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$CRE.low.LOK.count[i]=with(tmp,
                                  ifelse(CRE.low.count[i]==1,
                                         ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=750,1,0),0))
    tmp$CRE.low.basin.count[i]=with(tmp,CRE.low.count[i]-CRE.low.LOK.count[i])
    
    tmp$CRE.low1.count[i]=with(tmp,ifelse(CRE.low1[i]==1&sum(CRE.low1.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$CRE.low2.count[i]=with(tmp,ifelse(CRE.low2[i]==1&sum(CRE.low2.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$CRE.opt.count[i]=with(tmp,ifelse(CRE.opt[i]==1&sum(CRE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.opt.LOK.count[i]=with(tmp,
                                  ifelse(CRE.opt.count[i]==1,
                                         ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=750,1,0),0))
    tmp$CRE.opt.basin.count[i]=with(tmp,CRE.opt.count[i]-CRE.opt.LOK.count[i])
    
    tmp$CRE.high.count[i]=with(tmp,ifelse(CRE.high[i]==1&sum(CRE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.high_2100.count[i]=with(tmp,ifelse(CRE.high_2100[i]==1&sum(CRE.high_2100.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.high1.count[i]=with(tmp,ifelse(CRE.high1[i]==1&sum(CRE.high1.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.high2.count[i]=with(tmp,ifelse(CRE.high2[i]==1&sum(CRE.high2.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.high3.count[i]=with(tmp,ifelse(CRE.high3[i]==1&sum(CRE.high3.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.high.LOK.count[i]=with(tmp,
                                   ifelse(CRE.high.count[i]==1,
                                          ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=2100,1,0),0))
    tmp$CRE.high.basin.count[i]=with(tmp,CRE.high.count[i]-CRE.high.LOK.count[i])
    tmp$CRE.dam.count[i]=with(tmp,ifelse(CRE.dam[i]==1&sum(CRE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.dam.LOK.count[i]=with(tmp,
                                  ifelse(CRE.dam.count[i]==1,
                                         ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=2600,1,0),0))
    tmp$CRE.dam.basin.count[i]=with(tmp,CRE.dam.count[i]-CRE.dam.LOK.count[i])
    
    tmp$CRE.high3.LOK.count[i]=with(tmp,
                                    ifelse(CRE.high3.count[i]==1,
                                           ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=6500,1,0),0))
    tmp$CRE.high3.basin.count[i]=with(tmp,CRE.high3.count[i]-CRE.high3.LOK.count[i])
    
    ## SLE
    tmp$SLE.low.count[i]=with(tmp,ifelse(SLE.low[i]==1&sum(SLE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0)) 
    tmp$SLE.low.LOK.count[i]=with(tmp,
                                  ifelse(SLE.low.count[i]==1,
                                         ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=150,1,0),0))
    tmp$SLE.low.basin.count[i]=with(tmp,SLE.low.count[i]-SLE.low.LOK.count[i])
    tmp$SLE.opt.count[i]=with(tmp,ifelse(SLE.opt[i]==1&sum(SLE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.opt.LOK.count[i]=with(tmp,
                                  ifelse(SLE.opt.count[i]==1,
                                         ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=150,1,0),0))
    tmp$SLE.opt.basin.count[i]=with(tmp,SLE.opt.count[i]-SLE.opt.LOK.count[i])
    tmp$SLE.high.count[i]=with(tmp,ifelse(SLE.high[i]==1&sum(SLE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high_1400.count[i]=with(tmp,ifelse(SLE.high_1400[i]==1&sum(SLE.high_1400.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high.LOK.count[i]=with(tmp,
                                   ifelse(SLE.high.count[i]==1,
                                          ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=1400,1,0),0))
    tmp$SLE.high.basin.count[i]=with(tmp,SLE.high.count[i]-SLE.high.LOK.count[i])
    tmp$SLE.dam.count[i]=with(tmp,ifelse(SLE.dam[i]==1&sum(SLE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.dam.LOK.count[i]=with(tmp,
                                  ifelse(SLE.dam.count[i]==1,
                                         ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=1700,1,0),0))
    tmp$SLE.dam.basin.count[i]=with(tmp,SLE.dam.count[i]-SLE.dam.LOK.count[i])
    tmp$SLE.high1.count[i]=with(tmp,ifelse(SLE.high1[i]==1&sum(SLE.high1.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high2.count[i]=with(tmp,ifelse(SLE.high2[i]==1&sum(SLE.high2.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high2.LOK.count[i]=with(tmp,
                                    ifelse(SLE.high2.count[i]==1,
                                           ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=4000,1,0),0))
    tmp$SLE.high2.basin.count[i]=with(tmp,SLE.high2.count[i]-SLE.high2.LOK.count[i])
  }
  q.dat.xtab2=rbind(q.dat.xtab2,tmp)
  print(j)
}
q.dat.xtab2

##
vars.CRE=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high3.count","high3.LOK.count"),sep=".")
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars.CRE)],id.vars = "Alt")
CRE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
CRE.SalEnv_count=CRE.SalEnv_count[match(alts.sort,CRE.SalEnv_count$Alt),]

extremeQ_consec=data.frame()
for(j in 1:length(alts.sort)){
  tmp=subset(q.dat.xtab,Alt==alts.sort[j])
  tmp$CRE.Q6500=0
  tmp$SLE.Q4000=0
  tmp$CRE.Q6500.LOK=0
  tmp$SLE.Q4000.LOK=0
  for(i in 2:nrow(tmp)){
    tmp$CRE.Q6500[i]=with(tmp,ifelse(da.CRE.high3[i-1]==0&da.CRE.high3[i]>0,1,
                                     ifelse(da.CRE.high3[i-1]>0&da.CRE.high3[i]>0,1,0)))
    
  }
  for(i in 2:nrow(tmp)){
    tmp$SLE.Q4000[i]=with(tmp,ifelse(da.SLE.high2[i-1]==0&da.SLE.high2[i]>0,1,
                                     ifelse(da.SLE.high2[i-1]>0&da.SLE.high2[i]>0,1,0)))
    
  }
  for(i in 2:nrow(tmp)){
    tmp$CRE.Q6500.LOK[i]=with(tmp,ifelse(da.CRE.high3.LOK[i-1]==0&da.CRE.high3.LOK[i]>0,1,
                                         ifelse(da.CRE.high3.LOK[i-1]>0&da.CRE.high3.LOK[i]>0,1,0)))
    
  }
  for(i in 2:nrow(tmp)){
    tmp$SLE.Q4000.LOK[i]=with(tmp,ifelse(da.SLE.high2.LOK[i-1]==0&da.SLE.high2.LOK[i]>0,1,
                                         ifelse(da.SLE.high2.LOK[i-1]>0&da.SLE.high2.LOK[i]>0,1,0)))
    
  }
  CRE.highQ=consec.startend(tmp$CRE.Q6500>0)
  tmp$sum.CRE.Q6500=0
  for(i in 1:length(CRE.highQ$ends)){
    tmp[CRE.highQ$ends[i],]$sum.CRE.Q6500=with(tmp[c(CRE.highQ$starts[i]:CRE.highQ$ends[i]),],sum(CRE.Q6500,na.rm=T))
  }
  CRE.highQ=consec.startend(tmp$CRE.Q6500.LOK>0)
  tmp$sum.CRE.Q6500.LOK=0
  for(i in 1:length(CRE.highQ$ends)){
    tmp[CRE.highQ$ends[i],]$sum.CRE.Q6500.LOK=with(tmp[c(CRE.highQ$starts[i]:CRE.highQ$ends[i]),],sum(CRE.Q6500.LOK,na.rm=T))
  }
  
  SLE.highQ=consec.startend(tmp$SLE.Q4000>0)
  tmp$sum.SLE.Q4000=0
  for(i in 1:length(SLE.highQ$ends)){
    tmp[SLE.highQ$ends[i],]$sum.SLE.Q4000=with(tmp[c(SLE.highQ$starts[i]:SLE.highQ$ends[i]),],sum(SLE.Q4000,na.rm=T))
  }
  SLE.highQ=consec.startend(tmp$SLE.Q4000.LOK>0)
  tmp$sum.SLE.Q4000.LOK=0
  for(i in 1:length(SLE.highQ$ends)){
    tmp[SLE.highQ$ends[i],]$sum.SLE.Q4000.LOK=with(tmp[c(SLE.highQ$starts[i]:SLE.highQ$ends[i]),],sum(SLE.Q4000.LOK,na.rm=T))
  }
  extremeQ_consec=rbind(tmp,extremeQ_consec)
  print(j)
}
extremeQ_consec


plot(sum.CRE.Q6500~Date,subset(extremeQ_consec,Alt=="CC"))
with(subset(extremeQ_consec,Alt=="CC"),points(sum.CRE.Q6500.LOK~Date,pch=21,bg="red"))
with(subset(extremeQ_consec,Alt=="CC"&sum.CRE.Q6500.LOK>14&sum.CRE.Q6500.LOK<30),points(sum.CRE.Q6500.LOK~Date,pch=21,bg="green"))
with(subset(extremeQ_consec,Alt=="CC"&sum.CRE.Q6500>14&sum.CRE.Q6500<30),points(sum.CRE.Q6500~Date,pch=21,bg="yellow"))

ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500"),summarise,count.event=N.obs(sum.CRE.Q6500))
ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500.LOK"),summarise,count.event=N.obs(sum.CRE.Q6500.LOK))

rslt.CREHighQ=ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500"),summarise,count.event=N.obs(sum.CRE.Q6500))
rslt.CREHighQ$cat=with(rslt.CREHighQ,ifelse(sum.CRE.Q6500>0&sum.CRE.Q6500<14,1,
                                            ifelse(sum.CRE.Q6500>=14&sum.CRE.Q6500<30,2,
                                                   ifelse(sum.CRE.Q6500>=30&sum.CRE.Q6500<60,3,
                                                          ifelse(sum.CRE.Q6500>=60&sum.CRE.Q6500<90,4,
                                                                 ifelse(sum.CRE.Q6500>=90,5,NA))))))

rslt.CREHighQ.LOK=ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500.LOK"),summarise,count.event=N.obs(sum.CRE.Q6500.LOK))
rslt.CREHighQ.LOK$cat=with(rslt.CREHighQ.LOK,ifelse(sum.CRE.Q6500.LOK>0&sum.CRE.Q6500.LOK<14,1,
                                                    ifelse(sum.CRE.Q6500.LOK>=14&sum.CRE.Q6500.LOK<30,2,
                                                           ifelse(sum.CRE.Q6500.LOK>=30&sum.CRE.Q6500.LOK<60,3,
                                                                  ifelse(sum.CRE.Q6500.LOK>=60&sum.CRE.Q6500.LOK<90,4,
                                                                         ifelse(sum.CRE.Q6500.LOK>=90,5,NA))))))
(1:100)-(1:100)%%14
with(rslt.CREHighQ.LOK,sum.CRE.Q6500.LOK-sum.CRE.Q6500.LOK%%14)
plot(count.event~sum.CRE.Q6500.LOK,subset(rslt.CREHighQ.LOK,Alt==260467&sum.CRE.Q6500.LOK!=0))
barplot(subset(rslt.CREHighQ.LOK,Alt==260467&sum.CRE.Q6500.LOK!=0)$count.event)
# rslt.SLEHighQ=reshape2::dcast(extremeQ_consec,sum.SLE.Q4000~Alt,value.var = "sum.SLE.Q4000",fun.aggregate = function(x)N.obs(x))
rslt.SLEHighQ=ddply(extremeQ_consec,c("Alt","sum.SLE.Q4000"),summarise,count.event=N.obs(sum.SLE.Q4000))
rslt.SLEHighQ$cat=with(rslt.SLEHighQ,ifelse(sum.SLE.Q4000>0&sum.SLE.Q4000<14,1,
                                            ifelse(sum.SLE.Q4000>=14&sum.SLE.Q4000<30,2,
                                                   ifelse(sum.SLE.Q4000>=30&sum.SLE.Q4000<60,3,
                                                          ifelse(sum.SLE.Q4000>=60&sum.SLE.Q4000<90,4,
                                                                 ifelse(sum.SLE.Q4000>=90,5,NA))))))
rslt.SLEHighQ.LOK=ddply(extremeQ_consec,c("Alt","sum.SLE.Q4000.LOK"),summarise,count.event=N.obs(sum.SLE.Q4000))
rslt.SLEHighQ.LOK$cat=with(rslt.SLEHighQ.LOK,ifelse(sum.SLE.Q4000.LOK>0&sum.SLE.Q4000.LOK<14,1,
                                                    ifelse(sum.SLE.Q4000.LOK>=14&sum.SLE.Q4000.LOK<30,2,
                                                           ifelse(sum.SLE.Q4000.LOK>=30&sum.SLE.Q4000.LOK<60,3,
                                                                  ifelse(sum.SLE.Q4000.LOK>=60&sum.SLE.Q4000.LOK<90,4,
                                                                         ifelse(sum.SLE.Q4000.LOK>=90,5,NA))))))


rslt.CREHigh.sum=reshape2::dcast(subset(rslt.CREHighQ,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.CREHigh.LOK.sum=reshape2::dcast(subset(rslt.CREHighQ.LOK,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.SLEHigh.sum=reshape2::dcast(subset(rslt.SLEHighQ,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.SLEHigh.LOK.sum=reshape2::dcast(subset(rslt.SLEHighQ.LOK,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)

rslt.CREHigh.sum=rslt.CREHigh.sum[,c("cat",alts.sort)]
rslt.CREHigh.LOK.sum=rslt.CREHigh.LOK.sum[,c("cat",alts.sort)]
tmp=rslt.CREHigh.LOK.sum[4,]
tmp$cat=4
tmp[,2:ncol(tmp)]=0
rslt.CREHigh.LOK.sum=rbind(rslt.CREHigh.LOK.sum,tmp)
tmp=rslt.CREHigh.LOK.sum[4,]
tmp$cat=5
tmp[,2:ncol(tmp)]=0
rslt.CREHigh.LOK.sum=rbind(rslt.CREHigh.LOK.sum,tmp)

rslt.SLEHigh.LOK.sum=rslt.SLEHigh.LOK.sum[,c("cat",alts.sort)]
tmp=rslt.SLEHigh.LOK.sum[2,]
tmp$cat=3
tmp[,2:ncol(tmp)]=0
rslt.SLEHigh.LOK.sum=rbind(rslt.SLEHigh.LOK.sum,tmp)
tmp=rslt.SLEHigh.LOK.sum[2,]
tmp$cat=4
tmp[,2:ncol(tmp)]=0
rslt.SLEHigh.LOK.sum=rbind(rslt.SLEHigh.LOK.sum,tmp)


plot(sum.CRE.Q6500~Date,subset(extremeQ_consec,Alt=="260467"))
with(subset(extremeQ_consec,Alt=="260467"),points(sum.CRE.Q6500.LOK~Date,pch=21,bg="red"))
with(subset(extremeQ_consec,Alt=="260467"&sum.CRE.Q6500.LOK>14&sum.CRE.Q6500.LOK<30),points(sum.CRE.Q6500.LOK~Date,pch=21,bg="green"))
with(subset(extremeQ_consec,Alt=="260467"&sum.CRE.Q6500>14&sum.CRE.Q6500<30),points(sum.CRE.Q6500~Date,pch=21,bg="yellow"))

subset(extremeQ_consec,Alt=="260467"&sum.CRE.Q6500>100)



xlim.val=date.fun(c("1965-01-01","2016-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ylim.val=c(0,120);by.y=28;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/CRE_highQ_events_TSP.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.5,1),oma=c(0.5,2,1,0.25),lwd=0.5);
layout(matrix(1:2,2,1),heights=c(1,0.2))

plot(sum.CRE.Q6500~Date,subset(extremeQ_consec,Alt=="260467"),type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(extremeQ_consec,Alt=="260467"&sum.CRE.Q6500>0),points(sum.CRE.Q6500~Date,pch=21,bg="goldenrod1",cex=1.25,lwd=0.1))
with(subset(extremeQ_consec,Alt=="260467"&sum.CRE.Q6500.LOK>0),points(sum.CRE.Q6500.LOK~Date,pch=21,bg="darkslategray1"))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
abline(h=c(14,30,60,90),lty=2,col=adjustcolor("indianred1",0.5),lwd=1.25)
mtext(side=3,adj=0,"CRE - Consecutive Extreme Events (> 6500 cfs)")
mtext(side=3,adj=1,"Model Index: 260467")
mtext(side=1,line=1.5,"Date")
mtext(side=2,line=2.5,"Event Duration (Days)")

par(mar=c(0.1,0.1,0.1,0.1))
plot(0:1,0:1,type="n",ann=F,axes=F)
legend(0.5,0.5,legend=c("Basin + LOK","LOK"),
       pch=c(21),lwd=c(0.1),lty=0,
       pt.bg=c("goldenrod1","darkslategray1"),pt.cex=1.5,ncol=2,
       cex=1,bty="n",y.intersp=1,
       x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()



xlim.val=date.fun(c("1997-01-01","1998-10-01"));xmaj=seq(xlim.val[1],xlim.val[2],"6 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(0,16e3);by.y=5e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/CRE_highQ_event1998_TSP.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3,0.5,1),oma=c(0.5,2,1,0.25),lwd=0.5);
# layout(matrix(1:2,2,1),heights=c(1,0.2))

plot(S79~Date,subset(extremeQ_consec,Alt=="260467"),type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,yaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(extremeQ_consec,Alt=="260467"),shaded.range(Date,rep(-100,length(Date)),S79,bg="dodgerblue1",lty=1))
# with(subset(extremeQ_consec,Alt=="260467"),lines(Date,S79,lwd=1.5,col="dodgerblue1"))
abline(h=c(750,2100,2600,6500),lty=2,col=adjustcolor("indianred1",0.5),lwd=1.25)
text(date.fun("1997-01-15"),6500,"Extreme",pos=3,col=adjustcolor("indianred1",0.75),font=4,offset=0.1,cex=0.5)
text(date.fun("1997-01-15"),2600+((6500-2600)/2),"Damaging",pos=3,col=adjustcolor("indianred1",0.75),font=4,offset=0.1,cex=0.5)
text(date.fun("1997-01-15"),2100+((2600-2100)/2),"Stress",col=adjustcolor("indianred1",0.75),font=4,cex=0.5)
text(date.fun("1997-01-15"),750+((2100-750)/2),"Optimum",col=adjustcolor("indianred1",0.75),font=4,cex=0.5)
text(date.fun("1997-01-15"),0+((750-0)/2),"Low",col=adjustcolor("indianred1",0.75),font=4,cex=0.5)

axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=1,"Model Index: 260467")
mtext(side=1,line=1.5,"Date (Month - Year)")
mtext(side=2,line=3.25,"S-79 Discharge (ft\u00B3 sec\u207B\u00B9)")
dev.off()



xlabs=c("< 14", "14 - 30","30 - 60","60 - 90","> 90")
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/CRE_highQ_events.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,4,1,0.25),lwd=0.5);
layout(matrix(c(1:10),5,2,byrow=F))

ylim.max=c(180,20,20,10,4)
for(i in 1:nrow(rslt.CREHigh.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.CREHigh.sum[i,2:ncol(rslt.CREHigh.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"),cex=0.75)
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"")}
}

for(i in 1:nrow(rslt.CREHigh.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.CREHigh.LOK.sum[i,2:ncol(rslt.CREHigh.LOK.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"CRE Extreme from LOK (> 6500 cfs)")}
}
mtext(side=1,line=2,"Alternatives",outer=T)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/SLE_highQ_events.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,4,1,0.25),lwd=0.5);
layout(matrix(c(1:8),4,2,byrow=F))

ylim.max=c(650,20,10,2,10)
for(i in 1:nrow(rslt.SLEHigh.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.SLEHigh.sum[i,2:ncol(rslt.SLEHigh.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==nrow(rslt.SLEHigh.sum)){axis_fun(1,x,x,alts.sort,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"))
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"SLE Extreme (> 4000 cfs)")}
}

ylim.max=c(650,20,10,2,10)
for(i in 1:nrow(rslt.SLEHigh.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.SLEHigh.LOK.sum[i,2:ncol(rslt.SLEHigh.LOK.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==nrow(rslt.SLEHigh.sum)){axis_fun(1,x,x,alts.sort,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"SLE Extreme from LOK (> 4000 cfs)")}
}
mtext(side=1,line=2,"Alternatives",outer=T)
dev.off()


# Lake Discharges ---------------------------------------------------------
q.dat.xtab$S79_GT2100=with(q.dat.xtab,ifelse(S79>=2100,1,0))
q.dat.xtab$S80_GT1400=with(q.dat.xtab,ifelse(S80>=1400,1,0))
q.dat.xtab$S79_LT2100=with(q.dat.xtab,ifelse(S79<2100,1,0))
q.dat.xtab$S80_LT1400=with(q.dat.xtab,ifelse(S80<1400,1,0))


CRE.GT2100_annual=ddply(q.dat.xtab,c("CY","Alt"),summarise,Q.Lake.GT2100=sum(cfs.to.acftd(S79_QPFCSOURCE_LAKE[S79_GT2100==1])/1000,na.rm=T))
CRE.GT2100_annual.mean=ddply(CRE.GT2100_annual,"Alt",summarise,mean.val=mean(Q.Lake.GT2100))
CRE.GT2100_annual.mean=CRE.GT2100_annual.mean[match(alts.sort,CRE.GT2100_annual.mean$Alt),]
CRE.GT2100_annual.mean

SLE.GT1400_annual=ddply(q.dat.xtab,c("CY","Alt"),summarise,Q.Lake.GT1400=sum(cfs.to.acftd(S80_QPFCSOURCE_LAKE[S80_GT1400==1])/1000,na.rm=T))
SLE.GT1400_annual.mean=ddply(SLE.GT1400_annual,"Alt",summarise,mean.val=mean(Q.Lake.GT1400))
SLE.GT1400_annual.mean=SLE.GT1400_annual.mean[match(alts.sort,SLE.GT1400_annual.mean$Alt),]
SLE.GT1400_annual.mean

# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/Lakedischarge_Annualmean.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:2),1,2,byrow=T))
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);

ylim.val=c(0,600);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(CRE.GT2100_annual.mean$mean.val,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(CRE.GT2100_annual.mean$mean.val,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"Mean Annual Discharge\n\u22652100 cfs at S-79",cex=0.75)
text(x,CRE.GT2100_annual.mean$mean.val,round(CRE.GT2100_annual.mean$mean.val,0),col="black",pos=1,cex=1)
# mtext(side=3,adj=1,"CY1965 - 2016",cex=0.75)
mtext(side=1,line=2.5,"Model Index")
mtext(side=2,line=2.5,"Lake Discharge\n(x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=3,line=-1.25,adj=0," CRE")
# mtext(side=1,line=3,adj=1,"Flow Tag: S79_QPFCSOURCE_LAKE",cex=0.5,col=adjustcolor("black",0.5),font=3)

ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(SLE.GT1400_annual.mean$mean.val,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(SLE.GT1400_annual.mean$mean.val,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"Mean Annual Discharge\n\u22651400 cfs at S-80",cex=0.75)
mtext(side=3,line=-1.25,adj=0," SLE")
text(x,SLE.GT1400_annual.mean$mean.val,round(SLE.GT1400_annual.mean$mean.val,0),col="black",pos=1,cex=1)
mtext(side=3,adj=1,"CY1965 - 2016",cex=0.75)
mtext(side=1,line=2.5,"Model Index")
dev.off()


# Lake Stage --------------------------------------------------------------

n.alts=length(alts.iter2)
lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts.iter2[i],"/RSMBN_output.dss"))
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts.iter2[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
unique(lakeO.stage$Alt)
range(lakeO.stage$Date)

n.alts=1
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3_Batch/20211027/RSMBN/model_",tsp.mod[i],"/RSMBN_output.dss"))  
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=tsp.mod[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
unique(lakeO.stage$Alt)


lakeO.stage$month=as.numeric(format(lakeO.stage$Date,'%m'))
lakeO.stage$day=as.numeric(format(lakeO.stage$Date,'%d'))
lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,'%Y'))
lakeO.stage$WY=WY(lakeO.stage$Date)
lakeO.stage$low.stg=with(lakeO.stage,ifelse(STAGE<11,1,0))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$High.stg=with(lakeO.stage,ifelse(STAGE>16,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))

ann.peak=ddply(lakeO.stage,c("CY","Alt"),summarise,max.stg=max(STAGE,na.rm=T))
ann.peak$GT17=with(ann.peak,ifelse(round(max.stg,1)>=16.9,1,0))
subset(ann.peak,Alt=="CC"&GT17==1)
subset(ann.peak,Alt=="CC")
subset(ann.peak,Alt=="NA25"&GT17==1)

ann.peak2=merge(subset(ann.peak,Alt%in%c("NA25","260467")),data.frame(Alt=c("NA25","260467"),plot.y=1:2),"Alt")
ann.peak=merge(ann.peak,data.frame(Alt=alts.sort,plot.y=1:4),"Alt")
subset(ann.peak,Alt=="ECBr")

# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/LOK_GT17_timeline.png"),width=7,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,1.25,0.5),oma=c(2,2,1,1),lwd=0.1);

xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0.75,4.25);by.y=1;ymaj=1:4#seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(plot.y~CY,ann.peak,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
for(i in 1:4){
points(plot.y~CY,subset(ann.peak,GT17==1&Alt==alts.sort[i]),pch=21,bg=adjustcolor(cols[i],0.5),cex=1.5)
}
axis_fun(2,1:4,1:4,alts.sort)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
mtext(side=2,line=4,"Model Index")
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=3,adj=0,line=1,"Lake Okeechobee")
mtext(side=3,adj=0,cex=0.75,"Annual maximum stage \u2265 17Ft NGVD29",col="grey50")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/LOK_GT17_timeline_FWOTSP.png"),width=7,height=2.25,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,1.25,0.5),oma=c(2,2,1,1),lwd=0.1);

xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0.75,2.25);by.y=1;ymaj=1:4#seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(plot.y~CY,ann.peak2,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
points(plot.y~CY,subset(ann.peak2,GT17==1&Alt==alts.sort[2]),pch=21,bg=adjustcolor(cols[2],0.5),cex=1.5)
points(plot.y~CY,subset(ann.peak2,GT17==1&Alt==alts.sort[4]),pch=21,bg=adjustcolor(cols[4],0.5),cex=1.5)
axis_fun(2,1:2,1:2,c("NA25","260467"))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
mtext(side=2,line=4,"Model Index")
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=1,line=2,adj=1,"RSMBN Period of Simulation: 1965 - 2016",cex=0.5,font=3,col="grey50")
mtext(side=3,adj=0,line=1,"Lake Okeechobee")
mtext(side=3,adj=0,cex=0.75,"Annual maximum stage \u2265 17 Ft NGVD29",col="grey50")

dev.off()

library(lmom)
recur.fun=function(x){
  #Sorting data by decreasing order
  sorted.values<-sort(x,decreasing=T)
  
  #Computing the empirical probabilities
  # p<-(c(1:length(sorted.values)))/(length(sorted.values)+1)
  # using ppoints
  # function (n, a = if (n <= 10) 3/8 else 1/2) 
  # {
  #   if (length(n) > 1L) 
  #     n <- length(n)
  #   if (n > 0) 
  #     (1L:n - a)/(n + 1 - 2 * a)
  #   else numeric()
  # }
  p<-ppoints(sorted.values)
  
  #Computing the empirical recurrence time
  tr<-1/p
  
  #Estimating the parameters for Gumbel distribution
  fit<-samlmu(x)
  para<-pelgum(fit)
  para
  
  #Estimating the parameters for Log Pearson type 3 distribution
  para3<-pelpe3(fit)
  para3
  
  
  rslt=data.frame(dat.val=sorted.values,
                  emp.rec.tim=tr,
                  gumbel=1/(1-cdfgum(sorted.values,para)),
                  LP3=1/(1-cdfpe3(sorted.values,para3)))
  
  return(rslt)  
}

recur.alts=data.frame()
for(i in 1:length(alts.sort)){
  tmp=subset(ann.peak,Alt==alts.sort[i])
  recur.tmp=recur.fun(tmp$max.stg)
  
  recur.alts=rbind(recur.alts,
                   data.frame(Alt=alts.sort[i],
                              min16.emp=min(subset(recur.tmp,dat.val>=16)$emp.rec.tim),
                              min16.gumbel=min(subset(recur.tmp,dat.val>=16)$gumbel),
                              min16.LP3=min(subset(recur.tmp,dat.val>=16)$LP3),
                              min17.emp=min(subset(recur.tmp,dat.val>=17)$emp.rec.tim),
                              min17.gumbel=min(subset(recur.tmp,dat.val>=17)$gumbel),
                              min17.LP3=min(subset(recur.tmp,dat.val>=17)$LP3)))
  print(i)
}
recur.alts
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/LOK_GT17_recurrance.png"),width=6.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,1.25,0.5),oma=c(2,2,1,1),lwd=0.1);
ylim.val=c(0,15);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(recur.alts$min17.gumbel,col=NA,border=NA,
          ylim=ylim.val,axes=F,ann=F,
          names.arg = rep(NA,length(recur.alts$min16.LP3)),
          space=c(0),yaxs="i",xaxs="i")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(recur.alts$min17.gumbel,col=adjustcolor(cols,0.5),
          ylim=ylim.val,axes=F,ann=F,
          names.arg = rep(NA,length(recur.alts$min16.LP3)),
          space=c(0),yaxs="i",xaxs="i",add=T)
text(x,recur.alts$min17.gumbel,format(round(recur.alts$min17.gumbel,1)),pos=1,offset=0.25)
axis_fun(1,x,x,alts.sort,las=1,cex=0.8,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,line=1,"Lake Okeechobee")
mtext(side=3,adj=0,"Annual Max Stage \u2265 17 Ft NGVD (Gumbel Distribution)")
mtext(side=3,adj=1,"CY1965 - 2016")
mtext(side=1,line=1.5,"Model Index")
mtext(side=2,line=2,"Return Period (years)")
dev.off()

# Consecutive Events Stage ------------------------------------------------
highstg_consec=data.frame()
for(j in 1:length(alts.sort)){
  tmp=subset(lakeO.stage,Alt==alts.sort[j])
  tmp$stg17=0
  tmp$stg16=0
  for(i in 2:nrow(tmp)){
    # tmp$consec[i]=with(tmp,ifelse(Q_GT2100[i-1]>0&Q_GT2100[i]>0,1,0))
    tmp$stg16[i]=with(tmp,ifelse(High.stg[i-1]==0&High.stg[i]>0,1,
                                 ifelse(High.stg[i-1]>0&High.stg[i]>0,1,0)))
    tmp$stg17[i]=with(tmp,ifelse(vHigh.stg[i-1]==0&vHigh.stg[i]>0,1,
                                 ifelse(vHigh.stg[i-1]>0&vHigh.stg[i]>0,1,0)))
    
  }
  
  highstg=consec.startend(tmp$stg16>0)
  tmp$sum.stg16=0
  for(i in 1:length(highstg$ends)){
    tmp[highstg$ends[i],]$sum.stg16=with(tmp[c(highstg$starts[i]:highstg$ends[i]),],sum(stg16,na.rm=T))
  }
  vhighstg=consec.startend(tmp$stg17>0)
  tmp$sum.stg17=0
  for(i in 1:length(vhighstg$ends)){
    tmp[vhighstg$ends[i],]$sum.stg17=with(tmp[c(vhighstg$starts[i]:vhighstg$ends[i]),],sum(stg17,na.rm=T))
  }
  
  highstg_consec=rbind(tmp,highstg_consec)
  print(j)
}

rslt.stg16=reshape2::dcast(highstg_consec,sum.stg16~Alt,value.var = "sum.stg16",fun.aggregate = function(x)N.obs(x))
rslt.stg16=ddply(highstg_consec,c("Alt","sum.stg16"),summarise,count.event=N.obs(sum.stg16))
max(rslt.stg16$sum.stg16)
rslt.stg16$cat=with(rslt.stg16,ifelse(sum.stg16>0&sum.stg16<15,1,
                                      ifelse(sum.stg16>=15&sum.stg16<30,2,
                                             ifelse(sum.stg16>=30&sum.stg16<60,3,
                                                    ifelse(sum.stg16>=60&sum.stg16<90,4,
                                                           ifelse(sum.stg16>=90&sum.stg16<180,5,
                                                                  ifelse(sum.stg16>=180,6,NA)))))))
rslt.stg17=ddply(highstg_consec,c("Alt","sum.stg17"),summarise,count.event=N.obs(sum.stg17))
max(rslt.stg17$sum.stg17)
rslt.stg17$cat=with(rslt.stg17,ifelse(sum.stg17>0&sum.stg17<15,1,
                                      ifelse(sum.stg17>=15&sum.stg17<30,2,
                                             ifelse(sum.stg17>=30&sum.stg17<60,3,
                                                    ifelse(sum.stg17>=60&sum.stg17<90,4,
                                                           ifelse(sum.stg17>=90&sum.stg17<180,5,
                                                                  ifelse(sum.stg17>=180,6,NA)))))))


# ddply(subset(rslt.stg16,is.na(cat)==F),c("Alt","cat"),summarise,sum.event=sum(count.event,na.rm=T))
rslt.stg16.sum=reshape2::dcast(subset(rslt.stg16,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.stg17.sum=reshape2::dcast(subset(rslt.stg17,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)

rslt.stg16.sum=rslt.stg16.sum[,c("cat",alts.sort)]
rslt.stg17.sum=rslt.stg17.sum[,c("cat",alts.sort)]
xlabs=c("< 15", "15 - 30","30 - 60","60 - 90","90 - 180", "> 180")
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/LOK_highstg_events.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(4,4,1,0.25),lwd=0.5);
layout(matrix(c(1:12),6,2,byrow=F))

# ylim.max=c(150,20,20,10,4)
for(i in 1:length(xlabs)){
  ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.stg16.sum[i,2:ncol(rslt.stg16.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.25)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"),cex=0.75)
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"LOK Daily Stage > 16 Ft NGVD")}
}
for(i in 1:4){
  tmp=t(rslt.stg17.sum[i,2:ncol(rslt.stg17.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.25)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==6){axis_fun(1,x,x,alts.sort,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  # mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"))
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"LOK Daily Stage \u2265 17 Ft NGVD")}
}
for(i in 1:2){
x=barplot(rep(0,4),beside=T,col=adjustcolor(cols,0.5),
          ylim=ylim.val,axes=F,
          names.arg = rep(NA,length(tmp)),
          space=c(0),yaxs="i",xaxs="i")
text(x,rep(0,4),rep(0,4),pos=3,offset=0.25)
axis_fun(2,ymaj,ymin,format(ymaj))
if(i==2){axis_fun(1,x,x,alts.sort,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
box(lwd=1)
}
mtext(side=1,outer=T,line=2.5,"Model Index")

dev.off()


# Regulation Schedules ----------------------------------------------------
zones=c(paste("LOK",paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),sep="-"),
        paste("LOK",paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_"),sep="_"))
zone.alias=data.frame(zone=zones,
                      zone2=c(paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_")))
n.alts=length(alts.iter2)
reg.sch=data.frame()
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts.iter2[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(zones)){
    paths=paste0("/RSMBN/",zones[i],"/STAGE/01JAN1965 - 01JAN2016/1DAY/INPUT/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$zone=zones[i]
    tmp$Alt=alts.iter2[j]
    reg.sch=rbind(tmp,reg.sch)
    print(i)
  }
}

zones=c(paste("LOK",paste("ZONE",c("A","BC","D1","D2"),sep="_"),sep="-"),
        paste("LOWSM",c(30,45,60,"D3_15"),"LEVEL",sep="_"))
zone.alias=rbind(zone.alias,
                 data.frame(
                   zone=c("LOK-ZONE_BC","LOWSM_D3_15_LEVEL"),
                   zone2=c("ZONE_BC","LOWSM_D3_15_LEVEL")))

n.alts=1
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3_Batch/20211027/RSMBN/model_",tsp.mod[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(zones)){
    paths=paste0("/RSMBN/",zones[i],"/STAGE/01JAN1965 - 01JAN2016/1DAY/INPUT/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$zone=zones[i]
    tmp$Alt=tsp.mod[j]
    reg.sch=rbind(tmp,reg.sch)
    print(i)
  }
}

reg.sch$DOY=as.numeric(format(reg.sch$Date,"%j"))
reg.sch=merge(reg.sch,zone.alias,"zone")
reg.sch2=reshape2::dcast(reg.sch,Alt+DOY~zone2,value.var = "STAGE",mean)
reg.sch2$ZONE_D1=with(reg.sch2,ifelse(Alt=="260467"&ZONE_D1<=ZONE_D2,ZONE_D2,ZONE_D1))

max(subset(reg.sch2,Alt=="NA25")$ZONE_A)
max(subset(reg.sch2,Alt=="260467")$ZONE_A)

# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/CC_TSP_REGSCH.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:3,1,3,byrow=T),widths=c(1,1))

xlim.val.date=date.fun(c("1965-01-01","1965-12-31"));xmaj.dat=seq(xlim.val.date[1],xlim.val.date[2],"2 month");xmin.dat=seq(xlim.val.date[1],xlim.val.date[2],"1 month")
xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.dat,"%j"));xmin=as.numeric(format(xmin.dat,"%j"));
ylim.val=c(10,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/0.5)

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="NA25"),lines(DOY,ZONE_A,col="red",lwd=2))
with(subset(reg.sch2,Alt=="NA25"),lines(DOY,ZONE_B,col="deepskyblue2",lwd=2))
with(subset(reg.sch2,Alt=="NA25"),lines(DOY,ZONE_C,col="orange",lwd=2))
with(subset(reg.sch2,Alt=="NA25"),lines(DOY,ZONE_D1,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="NA25"),lines(DOY,ZONE_D2,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="NA25"),lines(DOY,ZONE_D3,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="NA25"),lines(DOY,ZONE_D0,col="grey",lwd=2))
with(subset(reg.sch2,Alt=="NA25"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))
with(subset(reg.sch2,Alt=="NA25"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25"&DOY==90),text(85,ZONE_B,"Zone B",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25"&DOY==105),text(105,ZONE_C,"Zone C",pos=3,col="orange",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25"&DOY==30),text(30,ZONE_D0+(ZONE_C-ZONE_D0)/2,"Zone D",col="grey",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25"&DOY==30),text(30,LOWSM_15_LEVEL+(13-LOWSM_15_LEVEL)/2,"Zone F",col="purple",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Date (Month)")
mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Alternative NA25 (LORS08)")

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_A,col="red",lwd=2))
with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_B,col="deepskyblue2",lwd=2))
with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_C,col="orange",lwd=2))
with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_D1,col="grey",lwd=2))
abline(h=13,col="green3",lwd=2)
with(subset(reg.sch2,Alt=="CC"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))
with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="CC"&DOY==90),text(85,ZONE_B,"Zone B",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="CC"&DOY==105),text(105,ZONE_C,"Zone C",pos=3,col="orange",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,ZONE_D1+(ZONE_C-ZONE_D1)/2,"Zone D",col="grey",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="CC"&DOY==300),text(300,13,"Zone E",pos=3,col="green3",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,LOWSM_15_LEVEL+(13-LOWSM_15_LEVEL)/2,"Zone F",col="purple",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="CC"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Date (Month)")
# mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Alternative CC")

# xlim.val.date=date.fun(c("1965-01-01","1965-12-31"));xmaj.dat=seq(xlim.val.date[1],xlim.val.date[2],"2 month");xmin.dat=seq(xlim.val.date[1],xlim.val.date[2],"1 month")
# xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.dat,"%j"));xmin=as.numeric(format(xmin.dat,"%j"));
# ylim.val=c(10,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/0.5)

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,ZONE_A,col="red",lwd=2))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,ZONE_BC,col="deepskyblue2",lwd=2))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,ZONE_D1,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,ZONE_D2,col="grey",lwd=2))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,LOWSM_D3_15_LEVEL,col="purple",lwd=2))

with(subset(reg.sch2,Alt=="260467"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="260467"&DOY==90),text(85,ZONE_BC,"Zone BC",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="260467"&DOY==30),text(85,LOWSM_D3_15_LEVEL+(ZONE_BC-LOWSM_D3_15_LEVEL)/2,"Zone D",col="grey",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="260467"&DOY==250),text(250,10+(LOWSM_D3_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Date (Month)")
# mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Iteration 3 - Run 260467")
dev.off()


##
head(lakeO.stage)
reg.sch3=reshape2::dcast(reg.sch,Alt+Date~zone2,value.var = "STAGE",mean)

vars=c("Alt","Date","STAGE")
lakeO.stage.reg=merge(lakeO.stage[,vars],
                      reg.sch3,c("Alt","Date"))


lakeO.stage.reg1=subset(lakeO.stage.reg,Alt!="260467")
lakeO.stage.reg1$above.ZoneD=with(lakeO.stage.reg1,ifelse(STAGE>ZONE_C,1,0))
lakeO.stage.reg1$within.ZoneD=with(lakeO.stage.reg1,ifelse(STAGE<ZONE_C&STAGE>ZONE_D0,1,0))
lakeO.stage.reg1$below.ZoneD=with(lakeO.stage.reg1,ifelse(STAGE<ZONE_D0,1,0))

lakeO.stage.reg2=subset(lakeO.stage.reg,Alt=="260467")
lakeO.stage.reg2$above.ZoneD=with(lakeO.stage.reg2,ifelse(STAGE>ZONE_BC,1,0))
lakeO.stage.reg2$within.ZoneD=with(lakeO.stage.reg2,ifelse(STAGE<ZONE_BC&STAGE>LOWSM_D3_15_LEVEL,1,0))
lakeO.stage.reg2$below.ZoneD=with(lakeO.stage.reg2,ifelse(STAGE<LOWSM_D3_15_LEVEL,1,0))

lakeO.stage.reg3=rbind(lakeO.stage.reg1,lakeO.stage.reg2)

zone.freq1=ddply(subset(lakeO.stage.reg,Alt!="260467"),"Alt",summarise,
                above.zoneD=sum(STAGE>ZONE_C),
                zoneD=sum(ifelse(STAGE<ZONE_C&STAGE>ZONE_D0,1,0)),
                below.zoneD=sum(STAGE<ZONE_D0))
zone.freq2=ddply(subset(lakeO.stage.reg,Alt=="260467"),"Alt",summarise,
                above.zoneD=sum(STAGE>ZONE_BC),
                zoneD=sum(ifelse(STAGE<ZONE_BC&STAGE>LOWSM_D3_15_LEVEL,1,0)),
                below.zoneD=sum(STAGE<LOWSM_D3_15_LEVEL))
zone.freq=rbind(zone.freq1,zone.freq2)
zone.freq=zone.freq[match(alts.sort,zone.freq$Alt),]
zone.freq[,2:4]=(zone.freq[,2:4]/18993)*100
zone.freq.all=zone.freq
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/LOK_Stage_RegSch.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

# cols=c("orange","grey","purple")
cols4=wesanderson::wes_palette("Zissou1", 3, type = "continuous")
zone.freq2=t(zone.freq[,2:4])[3:1,]
x=barplot(zone.freq2,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,length(alts.sort)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(zone.freq2,beside=F,ylim=ylim.val,col=adjustcolor(cols4,0.5),axes=F,ann=F,names.arg=rep(NA,length(alts.sort)),add=T)
text(x,zone.freq2[1,]/2,format(round(zone.freq2[1,],1)),xpd=NA,font=2)
text(x,zone.freq2[1,]+zone.freq2[2,]/2,format(round(zone.freq2[2,],1)),xpd=NA,font=2)
text(x,colSums(zone.freq2[1:2,])+(zone.freq2[3,])/2,format(round(zone.freq2[3,],1)),xpd=NA,font=2)
axis_fun(1,x,x,alts.sort)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Percent of Days\nDuring Period of Simulation")
mtext(side=1,line=1.75,"Model Index")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("Above Zone D","Within Zone D","Below Zone D"),
       pch=c(22,22,22),
       lty=0,lwd=0.01,
       col="black",
       pt.bg=adjustcolor(rev(cols4),0.5),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=1,line=-1,adj=1,"Simulation Period of Record\n(1965-2016; 18993 days)",cex=0.75)
dev.off()



##### 
zone.freq=subset(zone.freq,Alt%in%c("NA25","260467"))
sch.LORS=read.csv("C:/Julian_LaCie/_Github/CRE_Conditions/Data/LORS.csv")
sch.LORS$Date=with(sch.LORS,date.fun(paste("1900",Month,Day,sep="-")))



txt.cex=0.75
title.cex=0.75
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/LORS_TSP.png"),width=7,height=3,units="in",res=200,type="windows",bg="white")
old.par=par(family="serif",mar=c(2,3,0.5,1),oma=c(2,1.5,1,1));
layout(matrix(c(1:3),1,3,byrow=F))
cols4=wesanderson::wes_palette("Zissou1", 3, type = "continuous")

ylim.val=c(10,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1900-01-01","1900-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
plot(High~Date,sch.LORS,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",type="n")
# abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
xx=with(sch.LORS,c(Date,rev(Date)))
with(sch.LORS,polygon(x=xx,c(rep(19,length(Date)),rev(Low)),col=adjustcolor(cols4[3],0.5),border=NA))
with(sch.LORS,polygon(x=xx,c(Low,rev(BeneficialUse)),col=adjustcolor(cols4[2],0.5),border=NA))
with(sch.LORS,polygon(x=xx,c(BeneficialUse,rep(9,length(Date))),col=adjustcolor(cols4[1],0.5),border=NA))

with(sch.LORS,lines(High~Date,lwd=1.25))
with(sch.LORS,lines(Intermediate~Date,lwd=1.25))
with(sch.LORS,lines(Low~Date,lwd=1.25))
with(sch.LORS,lines(LowMid~Date,lty=2,col="grey40"))
with(sch.LORS,lines(Inter1ft~Date,lty=2))
with(sch.LORS,lines(LowLow~Date,lty=2,col="grey40"))
with(sch.LORS,lines(BaseFlow~Date,lwd=1.25))
with(sch.LORS,lines(BeneficialUse~Date,lwd=1.25))
with(sch.LORS,lines(WSM~Date,col="grey40",lwd=1.25))
text(date.fun("1900-07-15"),17.5,"High Lake Management Band",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),17,"High",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),16.25,"Intermediate",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),14.5,"Low",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),13,"Base Flow",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),12,"Beneficial Use",font=4,cex=txt.cex)
text(date.fun("1900-08-01"),10.25,"Water Shortage Management Band",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=3,adj=0,"LORS08 (2008)",cex=title.cex)
mtext(side=2,line=2.25,"Stage Elevation (Ft, NGVD29)")

par(mar=c(2,4,0.5,1))
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
zone.freq2=t(zone.freq[,2:4])[3:1,]
x=barplot(zone.freq2,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(zone.freq)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(zone.freq2,beside=F,ylim=ylim.val,col=adjustcolor(cols4,0.5),axes=F,ann=F,names.arg=rep(NA,nrow(zone.freq)),add=T)
text(x,zone.freq2[1,]/2,format(round(zone.freq2[1,],1)),xpd=NA,font=2)
text(x,zone.freq2[1,]+zone.freq2[2,]/2,format(round(zone.freq2[2,],1)),xpd=NA,font=2)
text(x,colSums(zone.freq2[1:2,])+(zone.freq2[3,])/2,format(round(zone.freq2[3,],1)),xpd=NA,font=2)
axis_fun(1,x,x,c("NA25\n(FWO)","260467\n(TSP)"),line=0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Percent of Days\nDuring Period of Simulation",cex=0.9)
mtext(side=1,line=2.75,"Model Index")

par(mar=c(2,3,0.5,1))
xlim.val.date=date.fun(c("1965-01-01","1965-12-31"));xmaj.dat=seq(xlim.val.date[1],xlim.val.date[2],"2 month");xmin.dat=seq(xlim.val.date[1],xlim.val.date[2],"1 month")
xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.dat,"%j"));xmin=as.numeric(format(xmin.dat,"%j"));
ylim.val=c(10,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
# abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
xx=with(subset(reg.sch2,Alt=="260467"),c(DOY,rev(DOY)))
with(subset(reg.sch2,Alt=="260467"),polygon(x=xx,c(rep(19,length(DOY)),rev(ZONE_BC)),col=adjustcolor(cols4[3],0.5),border=NA))
with(subset(reg.sch2,Alt=="260467"),polygon(x=xx,c(ZONE_BC,rev(LOWSM_D3_15_LEVEL)),col=adjustcolor(cols4[2],0.5),border=NA))
with(subset(reg.sch2,Alt=="260467"),polygon(x=xx,c(LOWSM_D3_15_LEVEL,rep(9,length(DOY))),col=adjustcolor(cols4[1],0.5),border=NA))

with(subset(reg.sch2,Alt=="260467"),lines(DOY,ZONE_A,col="black",lwd=1.25))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,ZONE_BC,col="black",lwd=1.25))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,ZONE_D1,col="grey40",lwd=1.25,lty=2))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,ZONE_D2,col="grey40",lwd=1.25))
with(subset(reg.sch2,Alt=="260467"),lines(DOY,LOWSM_D3_15_LEVEL,col="black",lwd=1.25))
with(subset(reg.sch2,Alt=="260467"&DOY==30),text(196,ZONE_A,"Zone A\n(High Lake Management Band)",pos=3,col="black",cex=txt.cex,font=4))
with(subset(reg.sch2,Alt=="260467"&DOY==166),text(166,ZONE_BC-(ZONE_BC*0.025),"Zone BC\n(High & Intermediate Band)",pos=3,col="black",cex=txt.cex,font=4))
with(subset(reg.sch2,Alt=="260467"&DOY==30),text(105,LOWSM_D3_15_LEVEL+(ZONE_BC-LOWSM_D3_15_LEVEL)/2,"Zone D\n(Low Band)",col="black",cex=txt.cex,font=4))
with(subset(reg.sch2,Alt=="260467"&DOY==250),text(260,10+(LOWSM_D3_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="black",cex=txt.cex,font=4))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=2,line=2.25,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Run 260467 (LOSOM)",cex=title.cex)
dev.off()



unique( q.dat.xtab$Alt)

head(lakeO.stage.reg3)
vars=c("Date","Alt","above.ZoneD","within.ZoneD","below.ZoneD")

q.dat.xtab.zones=merge(q.dat.xtab,lakeO.stage.reg3[,vars],c("Date","Alt"))

q.dat.xtab.zones$above.ZoneD.CRE=with(q.dat.xtab.zones,S79*above.ZoneD)
q.dat.xtab.zones$within.ZoneD.CRE=with(q.dat.xtab.zones,S79*within.ZoneD)
q.dat.xtab.zones$below.ZoneD.CRE=with(q.dat.xtab.zones,S79*below.ZoneD)
q.dat.xtab.zones$above.ZoneD.CRE.lake=with(q.dat.xtab.zones,S79_QPFCSOURCE_LAKE*above.ZoneD)
q.dat.xtab.zones$within.ZoneD.CRE.lake=with(q.dat.xtab.zones,S79_QPFCSOURCE_LAKE*within.ZoneD)
q.dat.xtab.zones$below.ZoneD.CRE.lake=with(q.dat.xtab.zones,S79_QPFCSOURCE_LAKE*below.ZoneD)

q.dat.xtab.zones$above.ZoneD.SLE=with(q.dat.xtab.zones,S80*above.ZoneD)
q.dat.xtab.zones$within.ZoneD.SLE=with(q.dat.xtab.zones,S80*within.ZoneD)
q.dat.xtab.zones$below.ZoneD.SLE=with(q.dat.xtab.zones,S80*below.ZoneD)
q.dat.xtab.zones$above.ZoneD.SLE.lake=with(q.dat.xtab.zones,S80_QPFCSOURCE_LAKE*above.ZoneD)
q.dat.xtab.zones$within.ZoneD.SLE.lake=with(q.dat.xtab.zones,S80_QPFCSOURCE_LAKE*within.ZoneD)
q.dat.xtab.zones$below.ZoneD.SLE.lake=with(q.dat.xtab.zones,S80_QPFCSOURCE_LAKE*below.ZoneD)

vars=c("Date","Alt","above.ZoneD.CRE","within.ZoneD.CRE","below.ZoneD.CRE")
q.dat.xtab.zones.melt=reshape2::melt(q.dat.xtab.zones[,vars],id.vars=vars[1:2])
q.dat.xtab.zones.melt2=subset(q.dat.xtab.zones.melt,Alt%in%c("NA25",'260467'))

library(ggplot2)
q.dat.xtab.zones.melt$log.value=log(q.dat.xtab.zones.melt$value)
tmp=ggplot(subset(q.dat.xtab.zones.melt,Alt=="260467"),aes(x=log.value,fill=variable))+
  # scale_x_continuous(trans="log")+
  geom_density(alpha=0.25)
ggplot(subset(q.dat.xtab.zones.melt,Alt=="NA25"),aes(x=log.value,fill=variable))+
  # scale_x_continuous(trans="log")+
  geom_density(alpha=0.25)
tmp
p=ggplot_build(tmp)
head(p$data[[1]])
p$data[[1]]$x=exp(p$data[[1]]$x)
plot(y~x,subset(p$data[[1]],group==3),log="x")
with(subset(p$data[[1]],group==2),lines(x,y,col="red"))
with(subset(p$data[[1]],group==1),lines(x,y,col="blue"))

# get("compute_group", ggplot2::StatDensity)
# get("compute_group", ggvis::compute_density)

ggplot(subset(q.dat.xtab.zones.melt,Alt=="260467"&variable=="below.ZoneD"),aes(x=value))+
  geom_density()


# https://rdrr.io/cran/ggplot2/src/R/stat-density.r
# tmp=subset(q.dat.xtab.zones.melt,Alt=="260467"&variable=="below.ZoneD")
# nx=length(tmp$value)
# w=rep(1/nx,nx)
# from=min(subset(q.dat.xtab.zones.melt,Alt=="260467")$value,na.rm=T)
# to=max(subset(q.dat.xtab.zones.melt,Alt=="260467")$value,na.rm=T)
# bw = "nrd0"
# adjust = 1
# kernel = "gaussian"
# n = 512
# dens <- stats::density(tmp$value, weights = w, bw = bw, adjust = adjust,
#                        kernel = kernel, n = n, from = from, to = to)
# plot(dens,log="x")

vars=c("above.ZoneD.CRE","within.ZoneD.CRE","below.ZoneD.CRE")
density.group.TSP=data.frame()
for(i in 1:3){
tmp=subset(q.dat.xtab.zones.melt,Alt=="260467")
rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
nx=length(subset(tmp,variable==vars[i])$value)
w=rep(1/nx,nx)
n.den=512#nx; #default is 512
dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
             # weights=w,   
             kernel="gaussian",
             from=rng.val[1],
             to=rng.val[2],
             n = n.den)
dens=data.frame(x=exp(dens$x),
                y=dens$y,
                scaled =  dens$y / max(dens$y, na.rm = TRUE),
                ndensity = dens$y / max(dens$y, na.rm = TRUE),
                count=dens$y*nx,
                n=nx,
                group=vars[i])
density.group.TSP=rbind(density.group.TSP,dens)
}
plot(y~x,density.group.TSP,log="x")

density.group.NA25=data.frame()
for(i in 1:3){
  tmp=subset(q.dat.xtab.zones.melt,Alt=="NA25")
  rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
  nx=length(subset(tmp,variable==vars[i])$value)
  w=rep(1/nx,nx)
  n.den=512# nx; #default is 512
  dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
               # weights=w,   
               kernel="gaussian",
               from=rng.val[1],
               to=rng.val[2],
               n =n.den)
  dens=data.frame(x=exp(dens$x),
                  y=dens$y,
                  scaled =  dens$y / max(dens$y, na.rm = TRUE),
                  ndensity = dens$y / max(dens$y, na.rm = TRUE),
                  count=dens$y*nx,
                  n=nx,
                  group=vars[i])
  density.group.NA25=rbind(density.group.NA25,dens)
}
plot(y~x,density.group.NA25,log="x")

# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/TSP_S79Q_zones.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,1.5,1,0.5));
layout(matrix(c(1:2),2,1,byrow=F))

ylim.val=c(0,2);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(10,20000);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(y~x,density.group.NA25,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(density.group.NA25,group=="below.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(density.group.NA25,group=="within.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(density.group.NA25,group=="above.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Density")
mtext(side=3,adj=0,"NA25 (FWO)")

legend("topleft",legend=c("Above Zone D","Within Zone D","Below Zone D"),
       pch=c(22,22,22),
       lty=0,lwd=0.01,
       col=rev(cols4),
       pt.bg=adjustcolor(rev(cols4),0.5),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

plot(y~x,density.group.TSP,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(density.group.TSP,group=="below.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(density.group.TSP,group=="within.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(density.group.TSP,group=="above.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Density")
mtext(side=3,adj=0,"Run 260467")
mtext(side=1,line=2,"S-79 Daily Discharge (ft\u00B3 sec\u207B\u00B9)")
dev.off()

vars=c("above.ZoneD.CRE.lake","within.ZoneD.CRE.lake","below.ZoneD.CRE.lake")
vars2=c("Date","Alt",vars)
q.dat.xtab.zones.melt=reshape2::melt(q.dat.xtab.zones[,vars2],id.vars=vars2[1:2])
q.dat.xtab.zones.melt2=subset(q.dat.xtab.zones.melt,Alt%in%c("NA25",'260467'))

density.group.TSP=data.frame()
for(i in 1:3){
  tmp=subset(q.dat.xtab.zones.melt,Alt=="260467")
  rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
  nx=length(subset(tmp,variable==vars[i]&value!=0)$value)
  w=rep(1/nx,nx)
  n.den=nx*2; #default is 512
  if(length(subset(tmp,variable==vars[i]&value!=0)$value)<2){
    dens=data.frame(x=NA,
                    y=NA,
                    scaled =NA,
                    ndensity = NA,
                    count=NA,
                    n=NA,
                    group=vars[i])
  }else{
    dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
                 weights=w,   
                 kernel="gaussian",
                 from=rng.val[1],
                 to=rng.val[2],
                 n = n.den)
    dens=data.frame(x=exp(dens$x),
                    y=dens$y,
                    scaled =  dens$y / max(dens$y, na.rm = TRUE),
                    ndensity = dens$y / max(dens$y, na.rm = TRUE),
                    count=dens$y*nx,
                    n=nx,
                    group=vars[i])}
  density.group.TSP=rbind(density.group.TSP,dens)
}
plot(y~x,density.group.TSP,log="x")

density.group.NA25=data.frame()
for(i in 1:3){
  tmp=subset(q.dat.xtab.zones.melt,Alt=="NA25")
  rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
  nx=length(subset(tmp,variable==vars[i]&value!=0)$value)
  w=rep(1/nx,nx)
  n.den=nx*2; #default is 512
  if(length(subset(tmp,variable==vars[i]&value!=0)$value)<2){
    dens=data.frame(x=NA,
                    y=NA,
                    scaled =NA,
                    ndensity = NA,
                    count=NA,
                    n=NA,
                    group=vars[i])
  }else{
    dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
                 weights=w,   
                 kernel="gaussian",
                 from=rng.val[1],
                 to=rng.val[2],
                 n = n.den)
    dens=data.frame(x=exp(dens$x),
                    y=dens$y,
                    scaled =  dens$y / max(dens$y, na.rm = TRUE),
                    ndensity = dens$y / max(dens$y, na.rm = TRUE),
                    count=dens$y*nx,
                    n=nx,
                    group=vars[i])}
  density.group.NA25=rbind(density.group.NA25,dens)
}
plot(y~x,density.group.NA25,log="x")

# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/TSP_S79_QFC_Q_zones.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,1.5,1,0.5));
layout(matrix(c(1:2),2,1,byrow=F))

ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10000);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(y~x,density.group.NA25,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(density.group.NA25,group=="below.ZoneD.CRE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(density.group.NA25,group=="within.ZoneD.CRE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(density.group.NA25,group=="above.ZoneD.CRE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Density")
mtext(side=3,adj=0,"NA25 (FWO)")

legend("topleft",legend=c("Above Zone D","Within Zone D","Below Zone D"),
       pch=c(22,22,22),
       lty=0,lwd=0.01,
       col=rev(cols4),
       pt.bg=adjustcolor(rev(cols4),0.5),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

ylim.val=c(0,0.4);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(y~x,density.group.TSP,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(density.group.TSP,group=="below.ZoneD.CRE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(density.group.TSP,group=="within.ZoneD.CRE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(density.group.TSP,group=="above.ZoneD.CRE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Density")
mtext(side=3,adj=0,"Run 260467")
mtext(side=1,line=2,"S-79 from Lake Daily Discharge (ft\u00B3 sec\u207B\u00B9)")
dev.off()

###
vars=c("above.ZoneD.SLE","within.ZoneD.SLE","below.ZoneD.SLE")
vars2=c("Date","Alt",vars)
q.dat.xtab.zones.melt=reshape2::melt(q.dat.xtab.zones[,vars2],id.vars=vars2[1:2])
q.dat.xtab.zones.melt2=subset(q.dat.xtab.zones.melt,Alt%in%c("NA25",'260467'))

ggplot(subset(q.dat.xtab.zones.melt,Alt=="260467"),aes(x=value,fill=variable))+
  scale_x_continuous(trans="log")+
  geom_density(alpha=0.25)
ggplot(subset(q.dat.xtab.zones.melt,Alt=="NA25"),aes(x=value,fill=variable))+
  scale_x_continuous(trans="log")+
  geom_density(alpha=0.25)

vars=c("above.ZoneD.SLE","within.ZoneD.SLE","below.ZoneD.SLE")
density.group.TSP=data.frame()
for(i in 1:3){
  tmp=subset(q.dat.xtab.zones.melt,Alt=="260467")
  rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
  nx=length(subset(tmp,variable==vars[i]&value!=0)$value)
  w=rep(1/nx,nx)
  n.den=nx; #default is 512
  if(length(subset(tmp,variable==vars[i]&value!=0)$value)<2){
    dens=data.frame(x=NA,
                    y=NA,
                    scaled =NA,
                    ndensity = NA,
                    count=NA,
                    n=NA,
                    group=vars[i])
  }else{
  dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
               weights=w,   
               kernel="gaussian",
               from=rng.val[1],
               to=rng.val[2],
               n = n.den)
  dens=data.frame(x=exp(dens$x),
                  y=dens$y,
                  scaled =  dens$y / max(dens$y, na.rm = TRUE),
                  ndensity = dens$y / max(dens$y, na.rm = TRUE),
                  count=dens$y*nx,
                  n=nx,
                  group=vars[i])}
  density.group.TSP=rbind(density.group.TSP,dens)
}
plot(y~x,density.group.TSP,log="x")

density.group.NA25=data.frame()
for(i in 1:3){
  tmp=subset(q.dat.xtab.zones.melt,Alt=="NA25")
  rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
  nx=length(subset(tmp,variable==vars[i]&value!=0)$value)
  w=rep(1/nx,nx)
  n.den=nx; #default is 512
  if(length(subset(tmp,variable==vars[i]&value!=0)$value)<2){
    dens=data.frame(x=NA,
                    y=NA,
                    scaled =NA,
                    ndensity = NA,
                    count=NA,
                    n=NA,
                    group=vars[i])
  }else{
    dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
                 weights=w,   
                 kernel="gaussian",
                 from=rng.val[1],
                 to=rng.val[2],
                 n = n.den)
    dens=data.frame(x=exp(dens$x),
                    y=dens$y,
                    scaled =  dens$y / max(dens$y, na.rm = TRUE),
                    ndensity = dens$y / max(dens$y, na.rm = TRUE),
                    count=dens$y*nx,
                    n=nx,
                    group=vars[i])}
  density.group.NA25=rbind(density.group.NA25,dens)
}
plot(y~x,density.group.NA25,log="x")



# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/TSP_S80Q_zones.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,1.5,1,0.5));
layout(matrix(c(1:2),2,1,byrow=F))

ylim.val=c(0,4.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10000);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(y~x,density.group.NA25,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(density.group.NA25,group=="below.ZoneD.SLE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(density.group.NA25,group=="within.ZoneD.SLE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(density.group.NA25,group=="above.ZoneD.SLE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Density")
mtext(side=3,adj=0,"NA25 (FWO)")

legend("topleft",legend=c("Above Zone D","Within Zone D","Below Zone D"),
       pch=c(22,22,22),
       lty=0,lwd=0.01,
       col=rev(cols4),
       pt.bg=adjustcolor(rev(cols4),0.5),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

# ylim.val=c(0,4.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(y~x,density.group.TSP,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(density.group.TSP,group=="below.ZoneD.SLE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(density.group.TSP,group=="within.ZoneD.SLE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(density.group.TSP,group=="above.ZoneD.SLE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Density")
mtext(side=3,adj=0,"Run 260467")
mtext(side=1,line=2,"S-80 Daily Discharge (ft\u00B3 sec\u207B\u00B9)")
dev.off()


### 
vars=c("above.ZoneD.SLE.lake","within.ZoneD.SLE.lake","below.ZoneD.SLE.lake")
vars2=c("Date","Alt",vars)
q.dat.xtab.zones.melt=reshape2::melt(q.dat.xtab.zones[,vars2],id.vars=vars2[1:2])
q.dat.xtab.zones.melt2=subset(q.dat.xtab.zones.melt,Alt%in%c("NA25",'260467'))

ggplot(subset(q.dat.xtab.zones.melt,Alt=="260467"),aes(x=value,fill=variable))+
  scale_x_continuous(trans="log")+
  geom_density(alpha=0.25)
ggplot(subset(q.dat.xtab.zones.melt,Alt=="NA25"),aes(x=value,fill=variable))+
  scale_x_continuous(trans="log")+
  geom_density(alpha=0.25)

density.group.TSP=data.frame()
for(i in 1:3){
  tmp=subset(q.dat.xtab.zones.melt,Alt=="260467")
  rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
  nx=length(subset(tmp,variable==vars[i]&value!=0)$value)
  w=rep(1/nx,nx)
  n.den=512#nx; #default is 512
  if(length(subset(tmp,variable==vars[i]&value!=0)$value)<2){
    dens=data.frame(x=NA,
                    y=NA,
                    scaled =NA,
                    ndensity = NA,
                    count=NA,
                    n=NA,
                    group=vars[i])
  }else{
    dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
                 weights=w,   
                 kernel="gaussian",
                 from=rng.val[1],
                 to=rng.val[2],
                 n = n.den)
    dens=data.frame(x=exp(dens$x),
                    y=dens$y,
                    scaled =  dens$y / max(dens$y, na.rm = TRUE),
                    ndensity = dens$y / max(dens$y, na.rm = TRUE),
                    count=dens$y*nx,
                    n=nx,
                    group=vars[i])}
  density.group.TSP=rbind(density.group.TSP,dens)
}
plot(y~x,density.group.TSP,log="x")
plot(count~x,density.group.TSP,log="x")

density.group.NA25=data.frame()
for(i in 1:3){
  tmp=subset(q.dat.xtab.zones.melt,Alt=="NA25")
  rng.val=range(log(tmp[tmp$value!=0,]$value),na.rm=T)
  nx=length(subset(tmp,variable==vars[i]&value!=0)$value)
  w=rep(1/nx,nx)
  n.den=512#nx; #default is 512
  if(length(subset(tmp,variable==vars[i]&value!=0)$value)<2){
    dens=data.frame(x=NA,
                    y=NA,
                    scaled =NA,
                    ndensity = NA,
                    count=NA,
                    n=NA,
                    group=vars[i])
  }else{
    dens=density(log(subset(tmp,variable==vars[i]&value!=0)$value),
                 weights=w,   
                 kernel="gaussian",
                 from=rng.val[1],
                 to=rng.val[2],
                 n = n.den)
    dens=data.frame(x=exp(dens$x),
                    y=dens$y,
                    scaled =  dens$y / max(dens$y, na.rm = TRUE),
                    ndensity = dens$y / max(dens$y, na.rm = TRUE),
                    count=dens$y*nx,
                    n=nx,
                    group=vars[i])}
  density.group.NA25=rbind(density.group.NA25,dens)
}
plot(y~x,density.group.NA25,log="x")
plot(count~x,density.group.NA25,log="x")
# png(filename=paste0(plot.path,"Iteration3_Batch_TSP/TSP_S80_QFC_Q_zones.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,1.5,1,0.5));
layout(matrix(c(1:2),2,1,byrow=F))

ylim.val=c(0,4);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,10000);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(y~x,density.group.NA25,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(density.group.NA25,group=="below.ZoneD.SLE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(density.group.NA25,group=="within.ZoneD.SLE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(density.group.NA25,group=="above.ZoneD.SLE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Density")
mtext(side=3,adj=0,"NA25 (FWO)")

legend("topleft",legend=c("Above Zone D","Within Zone D","Below Zone D"),
       pch=c(22,22,22),
       lty=0,lwd=0.01,
       col=rev(cols4),
       pt.bg=adjustcolor(rev(cols4),0.5),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

ylim.val=c(0,4.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(y~x,density.group.TSP,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(density.group.TSP,group=="below.ZoneD.SLE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(density.group.TSP,group=="within.ZoneD.SLE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(density.group.TSP,group=="above.ZoneD.SLE.lake"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=2,"Density")
mtext(side=3,adj=0,"Run 260467")
mtext(side=1,line=1.75,"S-80 from Lake Daily Discharge (ft\u00B3 sec\u207B\u00B9)")
dev.off()