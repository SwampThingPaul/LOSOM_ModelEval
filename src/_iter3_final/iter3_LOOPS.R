## 
## LOSOM
##
## Iteration 3 final modeling
## LOOPs output for estuaries
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
cols=c(grey.colors(3)[3],scico::scico(n=4,palette="vik"))
alts.sort=c("NA25`", "PA25`", "RMtst4", "Test14", "Test16")
n.alts=length(alts.sort)

run1=read.xlsx(paste0(data.path,"Iteration_3/LOOPS/LOOPS_OutputData.xlsx"),sheet=1,cols=1:12)
run1$Date=date.fun(convertToDate(run1$Date))
run1$mod="NA25`"

run3=read.xlsx(paste0(data.path,"Iteration_3/LOOPS/LOOPS_OutputData.xlsx"),sheet=2,cols=1:12)
run3$Date=date.fun(convertToDate(run3$Date))
run3$mod="PA25`"

run7=read.xlsx(paste0(data.path,"Iteration_3/LOOPS/LOOPS_OutputData.xlsx"),sheet=3,cols=1:12)
run7$Date=date.fun(convertToDate(run7$Date))
run7$mod="RMtst4"

run14=read.xlsx(paste0(data.path,"Iteration_3/LOOPS/LOOPS_OutputData.xlsx"),sheet=4,cols=1:12)
run14$Date=date.fun(convertToDate(run14$Date))
run14$mod="Test14"

run16=read.xlsx(paste0(data.path,"Iteration_3/LOOPS/LOOPS_OutputData.xlsx"),sheet=4,cols=1:12)
run16$Date=date.fun(convertToDate(run16$Date))
run16$mod="Test16"

LOOPs.est=rbind(run1,run3,run7,run14,run16)

# CRE and SLE in CFS
# REG flow in x1000 AcfFt
# Stage in Ft

# RECOVER Salinity Envelope -----------------------------------------------
## CRE
LOOPs.est$S79.14d=with(LOOPs.est,ave(CRE,mod,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
LOOPs.est$S79_SRC_LO.14d=with(LOOPs.est,ave(S79_SRC_LO,mod,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))

LOOPs.est$CRE.low=with(LOOPs.est,ifelse(S79.14d<750,1,0)) # RECOVER Low
LOOPs.est$CRE.opt=with(LOOPs.est,ifelse(S79.14d>=750&S79.14d<2100,1,0)) # RECOVER Optimum
LOOPs.est$CRE.high=with(LOOPs.est,ifelse(S79.14d>=2100&S79.14d<2600,1,0)) # RECOVER Stress
LOOPs.est$CRE.dam=with(LOOPs.est,ifelse(S79.14d>=2600,1,0)) # RECOVER Damaging
LOOPs.est$CRE.high3=with(LOOPs.est,ifelse(S79.14d>=6500,1,0))

## SLE
LOOPs.est$SLE.S80trib.14d=with(LOOPs.est,ave(SLE,mod,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
LOOPs.est$S80_SRC_LO.14d=with(LOOPs.est,ave(S80_SRC_LO,mod,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
LOOPs.est$SLE.low=with(LOOPs.est,ifelse(SLE.S80trib.14d<150,1,0)) # RECOVER Low
LOOPs.est$SLE.opt=with(LOOPs.est,ifelse(SLE.S80trib.14d>=150&SLE.S80trib.14d<1400,1,0)) # RECOVER Optimum
LOOPs.est$SLE.high=with(LOOPs.est,ifelse(SLE.S80trib.14d>=1400&SLE.S80trib.14d<1700,1,0)) # RECOVER stress
LOOPs.est$SLE.dam=with(LOOPs.est,ifelse(SLE.S80trib.14d>=1700,1,0)) # RECOVER damaging
LOOPs.est$SLE.high2=with(LOOPs.est,ifelse(SLE.S80trib.14d>=4000,1,0))

##
LOOPs.est$CRE.low.count=0
LOOPs.est$CRE.opt.count=0
LOOPs.est$CRE.high.count=0
LOOPs.est$CRE.high.LOK.count=0
LOOPs.est$CRE.high.basin.count=0
LOOPs.est$CRE.dam.count=0
LOOPs.est$CRE.dam.LOK.count=0
LOOPs.est$CRE.dam.basin.count=0
LOOPs.est$CRE.high3.count=0
LOOPs.est$CRE.high3.LOK.count=0
LOOPs.est$CRE.high3.basin.count=0

LOOPs.est$SLE.low.count=0
LOOPs.est$SLE.opt.count=0
LOOPs.est$SLE.high.count=0
LOOPs.est$SLE.high.LOK.count=0
LOOPs.est$SLE.high.basin.count=0
LOOPs.est$SLE.dam.count=0
LOOPs.est$SLE.dam.LOK.count=0
LOOPs.est$SLE.dam.basin.count=0
LOOPs.est$SLE.high2.count=0
LOOPs.est$SLE.high2.LOK.count=0
LOOPs.est$SLE.high2.basin.count=0

LOOPs.est2=data.frame()
for(j in 1:length(alts.sort)){
  tmp=subset(LOOPs.est,mod==alts.sort[j])
  for(i in 14:nrow(tmp)){
    ## CRE
    tmp$CRE.low.count[i]=with(tmp,ifelse(CRE.low[i]==1&sum(CRE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$CRE.opt.count[i]=with(tmp,ifelse(CRE.opt[i]==1&sum(CRE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    
    tmp$CRE.high.count[i]=with(tmp,ifelse(CRE.high[i]==1&sum(CRE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.high3.count[i]=with(tmp,ifelse(CRE.high3[i]==1&sum(CRE.high3.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.high.LOK.count[i]=with(tmp,
                                   ifelse(CRE.high.count[i]==1,
                                          ifelse((S79.14d[i]-S79_SRC_LO.14d[i])<=2100,1,0),0))
    tmp$CRE.high.basin.count[i]=with(tmp,CRE.high.count[i]-CRE.high.LOK.count[i])
    tmp$CRE.dam.count[i]=with(tmp,ifelse(CRE.dam[i]==1&sum(CRE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$CRE.dam.LOK.count[i]=with(tmp,
                                  ifelse(CRE.dam.count[i]==1,
                                         ifelse((S79.14d[i]-S79_SRC_LO.14d[i])<=2600,1,0),0))
    tmp$CRE.dam.basin.count[i]=with(tmp,CRE.dam.count[i]-CRE.dam.LOK.count[i])
    
    tmp$CRE.high3.LOK.count[i]=with(tmp,
                                    ifelse(CRE.high3.count[i]==1,
                                           ifelse((S79.14d[i]-S79_SRC_LO.14d[i])<=6500,1,0),0))
    tmp$CRE.high3.basin.count[i]=with(tmp,CRE.high3.count[i]-CRE.high3.LOK.count[i])
    
    ## SLE
    tmp$SLE.low.count[i]=with(tmp,ifelse(SLE.low[i]==1&sum(SLE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0)) 
    tmp$SLE.opt.count[i]=with(tmp,ifelse(SLE.opt[i]==1&sum(SLE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.high.count[i]=with(tmp,ifelse(SLE.high[i]==1&sum(SLE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high.LOK.count[i]=with(tmp,
                                   ifelse(SLE.high.count[i]==1,
                                          ifelse((SLE.S80trib.14d[i]-S80_SRC_LO.14d[i])<=1400,1,0),0))
    tmp$SLE.high.basin.count[i]=with(tmp,SLE.high.count[i]-SLE.high.LOK.count[i])
    tmp$SLE.dam.count[i]=with(tmp,ifelse(SLE.dam[i]==1&sum(SLE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.dam.LOK.count[i]=with(tmp,
                                  ifelse(SLE.dam.count[i]==1,
                                         ifelse((SLE.S80trib.14d[i]-S80_SRC_LO.14d[i])<=1700,1,0),0))
    tmp$SLE.dam.basin.count[i]=with(tmp,SLE.dam.count[i]-SLE.dam.LOK.count[i])
    tmp$SLE.high2.count[i]=with(tmp,ifelse(SLE.high2[i]==1&sum(SLE.high2.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high2.LOK.count[i]=with(tmp,
                                    ifelse(SLE.high2.count[i]==1,
                                           ifelse((SLE.S80trib.14d[i]-S80_SRC_LO.14d[i])<=4000,1,0),0))
    tmp$SLE.high2.basin.count[i]=with(tmp,SLE.high2.count[i]-SLE.high2.LOK.count[i])
  }
  LOOPs.est2=rbind(LOOPs.est2,tmp)
  print(j)
}
LOOPs.est2


##
vars=c(paste0("CRE.",c("low.count","opt.count","high.LOK.count","high.basin.count","dam.LOK.count","dam.basin.count","high3.count")),
       paste0("SLE.",c("low.count","opt.count","high.LOK.count","high.basin.count","dam.LOK.count","dam.basin.count","high2.count")))

tmp=reshape2::melt(LOOPs.est2[,c("mod",vars)],id.vars = "mod")
SalEnv_count=reshape2::dcast(tmp,mod~variable,value.var = "value",sum)
SalEnv_count=SalEnv_count[match(alts.sort,SalEnv_count$mod),]
SalEnv_count

vars.CRE=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high3.count"),sep=".")
vars.SLE=paste("SLE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high2.count"),sep=".")
CRE.SalEnv_count=SalEnv_count[,c("mod",vars.CRE)]
SLE.SalEnv_count=SalEnv_count[,c("mod",vars.SLE)]
CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
SLE.labs=c("Low Flow\n(<150 cfs)","Optimum\n(150 - 1400 cfs)","Stress From LOK\n(1400 - 1700 cfs)","Damaging From LOK\n(>1700 cfs)","Extreme\n(>4000 cfs)")

# png(filename=paste0(plot.path,"Iteration3_Final/RECOVER_SalEnv_LOOPS.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2.5,2,2,1),lwd=0.5);

ymax=c(800,1000,400,300,100)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(CRE.SalEnv_count[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count[,i],round(CRE.SalEnv_count[,i],0),font=2,col="black",pos=1,cex=0.75,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ymax=c(200,1000,200,200,200)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(SLE.SalEnv_count[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=c(0),axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count[,i],round(SLE.SalEnv_count[,i],0),font=2,col="black",pos=1,cex=0.75,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=1,outer=T,"LOOPS Model Alternative")
mtext(side=2,line=0.75,outer=T,"Count of 14-Day Periods")

dev.off()



# Monthly Data ------------------------------------------------------------
monthly.mean=ddply(LOOPs.est,
                   c("mod","Mon","Year"),summarise,
                   Mean.CRE=mean(CRE,na.rm=T))
LOOPs.est$Alt=LOOPs.est$mod
LOOPs.est$CY=as.numeric(format(LOOPs.est$Date,'%Y'))
LOOPs.est$month=as.numeric(format(LOOPs.est$Date,"%m"))

q.dat.xtab.mon=ddply(LOOPs.est,c("Alt","CY","month"),summarise,
                     S79=mean(CRE,na.rm=T),
                     S79QFC=mean(S79_SRC_LO,na.rm=T),
                     SLE.S80trib=mean(SLE,na.rm=T),
                     S80_QFC=mean(S80_SRC_LO,na.rm=T))

q.dat.xtab.mon$monCY.date=with(q.dat.xtab.mon,date.fun(paste(CY,month,"01",sep="-")))


q.dat.xtab.mon$CRE.low=with(q.dat.xtab.mon,ifelse(S79<750,1,0)) # RECOVER Low
q.dat.xtab.mon$CRE.opt=with(q.dat.xtab.mon,ifelse(S79>=750&S79<2100,1,0)) # RECOVER Optimum
q.dat.xtab.mon$CRE.high=with(q.dat.xtab.mon,ifelse(S79>=2100&S79<2600,1,0)) # RECOVER Stress
q.dat.xtab.mon$CRE.high.LOK=with(q.dat.xtab.mon,ifelse(CRE.high==1,
                                                       ifelse(S79-S79QFC<=2100,1,0),0))
q.dat.xtab.mon$CRE.dam=with(q.dat.xtab.mon,ifelse(S79>=2600,1,0)) # RECOVER Damaging
q.dat.xtab.mon$CRE.dam.LOK=with(q.dat.xtab.mon,ifelse(CRE.dam==1,
                                                      ifelse(S79-S79QFC<=2600,1,0),0))
q.dat.xtab.mon$CRE.high3=with(q.dat.xtab.mon,ifelse(S79>=6500,1,0))

q.dat.xtab.mon$SLE.low=with(q.dat.xtab.mon,ifelse(SLE.S80trib<150,1,0)) # RECOVER Low
q.dat.xtab.mon$SLE.opt=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=150&SLE.S80trib<1400,1,0)) # RECOVER Optimum
q.dat.xtab.mon$SLE.high=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=1400&SLE.S80trib<1700,1,0)) # RECOVER stress
q.dat.xtab.mon$SLE.high.LOK=with(q.dat.xtab.mon,ifelse(SLE.high==1,
                                                       ifelse(SLE.S80trib-S80_QFC<=1400,1,0),0))
q.dat.xtab.mon$SLE.dam=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=1700,1,0)) # RECOVER damaging
q.dat.xtab.mon$SLE.dam.LOK=with(q.dat.xtab.mon,ifelse(SLE.dam==1,
                                                      ifelse(SLE.S80trib-S80_QFC<=1700,1,0),0))
q.dat.xtab.mon$SLE.high2=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=4000,1,0))

q.dat.xtab.mon$CRE.StressDam=with(q.dat.xtab.mon,ifelse(S79>=2100,1,0)) 
q.dat.xtab.mon$CRE.StressDam.LOK=with(q.dat.xtab.mon,ifelse(CRE.StressDam==1,
                                                       ifelse(S79-S79QFC<=2100,1,0),0))
q.dat.xtab.mon$SLE.StressDam=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=1400,1,0)) 
q.dat.xtab.mon$SLE.StressDam.LOK=with(q.dat.xtab.mon,ifelse(SLE.StressDam==1,
                                                            ifelse(SLE.S80trib-S80_QFC<=1400,1,0),0))



ddply(q.dat.xtab.mon,c("Alt"),summarise,CRE.High=sum(CRE.StressDam),CRE.High.LOK=sum(CRE.StressDam.LOK))

mon.vars=c(paste0("CRE.",c("low","opt","high.LOK","dam.LOK","high3")),
           paste0("SLE.",c("low","opt","high.LOK","dam.LOK","high2")))

tmp=reshape2::melt(q.dat.xtab.mon[,c("Alt",mon.vars)],id.vars = "Alt")
SalEnv_count.mon=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)

mon.vars.CRE=paste0("CRE.",c("low","opt","high.LOK","dam.LOK","high3"))
mon.vars.SLE=paste0("SLE.",c("low","opt","high.LOK","dam.LOK","high2"))
CRE.SalEnv_count.mon=SalEnv_count.mon[,c("Alt",mon.vars.CRE)]
SLE.SalEnv_count.mon=SalEnv_count.mon[,c("Alt",mon.vars.SLE)]

CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
SLE.labs=c("Low Flow\n(<150 cfs)","Optimum\n(150 - 1400 cfs)","Stress From LOK\n(1400 - 1700 cfs)","Damaging From LOK\n(>1700 cfs)","Extreme\n(>4000 cfs)")
# png(filename=paste0(plot.path,"Iteration3_Final/Month_SalEnv_LOOPS.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2.5,2,2,1),lwd=0.5);

ymax=c(300,400,50,100,40)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(CRE.SalEnv_count.mon[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=c(0),axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count.mon[,i],round(CRE.SalEnv_count.mon[,i],0),font=2,col="black",pos=1,cex=0.75,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ymax=c(40,400,30,60,60)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(SLE.SalEnv_count.mon[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=c(0),axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count.mon[,i],round(SLE.SalEnv_count.mon[,i],0),font=2,col="black",pos=1,cex=0.75,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=1,outer=T,"LOOPS Model Alternative")
mtext(side=2,line=0.75,outer=T,"Count of Months")
dev.off()

SalEnv_count.comp=reshape2::dcast(tmp,variable~Alt,value.var = "value",sum)
colnames(SalEnv_count.comp)=c("variable", "NA25", "PA25", "RMtst4", "Test14", "Test16")

SalEnv_count.comp$PA25.PerFWO=with(SalEnv_count.comp,((PA25-NA25)/NA25)*100)
SalEnv_count.comp$Test14.PerPA25=with(SalEnv_count.comp,((Test14-PA25)/Test14)*100)
SalEnv_count.comp$Test16.PerPA25=with(SalEnv_count.comp,((Test16-PA25)/Test16)*100)



# Regulatory Flow ---------------------------------------------------------
vars=c("mod","Date",paste0(c("WCA","L8","S77","S308"),"REG"))

regQ=LOOPs.est[,vars]
regQ$CY=as.numeric(format(regQ$Date,"%Y"))
regQ.melt=reshape2::melt(regQ,id.vars=c("mod","Date",'CY'))

regQ.CY=ddply(regQ.melt,c("mod",'CY',"variable"),summarise,Q.kacft=sum(value,na.rm=T))
# regQ.CY.avg=ddply(regQ.CY,c("mod","variable"),summarise,avgQ=mean(Q.kacft))
regQ.CY.avg=reshape2::dcast(regQ.CY,mod~variable,value.var="Q.kacft",mean)

colnames(regQ.CY.avg)=c("mod","WCAs","LWLagoon","Cal","StL")

tmp=regQ.CY.avg[,c("WCAs","Cal","StL","LWLagoon")]
rownames(tmp)<-alts.sort
cols2=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))
n.alts=length(alts.sort)

ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Final/LOOPS_PA_AvgFloodControl.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),heights=c(1,0.4))

x=barplot(t(tmp),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp),beside=F,col=cols2,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts),add=T)
with(regQ.CY.avg,text(x,WCAs/2,round(WCAs,0),cex=0.75,col="white"))
with(regQ.CY.avg,text(x,WCAs+(((Cal+WCAs)-WCAs)/2),round(Cal,0),cex=0.75))
with(regQ.CY.avg,text(x,(WCAs+Cal)+(((Cal+WCAs+StL)-(Cal+WCAs))/2),round(StL,0),cex=0.75))
with(regQ.CY.avg,text(x,Cal+WCAs+StL+LWLagoon,round(LWLagoon,0),pos=3,offset=0.1,cex=0.75))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,line=-0.25,las=2,cex=0.8);box(lwd=1)
mtext(side=2,line=3,"Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=3,"LOOPS Model Alternatives")

# par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(-0.15,0.75,legend=c("Water Conservation Areas (S351 & S354)","Caloosahatchee River (S77)","St. Lucie River (S308)","Lake Worth Lagoon (S271)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols2,
       pt.cex=1.25,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)
text(1,0,"LOOPS Lake Recovery results. Mean annual flood\ncontrol releases from Lake Okeechobee for the\n52 year (1965 - 2016)simulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()

regQ.CY.avg2=reshape2::dcast(regQ.CY,variable~mod,value.var="Q.kacft",mean)
colnames(regQ.CY.avg2)=c("variable", "NA25", "PA25", "RMtst4", "Test14", "Test16")
regQ.CY.avg2$Test14.PerPA25=with(regQ.CY.avg2,((Test14-PA25)/Test14)*100)
regQ.CY.avg2$Test14.PerNA25=with(regQ.CY.avg2,((Test14-NA25)/Test14)*100)

((501-523)/501)*100


# LOK Stage ---------------------------------------------------------------

lakeO.stage=LOOPs.est[,c("mod","Date",'LOK_Stage')]
colnames(lakeO.stage)=c("Alt","Date",'STAGE')

lakeO.stage$month=as.numeric(format(lakeO.stage$Date,'%m'))
lakeO.stage$day=as.numeric(format(lakeO.stage$Date,'%d'))
lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,'%Y'))
lakeO.stage$DoY=as.numeric(format(lakeO.stage$Date,'%j'))
lakeO.stage$WY=WY(lakeO.stage$Date)
lakeO.stage$low.stg=with(lakeO.stage,ifelse(STAGE<11,1,0))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$High.stg=with(lakeO.stage,ifelse(STAGE>16,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))

ann.peak=ddply(lakeO.stage,c("CY","Alt"),summarise,max.stg=max(STAGE,na.rm=T))
ann.peak$GT17=with(ann.peak,ifelse(round(max.stg,1)>=16.9,1,0))

ann.peak=merge(ann.peak,data.frame(Alt=alts.sort,plot.y=1:5),"Alt")

par(family="serif",mar=c(1,3,1.25,0.5),oma=c(2,2,1,1),lwd=0.1);

xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0.75,5.25);by.y=1;ymaj=1:5#seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(plot.y~CY,ann.peak,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
for(i in 1:5){
  points(plot.y~CY,subset(ann.peak,GT17==1&Alt==alts.sort[i]),pch=21,bg=adjustcolor(cols[i],0.5),cex=1.5)
}
axis_fun(2,1:5,1:5,alts.sort)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
mtext(side=2,line=4,"Alternative")
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=3,adj=0,line=1,"Lake Okeechobee")
mtext(side=3,adj=0,cex=0.75,"Annual maximum stage \u2265 17Ft NGVD29",col="grey50")

# png(filename=paste0(plot.path,"Iteration3_Final/LOK_totalDays_LOOPS.png"),width=6,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,0.25,1),oma=c(1,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(2000,3000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,4000);by.x=1000;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
days.POS=ddply(lakeO.stage,"Alt",summarise,N.days=N.obs(STAGE),sum.low=sum(low.stg),sum.vlow=sum(vlow.stg),sum.High=sum(High.stg),sum.vHigh=sum(vHigh.stg))
# days.POS$Alt=factor(days.POS$Alt,level=alts.sort)
days.POS=days.POS[match(alts.sort,days.POS$Alt),]

plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(days.POS,points(sum.High,sum.low,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1))
with(days.POS,text(sum.High,sum.low,Alt,pos=3,col=adjustcolor("black",0.5),cex=0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u003E 16 ft NGVD29")
mtext(side=2,line=2.75,"Days < 11 ft NGVD29")
mtext(side=3,adj=0, "Cumulative days over the enitre period of simulation")

ylim.val=c(500,1500);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,300);by.x=100;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# with(days.POS,points(sum.vHigh,sum.vlow,pch=19,col=adjustcolor(cols,0.5),lwd=0.1))
with(days.POS,points(sum.vHigh,sum.vlow,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1))
with(days.POS,text(sum.vHigh,sum.vlow,Alt,pos=3,col=adjustcolor("black",0.5),cex=0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u2265 17 ft NGVD29")
mtext(side=2,line=2.75,"Days \u2264 10 ft NGVD29")
dev.off()

labs=c("% LOK Stage >17 Ft NGVD29","% LOK Stage >16 Ft NGVD29","% LOK Stage <10 Ft NGVD29")
# png(filename=paste0(plot.path,"Iteration3_Final/LOK_stg_LOOPS.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:3,3,1,byrow=F))

ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.vHigh/18993)*100,col=adjustcolor(cols,0.5),
          ylim=ylim.val,space=c(0),axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
# axis_fun(1,x,x,c("PA22","PA25"),cex=0.8,las=2)
axis_fun(1,x,x,NA,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,line=1.25,"LOK")
mtext(side=3,adj=0,labs[1],cex=0.7)
text(x,(days.POS$sum.vHigh/18993)*100,
     round((days.POS$sum.vHigh/18993)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)

ylim.val=c(0,25);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.High/18993)*100,col=adjustcolor(cols,0.5),
          ylim=ylim.val,space=c(0),axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
# axis_fun(1,x,x,c("PA22","PA25"),cex=0.8,las=2)
axis_fun(1,x,x,NA,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,labs[2],cex=0.7)
text(x,(days.POS$sum.High/18993)*100,
     round((days.POS$sum.High/18993)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)

ylim.val=c(0,10);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.vlow/18993)*100,col=adjustcolor(cols,0.5),
          ylim=ylim.val,space=c(0),axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,cex=1,las=1,line=-0.5)
box(lwd=1)
mtext(side=3,adj=0,labs[3],cex=0.7)
text(x,(days.POS$sum.vlow/18993)*100,
     round((days.POS$sum.vlow/18993)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=2,"LOOPS Model Alternative",outer=T)
dev.off()


# RECOVER Stage Envelope --------------------------------------------------
## 
AprSep=seq(date.fun("1965-04-15"),date.fun("1965-09-15"),"1 days")
MayAug=seq(date.fun("1965-05-01"),date.fun("1965-09-01"),"1 days")

stg.env=lakeO.stage
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

# 1 = Normal
# 2 = Recovery
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

env.rslt$env2=with(env.rslt,ifelse(env==2,0,1))
env.rslt=merge(env.rslt,data.frame(Alt=alts.sort,PlotOffset=rev(seq(2,10,2))),"Alt")
env.rslt$env.plt=with(env.rslt,env2+PlotOffset)
env.rslt$env.f=with(env.rslt,ifelse(env==1,"normal","recovery"))
env.rslt$Alt=factor(env.rslt$Alt,levels=alts.sort)


env.count=ddply(env.rslt,c("Alt","env.f"),summarise,N.val=N.obs(env.f))
env.count=env.count[c(1,2,3,4,7,8,5,6,9,10),]
env.count$axs.val=11:2

# png(filename=paste0(plot.path,"Iteration3_Final/LOK_Env_LOOPS.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1.75,1),oma=c(2,2,1,2));

ylim.val=c(1.5,11.5)
xlim.val=c(1965,2016);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(env2~CY,env.rslt,type="n",axes=F,ann=F,xlim=xlim.val,xaxs="i",yaxs="i",ylim=ylim.val)
abline(v=c(xmaj,xmin),h=c(2:19),lty=c(1,3),lwd=0.5,col=adjustcolor("grey",0.5))
abline(h=c(2:19),lty=c(3),lwd=0.5,col=c("grey"))
for(i in 1:length(alts.sort)){
  with(subset(env.rslt,Alt==alts.sort[i]),lines(env.plt~CY,type="s",col=cols[i],lwd=3))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.75,cex=0.75)
axis_fun(2,seq(3,11,2),seq(3,11,2),rev(alts.sort))
axis_fun(4,env.count$axs.val,env.count$axs.val,env.count$N.val,cex=0.6)
abline(h=seq(3.5,19.5,2))
box(lwd=1)
mtext(side=3,adj=1,"Upper Step = Normal Envelope\nLower Step = Recovery Envelope",font=3)
mtext(side=1,line=1.25,"Calendar Year")
mtext(side=4,line=1.25,"Normal/Recovery Envelop Count")
dev.off()

library(LORECOVER)
lakeO.stage$Data.Value=lakeO.stage$STAGE

norm.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts.sort[i])[,c("Date","Data.Value")]
  rslt=LORECOVER::norm_env(tmp)
  rslt$Alt=alts.sort[i]
  norm.lakeO.stage.scr=rbind(norm.lakeO.stage.scr,rslt)
  print(i)
}

norm.lakeO.stage.scr=rename(norm.lakeO.stage.scr,c("penalty"="norm.score"))
norm.lakeO.stage.scr$WY=WY(norm.lakeO.stage.scr$Date)

rec.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts.sort[i])[,c("Date","Data.Value")]
  rslt=LORECOVER::rec_env(tmp)
  rslt$Alt=alts.sort[i]
  rec.lakeO.stage.scr=rbind(rec.lakeO.stage.scr,rslt)
  print(i)
}
rec.lakeO.stage.scr=rename(rec.lakeO.stage.scr,c("penalty"="rec.score"))
rec.lakeO.stage.scr$WY=WY(rec.lakeO.stage.scr$Date)

lakeO.stage.scr=merge(norm.lakeO.stage.scr,rec.lakeO.stage.scr[,c("Date","Alt","rec.score")],c("Date","Alt"))
lakeO.stage.scr$CY=as.numeric(format(lakeO.stage.scr$Date,"%Y"))
lakeO.stage.scr$WY=WY(lakeO.stage.scr$Date)
lakeO.stage.scr=lakeO.stage.scr[order(lakeO.stage.scr$Alt,lakeO.stage.scr$Date),]

vars=c("Alt","CY","env")
lakeO.stage.scr=merge(lakeO.stage.scr,env.rslt[,vars],c("Alt","CY"))
lakeO.stage.scr$score=with(lakeO.stage.scr,ifelse(env==1,norm.score,rec.score))
lakeO.stage.scr$Alt_CY=with(lakeO.stage.scr,paste(Alt,CY,sep="_"))
lakeO.stage.scr$cum.abs.pen=with(lakeO.stage.scr,ave(score,Alt_CY,FUN=function(x)cumsum(abs(x))))
lakeO.stage.scr$Alt=factor(lakeO.stage.scr$Alt,levels=alts.sort)

boxplot(cum.abs.pen~Alt,lakeO.stage.scr,outline=F)
stg.scr.sum=ddply(lakeO.stage.scr,"Alt",summarise,mean.val=mean(cum.abs.pen,na.rm=T))
stg.scr.sum$FWO.perdiff=with(stg.scr.sum,((mean.val-mean.val[1])/mean.val[1])*100)

env.pen.sum=ddply(lakeO.stage.scr,"Alt",summarise,
                  N.val=N.obs(score),
                  pen_above=sum(score[score>0],na.rm=T),
                  pen_below=sum(abs(score)[score<0],na.rm=T),
                  per_below=(sum(score<0)/N.obs(score))*100,
                  per0=(sum(score==0,na.rm=T)/N.obs(score))*100,
                  per_above=(sum(score>0)/N.obs(score))*100)
env.pen.sum$total.pen=rowSums(env.pen.sum[,c("pen_above","pen_below")])
env.pen.sum=env.pen.sum[match(alts.sort,env.pen.sum$Alt),]
env.pen.sum$FWO_PerBelow=with(env.pen.sum,(per_below-per_below[1])/per_below[1])*100
env.pen.sum$FWO_PerWith=with(env.pen.sum,(per0-per0[1])/per0[1])*100
env.pen.sum$FWO_PerAbove=with(env.pen.sum,(per_above-per_above[1])/per_above[1])*100
env.pen.sum=env.pen.sum[match(alts.sort2,env.pen.sum$Alt),]

env.pen.sum$PerDiff_PA25=((env.pen.sum$total.pen-env.pen.sum$total.pen[2])/env.pen.sum$total.pen)*100

env.pen.sum$total.pen[2]


cols.IMC=c(rgb(238,232,170,maxColorValue = 255),rgb(143,188,143,maxColorValue = 255))
# png(filename=paste0(plot.path,"Iteration3_Final/LOK_EnvScore_AllYrs_LOOPS.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,50000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(t(env.pen.sum[,c("pen_below","pen_above")]),beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(t(env.pen.sum[,c("pen_below","pen_above")]),beside=F,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)),add=T)
with(env.pen.sum,text(x,pen_below/2,round(pen_below,0)))
with(env.pen.sum,text(x,pen_below+abs((pen_below-total.pen)/2),round(pen_above,0)))
with(env.pen.sum,text(x,total.pen,round(total.pen,0),pos=3))
axis_fun(1,x,x,alts.sort,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Stage Envelope Penalty Score")
mtext(side=1,line=1.75,"LOOPS Model Alternative")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
mtext(side=3,line=-2,"Lake Okeechobee\nEnvelope Penalty Score\nAll Years")
legend(0.5,0.5,legend=c("Lower Penalty\n(Below Envelope)","Upper Penalty\n(Above Envelope)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c(cols.IMC),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Total Score")
mtext(side=1,line=-1,adj=1,"Simulation Period of Record\n(1965-2016) using LOOPS model\noutputs. Includes Normal and\nRecovery envelope.",cex=0.75)
dev.off()