## 
## LOSOM
##
## Iteration 3 final array
## NOTES: C10A renamed to S271
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
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_3/Model_Output/"))
alts=alts[!alts%in%c("Northern_Estuaries","Lake_Okeechobee")]
n.alts=length(alts)
alts.sort=c("ECB19","NA22f","NA25f","PA22","PA25")

cols.alts=c(grey.colors(3),wesanderson::wes_palette("Zissou1",2,"continuous"))

cols.alts2=c("#4D4D4D", "#AEAEAE", "#3B9AB2", "#E6E6E6", "#F21A00")
alts.sort2=c("ECB19","NA22f","PA22","NA25f","PA25")

# Discharge ---------------------------------------------------------------
RSM.sites=c("S79","S80","S80_QPFCSOURCE_LAKE","S79_QPFCSOURCE_LAKE",
            "S77","S308","S77_QFC","S308_QFC","S308BF","S77BF",
            "TMC2EST","S48","S49","NSF2EST","S2","S3","S4BP")

q.dat=data.frame()
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
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
unique(q.dat$Alt)

head(q.dat,20)
q.dat=q.dat[order(q.dat$Alt,q.dat$SITE,q.dat$Date),]
q.dat$WY=WY(q.dat$Date)
q.dat$CY=as.numeric(format(q.dat$Date,"%Y"))
q.dat$month=as.numeric(format(q.dat$Date,"%m"))

q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+CY+month~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
head(q.dat.xtab)
q.dat.xtab$S79.14d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80.14d=with(q.dat.xtab,ave(S80,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S79.30d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,29),rollapply(x,width=30,FUN=function(x)mean(x,na.rm=T)))))

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
vars=c(paste0("CRE.",c("low.count","opt.count","high.LOK.count","high.basin.count","dam.LOK.count","dam.basin.count","high3.count")),
       paste0("SLE.",c("low.count","opt.count","high.LOK.count","high.basin.count","dam.LOK.count","dam.basin.count","high2.count")))

tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars)],id.vars = "Alt")
SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
SalEnv_count=SalEnv_count[match(alts.sort,SalEnv_count$Alt),]
SalEnv_count

SalEnv_count1=subset(SalEnv_count,Alt%in%c("ECB19","NA22f","PA22"))
SalEnv_count2=subset(SalEnv_count,Alt%in%c("ECB19","NA25f","PA25"))
SalEnv_count3=SalEnv_count[match(c("ECB19","NA22f","PA22","NA25f","PA25"),SalEnv_count$Alt),]

vars.CRE=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high3.count"),sep=".")
vars.SLE=paste("SLE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high2.count"),sep=".")
CRE.SalEnv_count3=SalEnv_count3[,c("Alt",vars.CRE)]
SLE.SalEnv_count3=SalEnv_count3[,c("Alt",vars.SLE)]
CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
SLE.labs=c("Low Flow\n(<150 cfs)","Optimum\n(150 - 1400 cfs)","Stress From LOK\n(1400 - 1700 cfs)","Damaging From LOK\n(>1700 cfs)","Extreme\n(>4000 cfs)")
# png(filename=paste0(plot.path,"Iteration3_Final/RECOVER_SalEnv_.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,2,1),lwd=0.5);

ymax=c(800,1000,400,300,100)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(CRE.SalEnv_count3[,i],col=adjustcolor(cols.alts2,0.5),ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count3[,i],round(CRE.SalEnv_count3[,i],0),font=2,col="black",pos=1,cex=0.5,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ymax=c(200,1000,200,200,200)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(SLE.SalEnv_count3[,i],col=adjustcolor(cols.alts2,0.5),ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort2,cex=0.8,las=2)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count3[,i],round(SLE.SalEnv_count3[,i],0),font=2,col="black",pos=1,cex=0.5,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"Count of 14-Day Periods")

dev.off()

SalEnv_count.FWO=reshape2::dcast(tmp,variable~Alt,value.var = "value",sum)
SalEnv_count.FWO$PA22.PerFWO=with(SalEnv_count.FWO,((PA22-NA22f)/NA22f)*100)
SalEnv_count.FWO$PA25.PerFWO=with(SalEnv_count.FWO,((PA25-NA25f)/NA25f)*100)
# write.csv(tmp,paste0(export.path,"Iteration3/PrefAlt/SalEnv.csv"),row.names = F)


salenv.cnt.dat=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
CRE.sal.env=salenv.cnt.dat[,c("Alt",vars.CRE),]
CRE.sal.env$Area="CRE"
CRE.sal.env=CRE.sal.env[,c(7,1:6)]
colnames(CRE.sal.env)=c("Area","Alt","low","opt","stress.lok","dam.lok","high")

SLE.sal.env=salenv.cnt.dat[,c("Alt",vars.SLE),]
SLE.sal.env$Area="SLE"
SLE.sal.env=SLE.sal.env[,c(7,1:6)]
colnames(SLE.sal.env)=c("Area","Alt","low","opt","stress.lok","dam.lok","high")

rbind(CRE.sal.env,SLE.sal.env)%>%
  flextable()%>%
  merge_v(j=1)%>%
  fix_border_issues()%>%
  valign(j=1,valign="top")%>%
  hline(i=5)%>%
  set_header_labels("low"="Low\nEvents",
                    "opt"="Optimum\nEvents",
                    "stress.lok"="Stress Events from LOK",
                    "dam.lok"="Damaging Events from LOK",
                    "high"="Extreme Events")%>%
  padding(padding=1,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=9,part="body")%>%
  fontsize(size=10,part="header")%>%
  align(j=3:7,align="center",part="all")%>%
  footnote(i=c(1,6),j=1,value=as_paragraph("CRE: Caloosahatchee Estuary; SLE: St Lucie Estuary"),ref_symbols=" 1 ",inline = T)%>%
  footnote(j=c(3),part="header",value=as_paragraph(as_b("Low Flows"), " CRE: < 750 cfs; SLE: < 150 cfs"),ref_symbols=" ",inline = F)%>%
  footnote(j=c(4),part="header",value=as_paragraph(as_b("Optimum Flows")," CRE: \u2265 750 cfs & < 2100 cfs; SLE: \u2265 150 cfs & < 1400 cfs cfs"),ref_symbols=" ")%>%
  footnote(j=c(5),part="header",value=as_paragraph(as_b("Stressful Flows")," CRE: \u2265 2100 cfs & < 2600 cfs; SLE: \u2265 1400 cfs & < 1700 cfs"),ref_symbols=" ")%>%
  footnote(j=c(7),part="header",value=as_paragraph(as_b("Damaging Flows")," CRE: > 2600 cfs; SLE:> 1700 cfs"),ref_symbols=" ")%>%
  footnote(j=c(1),part="header",value=as_paragraph(as_b("Data Source:")," USACE and SFWMD Interagency Modeling Center."),ref_symbols=" ")%>%
  font(fontname="Times New Roman",part="all")%>%
  # bold(j=9:14,part="body")%>%
  fontsize(size=9,part="body")%>%
  set_caption(caption="RECOVER Estuary salinit envelope 14-day period count of low, optimum, stress, damaging and extreme flow events for Caloosatchee and St Lucie estuaries based on 14-day moving average discharge data. ")

# vars.CRE=paste("CRE",c("low","opt","high.LOK","dam.LOK","high3"),sep=".")
# vars.SLE=paste("SLE",c("low","opt","high.LOK","dam.LOK","high2"),sep=".")

# png(filename=paste0(plot.path,"Iteration3_Final/RECOVER_SalEnv_FWO.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,2,1),lwd=0.5);

# subset(SalEnv_count.FWO,variable%in%vars.CRE)
ymax.val=c(0,40,0,0,40)
ymin.val=c(-20,0,-80,-80,0)
yval=c(10,20,40,40,20)
for(i in 1:5){
  ylim.val=c(ymin.val[i],ymax.val[i]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i]);ymin=seq(ylim.val[1],ylim.val[2],yval[i]/2)
  tmp.FWO=t(subset(SalEnv_count.FWO,variable==vars.CRE[i])[,c("PA22.PerFWO","PA25.PerFWO")])
  x=barplot(tmp.FWO,col=adjustcolor(cols.alts[4:5],0.5),
            ylim=ylim.val,space=0,axes=F,ann=F,beside=T,
            names.arg = rep(NA,2))
  axis_fun(2,ymaj,ymin,ymaj)
  # axis_fun(1,x,x,c("PA22","PA25"),cex=0.8,las=2)
  axis_fun(1,x,x,NA,cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i],cex=0.7)
  text(x,tmp.FWO,format(round(tmp.FWO,1)),font=2,col="black",pos=1,cex=0.75,offset=0.25)
}
mtext(side=4,line=0.5,"Caloosahatchee")

# subset(SalEnv_count.FWO,variable%in%vars.SLE)
ymax.val=c(80,10,0,0,5)
ymin.val=c(0,0,-100,-80,0)
yval=c(40,5,50,40,2.5)
for(i in 1:5){
  ylim.val=c(ymin.val[i],ymax.val[i]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i]);ymin=seq(ylim.val[1],ylim.val[2],yval[i]/2)
  tmp.FWO=t(subset(SalEnv_count.FWO,variable==vars.SLE[i])[,c("PA22.PerFWO","PA25.PerFWO")])
  x=barplot(tmp.FWO,col=adjustcolor(cols.alts[4:5],0.5),
            ylim=ylim.val,space=0,axes=F,ann=F,beside=T,
            names.arg = rep(NA,2))
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,c("PA22","PA25"),cex=0.8,las=2)
  
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i],cex=0.7)
  text(x,tmp.FWO,format(round(tmp.FWO,1)),font=2,col="black",pos=1,cex=0.75,offset=0.25)
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"% Difference relative to FWO")
dev.off()


vars=c(paste0("CRE.",c("high3.LOK.count","high3.basin.count")),
       paste0("SLE.",c("high2.LOK.count","high2.basin.count")))
extreme.Q.salenv.melt=reshape2::melt(q.dat.xtab2[,c("Alt",vars)],id.vars = "Alt")
extreme.Q.salenv=reshape2::dcast(extreme.Q.salenv.melt,Alt~variable,value.var = "value",sum)

extreme.Q.salenv=extreme.Q.salenv[match(alts.sort2,extreme.Q.salenv$Alt),]
extreme.Q.salenv

extreme.Q.salenv.FWO=reshape2::dcast(extreme.Q.salenv.melt,variable~Alt,value.var = "value",sum)
extreme.Q.salenv.FWO$PA22.PerFWO=with(extreme.Q.salenv.FWO,((PA22-NA22f)/NA22f)*100)
extreme.Q.salenv.FWO$PA25.PerFWO=with(extreme.Q.salenv.FWO,((PA25-NA25f)/NA25f)*100)

cols=viridis::cividis(2,0.75)
# png(filename=paste0(plot.path,"Iteration3_Final/RECOVER_SalEnv_extreme.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,2),oma=c(3,2,0.5,0.5),lwd=0.5);
layout(matrix(c(1:4),2,2,byrow=F))

ylim.val=c(0,110);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(t(extreme.Q.salenv[,c("CRE.high3.basin.count","CRE.high3.LOK.count")]),col=cols,
        ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F,names.arg = rep(NA,5))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA)
box(lwd=1)
mtext(side=3,adj=0,"CRE - Extreme (>6500 cfs)",cex=1)

with(extreme.Q.salenv,text(x,CRE.high3.basin.count,CRE.high3.basin.count,
                           font=2,col="white",pos=1,cex=0.75,offset=0.25))
with(extreme.Q.salenv,text(x,CRE.high3.basin.count+CRE.high3.LOK.count,CRE.high3.LOK.count,
                           font=2,col="black",pos=1,cex=0.75,offset=0.25))
with(extreme.Q.salenv,text(x,CRE.high3.basin.count+CRE.high3.LOK.count,
                           CRE.high3.basin.count+CRE.high3.LOK.count,
                           font=2,col="grey",pos=3,cex=0.75,offset=0.25))
mtext(side=2,line=2.5,"Count of 14-Day Period",cex=0.8)
legend("topleft",legend=c("Basin","LOK"),
       pch=22,pt.bg=cols,lwd=0.1,lty=0,
       pt.cex=1.25,ncol=2,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(t(extreme.Q.salenv[,c("SLE.high2.basin.count","SLE.high2.LOK.count")]),col=cols,
          ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F,names.arg = rep(NA,5))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort2,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"SLE - Extreme (>4000 cfs)",cex=1)

with(extreme.Q.salenv,text(x,SLE.high2.basin.count,SLE.high2.basin.count,
                           font=2,col="white",pos=1,cex=0.75,offset=0.25))
with(extreme.Q.salenv,text(x,SLE.high2.basin.count+SLE.high2.LOK.count,SLE.high2.LOK.count,
                           font=2,col="black",pos=1,cex=0.75,offset=0.25))
with(extreme.Q.salenv,text(x,SLE.high2.basin.count+SLE.high2.LOK.count,
                           SLE.high2.basin.count+SLE.high2.LOK.count,
                           font=2,col="grey",pos=3,cex=0.75,offset=0.25))
mtext(side=2,line=2.5,"Count of 14-Day Period",cex=0.8)
mtext(side=1,line=3,"Alternative")

tmp.FWO=t(subset(extreme.Q.salenv.FWO,variable%in%c("CRE.high3.basin.count","CRE.high3.LOK.count"))[,c('PA22.PerFWO','PA25.PerFWO')])
ylim.val=c(-10,40);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(tmp.FWO,beside=T,space=c(0,0,0.2,0),
          col=c(cols[2],cols[2],cols[1],cols[1]),ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,2))
abline(h=0,lwd=1)
lab.val=c(tmp.FWO[,1],tmp.FWO[,2])
text(c(x[,1],x[,2]),lab.val,format(round(lab.val,1)),
     pos=ifelse(lab.val<0,3,1),col=c(rep("black",2),rep("white",2)),
     cex=0.8,offset=0.25)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,c(x[,1],x[,2]),c(x[,1],x[,2]),NA)
# axis_fun(1,c(x[,1],x[,2]),c(x[,1],x[,2]),c("PA22","PA25","PA22","PA25"))
# axis(1,x[1,]+(x[2,]-x[1,])/2,c("LOK","Basin"),lty=0,line=2.5)
box(lwd=1)
mtext(side=2,line=2.25,"% Diff to FWO",cex=0.8)

tmp.FWO=t(subset(extreme.Q.salenv.FWO,variable%in%c("SLE.high2.basin.count","SLE.high2.LOK.count"))[,c('PA22.PerFWO','PA25.PerFWO')])
ylim.val=c(-30,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(tmp.FWO,beside=T,space=c(0,0,0.2,0),
          col=c(cols[2],cols[2],cols[1],cols[1]),ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,2))
abline(h=0,lwd=1)
lab.val=c(tmp.FWO[,1],tmp.FWO[,2])
text(c(x[,1],x[,2]),lab.val,format(round(lab.val,1)),
     pos=ifelse(lab.val<0,3,1),col=c(rep("black",2),rep("white",2)),
     cex=0.8,offset=0.25)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,c(x[,1],x[,2]),c(x[,1],x[,2]),c("PA22","PA25","PA22","PA25"),line=-0.5)
axis(1,x[1,]+(x[2,]-x[1,])/2,c("LOK","Basin"),lty=0,line=0.75)
box(lwd=1)
mtext(side=2,line=2.5,"% Diff to FWO",cex=0.8)
dev.off()



# Extreme daily eval -----------------------------------------------------
da.CRE=ddply(q.dat.xtab,c('Alt'),summarise,
             N.6500=sum(da.CRE.high3),
             Q.6500=sum(S79[S79>=6500],na.rm=T),
             Q.all=sum(S79))

da.CRE=da.CRE[match(alts.sort,da.CRE$Alt),]
da.CRE$perdiff.N.ECB=with(da.CRE,((N.6500-N.6500[1])/N.6500[1])*100)
da.CRE$perdiff.N.NA22f=with(da.CRE,((N.6500-N.6500[2])/N.6500[2])*100)
da.CRE$perdiff.N.NA25f=with(da.CRE,((N.6500-N.6500[3])/N.6500[3])*100)

da.CRE$perdiff.Q.ECB=with(da.CRE,((Q.6500-Q.6500[1])/Q.6500[1])*100)
da.CRE$perdiff.Q.NA22f=with(da.CRE,((Q.6500-Q.6500[2])/Q.6500[2])*100)
da.CRE$perdiff.Q.NA25f=with(da.CRE,((Q.6500-Q.6500[3])/Q.6500[3])*100)

# da.CRE$perdiff.Qall.ECB=with(da.CRE,((Q.all-Q.all[1])/Q.all[1])*100)
# da.CRE$perdiff.Qall.NA22f=with(da.CRE,((Q.all-Q.all[2])/Q.all[2])*100)
# da.CRE$perdiff.Qall.NA25f=with(da.CRE,((Q.all-Q.all[3])/Q.all[3])*100)

da.CRE


# monthly discharge -------------------------------------------------------
q.dat.xtab.mon=ddply(q.dat.xtab,c("Alt","CY","month"),summarise,
      S79=mean(S79,na.rm=T),
      S79QFC=mean(S79_QPFCSOURCE_LAKE,na.rm=T),
      SLE.S80trib=mean(SLE.S80trib,na.rm=T),
      S80_QFC=mean(S80_QPFCSOURCE_LAKE,na.rm=T))

q.dat.xtab.mon$monCY.date=with(q.dat.xtab.mon,date.fun(paste(CY,month,"01",sep="-")))

## 
mon.CRE=ddply(q.dat.xtab.mon,c('Alt'),summarise,
             N.6500=sum(S79>=6500),
             Q.6500=sum(S79[S79>=6500],na.rm=T),
             Q.all=sum(S79))
mon.CRE=mon.CRE[match(alts.sort,mon.CRE$Alt),]
mon.CRE$perdiff.N.ECB=with(mon.CRE,((N.6500-N.6500[1])/N.6500[1])*100)
mon.CRE$perdiff.N.NA22f=with(mon.CRE,((N.6500-N.6500[2])/N.6500[2])*100)
mon.CRE$perdiff.N.NA25f=with(mon.CRE,((N.6500-N.6500[3])/N.6500[3])*100)

mon.CRE$perdiff.Q.ECB=with(mon.CRE,((Q.6500-Q.6500[1])/Q.6500[1])*100)
mon.CRE$perdiff.Q.NA22f=with(mon.CRE,((Q.6500-Q.6500[2])/Q.6500[2])*100)
mon.CRE$perdiff.Q.NA25f=with(mon.CRE,((Q.6500-Q.6500[3])/Q.6500[3])*100)
# 
# mon.CRE$perdiff.Qall.ECB=with(mon.CRE,((Q.all-Q.all[1])/Q.all[1])*100)
# mon.CRE$perdiff.Qall.NA22f=with(mon.CRE,((Q.all-Q.all[2])/Q.all[2])*100)
# mon.CRE$perdiff.Qall.NA25f=with(mon.CRE,((Q.all-Q.all[3])/Q.all[3])*100)

mon.CRE
## 
# png(filename=paste0(plot.path,"Iteration3_Final/MonthQ_S79_PA22.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:2),2,1,byrow=T))
par(family="serif",mar=c(2,3.25,0.5,1),oma=c(2,2,0.5,0.5),lwd=0.5);
cols2=c("grey","black",cols.alts[4],"black",cols.alts[5])

ylim.val=c(0,16000);by.y=4000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1965-01-01","1990-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(S79~monCY.date,q.dat.xtab.mon,ylim=ylim.val,xlim=xlim.val,type="n",xaxs="i",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
for(i in c(2,3)){
  with(subset(q.dat.xtab.mon,Alt==alts.sort2[i]),
       lines(S79~monCY.date,col=adjustcolor(cols2[i],0.5),lwd=1.25))
}
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"S-79")
legend("topleft",legend=c(alts.sort2[4:5]),
       lty=1,col=adjustcolor(cols2[2:3],0.5),lwd=2,
       pt.cex=1.25,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

xlim.val=date.fun(c("1991-01-01","2016-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(S79~monCY.date,q.dat.xtab.mon,ylim=ylim.val,xlim=xlim.val,type="n",xaxs="i",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
for(i in c(2,3)){
  with(subset(q.dat.xtab.mon,Alt==alts.sort2[i]),
       lines(S79~monCY.date,col=adjustcolor(cols2[i],0.5),lwd=1.25))
}
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Date (Month-Year)")
mtext(side=2,outer=T,line=0.25,"Monthly Avg Discharge (cfs)")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Final/MonthQ_S79_PA25.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:2),2,1,byrow=T))
par(family="serif",mar=c(2,3.25,0.5,1),oma=c(2,2,0.5,0.5),lwd=0.5);
cols2=c("grey","black",cols.alts[4],"black",cols.alts[5])

ylim.val=c(0,16000);by.y=4000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1965-01-01","1990-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(S79~monCY.date,q.dat.xtab.mon,ylim=ylim.val,xlim=xlim.val,type="n",xaxs="i",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
for(i in c(4,5)){
  with(subset(q.dat.xtab.mon,Alt==alts.sort2[i]),
       lines(S79~monCY.date,col=adjustcolor(cols2[i],0.5),lwd=1.25))
}
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"S-79")
legend("topleft",legend=c(alts.sort2[4:5]),
       lty=1,col=adjustcolor(cols2[4:5],0.5),lwd=2,
       pt.cex=1.25,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)

xlim.val=date.fun(c("1991-01-01","2016-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"5 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(S79~monCY.date,q.dat.xtab.mon,ylim=ylim.val,xlim=xlim.val,type="n",xaxs="i",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
for(i in c(4:5)){
  with(subset(q.dat.xtab.mon,Alt==alts.sort2[i]),
       lines(S79~monCY.date,col=adjustcolor(cols2[i],0.5),lwd=1.25))
}
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Date (Month-Year)")
mtext(side=2,outer=T,line=0.25,"Monthly Avg Discharge (cfs)")
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Final/MonthQ_S79_season.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3.25,0.5,1),oma=c(2,2,1,0.5),lwd=0.5);
layout(matrix(c(1:5),5,1,byrow=T))
ylim.val=c(0,16000);by.y=4000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,12);by.x=2;xmaj=c(1,3,6,9,12)# seq(xlim.val[1],xlim.val[2],by.x);
xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
for(i in 1:length(alts.sort2)){
  plot(S79~month,q.dat.xtab.mon,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
  abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
  with(subset(q.dat.xtab.mon,Alt==alts.sort2[i]),
       points(S79~month,pch=21,cex=1.5,
              bg=adjustcolor(cols.alts2[i],0.75),
              col=adjustcolor(cols.alts2[i],0.75),lwd=0.1))
  k=loess(S79~month,subset(q.dat.xtab.mon,Alt==alts.sort2[i]))
  x.val=seq(1,12,length.out=50)
  pred.k=predict(k,data.frame(month=x.val),se=T)
  lines(x.val,pred.k$fit,col=adjustcolor("indianred1",0.5),lwd=2)
  lines(x.val,pred.k$fit - qt(0.975,pred.k$df)*pred.k$se,col=adjustcolor("indianred1",0.5), lty=2)
  lines(x.val,pred.k$fit + qt(0.975,pred.k$df)*pred.k$se,col=adjustcolor("indianred1",0.5), lty=2)
  if(i==5){axis_fun(1,xmaj,xmin,xmaj,line=-0.5)}else{axis_fun(1,xmaj,xmin,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,alts.sort2[i])
  }
mtext(side=1,line=2,"Month)")
mtext(side=2,outer=T,line=0.25,"Monthly Avg Discharge (cfs)")
dev.off()

# date.fill not needed
# date.fill=data.frame(expand.grid(CY=seq(1965,2016,1),month=1:12,Alt=alts.sort2))

bks=c(0,457,750,2100,2600,6500,20000)
q.dat.xtab.mon$S79.cat=as.factor(findInterval(q.dat.xtab.mon$S79,
                                              bks))#,rightmost.closed = T,left.open = T))

cols.vir=rev(viridis::inferno(6))
q.dat.xtab.mon$cols=cols.vir[q.dat.xtab.mon$S79.cat]

cols.vir=rev(viridis::inferno(6))
cols=c("1"=cols.vir[1],"2"=cols.vir[2],"3"=cols.vir[3],"4"=cols.vir[4],"5"=cols.vir[5],"6"=cols.vir[6])
ggplot(subset(q.dat.xtab.mon,Alt==alts.sort2[5]), aes(x = month, y = CY, fill = S79.cat)) +
  geom_tile(aes(group = S79.cat), colour = 'black')+
  #geom_text(aes(label=format(round(mean.flow,0),nsmall=0),fontface = "bold"),size=2,colour=q.dat.wk.mean$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = 1965:2016) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1,12,2),labels=seq(1,12,2))+
  scale_fill_manual(values = cols,
                    name="Monthly Average\nDischarge\nCategories\n(CFS)",
                    breaks=1:6,
                    labels=c("< 457","457 - 750", "750 - 2100","2100 - 2600","2600 - 6500",">6500")) +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Caloosahatchee River Estuary (S79)",
       subtitle = "Average weekly discharge",
       caption = paste0("Produced: ",format(Sys.Date(),"%d %b %Y")),
       x="Week",
       y="Year")

plot(CY~month,subset(q.dat.xtab.mon,Alt==alts.sort2[5]),ylim=c(2016,1965))

ylim.val=c(1965,2016);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/by.y)
ylim.val2=ylim.val
ylim.val=c(ylim.val[1]-0.5,ylim.val[2]+0.5)
xlim.val=c(1,12);by.x=5;xmaj=c(1,6,12);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
xlim.val=c(xlim.val[1]-0.5,xlim.val[2]+0.5)
# png(filename=paste0(plot.path,"Iteration3_Final/MonthQ_S79_cat.png"),width=7.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1,1,0.75),oma=c(1,3,0.5,1));
layout(matrix(c(1:6),1,6,byrow=T),widths=c(rep(1,5),0.5))
for(j in 1:length(alts.sort2)){
  plot(CY~month,q.dat.xtab.mon,xlim=xlim.val,ylim=rev(ylim.val),type="n",xaxs="i",yaxs="i",axes=F,ann=F)
for(i in seq(ylim.val2[1],ylim.val2[2],1)){
  tmp=subset(q.dat.xtab.mon,Alt==alts.sort2[j]&CY==i)
  rect(seq(0.5,12.5,1),i+0.5,13.5,i-0.5,col=tmp$cols,border=adjustcolor("grey",0.3),lwd=0.1)
}
axis_fun(1,xmaj,xmin,month.abb[xmaj],line=-0.75,cex=0.80)
if(j==1){axis_fun(2,rev(ymaj),ymin,rev(ymaj),cex=0.75)}else{axis_fun(2,rev(ymaj),ymin,NA)}
box(lwd=1)
mtext(side=3,adj=0,alts.sort2[j])
if(j==1){mtext(side=2,line=2.5,"Year")}
mtext(side=1,line=1.75,"Month",cex=0.95)
}
plot(0:1,0:1,axes=F,ann=F,type="n")
legend("center",legend=c("< 457","457 - 750", "750 - 2100","2100 - 2600","2600 - 6500",">6500"),
       lty=0,col="black",pch=22,
       pt.bg=cols.vir,lwd=0.1,
       pt.cex=1.5,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title="Monthly Average\nDischarge\nCategories\n(CFS)",)
dev.off()

### 
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

mon.vars=c(paste0("CRE.",c("low","opt","high.LOK","dam.LOK","high3")),
       paste0("SLE.",c("low","opt","high.LOK","dam.LOK","high2")))

tmp=reshape2::melt(q.dat.xtab.mon[,c("Alt",mon.vars)],id.vars = "Alt")
SalEnv_count.mon=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)

mon.vars.CRE=paste0("CRE.",c("low","opt","high.LOK","dam.LOK","high3"))
mon.vars.SLE=paste0("SLE.",c("low","opt","high.LOK","dam.LOK","high2"))
CRE.SalEnv_count.mon=SalEnv_count.mon[match(alts.sort2,SalEnv_count.mon$Alt),c("Alt",mon.vars.CRE)]
SLE.SalEnv_count.mon=SalEnv_count.mon[match(alts.sort2,SalEnv_count.mon$Alt),c("Alt",mon.vars.SLE)]

CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
SLE.labs=c("Low Flow\n(<150 cfs)","Optimum\n(150 - 1400 cfs)","Stress From LOK\n(1400 - 1700 cfs)","Damaging From LOK\n(>1700 cfs)","Extreme\n(>4000 cfs)")
# png(filename=paste0(plot.path,"Iteration3_Final/Month_SalEnv_.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2.5,2,2,1),lwd=0.5);

ymax=c(300,400,40,100,40)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(CRE.SalEnv_count.mon[,i],col=adjustcolor(cols.alts2,0.5),ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count.mon[,i],round(CRE.SalEnv_count.mon[,i],0),font=2,col="black",pos=1,cex=0.5,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ymax=c(40,400,40,60,60)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1]/2)
  x=barplot(SLE.SalEnv_count.mon[,i],col=adjustcolor(cols.alts2,0.5),ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort2,cex=0.8,las=2)
  # abline(v=c(x[1]+(x[2]-x[1])/2,x[3]+(x[4]-x[3])/2),lwd=1)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count.mon[,i],round(SLE.SalEnv_count.mon[,i],0),font=2,col="black",pos=1,cex=0.5,offset=0.25)
  # if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=1,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"Count of Months")
dev.off()

SalEnv_count.FWO=reshape2::dcast(tmp,variable~Alt,value.var = "value",sum)
SalEnv_count.FWO$PA22.PerFWO=with(SalEnv_count.FWO,((PA22-NA22f)/NA22f)*100)
SalEnv_count.FWO$PA25.PerFWO=with(SalEnv_count.FWO,((PA25-NA25f)/NA25f)*100)


# png(filename=paste0(plot.path,"Iteration3_Final/Month_SalEnv_FWO.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,2,1),lwd=0.5);

# subset(SalEnv_count.FWO,variable%in%mon.vars.CRE)
ymax.val=c(0,60,0,0,40)
ymin.val=c(-20,0,-80,-80,0)
yval=c(10,30,40,40,20)
for(i in 1:5){
  ylim.val=c(ymin.val[i],ymax.val[i]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i]);ymin=seq(ylim.val[1],ylim.val[2],yval[i]/2)
  tmp.FWO=t(subset(SalEnv_count.FWO,variable==mon.vars.CRE[i])[,c("PA22.PerFWO","PA25.PerFWO")])
  x=barplot(tmp.FWO,col=adjustcolor(cols.alts[4:5],0.5),
            ylim=ylim.val,space=0,axes=F,ann=F,beside=T,
            names.arg = rep(NA,2))
  axis_fun(2,ymaj,ymin,ymaj)
  # axis_fun(1,x,x,c("PA22","PA25"),cex=0.8,las=2)
  axis_fun(1,x,x,NA,cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i],cex=0.7)
  text(x,tmp.FWO,round(tmp.FWO,1),font=2,col="black",pos=1,cex=0.5,offset=0.25)
}
mtext(side=4,line=0.5,"Caloosahatchee")

# subset(SalEnv_count.FWO,variable%in%mon.vars.SLE)
ymax.val=c(80,20,0,0,10)
ymin.val=c(0,0,-100,-80,0)
yval=c(40,10,50,40,2.5)
for(i in 1:5){
  ylim.val=c(ymin.val[i],ymax.val[i]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i]);ymin=seq(ylim.val[1],ylim.val[2],yval[i]/2)
  tmp.FWO=t(subset(SalEnv_count.FWO,variable==mon.vars.SLE[i])[,c("PA22.PerFWO","PA25.PerFWO")])
  x=barplot(tmp.FWO,col=adjustcolor(cols.alts[4:5],0.5),
            ylim=ylim.val,space=0,axes=F,ann=F,beside=T,
            names.arg = rep(NA,2))
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,c("PA22","PA25"),cex=0.8,las=2)
  
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i],cex=0.7)
  text(x,tmp.FWO,round(tmp.FWO,1),font=2,col="black",pos=1,cex=0.5,offset=0.25)
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"% Difference relative to FWO")
dev.off()

### 
ann.Q=ddply(q.dat.xtab,c("Alt","CY"),summarise,S77Q=sum(cfs.to.acftd(S77_QFC)),S308Q=sum(cfs.to.acftd(S308_QFC)))
mean.ann.Q=ddply(ann.Q,"Alt",summarise,S77.kacft=mean(S77Q/1000),S308.kacft=mean(S308Q/1000))
mean.ann.Q=mean.ann.Q[match(alts.sort,mean.ann.Q$Alt),]

mean.ann.Q=mean.ann.Q[match(alts.sort2,mean.ann.Q$Alt),]


# png(filename=paste0(plot.path,"Iteration3_Final/mean_RegQ.png"),width=6.5,height=3.25,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:2),1,2,byrow=T))
par(family="serif",mar=c(2,2.5,1,1),oma=c(2,2,2,1),lwd=0.5);

ylim.val=c(0,600);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(mean.ann.Q$S77.kacft,
          col=adjustcolor(cols.alts2,0.5),space=c(0,0.2,0,0.2,0),
          ylim=ylim.val,axes=F,ann=F,beside=T)
text(x,mean.ann.Q$S77.kacft,round(mean.ann.Q$S77.kacft,0),pos=3,offset=0.1)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort2,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"S77 Regulatory Discharge",cex=1)
mtext(side=2,line=2.5,"Discharge (x1000 Ac-Ft Y\u207B\u00B9)")

ylim.val=c(0,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(mean.ann.Q$S308.kacft,
          col=adjustcolor(cols.alts2,0.5),space=c(0,0.2,0,0.2,0),
          ylim=ylim.val,axes=F,ann=F,beside=T)
text(x,mean.ann.Q$S308.kacft,round(mean.ann.Q$S308.kacft,0),pos=3,offset=0.1)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort2,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"S308 Regulatory Discharge",cex=1)
mtext(side=1,line=1,outer=T,"Alternative")
dev.off()

# Lake Discharges ---------------------------------------------------------
q.dat.xtab$S79_GT2100=with(q.dat.xtab,ifelse(S79>=2100,1,0))
q.dat.xtab$S80_GT1400=with(q.dat.xtab,ifelse(S80>=1400,1,0))
q.dat.xtab$S79_LT2100=with(q.dat.xtab,ifelse(S79<2100,1,0))
q.dat.xtab$S80_LT1400=with(q.dat.xtab,ifelse(S80<1400,1,0))

CRE.GT2100_annual=ddply(q.dat.xtab,c("CY","Alt"),summarise,Q.Lake.GT2100=sum(cfs.to.acftd(S79_QPFCSOURCE_LAKE[S79_GT2100==1])/1000,na.rm=T))
CRE.GT2100_annual.mean=ddply(CRE.GT2100_annual,"Alt",summarise,mean.val=mean(Q.Lake.GT2100))
CRE.GT2100_annual.mean=CRE.GT2100_annual.mean[match(alts.sort,CRE.GT2100_annual.mean$Alt),]
CRE.GT2100_annual.mean=CRE.GT2100_annual.mean[match(alts.sort2,CRE.GT2100_annual.mean$Alt),]

SLE.GT1400_annual=ddply(q.dat.xtab,c("CY","Alt"),summarise,Q.Lake.GT1400=sum(cfs.to.acftd(S80_QPFCSOURCE_LAKE[S80_GT1400==1])/1000,na.rm=T))
SLE.GT1400_annual.mean=ddply(SLE.GT1400_annual,"Alt",summarise,mean.val=mean(Q.Lake.GT1400))
SLE.GT1400_annual.mean=SLE.GT1400_annual.mean[match(alts.sort,SLE.GT1400_annual.mean$Alt),]
SLE.GT1400_annual.mean=SLE.GT1400_annual.mean[match(alts.sort2,SLE.GT1400_annual.mean$Alt),]

# png(filename=paste0(plot.path,"Iteration3_Final/Lakedischarge_Annualmean.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:2),1,2,byrow=T))
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);

ylim.val=c(0,600);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(CRE.GT2100_annual.mean$mean.val,col=NA,border=NA,ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(CRE.GT2100_annual.mean$mean.val,
          col=adjustcolor(cols.alts2,0.5),space=c(0,0.2,0,0.2,0),ylim=ylim.val,axes=F,ann=F,add=T)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort2,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"Mean Annual Discharge\n\u22652100 cfs at S-79",cex=0.75)
text(x,CRE.GT2100_annual.mean$mean.val,round(CRE.GT2100_annual.mean$mean.val,0),col="black",pos=1,cex=1)
# mtext(side=3,adj=1,"CY1965 - 2016",cex=0.75)
mtext(side=1,line=2.5,"Alternative")
mtext(side=2,line=2.5,"Lake Discharge\n(x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=3,line=-1.25,adj=0," CRE")
# mtext(side=1,line=3,adj=1,"Flow Tag: S79_QPFCSOURCE_LAKE",cex=0.5,col=adjustcolor("black",0.5),font=3)

ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(SLE.GT1400_annual.mean$mean.val,col=NA,border=NA,ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(SLE.GT1400_annual.mean$mean.val,
          col=adjustcolor(cols.alts2,0.5),space=c(0,0.2,0,0.2,0),ylim=ylim.val,axes=F,ann=F,add=T)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort2,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"Mean Annual Discharge\n\u22651400 cfs at S-80",cex=0.75)
mtext(side=3,line=-1.25,adj=0," SLE")
text(x,SLE.GT1400_annual.mean$mean.val,round(SLE.GT1400_annual.mean$mean.val,0),col="black",pos=1,cex=1)
mtext(side=3,adj=1,"CY1965 - 2016",cex=0.75)
mtext(side=1,line=2.5,"Alternative")
dev.off()


# Flood Control -----------------------------------------------------------
n.alts=length(alts.sort)
RSM.sites=c("S351_QFC","S351_FC_SHIFT2_ENVTARG","S354_QFC","S354_FC_SHIFT2_ENVTARG",
            "S77_QFC","S308_QFC","S271_QFC")
regq.dat=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    regq.dat=rbind(tmp,regq.dat)
    print(i)
  }
}
RSM.sites.region=data.frame(SITE=RSM.sites,Region=c(rep("WCAs",4),"Cal",'StL',"LWLagoon"))

regq.dat=merge(regq.dat,RSM.sites.region,"SITE")
regq.dat$CY=as.numeric(format(regq.dat$Date,"%Y"))
regq.dat$month=as.numeric(format(regq.dat$Date,"%m"))

regq.dat.CY=ddply(regq.dat,c("CY","Alt",'Region'),summarise,TFlow.kAcft=sum(cfs.to.acftd(FLOW),na.rm=T)/1000)

regq.dat.CY.mean=reshape2::dcast(regq.dat.CY,Alt~Region,value.var = "TFlow.kAcft",mean)
regq.dat.CY.mean=regq.dat.CY.mean[match(alts.sort,regq.dat.CY.mean$Alt),]
regq.dat.CY.mean=regq.dat.CY.mean[,c("Alt","WCAs","Cal","StL","LWLagoon")]
regq.dat.CY.mean=regq.dat.CY.mean[match(alts.sort2,regq.dat.CY.mean$Alt),]

tmp=regq.dat.CY.mean[,c("WCAs","Cal","StL","LWLagoon")]
rownames(tmp)<-alts.sort
cols2=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))

ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Final/iter3_PA_AvgFloodControl.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),heights=c(1,0.4))

x=barplot(t(tmp),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp),beside=F,col=cols2,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts),add=T)
with(regq.dat.CY.mean,text(x,WCAs/2,round(WCAs,0),cex=0.75,col="white"))
with(regq.dat.CY.mean,text(x,WCAs+(((Cal+WCAs)-WCAs)/2),round(regq.dat.CY.mean$Cal,0),cex=0.75))
with(regq.dat.CY.mean,text(x,(WCAs+Cal)+(((Cal+WCAs+StL)-(Cal+WCAs))/2),round(regq.dat.CY.mean$StL,0),cex=0.75))
with(regq.dat.CY.mean,text(x,Cal+WCAs+StL+LWLagoon,round(regq.dat.CY.mean$LWLagoon,0),pos=3,offset=0.1,cex=0.75))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort2,line=-0.25,las=2);box(lwd=1)
mtext(side=2,line=3,"Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=3,"Alternatives")

# par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(-0.15,0.75,legend=c("Water Conservation Areas (S351 & S354)","Caloosahatchee River (S77)","St. Lucie River (S308)","Lake Worth Lagoon (S271)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols2,
       pt.cex=1.25,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)
text(1,0.2,"Iteration 3 Preferred Alternative results. Mean annual flood\ncontrol releases from Lake Okeechobee for the\n52 year (1965 - 2016)simulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()

# Consecutive days Extremes -----------------------------------------------
# q.dat.xtab$da.CRE.high3
# q.dat.xtab$da.SLE.high2
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

bks=c(1,14,30,60,90,180)
rslt.CREHighQ=ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500"),summarise,count.event=N.obs(sum.CRE.Q6500))
rslt.CREHighQ$cat=findInterval(rslt.CREHighQ$sum.CRE.Q6500,bks,left.open = FALSE,rightmost.closed = TRUE)
rslt.CREHighQ.LOK=ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500.LOK"),summarise,count.event=N.obs(sum.CRE.Q6500.LOK))
rslt.CREHighQ.LOK$cat=findInterval(rslt.CREHighQ.LOK$sum.CRE.Q6500.LOK,bks,left.open = FALSE,rightmost.closed = TRUE)

rslt.SLEHighQ=ddply(extremeQ_consec,c("Alt","sum.SLE.Q4000"),summarise,count.event=N.obs(sum.SLE.Q4000))
rslt.SLEHighQ$cat=findInterval(rslt.SLEHighQ$sum.SLE.Q4000,bks,left.open = FALSE,rightmost.closed = TRUE)
rslt.SLEHighQ.LOK=ddply(extremeQ_consec,c("Alt","sum.SLE.Q4000.LOK"),summarise,count.event=N.obs(sum.SLE.Q4000))
rslt.SLEHighQ.LOK$cat=findInterval(rslt.SLEHighQ.LOK$sum.SLE.Q4000.LOK,bks,left.open = FALSE,rightmost.closed = TRUE)

rslt.CREHigh.sum=reshape2::dcast(subset(rslt.CREHighQ,cat!=0),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.CREHigh.LOK.sum=reshape2::dcast(subset(rslt.CREHighQ.LOK,cat!=0),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.SLEHigh.sum=reshape2::dcast(subset(rslt.SLEHighQ,cat!=0),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.SLEHigh.LOK.sum=reshape2::dcast(subset(rslt.SLEHighQ.LOK,cat!=0),cat~Alt,value.var="count.event",sum,na.rm=T)

tmp=rslt.CREHigh.LOK.sum[1,]
tmp=c(4,0,0,0,0,0)
tmp2=tmp
tmp2=c(5,0,0,0,0,0)
rslt.CREHigh.LOK.sum=rbind(rslt.CREHigh.LOK.sum,tmp,tmp2)

rslt.CREHigh.LOK.sum=rslt.CREHigh.LOK.sum[,c("cat",alts.sort2)]
rslt.CREHigh.sum=rslt.CREHigh.sum[,c("cat",alts.sort2)]

xlabs=c("< 14", "14 - 30","30 - 60","60 - 90","> 90")
# png(filename=paste0(plot.path,"Iteration3_Final/CRE_highQ_events.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.75,2,0.5,1),oma=c(2,4,1,0.25),lwd=0.5);
layout(matrix(c(1:10),5,2,byrow=F))

ylim.max=c(190,20,20,10,4)
for(i in 1:nrow(rslt.CREHigh.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.CREHigh.sum[i,2:ncol(rslt.CREHigh.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols.alts2,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.1,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,cex=1,line=-0.5)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"),cex=0.75)
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"CRE Extreme (> 6500 cfs)")}
}

for(i in 1:nrow(rslt.CREHigh.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.CREHigh.LOK.sum[i,2:ncol(rslt.CREHigh.LOK.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols.alts2,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.1,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,cex=1,line=-0.5)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"CRE Extreme from LOK (> 6500 cfs)")}
}
mtext(side=1,outer=T,line=1,"Alternatives")
dev.off()

tmp=rslt.SLEHigh.LOK.sum[1,]
tmp=c(3,rep(0,5))
tmp2=tmp
tmp2=c(4,rep(0,5))
rslt.SLEHigh.LOK.sum=rbind(rslt.SLEHigh.LOK.sum,tmp,tmp2)

xlabs=xlabs[1:4]
# png(filename=paste0(plot.path,"Iteration3_Final/SLE_highQ_events.png"),width=6.5,height=3.75,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.75,2,0.5,1),oma=c(2,4,1,0.25),lwd=0.5);
layout(matrix(c(1:8),4,2,byrow=F))

ylim.max=c(600,20,4,2)
for(i in 1:nrow(rslt.SLEHigh.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.SLEHigh.sum[i,2:ncol(rslt.SLEHigh.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols.alts2,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.1,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,cex=1,line=-0.5)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"),cex=0.75)
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"SLE Extreme (> 4000 cfs)")}
}

for(i in 1:nrow(rslt.SLEHigh.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.SLEHigh.LOK.sum[i,2:ncol(rslt.SLEHigh.LOK.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols.alts2,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.1,cex=0.8)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,cex=1,line=-0.5)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"SLE Extreme from LOK (> 4000 cfs)")}
}
mtext(side=1,outer=T,line=1,"Alternatives")
dev.off()

# Lake Stage --------------------------------------------------------------

n.alts=length(alts)
lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/RSMBN_output.dss"))
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
unique(lakeO.stage$Alt)
range(lakeO.stage$Date)

lakeO.stage$recess_7day=with(lakeO.stage,ave(STAGE,Alt,FUN=function(x) c(rep(NA,6),diff(x,lag=6))))

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

ann.peak=merge(ann.peak,data.frame(Alt=alts.sort2,plot.y=1:5),"Alt")
subset(ann.peak,Alt=="ECB19")

# png(filename=paste0(plot.path,"Iteration3_Final/LOK_GT17_timeline.png"),width=7,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,1.25,0.5),oma=c(2,2,1,1),lwd=0.1);

xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0.75,5.25);by.y=1;ymaj=1:5#seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(plot.y~CY,ann.peak,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
for(i in 1:5){
  points(plot.y~CY,subset(ann.peak,GT17==1&Alt==alts.sort2[i]),pch=21,bg=adjustcolor(cols.alts2[i],0.5),cex=1.5)
}
axis_fun(2,1:5,1:5,alts.sort2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
mtext(side=2,line=4,"Alternative")
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=3,adj=0,line=1,"Lake Okeechobee")
mtext(side=3,adj=0,cex=0.75,"Annual maximum stage \u2265 17Ft NGVD29",col="grey50")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Final/LOK_totalDays.png"),width=6,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,0.25,1),oma=c(1,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(2000,2500);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,4000);by.x=1000;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
days.POS=ddply(lakeO.stage,"Alt",summarise,N.days=N.obs(STAGE),sum.low=sum(low.stg),sum.vlow=sum(vlow.stg),sum.High=sum(High.stg),sum.vHigh=sum(vHigh.stg))
# days.POS$Alt=factor(days.POS$Alt,level=alts.sort)
days.POS=days.POS[match(alts.sort2,days.POS$Alt),]

plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(days.POS,points(sum.High,sum.low,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1,col=ifelse(Alt=="CC","black",adjustcolor(cols,0.5))))
with(subset(days.POS,Alt%in%c("PA25","NA25f")),text(sum.High,sum.low,Alt,pos=3,col=adjustcolor("black",0.5),cex=0.5))
with(subset(days.POS,!(Alt%in%c("PA25","NA25f"))),text(sum.High,sum.low,Alt,pos=4,col=adjustcolor("black",0.5),cex=0.5))
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
with(days.POS,points(sum.vHigh,sum.vlow,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1,col=ifelse(Alt=="CC","black",adjustcolor(cols,0.5))))
with(subset(days.POS,Alt%in%c("PA25","NA25f")),text(sum.vHigh,sum.vlow,Alt,pos=3,col=adjustcolor("black",0.5),cex=0.5))
with(subset(days.POS,!(Alt%in%c("PA25","NA25f"))),text(sum.vHigh,sum.vlow,Alt,pos=4,col=adjustcolor("black",0.5),cex=0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u2265 17 ft NGVD29")
mtext(side=2,line=2.75,"Days \u2264 10 ft NGVD29")
dev.off()

labs=c("% LOK Stage >17 Ft NGVD29","% LOK Stage >16 Ft NGVD29","% LOK Stage <10 Ft NGVD29")
# png(filename=paste0(plot.path,"Iteration3_Final/LOK_stg.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:3,3,1,byrow=F))

ylim.val=c(0,2);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.vHigh/18993)*100,col=adjustcolor(cols.alts2,0.5),
          ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
# axis_fun(1,x,x,c("PA22","PA25"),cex=0.8,las=2)
axis_fun(1,x,x,NA,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,line=1.25,"LOK")
mtext(side=3,adj=0,labs[1],cex=0.7)
text(x,(days.POS$sum.vHigh/18993)*100,
     round((days.POS$sum.vHigh/18993)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)

ylim.val=c(0,25);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.High/18993)*100,col=adjustcolor(cols.alts2,0.5),
          ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
# axis_fun(1,x,x,c("PA22","PA25"),cex=0.8,las=2)
axis_fun(1,x,x,NA,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,labs[2],cex=0.7)
text(x,(days.POS$sum.High/18993)*100,
     round((days.POS$sum.High/18993)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)

ylim.val=c(0,10);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot((days.POS$sum.vlow/18993)*100,col=adjustcolor(cols.alts2,0.5),
          ylim=ylim.val,space=c(0,0.2,0,0.2,0),axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort2,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,labs[3],cex=0.7)
text(x,(days.POS$sum.vlow/18993)*100,
     round((days.POS$sum.vlow/18993)*100,1),font=2,col="black",pos=3,cex=1,offset=0.25)
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=2,"Alternative",outer=T)
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
env.rslt=merge(env.rslt,data.frame(Alt=alts.sort2,PlotOffset=rev(seq(2,10,2))),"Alt")
env.rslt$env.plt=with(env.rslt,env2+PlotOffset)
env.rslt$env.f=with(env.rslt,ifelse(env==1,"normal","recovery"))
env.rslt$Alt=factor(env.rslt$Alt,levels=alts.sort)

#env.count=reshape2::dcast(env.rslt,Alt~env.f,value.var = "env",function(x) N.obs(x))
env.count=ddply(env.rslt,c("Alt","env.f"),summarise,N.val=N.obs(env.f))
env.count=env.count[c(1,2,3,4,7,8,5,6,9,10),]
env.count$axs.val=11:2

# png(filename=paste0(plot.path,"Iteration3_Final/LOK_Env.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1.75,1),oma=c(2,2,1,2));

ylim.val=c(1.5,11.5)
xlim.val=c(1965,2016);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(env2~CY,env.rslt,type="n",axes=F,ann=F,xlim=xlim.val,xaxs="i",yaxs="i",ylim=ylim.val)
abline(v=c(xmaj,xmin),h=c(2:19),lty=c(1,3),lwd=0.5,col=adjustcolor("grey",0.5))
abline(h=c(2:19),lty=c(3),lwd=0.5,col=c("grey"))
for(i in 1:length(alts.sort2)){
  with(subset(env.rslt,Alt==alts.sort2[i]),lines(env.plt~CY,type="s",col=cols.alts2[i],lwd=3))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.75,cex=0.75)
axis_fun(2,seq(3,11,2),seq(3,11,2),rev(alts.sort2))
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
  tmp=subset(lakeO.stage,Alt==alts[i])[,c("Date","Data.Value")]
  rslt=LORECOVER::norm_env(tmp)
  rslt$Alt=alts[i]
  norm.lakeO.stage.scr=rbind(norm.lakeO.stage.scr,rslt)
  print(i)
}

norm.lakeO.stage.scr=rename(norm.lakeO.stage.scr,c("penalty"="norm.score"))
norm.lakeO.stage.scr$WY=WY(norm.lakeO.stage.scr$Date)

rec.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts[i])[,c("Date","Data.Value")]
  rslt=LORECOVER::rec_env(tmp)
  rslt$Alt=alts[i]
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
# env.pen.sum$FWO_totalpen=with(env.pen.sum,(total.pen-total.pen[1])/total.pen[1])*100

env.pen.sum.melt=reshape2::melt(env.pen.sum[,c("Alt","pen_above","pen_below","total.pen")],id.var="Alt")
env.pen.sum.FWO=reshape2::dcast(env.pen.sum.melt,variable~Alt,value.var = "value")
env.pen.sum.FWO$PA22.PerFWO=with(env.pen.sum.FWO,((PA22-NA22f)/NA22f)*100)
env.pen.sum.FWO$PA25.PerFWO=with(env.pen.sum.FWO,((PA25-NA25f)/NA25f)*100)


lakeO.stage.scr$month=as.numeric(format(lakeO.stage.scr$Date,"%m"))
env.pen.sum.maysept=ddply(subset(lakeO.stage.scr,month%in%seq(5,9,1)),"Alt",summarise,
                          N.val=N.obs(score),
                          pen_above=sum(score[score>0],na.rm=T),
                          pen_below=sum(abs(score)[score<0],na.rm=T),
                          per_below=(sum(score<0)/N.obs(score))*100,
                          per0=(sum(score==0,na.rm=T)/N.obs(score))*100,
                          per_above=(sum(score>0)/N.obs(score))*100)
env.pen.sum.maysept$total.pen=rowSums(env.pen.sum.maysept[,c("pen_above","pen_below")])
env.pen.sum.maysept=env.pen.sum.maysept[match(alts.sort2,env.pen.sum.maysept$Alt),]

cols.IMC=c(rgb(238,232,170,maxColorValue = 255),rgb(143,188,143,maxColorValue = 255))
# png(filename=paste0(plot.path,"Iteration3_Final/LOK_EnvScore_AllYrs.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,50000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(t(env.pen.sum[,c("pen_below","pen_above")]),beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(t(env.pen.sum[,c("pen_below","pen_above")]),beside=F,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)),add=T)
with(env.pen.sum,text(x,pen_below/2,round(pen_below,0)))
with(env.pen.sum,text(x,pen_below+abs((pen_below-total.pen)/2),round(pen_above,0)))
with(env.pen.sum,text(x,total.pen,round(total.pen,0),pos=3))
axis_fun(1,x,x,alts.sort2,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Stage Envelope Penalty Score")
mtext(side=1,line=1.75,"Alternative")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
mtext(side=3,line=-2,"Lake Okeechobee\nEnvelope Penalty Score\nAll Years")
legend(0.5,0.5,legend=c("Lower Penalty\n(Below Envelope)","Upper Penalty\n(Above Envelope)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c(cols.IMC),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Total Score")
mtext(side=1,line=-1,adj=1,"Simulation Period of Record\n(1965-2016). Includes Normal\nand Recovery envelope.",cex=0.75)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Final/LOK_EnvScore_MaySep.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,20000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(t(env.pen.sum.maysept[,c("pen_below","pen_above")]),beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(t(env.pen.sum.maysept[,c("pen_below","pen_above")]),beside=F,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)),add=T)
with(env.pen.sum.maysept,text(x,pen_below/2,round(pen_below,0)))
with(env.pen.sum.maysept,text(x,pen_below+abs((pen_below-total.pen)/2),round(pen_above,0)))
with(env.pen.sum.maysept,text(x,total.pen,round(total.pen,0),pos=3))
axis_fun(1,x,x,alts.sort2,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Stage Envelope Penalty Score")
mtext(side=1,line=1.75,"Alternative")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
mtext(side=3,line=-3,"Lake Okeechobee\nEnvelope Penalty Score\nMay-Sep\nAll Years")
legend(0.5,0.5,legend=c("Lower Penalty\n(Below Envelope)","Upper Penalty\n(Above Envelope)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c(cols.IMC),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Total Score")
mtext(side=1,line=-1,adj=1,"Simulation Period of Record\n(1965-2016). Includes Normal\nand Recovery envelope.",cex=0.75)
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

bks=c(1,15,30,60,90,180,365)
rslt.stg16=ddply(highstg_consec,c("Alt","sum.stg16"),summarise,count.event=N.obs(sum.stg16))
rslt.stg16$cat=findInterval(rslt.stg16$sum.stg16,bks,left.open = FALSE,rightmost.closed = TRUE)
rslt.stg17=ddply(highstg_consec,c("Alt","sum.stg17"),summarise,count.event=N.obs(sum.stg17))
rslt.stg17$cat=findInterval(rslt.stg17$sum.stg17,bks,left.open = FALSE,rightmost.closed = TRUE)

rslt.stg16.sum=reshape2::dcast(subset(rslt.stg16,cat>0),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.stg17.sum=reshape2::dcast(subset(rslt.stg17,cat>0),cat~Alt,value.var="count.event",sum,na.rm=T)
# fill
tmp=rslt.stg17.sum[1,]
tmp[,1:6]=tmp[,1:6]=c(5,rep(0,5))
rslt.stg17.sum=rbind(rslt.stg17.sum,tmp)
tmp=rslt.stg17.sum[1,]
tmp[,1:6]=tmp[,1:6]=c(6,rep(0,5))
rslt.stg17.sum=rbind(rslt.stg17.sum,tmp)

rslt.stg16.sum=rslt.stg16.sum[,c("cat",alts.sort2)]
rslt.stg17.sum=rslt.stg17.sum[,c("cat",alts.sort2)]
xlabs=c("< 15", "15 - 30","30 - 60","60 - 90","90 - 180", "> 180")

# png(filename=paste0(plot.path,"Iteration3_Final/LOK_highstg_events.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.5,2,0.5,1),oma=c(4,3,1,0.25),lwd=0.5);
layout(matrix(c(1:12),6,2,byrow=F))

# ylim.max=c(150,20,20,10,4)
for(i in 1:length(xlabs)){
  ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.stg16.sum[i,2:ncol(rslt.stg16.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols.alts2,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.25)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"),cex=0.6)
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"LOK Daily Stage > 16 Ft NGVD",cex=0.8)}
}
for(i in 1:length(xlabs)){
  ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.stg17.sum[i,2:ncol(rslt.stg17.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols.alts2,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  text(x,tmp,tmp,pos=3,offset=0.25)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  # mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"),cex=0.75)
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"LOK Daily Stage \u2265 17 Ft NGVD",cex=0.8)}
}
mtext(side=1,outer=T,line=2.5,"Alternative")

dev.off()


# 7-day Recession Rate ----------------------------------------------------
date.fill=data.frame(expand.grid(CY=seq(min(lakeO.stage$CY),max(lakeO.stage$CY),1),
                                 DoY=1:366,
                                 Alt=alts.sort2))
snki.nest=seq(date.fun("1970-03-01"),date.fun("1970-06-15"),"1 days")

lakeO.stage$SNKI.period=with(lakeO.stage,ifelse(format(Date,"%m-%d")%in%format(snki.nest,"%m-%d"),1,0))
recess.dat=lakeO.stage[,c("Alt","CY","DoY","STAGE","SNKI.period","recess_7day")]
bks=c(min(recess.dat$recess_7day,na.rm=T),c(-0.16,-0.05,0.05,0.25),max(recess.dat$recess_7day,na.rm=T))
recess.dat$cat=as.factor(findInterval(recess.dat$recess_7day,bks,rightmost.closed = T,left.open = T))

ddply(recess.dat,c("Alt","cat"),summarise,min.val=min(recess_7day,na.rm=T),max.val=max(recess_7day,na.rm=T))


recess.dat=merge(recess.dat,date.fill,by=c("CY",'DoY','Alt'),all.y=T)
recess.dat=recess.dat[order(recess.dat$Alt,recess.dat$CY,recess.dat$DoY),]


SNKI.recess.cols=c("red","yellow","green","goldenrod2","darkred")
recess.dat$cols=SNKI.recess.cols[recess.dat$cat]

ylim.val=c(1965,2016);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/by.y)
ylim.val2=ylim.val
ylim.val=c(ylim.val[1]-0.5,ylim.val[2]+0.5)
xlim.val=c(1,366);by.x=90;by.x2=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x2)
xlim.val=c(1-0.5,xlim.val[2]+0.5)
# png(filename=paste0(plot.path,"Iteration3_Final/LOK_wkreccess.png"),width=9,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1,1,0.75),oma=c(1,3,0.5,1));
layout(matrix(c(1:6),1,6,byrow=T),widths=c(rep(1,5),0.75))
for(j in 1:length(alts.sort2)){
  plot(CY~DoY,recess.dat,xlim=xlim.val,ylim=rev(ylim.val),type="n",xaxs="i",yaxs="i",axes=F,ann=F)
  for(i in seq(ylim.val2[1],ylim.val2[2],1)){
    tmp=subset(recess.dat,Alt==alts.sort2[j]&CY==i)
    rect(seq(0.5,365.5,1),i+0.5,366.5,i-0.5,col=tmp$cols,border=adjustcolor("grey",0.3),lwd=0.1)
  }
  axis_fun(1,xmaj,xmin,format(as.Date(xmaj),"%b"),line=-0.75,cex=0.80)
  if(j==1){axis_fun(2,rev(ymaj),ymin,rev(ymaj),cex=0.75)}else{axis_fun(2,rev(ymaj),ymin,NA)}
  box(lwd=1)
  mtext(side=3,adj=0,alts.sort2[j])
  if(j==1){mtext(side=2,line=2.5,"Year")}
  mtext(side=1,line=1.75,"Month",cex=0.95)
}
plot(0:1,0:1,axes=F,ann=F,type="n")
# mtext(side=3,line=-3,"All Years/Days")
legend("center",legend=c("\u2264 -0.16","> -0.16 & < -0.05","> -0.05 & \u2264 0.05","> 0.05 & \u2264 0.25","> 0.25"),
       lty=0,col="black",pch=22,
       pt.bg=SNKI.recess.cols,lwd=0.1,
       pt.cex=1.5,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title="Weekly Recession\nRate (ft/wk)",)
dev.off()


recess.dat.snki=subset(recess.dat,SNKI.period==1)
ylim.val=c(1965,2016);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/by.y)
ylim.val2=ylim.val
ylim.val=c(ylim.val[1]-0.5,ylim.val[2]+0.5)
xlim.val=c(59,166);by.x=30;by.x2=15;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x2)
xlim.val=c(xlim.val[1]-0.5,xlim.val[2]+0.5)
# png(filename=paste0(plot.path,"Iteration3_Final/LOK_wkreccess_snki.png"),width=9,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1,1,0.75),oma=c(1,3,0.5,1));
layout(matrix(c(1:6),1,6,byrow=T),widths=c(rep(1,5),0.75))
for(j in 1:length(alts.sort2)){
  plot(CY~DoY,recess.dat,xlim=xlim.val,ylim=rev(ylim.val),type="n",xaxs="i",yaxs="i",axes=F,ann=F)
  for(i in seq(ylim.val2[1],ylim.val2[2],1)){
    tmp=subset(recess.dat.snki,Alt==alts.sort2[j]&CY==i)
    rect(seq(59.5,165.5,1),i+0.5,166.5,i-0.5,col=tmp$cols,border=adjustcolor("grey",0.3),lwd=0.1)
  }
  axis_fun(1,xmaj,xmin,format(as.Date(xmaj),"%m-%d"),line=-0.75,cex=0.80)
  if(j==1){axis_fun(2,rev(ymaj),ymin,rev(ymaj),cex=0.75)}else{axis_fun(2,rev(ymaj),ymin,NA)}
  box(lwd=1)
  mtext(side=3,adj=0,alts.sort2[j])
  if(j==1){mtext(side=2,line=2.5,"Year")}
  mtext(side=1,line=1.75,"Month-Day",cex=0.95)
}
plot(0:1,0:1,axes=F,ann=F,type="n")
mtext(side=3,line=-3,"SNKI Nest Period\nMar 1-Jun 15")
legend("center",legend=c("\u2264 -0.16","> -0.16 & < -0.05","> -0.05 & \u2264 0.05","> 0.05 & \u2264 0.25","> 0.25"),
       lty=0,col="black",pch=22,
       pt.bg=SNKI.recess.cols,lwd=0.1,
       pt.cex=1.5,ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title="Weekly Recession\nRate (ft/wk)",)
dev.off()


# PM eval - Haven paper ---------------------------------------------------

# Consecutive Events Stage ------------------------------------------------
lakeO.stage$Stage.m=ft.to.m(lakeO.stage$STAGE)
lakeO.stage$ExHigh=with(lakeO.stage,ifelse(Stage.m>ft.to.m(17),1,0))
lakeO.stage$ModHigh=with(lakeO.stage,ifelse(Stage.m>ft.to.m(16),1,0))
lakeO.stage$ExLow=with(lakeO.stage,ifelse(Stage.m<ft.to.m(10),1,0))
lakeO.stage$ModLow=with(lakeO.stage,ifelse(Stage.m<ft.to.m(11),1,0))


stg_consec=data.frame()
for(j in 1:length(alts.sort)){
  tmp=subset(lakeO.stage,Alt==alts.sort[j])
  tmp$ExHigh.stg=0
  tmp$ModHigh.stg=0
  tmp$ExLow.stg=0
  tmp$ModLow.stg=0
  for(i in 2:nrow(tmp)){
    tmp$ExHigh.stg[i]=with(tmp,ifelse(ExHigh[i-1]==0&ExHigh[i]>0,1,
                                      ifelse(ExHigh[i-1]>0&ExHigh[i]>0,1,0)))
    tmp$ModHigh.stg[i]=with(tmp,ifelse(ModHigh[i-1]==0&ModHigh[i]>0,1,
                                       ifelse(ModHigh[i-1]>0&ModHigh[i]>0,1,0)))
    tmp$ExLow.stg[i]=with(tmp,ifelse(ExLow[i-1]==0&ExLow[i]>0,1,
                                     ifelse(ExLow[i-1]>0&ExLow[i]>0,1,0)))
    tmp$ModLow.stg[i]=with(tmp,ifelse(ModLow[i-1]==0&ModLow[i]>0,1,
                                      ifelse(ModLow[i-1]>0&ModLow[i]>0,1,0)))
  }
  
  exhighstg=consec.startend(tmp$ExHigh.stg>0)
  tmp$sum.ExHigh.stg=0
  for(i in 1:length(exhighstg$ends)){
    tmp[exhighstg$ends[i],]$sum.ExHigh.stg=with(tmp[c(exhighstg$starts[i]:exhighstg$ends[i]),],sum(ExHigh.stg,na.rm=T))
  }
  modhighstg=consec.startend(tmp$ModHigh.stg>0)
  tmp$sum.ModHigh.stg=0
  for(i in 1:length(modhighstg$ends)){
    tmp[modhighstg$ends[i],]$sum.ModHigh.stg=with(tmp[c(modhighstg$starts[i]:modhighstg$ends[i]),],sum(ModHigh.stg,na.rm=T))
  }
  exlowstg=consec.startend(tmp$ExLow.stg>0)
  tmp$sum.ExLow.stg=0
  for(i in 1:length(exlowstg$ends)){
    tmp[exlowstg$ends[i],]$sum.ExLow.stg=with(tmp[c(exlowstg$starts[i]:exlowstg$ends[i]),],sum(ExLow.stg,na.rm=T))
  }
  modlowstg=consec.startend(tmp$ModLow.stg>0)
  tmp$sum.ModLow.stg=0
  for(i in 1:length(modlowstg$ends)){
    tmp[modlowstg$ends[i],]$sum.ModLow.stg=with(tmp[c(modlowstg$starts[i]:modlowstg$ends[i]),],sum(ModLow.stg,na.rm=T))
  }
  
  stg_consec=rbind(tmp,stg_consec)
  print(j)
}

bks=c(1,15,30,60,90,180)
# proof of concept
# test=ddply(stg_consec,c("Alt2","sum.ExLow.stg"),summarise,count.event=N.obs(sum.ExLow.stg))
# test$cat1=findInterval(test$sum.ExLow.stg,bks,left.open = FALSE,rightmost.closed = TRUE)
# test$cat2=with(test,ifelse(sum.ExLow.stg>0&sum.ExLow.stg<15,1,
#                                   ifelse(sum.ExLow.stg>=15&sum.ExLow.stg<30,2,
#                                          ifelse(sum.ExLow.stg>=30&sum.ExLow.stg<60,3,
#                                                 ifelse(sum.ExLow.stg>=60&sum.ExLow.stg<90,4,
#                                                        ifelse(sum.ExLow.stg>=90&sum.ExLow.stg<180,5,
#                                                               ifelse(sum.ExLow.stg>=180,6,NA)))))))
# test
# plot(cat1~cat2,test);abline(0,1)

# PMs=c("ExHigh","ModHigh","ModLow","ExLow")
# fill.vals=expand.grid(PM=PMs,
#             cat=c(1:6),
#             Alt2=alts.iter2)
## Doesn't exactly work the way I wanted it to...doesnt give correct sums.
# stg_consec.sum=data.frame()
# for(i in 1:length(PMs)){
#   tmp=ddply(stg_consec,c("Alt2",paste("sum",PMs[i],"stg",sep=".")),summarise,count.event=N.obs(paste("sum",PMs[i],"stg",sep=".")))
#   colnames(tmp)=c("Alt2","Event.Dur","count.event")
#   tmp$PM=PMs[i]
#   tmp$cat=findInterval(tmp$Event.Dur,bks,left.open = FALSE,rightmost.closed = TRUE)
#   stg_consec.sum=rbind(tmp,stg_consec.sum)
#   print(i)
# }

dec.mean=ddply(stg_consec,c("Alt"),summarise,
               ExHigh=sum(sum.ExHigh.stg>0,na.rm=T)/5.2,
               ModHigh=sum(sum.ModHigh.stg>90,na.rm=T)/5.2,
               ModLow=sum(sum.ModLow.stg>90,na.rm=T)/5.2,
               ExLow=sum(sum.ExLow.stg>0,na.rm=T)/5.2)
dec.mean
dec.mean[,2:ncol(dec.mean)]=round(dec.mean[,2:ncol(dec.mean)],0)

recess.dat.snki

rec.rate=ddply(subset(recess.dat.snki,SNKI.period==1),c("Alt","CY"),summarise,
               N.val=N.obs(Alt),
               recess.event=sum(cat==3,na.rm=T),
               min.val=min(ifelse(cat==3,recess_7day,NA),na.rm=T),
               max.val=max(ifelse(cat==3,recess_7day,NA),na.rm=T))
rec.rate$event.per=(rec.rate$recess.event/rec.rate$N.val)*100
rec.rate$event.per.score=with(rec.rate,ifelse(event.per<25,1,0))
rec.rate.dec=ddply(rec.rate,"Alt",summarise,sum.recevent=sum(event.per.score,na.rm=T)/5.2)
rec.rate.dec$sum.recevent=round(rec.rate.dec$sum.recevent,0)

stg.score=data.frame(val=0:52,score=c(1.0,0.9,0.7,0.4,0.3,0.2,rep(0,47)))

plot(score~val,stg.score,log="x")

hydro.score.sum=merge(dec.mean,rec.rate.dec,"Alt")
hydro.score.sum$ExHigh.score=stg.score[match(hydro.score.sum$ExHigh,stg.score$val),"score"]
hydro.score.sum$ModHigh.score=stg.score[match(hydro.score.sum$ModHigh,stg.score$val),"score"]
hydro.score.sum$ModLow.score=stg.score[match(hydro.score.sum$ModLow,stg.score$val),"score"]
hydro.score.sum$ExLow.score=stg.score[match(hydro.score.sum$ExLow,stg.score$val),"score"]

rec.score=data.frame(val=10:1,score=c(1.0,0.9,0.7,0.4,rep(0,6)))
hydro.score.sum$rec.score=rec.score[match(hydro.score.sum$sum.recevent,rec.score$val),"score"]

hydro.score.sum$S_ExHigh=hydro.score.sum$ExHigh.score*5
hydro.score.sum$S_ModHigh=hydro.score.sum$ModHigh.score*5
hydro.score.sum$S_ModLow=hydro.score.sum$ModLow.score*4
hydro.score.sum$S_ExLow=hydro.score.sum$ExLow.score*4
hydro.score.sum$S_rec=hydro.score.sum$rec.score*3

vars=c("S_ExHigh", "S_ModHigh", "S_ModLow", "S_ExLow", "S_rec")
hydro.score.sum$f.score=rowSums(hydro.score.sum[,vars])/21

# Regulation Schedules ----------------------------------------------------
zones=c(paste("LOK",paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),sep="-"),
        paste("LOK",paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_"),sep="_"))
zone.alias=data.frame(zone=zones,
                      zone2=c(paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_")))
n.alts=length(alts)
reg.sch=data.frame()
for(j in 1:3){
  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(zones)){
    paths=paste0("/RSMBN/",zones[i],"/STAGE/01JAN1965 - 01JAN2016/1DAY/INPUT/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$zone=zones[i]
    tmp$Alt=alts[j]
    reg.sch=rbind(tmp,reg.sch)
    print(i)
  }
}

zones=c(paste("LOK",paste("ZONE",c("A","BC","D1","D2"),sep="_"),sep="-"),
        paste("LOK",paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_"),sep="_"))
zone.alias=rbind(zone.alias,
                 data.frame(
                   zone=c("LOK-ZONE_BC","LOWSM_D3_15_LEVEL"),
                   zone2=c("ZONE_BC","LOWSM_D3_15_LEVEL")))
for(j in 4:5){
  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(zones)){
    paths=paste0("/RSMBN/",zones[i],"/STAGE/01JAN1965 - 01JAN2016/1DAY/INPUT/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$zone=zones[i]
    tmp$Alt=alts[j]
    reg.sch=rbind(tmp,reg.sch)
    print(i)
  }
}

reg.sch$DOY=as.numeric(format(reg.sch$Date,"%j"))
reg.sch=merge(reg.sch,zone.alias,"zone")
reg.sch2=reshape2::dcast(reg.sch,Alt+DOY~zone2,value.var = "STAGE",mean)
# reg.sch2$ZONE_D1=with(reg.sch2,ifelse(Alt=="260467"&ZONE_D1<=ZONE_D2,ZONE_D2,ZONE_D1))

head(lakeO.stage)
reg.sch3=reshape2::dcast(reg.sch,Alt+Date~zone2,value.var = "STAGE",mean)

vars=c("Alt","Date","STAGE")
lakeO.stage.reg=merge(lakeO.stage[,vars],
                      reg.sch3,c("Alt","Date"))


lakeO.stage.reg1=subset(lakeO.stage.reg,!(Alt%in%c("PA22","PA25")))
lakeO.stage.reg1$above.ZoneD=with(lakeO.stage.reg1,ifelse(STAGE>ZONE_C,1,0))
lakeO.stage.reg1$within.ZoneD=with(lakeO.stage.reg1,ifelse(STAGE<ZONE_C&STAGE>ZONE_D0,1,0))
lakeO.stage.reg1$below.ZoneD=with(lakeO.stage.reg1,ifelse(STAGE<ZONE_D0,1,0))

lakeO.stage.reg2=subset(lakeO.stage.reg,Alt%in%c("PA22","PA25"))
lakeO.stage.reg2$above.ZoneD=with(lakeO.stage.reg2,ifelse(STAGE>ZONE_BC,1,0))
lakeO.stage.reg2$within.ZoneD=with(lakeO.stage.reg2,ifelse(STAGE<ZONE_BC&STAGE>LOWSM_15_LEVEL,1,0))
lakeO.stage.reg2$below.ZoneD=with(lakeO.stage.reg2,ifelse(STAGE<LOWSM_15_LEVEL,1,0))
# lakeO.stage.reg2$WSM.buf=with(lakeO.stage.reg2,ifelse(STAGE<(LOWSM_15_LEVEL+1.5)&STAGE>LOWSM_15_LEVEL,1,0))

lakeO.stage.reg3=rbind(lakeO.stage.reg1,lakeO.stage.reg2)

zone.freq1=ddply(subset(lakeO.stage.reg,!(Alt%in%c("PA22","PA25"))),"Alt",summarise,
                 above.zoneD=sum(STAGE>ZONE_C),
                 zoneD=sum(ifelse(STAGE<ZONE_C&STAGE>ZONE_D0,1,0)),
                 below.zoneD=sum(STAGE<ZONE_D0))
zone.freq2=ddply(subset(lakeO.stage.reg,Alt%in%c("PA22","PA25")),"Alt",summarise,
                 above.zoneD=sum(STAGE>ZONE_BC),
                 zoneD=sum(ifelse(STAGE<ZONE_BC&STAGE>LOWSM_15_LEVEL,1,0)),
                 below.zoneD=sum(STAGE<LOWSM_15_LEVEL))
zone.freq=rbind(zone.freq1,zone.freq2)
zone.freq=zone.freq[match(alts.sort2,zone.freq$Alt),]
zone.freq[,2:4]=(zone.freq[,2:4]/18993)*100
zone.freq.all=zone.freq

ddply(subset(lakeO.stage.reg,Alt%in%c("PA22","PA25")),"Alt",summarise,
      ZoneD3=sum(ifelse(STAGE<ZONE_D2&STAGE>LOWSM_15_LEVEL,1,0),na.rm=T)/18993,
      WSM.buff=sum(ifelse(STAGE<(LOWSM_15_LEVEL+1.5)&STAGE>LOWSM_15_LEVEL,1,0),na.rm=T)/18993)


# png(filename=paste0(plot.path,"Iteration3_Final/TSP_REGSCH.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,1))

xlim.val.date=date.fun(c("1965-01-01","1965-12-31"));xmaj.dat=seq(xlim.val.date[1],xlim.val.date[2],"2 month");xmin.dat=seq(xlim.val.date[1],xlim.val.date[2],"1 month")
xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.dat,"%j"));xmin=as.numeric(format(xmin.dat,"%j"));
ylim.val=c(10,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/0.5)

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="NA25f"),lines(DOY,ZONE_A,col="red",lwd=2))
with(subset(reg.sch2,Alt=="NA25f"),lines(DOY,ZONE_B,col="deepskyblue2",lwd=2))
with(subset(reg.sch2,Alt=="NA25f"),lines(DOY,ZONE_C,col="orange",lwd=2))
with(subset(reg.sch2,Alt=="NA25f"),lines(DOY,ZONE_D1,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="NA25f"),lines(DOY,ZONE_D2,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="NA25f"),lines(DOY,ZONE_D3,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="NA25f"),lines(DOY,ZONE_D0,col="grey",lwd=2))
with(subset(reg.sch2,Alt=="NA25f"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))
with(subset(reg.sch2,Alt=="NA25f"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25f"&DOY==90),text(85,ZONE_B,"Zone B",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25f"&DOY==105),text(105,ZONE_C,"Zone C",pos=3,col="orange",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25f"&DOY==30),text(30,ZONE_D0+(ZONE_C-ZONE_D0)/2,"Zone D",col="grey",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25f"&DOY==30),text(30,LOWSM_15_LEVEL+(13-LOWSM_15_LEVEL)/2,"Zone F",col="purple",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="NA25f"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

mid.x=244;mx.x=15;mx.x2=9
min.y=subset(reg.sch2,Alt=="NA25f"&DOY==mid.x)$ZONE_D0
max.y=17.25;#subset(reg.sch2,Alt=="NA25f"&DOY==mid.x)$ZONE_A
h.val=0.75
xx=c(mid.x,mid.x-mx.x,mid.x-mx.x2,mid.x-mx.x2,mid.x-mx.x,
     mid.x,mid.x+mx.x,mid.x+mx.x2,mid.x+mx.x2,mid.x+mx.x,mid.x)
yy=c(min.y,min.y+h.val,min.y+h.val,
     max.y-h.val,max.y-h.val,max.y,max.y-h.val,max.y-h.val,
     min.y+h.val,min.y+h.val,min.y)
polygon(xx,yy,col=adjustcolor("deeppink",0.5),border="grey")
text(mid.x,min.y+(max.y-min.y)/2,"Flows South",srt=90,cex=0.8,offset=0)

mtext(side=1,line=2,"Date (Month)")
mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"LORS08")

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_A,col="red",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_BC,col="deepskyblue2",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D1,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D2,col="grey",lwd=2))
# with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D3,col="grey",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))

with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==90),text(85,ZONE_BC,"Zone BC",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(85,LOWSM_15_LEVEL+(ZONE_BC-LOWSM_15_LEVEL)/2,"Zone D",col="grey",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

mid.x=244;mx.x=15;mx.x2=9
min.y=subset(reg.sch2,Alt=="PA25"&DOY==mid.x)$LOWSM_15_LEVEL
max.y=18;#subset(reg.sch2,Alt=="PA25"&DOY==mid.x)$ZONE_A
h.val=0.75
xx=c(mid.x,mid.x-mx.x,mid.x-mx.x2,mid.x-mx.x2,mid.x-mx.x,
     mid.x,mid.x+mx.x,mid.x+mx.x2,mid.x+mx.x2,mid.x+mx.x,mid.x)
yy=c(min.y,min.y+h.val,min.y+h.val,
     max.y-h.val,max.y-h.val,max.y,max.y-h.val,max.y-h.val,
     min.y+h.val,min.y+h.val,min.y)
polygon(xx,yy,col=adjustcolor("deeppink",0.5),border="grey")
text(mid.x,min.y+(max.y-min.y)/2,"Flows South",srt=90,cex=0.8,offset=0)

mtext(side=1,line=2,"Date (Month)")
# mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Preferred Alternative (LOSOM)")
dev.off()

sch.LORS=read.csv("C:/Julian_LaCie/_Github/CRE_Conditions/Data/LORS.csv")
sch.LORS$Date=with(sch.LORS,date.fun(paste("1900",Month,Day,sep="-")))
txt.cex=0.75
title.cex=0.75
# png(filename=paste0(plot.path,"Iteration3_Final/LORS_TSP.png"),width=7,height=3,units="in",res=200,type="windows",bg="white")
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
zone.freq2=t(zone.freq[4:5,2:4])[3:1,]
x=barplot(zone.freq2,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,2))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(zone.freq2,beside=F,ylim=ylim.val,col=adjustcolor(cols4,0.5),axes=F,ann=F,names.arg=rep(NA,nrow(zone.freq)),add=T)
text(x,zone.freq2[1,]/2,format(round(zone.freq2[1,],1)),xpd=NA,font=2)
text(x,zone.freq2[1,]+zone.freq2[2,]/2,format(round(zone.freq2[2,],1)),xpd=NA,font=2)
text(x,colSums(zone.freq2[1:2,])+(zone.freq2[3,])/2,format(round(zone.freq2[3,],1)),xpd=NA,font=2)
axis_fun(1,x,x,c("NA25\n(FWO)","PA25\n(LOSOM)"),line=0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Percent of Days\nDuring Period of Simulation",cex=0.9)
mtext(side=1,line=2.75,"Model Index")

par(mar=c(2,3,0.5,1))
xlim.val.date=date.fun(c("1965-01-01","1965-12-31"));xmaj.dat=seq(xlim.val.date[1],xlim.val.date[2],"2 month");xmin.dat=seq(xlim.val.date[1],xlim.val.date[2],"1 month")
xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.dat,"%j"));xmin=as.numeric(format(xmin.dat,"%j"));
ylim.val=c(10,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
xx=with(subset(reg.sch2,Alt=="PA25"),c(DOY,rev(DOY)))
with(subset(reg.sch2,Alt=="PA25"),polygon(x=xx,c(rep(19,length(DOY)),rev(ZONE_BC)),col=adjustcolor(cols4[3],0.5),border=NA))
with(subset(reg.sch2,Alt=="PA25"),polygon(x=xx,c(ZONE_BC,rev(LOWSM_15_LEVEL)),col=adjustcolor(cols4[2],0.5),border=NA))
with(subset(reg.sch2,Alt=="PA25"),polygon(x=xx,c(LOWSM_15_LEVEL,rep(9,length(DOY))),col=adjustcolor(cols4[1],0.5),border=NA))

with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_A,col="black",lwd=1.25))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_BC,col="black",lwd=1.25))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D1,col="grey40",lwd=1.25,lty=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D2,col="grey40",lwd=1.25))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,LOWSM_15_LEVEL,col="black",lwd=1.25))
with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="black",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==90),text(85,ZONE_BC,"Zone BC",pos=3,col="black",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(85,LOWSM_15_LEVEL+(ZONE_BC-LOWSM_15_LEVEL)/2,"Zone D",col="black",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="black",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=2,line=2.25,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"LOSOM (2022)",cex=title.cex)
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Final/LOSOM.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(10,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/0.5)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_A,col="red",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_BC,col="deepskyblue2",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D1,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D2,col="grey",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))

with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==90),text(85,ZONE_BC,"Zone BC",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(85,LOWSM_15_LEVEL+(ZONE_BC-LOWSM_15_LEVEL)/2,"Zone D",col="grey",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

mtext(side=1,line=2,"Date (Month)")
mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Preferred Alternative (LOSOM)")
dev.off()


WSM.tx=data.frame()
for(i in seq(1,366,5)){
  tmp=with(subset(reg.sch2,Alt=="PA25")[i,],seq(LOWSM_15_LEVEL+0.25,(LOWSM_15_LEVEL+1.5)-0.25,length.out=4))
  WSM.tx=rbind(WSM.tx,data.frame(DOY=i,WSM.buf=tmp))
}

# png(filename=paste0(plot.path,"Iteration3_Final/LOSOM_buff.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(10,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/0.5)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_A,col="red",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_BC,col="deepskyblue2",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D1,col="grey",lwd=2,lty=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_D2,col="grey",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),
     polygon(c(DOY,rev(DOY)),
             c(LOWSM_15_LEVEL,rev(LOWSM_15_LEVEL+1.5)),
             col=adjustcolor("indianred1",0.5)))

with(subset(reg.sch2,Alt=="PA25"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==90),text(85,ZONE_BC,"Zone BC",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(85,LOWSM_15_LEVEL+(ZONE_BC-LOWSM_15_LEVEL)/2,"Zone D",col="grey",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
with(WSM.tx,text(DOY,WSM.buf,"?",cex=0.6,col="white"))
with(subset(reg.sch2,Alt=="PA25"&DOY==30),text(40,
                                               LOWSM_15_LEVEL+((LOWSM_15_LEVEL+1.5)-LOWSM_15_LEVEL)/2,
                                               "WSM 1.5 Ft\nBuffer",cex=0.75,font=2))

axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

mtext(side=1,line=2,"Date (Month)")
mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Preferred Alternative (LOSOM + WSM Buffer)")
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Final/LOK_Stage_RegSch.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

# cols=c("orange","grey","purple")
cols4=wesanderson::wes_palette("Zissou1", 3, type = "continuous")
zone.freq2=t(zone.freq[,2:4])
x=barplot(zone.freq2,beside=F,ylim=ylim.val,
          col=NA,border=NA,axes=F,ann=F,
          # space=c(0,0.2,0,0.2,0),
          names.arg=rep(NA,length(alts.sort)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(zone.freq2,beside=F,ylim=ylim.val,col=adjustcolor(cols4,0.5),
        axes=F,ann=F,
        # space=c(0,0.2,0,0.2,0),
        names.arg=rep(NA,length(alts.sort)),add=T)
text(x,zone.freq2[1,]/2,format(round(zone.freq2[1,],1)),xpd=NA,font=2)
text(x,zone.freq2[1,]+zone.freq2[2,]/2,format(round(zone.freq2[2,],1)),xpd=NA,font=2)
text(x,colSums(zone.freq2[1:2,])+(zone.freq2[3,])/2,format(round(zone.freq2[3,],1)),xpd=NA,font=2)
axis_fun(1,x,x,alts.sort2)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Percent of Days\nDuring Period of Simulation")
mtext(side=1,line=1.75,"Alternative")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("Above Zone D","Within Zone D","Below Zone D"),
       pch=c(22,22,22),
       lty=0,lwd=0.01,
       col="black",
       pt.bg=adjustcolor(rev(cols4),0.5),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=1,line=-1,adj=1,"Simulation Period of Record\n(1965-2016; 18993 days)",cex=0.75)
dev.off()



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
# q.dat.xtab.zones.melt2=subset(q.dat.xtab.zones.melt,Alt%in%c("NA25",'260467'))


vars=c("above.ZoneD.CRE","within.ZoneD.CRE","below.ZoneD.CRE")
density.group.all=data.frame()

for(j in 1:length(alts)){
for(i in 1:3){
  tmp=subset(q.dat.xtab.zones.melt,Alt==alts[j])
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
  dens$Alt=alts[j]
  density.group.all=rbind(density.group.all,dens)
}
print(j)
}

plot(y~x,density.group.all,log="x")


# png(filename=paste0(plot.path,"Iteration3_Final/TSP_S79Q_zones.png"),width=8,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1,0.5,0.5),oma=c(2.5,2.5,1,0.5));
layout(matrix(c(1:5),1,5,byrow=F))

ylim.val=c(0,1.75);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(10,20000);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
for(i in 1:length(alts.sort2)){
tmp=subset(density.group.all,Alt==alts.sort2[i])
  plot(y~x,tmp,log="x",xlim=xlim.val,ylim=ylim.val,yaxs="i",type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(tmp,group=="below.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[1],lty=1,col.adj = 0.5))
with(subset(tmp,group=="within.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[2],lty=1,col.adj = 0.5))
with(subset(tmp,group=="above.ZoneD.CRE"),shaded.range(x,rep(-1,length(x)),y,bg=cols4[3],lty=1,col.adj = 0.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
if(i==1){mtext(side=2,line=2,"Density")}
mtext(side=3,adj=0,alts.sort2[i])
if(i==1){
    legend("topleft",legend=c("Above Zone D","Within Zone D","Below Zone D"),
         pch=c(22,22,22),
         lty=0,lwd=0.01,
         col=rev(cols4),
         pt.bg=adjustcolor(rev(cols4),0.5),
         pt.cex=1,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
}
}
mtext(side=1,line=1,outer=T,"S-79 Daily Discharge (ft\u00B3 sec\u207B\u00B9)")
dev.off()



# Area/Volume -------------------------------------------------------------
# load("C:/Julian_LaCie/_Github/LakeO_Sediment/Export/LOK_stgvol_mod.Rdata")

PA25.WSM.vol=subset(reg.sch2,Alt=="PA25")
PA25.WSM.vol=cbind(PA25.WSM.vol,
                   data.frame(WSM.Vol.km3=predict(stg.vol,
                                                  newdata=data.frame(z=ft.to.m(PA25.WSM.vol$LOWSM_15_LEVEL)))))
PA25.WSM.vol=cbind(PA25.WSM.vol,
                   data.frame(WSM.buf.Vol.km3=predict(stg.vol,
                                                  newdata=data.frame(z=ft.to.m(PA25.WSM.vol$LOWSM_15_LEVEL+1.5)))))
range(((PA25.WSM.vol$WSM.buf.Vol.km3-PA25.WSM.vol$WSM.Vol.km3)*810714)/1000)



# WSM.ft=ft.to.m(subset(reg.sch2,Alt=="PA25")$LOWSM_15_LEVEL)
# -0.480+(WSM.ft*0.554)+(WSM.ft*0.124)^2

# load("C:/Julian_LaCie/_Github/LakeO_Sediment/Export/LOK_stgvol_ft_mod.Rdata")
predict(stg.vol.ft,data.frame(z=12.15))

z.val=subset(reg.sch2,Alt=="PA25")$LOWSM_15_LEVEL
wsm.vol=predict(stg.vol.ft,data.frame(z=z.val))
wsm.buf.vol=predict(stg.vol.ft,data.frame(z=z.val+1.5))
range((wsm.buf.vol-wsm.vol)/1000)
