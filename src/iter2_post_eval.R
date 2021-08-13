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
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
alts=alts[!alts%in%c("_Batch_Results","Northern_Estuaries","Iteration2_STL_Flows_13Jul2021.xlsx")]
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2","SR3.5")
cols=c("grey50","grey80",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-3,"continuous")),"deeppink")

cols=cols[alts.sort%in%c("NA25","ECBr","CC")]

# Discharge ---------------------------------------------------------------
RSM.sites=c("S79","S80","S80_QPFCSOURCE_LAKE","S79_QPFCSOURCE_LAKE",
            "S77","S308","S77_QFC","S308_QFC",
            "TMC2EST","S48","S49","NSF2EST")
q.dat=data.frame()
alts=c("NA25","ECBr","CC")
alts.sort=c("NA25","ECBr","CC")
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
q.dat$month=as.numeric(format(q.dat$Date,"%m"))

q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+CY~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
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

## SLE
sle.gw=openxlsx::read.xlsx(paste0(data.path,"Iteration_2/Model_Output/Iteration2_STL_Flows_13Jul2021.xlsx"))
sle.gw$Date=with(sle.gw,date.fun(paste(year,month,day,sep="-")))
sle.gw=sle.gw[,c("Date","sle_gw")]
q.dat.xtab=merge(q.dat.xtab,sle.gw,"Date")
q.dat.xtab=q.dat.xtab[order(q.dat.xtab$Alt,q.dat.xtab$Date),]

q.dat.xtab$SLE.S80trib=rowSums(q.dat.xtab[,c("S80","TMC2EST","S48","S49","NSF2EST","sle_gw")],na.rm=T)
q.dat.xtab$SLE.S80trib.14d=with(q.dat.xtab,ave(SLE.S80trib,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$SLE.S80_gw=rowSums(q.dat.xtab[,c("S80","sle_gw")],na.rm=T)
q.dat.xtab$SLE.S80_gw.14d=with(q.dat.xtab,ave(SLE.S80_gw,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80_QPFCSOURCE_LAKE.14d=with(q.dat.xtab,ave(S80_QPFCSOURCE_LAKE,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$SLE.low=with(q.dat.xtab,ifelse(SLE.S80trib.14d<150,1,0)) # RECOVER Low
q.dat.xtab$SLE.opt=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=150&SLE.S80trib.14d<1400,1,0)) # RECOVER Optimum
q.dat.xtab$SLE.high=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1400&SLE.S80trib.14d<1700,1,0)) # RECOVER stress
q.dat.xtab$SLE.high_1400=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1400,1,0)) 
q.dat.xtab$SLE.S80.high=with(q.dat.xtab,ifelse(SLE.S80_gw.14d>=1400&SLE.S80_gw.14d<1700,1,0)) 
q.dat.xtab$SLE.S80.high2=with(q.dat.xtab,ifelse(SLE.S80_gw.14d>=1400,1,0)) # RECOVER stress
q.dat.xtab$SLE.dam=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1700,1,0)) # RECOVER damaging
q.dat.xtab$SLE.high1=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1700&SLE.S80trib.14d<4000,1,0))
q.dat.xtab$SLE.high2=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=4000,1,0))


##
q.dat.xtab$CRE.low.count=0
q.dat.xtab$CRE.low1.count=0
q.dat.xtab$CRE.low2.count=0
q.dat.xtab$CRE.opt.count=0
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

q.dat.xtab$SLE.low.count=0
q.dat.xtab$SLE.opt.count=0
q.dat.xtab$SLE.high.count=0
q.dat.xtab$SLE.high_1400.count=0
q.dat.xtab$SLE.S80.high.count=0
q.dat.xtab$SLE.S80.high.LOK.count=0
q.dat.xtab$SLE.S80.high.count2=0
q.dat.xtab$SLE.S80.high.LOK.count2=0

q.dat.xtab$SLE.high1.count=0
q.dat.xtab$SLE.high2.count=0
q.dat.xtab$SLE.high.LOK.count=0
q.dat.xtab$SLE.high.basin.count=0
q.dat.xtab$SLE.dam.count=0
q.dat.xtab$SLE.dam.LOK.count=0
q.dat.xtab$SLE.dam.basin.count=0


q.dat.xtab2=data.frame()

for(j in 1:length(alts.sort)){
  tmp=subset(q.dat.xtab,Alt==alts.sort[j])
  for(i in 14:nrow(tmp)){
    ## CRE
    tmp$CRE.low.count[i]=with(tmp,ifelse(CRE.low[i]==1&sum(CRE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$CRE.low1.count[i]=with(tmp,ifelse(CRE.low1[i]==1&sum(CRE.low1.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$CRE.low2.count[i]=with(tmp,ifelse(CRE.low2[i]==1&sum(CRE.low2.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$CRE.opt.count[i]=with(tmp,ifelse(CRE.opt[i]==1&sum(CRE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))
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
    
    ## SLE
    tmp$SLE.low.count[i]=with(tmp,ifelse(SLE.low[i]==1&sum(SLE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.opt.count[i]=with(tmp,ifelse(SLE.opt[i]==1&sum(SLE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.high.count[i]=with(tmp,ifelse(SLE.high[i]==1&sum(SLE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high_1400.count[i]=with(tmp,ifelse(SLE.high_1400[i]==1&sum(SLE.high_1400.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high.LOK.count[i]=with(tmp,
                                   ifelse(SLE.high.count[i]==1,
                                          ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=1400,1,0),0))
    ##
    tmp$SLE.S80.high.count[i]=with(tmp,ifelse(SLE.S80.high[i]==1&sum(SLE.S80.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.S80.high.LOK.count[i]=with(tmp,
                                   ifelse(SLE.S80.high.count[i]==1,
                                          ifelse((SLE.S80_gw.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=1400,1,0),0))
    ##
    tmp$SLE.S80.high.count2[i]=with(tmp,ifelse(SLE.S80.high2[i]==1&sum(SLE.S80.high.count2[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.S80.high.LOK.count2[i]=with(tmp,
                                       ifelse(SLE.S80.high.count2[i]==1,
                                              ifelse((SLE.S80_gw.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=1400,1,0),0))
    
    tmp$SLE.high.basin.count[i]=with(tmp,SLE.high.count[i]-SLE.high.LOK.count[i])
    tmp$SLE.dam.count[i]=with(tmp,ifelse(SLE.dam[i]==1&sum(SLE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.dam.LOK.count[i]=with(tmp,
                                  ifelse(SLE.dam.count[i]==1,
                                         ifelse((SLE.S80trib.14d[i]-S80_QPFCSOURCE_LAKE.14d[i])<=1700,1,0),0))
    tmp$SLE.dam.basin.count[i]=with(tmp,SLE.dam.count[i]-SLE.dam.LOK.count[i])
    tmp$SLE.high1.count[i]=with(tmp,ifelse(SLE.high1[i]==1&sum(SLE.high1.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high2.count[i]=with(tmp,ifelse(SLE.high2[i]==1&sum(SLE.high2.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  }
  q.dat.xtab2=rbind(q.dat.xtab2,tmp)
  print(j)
}
q.dat.xtab2

# Verify FoEvergaldes analysis
# q.dat.xtab$Period.14D=as.numeric(format(q.dat.xtab2$Date,"%j"))%/%14L+1L

# nrow(ddply(subset(q.dat.xtab,Alt=="CC"),c("Date"),summarise,N.val=N.obs(S80)))
# nrow(ddply(subset(q.dat.xtab,Alt=="CC"),c("CY","Period.14D"),summarise,N.val=N.obs(S80)))
## 
# S80
#Daily Count
ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(S80>=1400,na.rm=T))
ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(ifelse(S80>=1400&S80<1700,1,0),na.rm=T))
ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(ifelse(SLE.S80trib>=1400&SLE.S80trib<1700,1,0),na.rm=T))
ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(SLE.S80trib>=1400,na.rm=T))
ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(ifelse(SLE.S80_gw.14d>=1400&SLE.S80_gw.14d<1700,1,0),na.rm=T))
ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(SLE.S80_gw.14d>=1400,na.rm=T))

# 14 Day Count
vars.SLE=c("SLE.high_1400.count","SLE.high.count","SLE.S80.high.count","SLE.S80.high.count2","SLE.S80.high.LOK.count","SLE.S80.high.LOK.count2")
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars.SLE)],id.vars = "Alt")

S80.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
S80.SalEnv_count=S80.SalEnv_count[match(alts.sort,S80.SalEnv_count$Alt),]
S80.SalEnv_count
# test=ddply(q.dat.xtab,c("Alt","CY","Period.14D"),summarise,S80.stress=sum(S80.14d>=1400,na.rm=T))
# ddply(test,"Alt",summarise,N.val=sum(S80.stress>0,na.rm=T))
# test=ddply(q.dat.xtab,c("Alt","CY","Period.14D"),summarise,S80.stress=sum(SLE.S80trib.14d>=1400,na.rm=T))
# ddply(test,"Alt",summarise,N.val=sum(S80.stress>0,na.rm=T))
# 
# ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(S80.14d>=1400,na.rm=T))
# ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(ifelse(S80.14d>=1400&S80.14d<1700,1,0),na.rm=T))
# ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(SLE.S80trib.14d>=1400,na.rm=T))
# ddply(q.dat.xtab,"Alt",summarise,S80.stress=sum(ifelse(SLE.S80trib.14d>=1400&SLE.S80trib.14d<1700,1,0),na.rm=T))


# Month Count
q.dat.xtab$CY=as.numeric(format(q.dat.xtab$Date,"%Y"))
q.dat.xtab$month=as.numeric(format(q.dat.xtab$Date,"%m"))
S80.monthlyQ=ddply(q.dat.xtab,c("Alt","CY","month"),summarise,
                   S80.month=mean(S80,na.rm=T),
                   S80trib.month=mean(SLE.S80trib,na.rm=T))

ddply(S80.monthlyQ,"Alt",summarise,N.val.S80=sum(S80.month>=1400,na.rm=T))
ddply(S80.monthlyQ,"Alt",summarise,N.val=sum(ifelse(S80.month>=1400&S80.month<1700,1,0),na.rm=T))
ddply(S80.monthlyQ,"Alt",summarise,N.val=sum(S80trib.month>1400,na.rm=T))

# S79
#Daily Count (5515 from Table)
ddply(q.dat.xtab,"Alt",summarise,S79.stress=sum(S79>=2100,na.rm=T))

# 14 Day Count (550 from Table)
vars.CRE=c("CRE.high.count","CRE.high.LOK.count","CRE.high_2100.count")
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars.CRE)],id.vars = "Alt")
S79.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
S79.SalEnv_count=S79.SalEnv_count[match(alts.sort,S79.SalEnv_count$Alt),]
S79.SalEnv_count

# tmp=seq(min(q.dat.xtab$Date),max(q.dat.xtab$Date),"1 days")
# n=15
# Period.14day=list(rep(1:length(tmp)%/%n+1),each=n,len=length(tmp))
# rslt=data.frame(Date=tmp,P14D=Period.14day[[1]])
# 
# q.dat.xtab=merge(q.dat.xtab,rslt,"Date")
# 
# test=ddply(q.dat.xtab,c("Alt","CY","P14D"),summarise,S79.mean=mean(S79,na.rm=T))
# ddply(test,"Alt",summarise,N.val=sum(S79.mean>=2100,na.rm=T))
# ddply(test,"Alt",summarise,N.val=sum(ifelse(S79.mean>=2100&S79.mean<2600,1,0),na.rm=T))
# 
# test=ddply(q.dat.xtab,c("Alt","CY","Period.14D"),summarise,S79.mean=mean(S79,na.rm=T))
# ddply(test,"Alt",summarise,N.val=sum(S79.mean>2100,na.rm=T))
# ddply(test,"Alt",summarise,N.val=sum(ifelse(S79.mean>=2100&S79.mean<2600,1,0),na.rm=T))
# 
# test=ddply(q.dat.xtab,c("Alt","CY","Period.14D"),summarise,S79.stress=sum(S79.14d>=2100,na.rm=T))
# ddply(test,"Alt",summarise,N.val=sum(S79.stress>0,na.rm=T))
# 
# ddply(q.dat.xtab,"Alt",summarise,S79.stress=sum(S79.14d>=2100,na.rm=T))

# Month Count (232 from Table)
S79.monthlyQ=ddply(q.dat.xtab,c("Alt","CY","month"),summarise,
                   S79.month=mean(S79,na.rm=T))

ddply(S79.monthlyQ,"Alt",summarise,N.val=sum(S79.month>2100,na.rm=T))

ddply(S79.monthlyQ,"Alt",summarise,N.val=sum(ifelse(S79.month>=2100&S79.month<2600,1,0),na.rm=T))


# table -------------------------------------------------------------------

ann.Q=ddply(q.dat.xtab,c("Alt","CY"),summarise,S77Q=sum(cfs.to.acftd(S77_QFC)),S308Q=sum(cfs.to.acftd(S308_QFC)))
mean.ann.Q=ddply(ann.Q,"Alt",summarise,S77.kacft=mean(S77Q/1000),S308.kacft=mean(S308Q/1000))
mean.ann.Q=mean.ann.Q[match(alts.sort,mean.ann.Q$Alt),]

vars=c(paste0("CRE.",c("low.count","opt.count","high.LOK.count","high.basin.count","dam.LOK.count","dam.basin.count")),
       paste0("SLE.",c("low.count","opt.count","high.LOK.count","high.basin.count","dam.LOK.count","dam.basin.count")))

tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars)],id.vars = "Alt")
SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
SalEnv_count=SalEnv_count[match(alts.sort,SalEnv_count$Alt),]
SalEnv_count

SalEnv_count$PerFWO_CRE.high.LOK=with(SalEnv_count,((CRE.high.LOK.count-CRE.high.LOK.count[1])/CRE.high.LOK.count[1])*100)
SalEnv_count$PerFWO_CRE.high.basin=with(SalEnv_count,((CRE.high.basin.count-CRE.high.basin.count[1])/CRE.high.basin.count[1])*100)
SalEnv_count$PerFWO_CRE.dam.LOK=with(SalEnv_count,((CRE.dam.LOK.count-CRE.dam.LOK.count[1])/CRE.dam.LOK.count[1])*100)
SalEnv_count$PerFWO_CRE.dam.basin=with(SalEnv_count,((CRE.dam.basin.count-CRE.dam.basin.count[1])/CRE.dam.basin.count[1])*100)

SalEnv_count$PerFWO_SLE.high.LOK=with(SalEnv_count,((SLE.high.LOK.count-SLE.high.LOK.count[1])/SLE.high.LOK.count[1])*100)
SalEnv_count$PerFWO_SLE.high.basin=with(SalEnv_count,((SLE.high.basin.count-SLE.high.basin.count[1])/SLE.high.basin.count[1])*100)
SalEnv_count$PerFWO_SLE.dam.LOK=with(SalEnv_count,((SLE.dam.LOK.count-SLE.dam.LOK.count[1])/SLE.dam.LOK.count[1])*100)
SalEnv_count$PerFWO_SLE.dam.basin=with(SalEnv_count,((SLE.dam.basin.count-SLE.dam.basin.count[1])/SLE.dam.basin.count[1])*100)


SalEnv_count.melt=reshape2::melt(SalEnv_count[,1:13],id.vars = "Alt")
spl=strsplit(as.character(SalEnv_count.melt$variable),split="\\.")
sep.vals=data.frame(variable=as.character(SalEnv_count.melt$variable),
                    region=sapply(spl,"[",1),
                    cat=sapply(spl,"[",2),
                    source=sapply(spl,"[",3))
SalEnv_count.melt=merge(SalEnv_count.melt,sep.vals,"variable")
SalEnv_count.melt$source[SalEnv_count.melt$source=="count"]="all"

tmp=subset(SalEnv_count.melt,Alt%in%c("NA25","ECBr","CC")&cat%in%c("high","dam"))
tmp$cat_source=with(tmp,paste(cat,source,sep="_"))

tmp2=reshape2::dcast(tmp,region+Alt~cat_source,value.var = "value",mean)
tmp3=subset(tmp2,region=="CRE")
tmp3=merge(tmp3,subset(mean.ann.Q,Alt%in%c("NA25","ECBr","CC"))[,c("Alt",'S77.kacft')],"Alt")
tmp3=rename(tmp3,c("S77.kacft"="RegFlow"))
tmp3=tmp3[match(tmp3$Alt,c("NA25","ECBr","CC")),]
tmp3$PerFWO_dam_basin=with(tmp3,(dam_basin-dam_basin[1])/dam_basin[1]*100)
tmp3$PerFWO_dam_LOK=with(tmp3,(dam_LOK-dam_LOK[1])/dam_LOK[1]*100)
tmp3$PerFWO_high_basin=with(tmp3,(high_basin-high_basin[1])/high_basin[1]*100)
tmp3$PerFWO_high_LOK=with(tmp3,(high_LOK-high_LOK[1])/high_LOK[1]*100)
tmp3$PerFWO_RegFlow=with(tmp3,(RegFlow-RegFlow[1])/RegFlow[1]*100)

tmp4=subset(tmp2,region=="SLE")
tmp4=merge(tmp4,subset(mean.ann.Q,Alt%in%c("NA25","ECBr","CC"))[,c("Alt",'S308.kacft')],"Alt")
tmp4=rename(tmp4,c("S308.kacft"="RegFlow"))
tmp4=tmp4[match(tmp4$Alt,c("NA25","ECBr","CC")),]
tmp4$PerFWO_dam_basin=with(tmp4,(dam_basin-dam_basin[1])/dam_basin[1]*100)
tmp4$PerFWO_dam_LOK=with(tmp4,(dam_LOK-dam_LOK[1])/dam_LOK[1]*100)
tmp4$PerFWO_high_basin=with(tmp4,(high_basin-high_basin[1])/high_basin[1]*100)
tmp4$PerFWO_high_LOK=with(tmp4,(high_LOK-high_LOK[1])/high_LOK[1]*100)
tmp4$PerFWO_RegFlow=with(tmp4,(RegFlow-RegFlow[1])/RegFlow[1]*100)

vars.sort=c("region","Alt","RegFlow","high_LOK","high_basin","dam_LOK","dam_basin",
            paste("PerFWO",c("RegFlow","high_LOK","high_basin","dam_LOK","dam_basin"),sep="_"))
tb.dat=rbind(tmp3[,vars.sort],tmp4[,vars.sort])
tb.dat[tb.dat==0]=NA
# write.csv(tb.dat,paste0(export.path,"post_iter2_estsum.csv"),row.names = F)

tb.dat%>%
  flextable()%>%
  colformat_double(j=3,digits=0)%>%
  colformat_double(j=8:12,digits=1,na_str = "---")%>%
  merge_v(j=1)%>%
  fix_border_issues()%>%
  valign(j=1,valign="top")%>%
  vline(j=7)%>%
  hline(i=3)%>%
  set_header_labels(
    "region"="Estuary",
    "RegFlow"="Regulatory Flows (kacft/yr)",
    "high_LOK"="Stress Events From LOK",
    "high_basin"="Stress Events From Basin",
    "dam_LOK"="Damaging Events From LOK",
    "dam_basin"="Damaging Events From Basin",
    "PerFWO_RegFlow"="Regulatory Flows (kacft/yr)",
    "PerFWO_high_LOK"="Stress Events From LOK",
    "PerFWO_high_basin"="Stress Events From Basin",
    "PerFWO_dam_LOK"="Damaging Events From LOK",
    "PerFWO_dam_basin"="Damaging Events From Basin")%>%
  add_header(
    "RegFlow"="Summarized Data",
   "high_LOK"="Summarized Data",
   "high_basin"="Summarized Data",
   "dam_LOK"="Summarized Data",
   "dam_basin"="Summarized Data",
   "PerFWO_RegFlow"="Percent Different from FWO",
   "PerFWO_high_LOK"="Percent Different from FWO",
   "PerFWO_high_basin"="Percent Different from FWO",
   "PerFWO_dam_LOK"="Percent Different from FWO",
   "PerFWO_dam_basin"="Percent Different from FWO")%>%
  merge_h(part="header")%>%
  align(align="center",part="header")%>%
  padding(padding=1,part="all")%>%
  fontsize(size=9,part="body")%>%
  fontsize(size=10,part="header")%>%
  align(j=3:12,align="center",part="body")%>%
  bg(i=c(1,4),j=8:12,bg="grey")%>%
  bg(i=~PerFWO_RegFlow<0,j=8,bg="lightgreen")%>%bg(i=~PerFWO_RegFlow>0,j=8,bg="tomato")%>%
  bg(i=~PerFWO_high_LOK<0,j=9,bg="lightgreen")%>%bg(i=~PerFWO_high_LOK>0,j=9,bg="tomato")%>%
  bg(i=~PerFWO_high_basin<0,j=10,bg="lightgreen")%>%bg(i=~PerFWO_high_basin>0,j=10,bg="tomato")%>%
  bg(i=~PerFWO_dam_LOK<0,j=11,bg="lightgreen")%>%bg(i=~PerFWO_dam_LOK>0,j=11,bg="tomato")%>%
  bg(i=~PerFWO_dam_basin<0,j=12,bg="lightgreen")%>%bg(i=~PerFWO_dam_basin>0,j=12,bg="tomato")%>%
  width(width=c(0.5,0.5,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75))%>%
  footnote(i=c(1,4),j=1,value=as_paragraph("CRE: Caloosahatchee Estuary; SLE: St Lucie Estuary"),ref_symbols=" 1 ")%>%
  footnote(i=c(1,4),j=2,value=as_paragraph("NA25 = Future without project (FWO)"),ref_symbols=" 2 ")%>%
  footnote(j=c(4,5,9,10),part="header",value=as_paragraph("Stressful Flows:\nCRE: \u2265 2100 cfs & < 2600 cfs\nSLE: \u2265 1400 cfs & < 1700 cfs"),ref_symbols=" 3 ")%>%
  footnote(j=c(6,7,11,12),part="header",value=as_paragraph("Damaging Flows:\nCRE: > 2600 cfs\nSLE:> 1700 cfs"),ref_symbols=" 4 ")%>%
  font(fontname="Times New Roman",part="all")
#%>%print("docx")
  

# Figures -----------------------------------------------------------------
vars=c(paste0("CRE.",c("low.count","opt.count","high.LOK.count","dam.LOK.count")),
       paste0("SLE.",c("low.count","opt.count","high.LOK.count","dam.LOK.count")))

tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars)],id.vars = "Alt")
SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
SalEnv_count=SalEnv_count[match(alts.sort,SalEnv_count$Alt),]
SalEnv_count

SalEnv_count.melt=reshape2::melt(SalEnv_count,id.vars = "Alt")
spl=strsplit(as.character(SalEnv_count.melt$variable),split="\\.")
spl
sep.vals=data.frame(variable=as.character(SalEnv_count.melt$variable),
                 region=sapply(spl,"[",1),
                 cat=sapply(spl,"[",2))
SalEnv_count.melt=merge(SalEnv_count.melt,sep.vals,"variable")
# CC_salenv=reshape2::dcast(subset(SalEnv_count.melt,Alt=='CC'),cat~region,value.var = "value",sum)
# CC_salenv$cat=as.factor(CC_salenv$cat)
# CC_salenv$cat=factor(CC_salenv$cat,levels=c("low","opt","high","dam"))
# barplot(t(CC_salenv[,2:3]),beside=T)

IMC.cols=c(rgb(0,255,0,maxColorValue =255),
           rgb(255,255,0,maxColorValue =255),
           rgb(186,85,211,maxColorValue =255),
           rgb(100,149,237,maxColorValue =255))

FWO_salenv=reshape2::dcast(subset(SalEnv_count.melt,Alt=='NA25'),region~cat,value.var = "value",mean)
FWO_salenv=FWO_salenv[,c("region","low","opt","high","dam")]

CC_salenv=reshape2::dcast(subset(SalEnv_count.melt,Alt=='CC'),region~cat,value.var = "value",mean)
CC_salenv=CC_salenv[,c("region","low","opt","high","dam")]

ylim.val=c(0,1200);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("Low","Optimal","Stress\n(LOK)","Damaging\n(LOK)")
# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_SLE_flow_envelope.png"),width=7,height=6,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1,2,3,3),2,2,byrow=F),widths=c(1,0.5))
par(family="serif",mar=c(2,3,0.25,1),oma=c(2,1,2,0.25),lwd=0.5);

tmp=t(FWO_salenv[,2:5])
x=barplot(tmp,beside=T,col=IMC.cols,ylim=ylim.val,axes=F,ann=F)
text(x[,1],tmp[,1],tmp[,1],pos=3)
text(x[,2],tmp[,2],tmp[,2],pos=3)
axis_fun(1,x,x,NA)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
abline(v=5.5)
text(min(x[,1])+(max(x[,1])-min(x[,1]))/2,ylim.val[2],"CRE",pos=1)
text(min(x[,2])+(max(x[,2])-min(x[,2]))/2,ylim.val[2],"SLE",pos=1)
mtext(side=3,adj=0,"Alternative: NA25 (FWO)")

tmp=t(CC_salenv[,2:5])
x=barplot(tmp,beside=T,col=IMC.cols,ylim=ylim.val,axes=F,ann=F)
text(x[,1],tmp[,1],tmp[,1],pos=3)
text(x[,2],tmp[,2],tmp[,2],pos=3)
axis_fun(1,x,x,rep(xlabs,2),line=-1.25,padj=1,cex=0.70)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
abline(v=5.5)
mtext(side=3,adj=0,"Alternative: CC")
mtext(side=2,outer=T,line=-0.5,"Count of Criteria Met")

axis(side=1,at=c(min(x[,1])+(max(x[,1])-min(x[,1]))/2,min(x[,2])+(max(x[,2])-min(x[,2]))/2),
     labels=c("CRE","SLE"),line=1.5,lty=0)

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
mtext(side=3,line=-3,adj=0,"Number of 14-Day\nPeriods during the\nperiod of simulation\n(1965-2016)")

legend(-0.25,0.85,legend=c("Low (< 750 cfs)",
                        "Optimal\n(\u2265 750 cfs & < 2100 cfs)",
                        "Stress - LOK Regulatory\n(\u2265 2100 cfs & < 2600 cfs)",
                        "Damaging - LOK Regulatory\n(\u2265 2600 cfs)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=IMC.cols,
       pt.cex=1.5,ncol=1,cex=0.85,bty="n",y.intersp=1.4,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title="CRE Salinity\nEnvelope Criteria")

legend(-0.25,0.35,legend=c("Low (< 150 cfs)",
                             "Optimal\n(\u2265 150 cfs & < 1400 cfs)",
                             "Stress - LOK Regulatory\n(\u2265 1400 cfs & < 1700 cfs)",
                             "Damaging - LOK Regulatory\n(\u2265 1700 cfs)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=IMC.cols,
       pt.cex=1.5,ncol=1,cex=0.85,bty="n",y.intersp=1.4,x.intersp=0.75,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title="SLE+Tributaries Salinity\nEnvelope Criteria")
dev.off()

SalEnv_count.melt.comp=reshape2::dcast(SalEnv_count.melt,region+cat~Alt,value.var="value",mean)
SalEnv_count.melt.comp$FWO_CC=with(SalEnv_count.melt.comp,((CC-NA25)/NA25)*100)
tmp=reshape2::dcast(SalEnv_count.melt.comp,region~cat,value.var = "FWO_CC",mean)
tmp=tmp[,c("region","low","opt","high","dam")]

# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_SLE_flow_envelope_FWO.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,3,0.25,1),oma=c(2,1,2,0.25),lwd=0.5);

ylim.val=c(-110,100);by.y=50;ymaj=seq(max(c(-100,ylim.val[1])),ylim.val[2],by.y);ymin=seq(max(c(-100,ylim.val[1])),ylim.val[2],by.y/2)
tmp=t(tmp[,2:5])
x=barplot(tmp,beside=T,col=IMC.cols,ylim=ylim.val,axes=F,ann=F)
abline(v=5.5)
abline(h=0)
text(x[,1],tmp[,1],format(round(tmp[,1],1),nsmall=1),pos=ifelse(tmp[,1]<0,1,3))
text(x[,2],tmp[,2],format(round(tmp[,2],1),nsmall=1),pos=ifelse(tmp[,2]<0,1,3))
axis_fun(1,x,x,rep(xlabs,2),line=-1.25,padj=1,cex=0.70)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Percent Difference to FWO")
mtext(side=3,adj=0,"Alternative: CC")

text(min(x[,1])+(max(x[,1])-min(x[,1]))/2,ylim.val[2],"CRE",pos=1)
text(min(x[,2])+(max(x[,2])-min(x[,2]))/2,ylim.val[2],"SLE",pos=1)
axis(side=1,at=c(min(x[,1])+(max(x[,1])-min(x[,1]))/2,min(x[,2])+(max(x[,2])-min(x[,2]))/2),
     labels=c("CRE","SLE"),line=1.5,lty=0)
dev.off()


##### RECOVER plots
vars=c("CRE.low.count","CRE.opt.count", 
       "CRE.high.basin.count", "CRE.high.LOK.count", 
       "CRE.dam.basin.count","CRE.dam.LOK.count",
       "CRE.low1.count","CRE.low2.count","CRE.high.count",
       "CRE.high1.count","CRE.high2.count","CRE.high3.count","CRE.dam.count")
# apply(tmp[,vars],2,FUN=function(x)sum(x,na.rm=T))
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars)],id.vars = "Alt")
CRE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
CRE.SalEnv_count=CRE.SalEnv_count[match(alts.sort,CRE.SalEnv_count$Alt),]
# write.csv(CRE.SalEnv_count,paste0(export.path,"CRE_SalEnv_count.csv"),row.names=F)

CRE.SalEnv_count$perFWO.opt=with(CRE.SalEnv_count,(CRE.opt.count-CRE.opt.count[1])/CRE.opt.count[1])*100
CRE.SalEnv_count$perFWO.stress=with(CRE.SalEnv_count,(CRE.high.count-CRE.high.count[1])/CRE.high.count[1])*100
CRE.SalEnv_count$perFWO.dam=with(CRE.SalEnv_count,(CRE.dam.count-CRE.dam.count[1])/CRE.dam.count[1])*100
CRE.SalEnv_count$perFWO_2600_4500=with(CRE.SalEnv_count,(CRE.high1.count-CRE.high1.count[1])/CRE.high1.count[1])*100
CRE.SalEnv_count$perFWO_4500_6500=with(CRE.SalEnv_count,(CRE.high2.count-CRE.high2.count[1])/CRE.high2.count[1])*100
CRE.SalEnv_count$perFWO_6500=with(CRE.SalEnv_count,(CRE.high3.count-CRE.high3.count[1])/CRE.high3.count[1])*100
CRE.SalEnv_count[,c('Alt',"perFWO.opt","perFWO.stress","perFWO.dam","perFWO_2600_4500","perFWO_4500_6500","perFWO_6500")]

vars=c("CRE.low1.count","CRE.low2.count","CRE.opt.count","CRE.high.count","CRE.high1.count","CRE.high2.count","CRE.high3.count")

CRE.SalEnv_count=CRE.SalEnv_count[,c("Alt",vars)]
labs=c("<457","457 - 750","750 - 2100","2100 - 2600","2600 - 4500","4500 - 6500",">6500")
# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_RECOVER_SalEnv_total.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:8),2,4,byrow=T))
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,2,3,0.25),lwd=0.5);

ymax=c(600,600,1000,400,400,200,100)
yval=ymax/2
for(i in 2:8){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(CRE.SalEnv_count[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(5:8)){axis_fun(1,x,x,alts.sort,cex=0.8,las=2)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  mtext(side=3,adj=0,paste(labs[i-1],"cfs"),cex=0.75)
  text(x,CRE.SalEnv_count[,i],round(CRE.SalEnv_count[,i],0),font=2,col="black",pos=1,cex=0.4)
}
plot(0:1,0:1,ann=F,axes=F,type="n")
text(1,0.15,"S79 Discharge\n \nPeriod of Simulation\n CY1965 - 2016.",adj=1)
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"Count of 14-Day Periods")
mtext(side=3,adj=0,outer=T,line=1.5,"Caloosahatchee Estuary - Salinity Envelope")
dev.off()


##### Lake Discharges
q.dat.xtab$S79_GT2100=with(q.dat.xtab,ifelse(S79>=2100,1,0))
q.dat.xtab$S80_GT1400=with(q.dat.xtab,ifelse(S80>=1400,1,0))

CRE.GT2100_annual=ddply(q.dat.xtab,c("CY","Alt"),summarise,Q.Lake.GT2100=sum(cfs.to.acftd(S79_QPFCSOURCE_LAKE[S79_GT2100==1])/1000,na.rm=T))
CRE.GT2100_annual.mean=ddply(CRE.GT2100_annual,"Alt",summarise,mean.val=mean(Q.Lake.GT2100))
CRE.GT2100_annual.mean=CRE.GT2100_annual.mean[match(alts.sort,CRE.GT2100_annual.mean$Alt),]
CRE.GT2100_annual.mean

SLE.GT1400_annual=ddply(q.dat.xtab,c("CY","Alt"),summarise,Q.Lake.GT1400=sum(cfs.to.acftd(S80_QPFCSOURCE_LAKE[S80_GT1400==1])/1000,na.rm=T))
SLE.GT1400_annual.mean=ddply(SLE.GT1400_annual,"Alt",summarise,mean.val=mean(Q.Lake.GT1400))
SLE.GT1400_annual.mean=SLE.GT1400_annual.mean[match(alts.sort,SLE.GT1400_annual.mean$Alt),]
SLE.GT1400_annual.mean


# png(filename=paste0(plot.path,"Post-Iteration_2/Lakedischarge_Annualmean.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:2),1,2,byrow=T))
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);

ylim.val=c(0,600);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(CRE.GT2100_annual.mean$mean.val,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(CRE.GT2100_annual.mean$mean.val,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"Mean Annual Discharge\n>2100 cfs at S-79",cex=0.75)
text(x,CRE.GT2100_annual.mean$mean.val,round(CRE.GT2100_annual.mean$mean.val,0),col="black",pos=1,cex=1)
# mtext(side=3,adj=1,"CY1965 - 2016",cex=0.75)
mtext(side=1,line=2.5,"Alternative")
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
mtext(side=3,adj=0,"Mean Annual Discharge\n>1400 cfs at S-80",cex=0.75)
mtext(side=3,line=-1.25,adj=0," SLE")
text(x,SLE.GT1400_annual.mean$mean.val,round(SLE.GT1400_annual.mean$mean.val,0),col="black",pos=1,cex=1)
mtext(side=3,adj=1,"CY1965 - 2016",cex=0.75)
mtext(side=1,line=2.5,"Alternative")
dev.off()

##### Flood Control 
n.alts=length(alts.sort)
RSM.sites=c("S351_QFC","S351_FC_SHIFT2_ENVTARG","S354_QFC","S354_FC_SHIFT2_ENVTARG",
            "S77_QFC","S308_QFC","C10A_QFC")
regq.dat=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
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

tmp=regq.dat.CY.mean[,c("WCAs","Cal","StL","LWLagoon")]
rownames(tmp)<-alts.sort
cols2=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))

ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Post-Iteration_2/AvgFloodControl.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),heights=c(1,0.4))

x=barplot(t(tmp),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,3))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp),beside=F,col=cols2,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,3),add=T)
with(regq.dat.CY.mean,text(x,WCAs/2,round(WCAs,0),cex=0.75,col="white"))
with(regq.dat.CY.mean,text(x,WCAs+(((Cal+WCAs)-WCAs)/2),round(regq.dat.CY.mean$Cal,0),cex=0.75))
with(regq.dat.CY.mean,text(x,(WCAs+Cal)+(((Cal+WCAs+StL)-(Cal+WCAs))/2),round(regq.dat.CY.mean$StL,0),cex=0.75))
with(regq.dat.CY.mean,text(x,Cal+WCAs+StL+LWLagoon,round(regq.dat.CY.mean$LWLagoon,0),pos=3,offset=0.1,cex=0.75))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,line=-0.5);box(lwd=1)
mtext(side=2,line=3,"Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=1.75,"Alternatives")

# par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0,0.75,legend=c("Water Conservation Areas","Caloosahatchee River","St. Lucie River","Lake Worth Lagoon"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols2,
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1)
text(1,0.2,"Iteration 2 results. Mean annual flood control releases\nfrom Lake Okeechobee for the 52 year (1965 - 2016)\nsimulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()
