## 
## LOSOM
##
## Iteration 3 - Phase 1
## Sensitivity Analysis
## Technical eval
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
alt.xwalk=read.csv(paste0(data.path,"Iteration_3_Tech_Discussion/Alt_crosswalk.csv"))
alt.xwalk=subset(alt.xwalk,DSS.alts!="")

alts.iter2=c("NA25","CC")

# Removed CCTSP and NA25
# CCsimp4 is after simp4S1 (the second CCsimp4)
alts=alt.xwalk[!(alt.xwalk$DSS.alts%in%c("CCTSP","NA25","CC")),"DSS.alts"]

alts.sort=alt.xwalk$DSS.alts
alts.sort=alts.sort[!(alts.sort%in%c("CCTSP","NA25","CC"))]
alts.sort=c("NA25","CC",alts.sort)
# alts.sort=c(alts.iter2,alts)       
# Discharge ---------------------------------------------------------------
RSM.sites=c("S79","S80","S80_QPFCSOURCE_LAKE","S79_QPFCSOURCE_LAKE",
            "S77_QFC","S308_QFC",
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

n.alts=length(alts)
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3_Tech_Discussion/DSSfiles/",alts[j],"/RSMBN_output.dss"))  
  
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


# head(q.dat,20)
# q.dat=q.dat[order(q.dat$Alt,q.dat$SITE,q.dat$Date),]
# q.dat$WY=WY(q.dat$Date)
q.dat$CY=as.numeric(format(q.dat$Date,"%Y"))
q.dat$month=as.numeric(format(q.dat$Date,"%m"))

q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+CY+month~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
head(q.dat.xtab)
q.dat.xtab$S79.14d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80.14d=with(q.dat.xtab,ave(S80,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))


q.dat.xtab.mon=reshape2::dcast(q.dat.xtab,Alt+CY~month,value.var ="S79",mean)
head(q.dat.xtab.mon)

# Export for Stakeholders -------------------------------------------------
vars=c("Alt","Date","S79","S79.14d")
# write.csv(q.dat.xtab[,vars],paste0(export.path,"Iteration3/S79Q/0_Iter3_Phase1_S79Daily_All.csv"),row.names=F)
# 
# for(i in 1:length(alts.sort)){
#   tmp=subset(q.dat.xtab.mon,Alt==alts.sort[i])
#   write.csv(tmp,paste0(export.path,"Iteration3/S79Q/",i,"_Iter3_Phase1_S79Month_",alts.sort[i],".csv"),row.names=F)
#   print(i)
# }
# 
# meta.data.export=data.frame(filename=c("0_Iter3_Phase1_S79Daily_All.csv",
#                                        paste0(1:length(alts.sort),"_Iter3_Phase1_S79Month_",alts.sort,".csv")),
#                             Descript=c("Daily S79 and 14d moving average for all alternatives in cfs",
#                                        paste0("monthly mean S79 discharges (cfs) for alt", alts.sort)))
# meta.data.export$PMgroup=c(NA,alt.xwalk[alt.xwalk$DSS.alts!="CCTSP",]$PM.grp)
# meta.data.export$PMgroup[1:3]=NA
# write.csv(meta.data.export,paste0(export.path,"Iteration3/S79Q/00_Iter3_Phase1_metadata.csv"),row.names=F)

# Single xlsx file
# wb=createWorkbook()
# 
# meta.data=data.frame(Sheet=c("PMgroup","S79Daily","AltName"),
#                      Descrtip=c("The performance measure group as used in data summaries provided by USACE and SFWMD",
#                                 "Daily S79 and 14d moving average for all alternatives in cfs",
#                                 "Monthly mean discharge (in cfs) for each alternative"))
# addWorksheet(wb,sheet="README")
# writeData(wb,sheet="README",meta.data)
# 
# tmp=alt.xwalk[alt.xwalk$DSS.alts!="CCTSP",2:3]
# colnames(tmp)=c("Alt","Group")
# addWorksheet(wb,sheet="PMgroup")
# writeData(wb,sheet="PMgroup",tmp)
# 
# addWorksheet(wb,sheet="S79Daily")
# writeData(wb,sheet="S79Daily",q.dat.xtab[,vars])
# 
# for(i in 1:length(alts.sort)){
#   tmp=subset(q.dat.xtab.mon,Alt==alts.sort[i])
#   addWorksheet(wb,sheet=alts.sort[i])
#   writeData(wb,sheet=alts.sort[i],tmp)
#   print(i)
# }
# saveWorkbook(wb, paste0(export.path,"Iteration3/S79Q/Iter3_Phase1_S79Q.xlsx"), overwrite = TRUE)

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
vars.CRE=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high3.count","high3.LOK.count"),sep=".")
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars.CRE)],id.vars = "Alt")
CRE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
CRE.SalEnv_count=CRE.SalEnv_count[match(alts.sort,CRE.SalEnv_count$Alt),]

vars.SLE=paste("SLE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high2.count"),sep=".")
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars.SLE)],id.vars = "Alt")
SLE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)

barplot(CRE.SalEnv_count$CRE.high3.LOK.count)


##

vars.CRE=paste("CRE",c(
               paste("low",c("LOK.count","basin.count"),sep="."),
               paste("opt",c("LOK.count","basin.count"),sep="."),
               paste("high",c("LOK.count","basin.count"),sep="."),
               paste("dam",c("LOK.count","basin.count"),sep="."),
               paste("high3",c("LOK.count","basin.count"),sep=".")),sep=".")
cats.source=data.frame(variable=vars.CRE)
tmp.val=strsplit(as.character(cats.source$variable),"\\.")
cats.source$flow.cat=sapply(tmp.val,"[",2)
cats.source$source=sapply(tmp.val,"[",3)

q.dat.CRE.cat=reshape2::melt(q.dat.xtab2[,c("Alt",vars.CRE)],id.vars = "Alt")
head(q.dat.CRE.cat)
q.dat.CRE.cat=merge(q.dat.CRE.cat,cats.source,"variable")
CRE.SalEnv_count2=reshape2::dcast(q.dat.CRE.cat,Alt+flow.cat~source,value.var = "value",sum)
CRE.SalEnv_count2$flow.cat=factor(CRE.SalEnv_count2$flow.cat,levels=c("low","opt","high","dam","high3"))

CRE.SalEnv_FWO=reshape2::dcast(CRE.SalEnv_count2,Alt~flow.cat,value.var = "LOK",sum)
CRE.SalEnv_FWO=CRE.SalEnv_FWO[match(alts.sort,CRE.SalEnv_FWO$Alt),]
for(i in 2:ncol(CRE.SalEnv_FWO)){
  tmp=data.frame(val=((CRE.SalEnv_FWO[,i]-CRE.SalEnv_FWO[1,i])/CRE.SalEnv_FWO[1,i])*100)
  colnames(tmp)=paste0(names(CRE.SalEnv_FWO)[i],".FWO")
  CRE.SalEnv_FWO=cbind(CRE.SalEnv_FWO,tmp)
}
subset(CRE.SalEnv_FWO,Alt=='CC')
subset(CRE.SalEnv_FWO,Alt=='CCR1')

cols=viridis::magma(2)
cat.val=c("low","opt","high","dam","high3")
CRE.labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress (2100 - 2600 cfs)","Damaging (>2600 cfs)","Extreme (>6500 cfs)")
# png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_RECOVER_SalEnv_LOKBasin2.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
# png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_RECOVER_SalEnv_LOKBasin.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:5),5,1,byrow=T))
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,2,1,0.5),lwd=0.5);

ymax=c(1200,1200,500,500,100)
yval=ymax/2

for(i in 1:5){
  ylim.val=c(0,ymax[i]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i]);ymin=seq(ylim.val[1],ylim.val[2],yval[i])
  tmp=subset(CRE.SalEnv_count2,flow.cat==cat.val[i])
  tmp=tmp[match(alts.sort,tmp$Alt),]
  tmp=t(tmp[,c("LOK","basin")])
  x=barplot(tmp,col=adjustcolor(cols,0.5),
            ylim=ylim.val,space=c(0,0),xaxs="i",
            axes=F,ann=F,names.arg = rep(NA,ncol(tmp)))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i!=5){axis_fun(1,x,x,NA)}else{axis_fun(1,x,x,alts.sort,las=2,line=-0.25,cex=0.8)}
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i],cex=0.7)
  if(rowSums(tmp)[2]==0){
    text(x,colSums(tmp),round(colSums(tmp),0),font=2,col="black",pos=3,cex=0.5,offset=0.25)
    }else{
      text(x,colSums(tmp),round(colSums(tmp),0),font=2,col="black",pos=3,cex=0.5,offset=0.25)
      text(x,colSums(tmp),tmp[2,],font=2,col="black",pos=1,cex=0.5,offset=0.25);#Basin
      text(x,tmp[1,],tmp[1,],font=2,col="white",pos=1,cex=0.5,offset=0.25);#LOK
  }
  
  if(i==1){
    # mtext(side=3,adj=0,line=-1.25," CRE")
    mtext(side=3,adj=1,"Caloosahatchee")
    
    legend("topleft",legend=c("Lake","Basin"),
           pch=c(22),lwd=c(0.1,0.1),lty=0,
           pt.bg=adjustcolor(cols,0.5),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
    
    }
}
mtext(side=2,line=0.5,outer=T,"Count of 14-Day Periods")
mtext(side=1,line=5,outer=T,"Alternative")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_RECOVER_SalEnv_LOKBasin_FWO2.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
# png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_RECOVER_SalEnv_LOKBasin_FWO.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:5),5,1,byrow=T))
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,2,1,0.5),lwd=0.5);

for(i in 1:5){
tmp=subset(CRE.SalEnv_count2,flow.cat==cat.val[i])
tmp=tmp[match(alts.sort,tmp$Alt),]
tmp$TCount=rowSums(tmp[,c("basin","LOK")])
tmp$basin.FWO=with(tmp,((basin-basin[1])/basin[1])*100)
tmp$basin.FWO=with(tmp,ifelse(is.nan(basin.FWO)==T,0,basin.FWO))
tmp$LOK.FWO=with(tmp,(LOK-LOK[1])/LOK[1])*100
tmp$FWO.diff=with(tmp,(TCount-TCount[1])/TCount[1])*100
tmp$Alt.plot=1:nrow(tmp)

cols=viridis::magma(3)

# rng.val=range(tmp[,c("LOK.FWO","basin.FWO","FWO.diff")],na.rm=T)
# ylim.val=c(round(floor(rng.val[1]+rng.val[1]*0.25),-1),ceiling(rng.val[2]+rng.val[2]*0.25))
ymin=c(-100,-100,-100,-100,-100)
ymax=c(10,300,100,10,100)
yval=c(50,100,50,50,50)

ylim.val=c(ymin[i],ymax[i]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i]);ymin=seq(ylim.val[1],ylim.val[2],yval[i])
xlim.val=c(1,nrow(tmp));xmaj=seq(xlim.val[1],xlim.val[2],1);xmin=seq(xlim.val[1],xlim.val[2],1)

shift.val=0.25
plot(FWO.diff~Alt.plot,tmp,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=c(0,xmaj)+0.5,lty=1,col=adjustcolor("grey",0.5))
abline(h=0)
with(tmp,segments(Alt.plot-shift.val,rep(0,nrow(tmp)),Alt.plot-shift.val,LOK.FWO,lwd=2,col=adjustcolor("black",0.5)))
with(tmp,points(Alt.plot-shift.val,LOK.FWO,pch=21,bg=adjustcolor(cols[1],1),lwd=0.1,cex=1.25))
with(tmp,segments(Alt.plot,rep(0,nrow(tmp)),Alt.plot,FWO.diff,lwd=2,col=adjustcolor("black",0.5)))
with(tmp,points(Alt.plot,FWO.diff,pch=21,bg=adjustcolor(cols[2],1),lwd=0.1,cex=1.25))
with(tmp,segments(Alt.plot+shift.val,rep(0,nrow(tmp)),Alt.plot+shift.val,basin.FWO,lwd=2,col=adjustcolor("black",0.5)))
with(tmp,points(Alt.plot+shift.val,basin.FWO,pch=21,bg=adjustcolor(cols[3],1),lwd=0.1,cex=1.25))
axis_fun(2,ymaj,ymin,ymaj)
if(i!=5){axis_fun(1,xmaj,xmaj,NA)}else{axis_fun(1,xmaj,xmaj,alts.sort,las=2,line=-0.25,cex=0.8)}
box(lwd=1)
mtext(side=3,adj=0,CRE.labs[i],cex=0.7)
if(i==1){
  # mtext(side=3,adj=0,line=-1.25," CRE")
  mtext(side=3,adj=1,"Caloosahatchee")
  
  legend("bottomleft",legend=c("Lake","Total","Basin"),
         pch=c(21),lwd=c(0.1),lty=0,
         pt.bg=cols,pt.cex=1.5,ncol=3,
         cex=0.75,bty="n",y.intersp=1,
         x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
}
}
mtext(side=2,line=0.5,outer=T,"Average Percent Difference to FWO")
mtext(side=1,line=5,outer=T,"Alternative")
dev.off()

# gc()
vars.SLE=paste("SLE",c(
  paste("low",c("LOK.count","basin.count"),sep="."),
  paste("opt",c("LOK.count","basin.count"),sep="."),
  paste("high",c("LOK.count","basin.count"),sep="."),
  paste("dam",c("LOK.count","basin.count"),sep="."),
  paste("high2",c("LOK.count","basin.count"),sep=".")),sep=".")
cats.source=data.frame(variable=vars.SLE)
tmp.val=strsplit(as.character(cats.source$variable),"\\.")
cats.source$flow.cat=sapply(tmp.val,"[",2)
cats.source$source=sapply(tmp.val,"[",3)

q.dat.SLE.cat=reshape2::melt(q.dat.xtab2[,c("Alt",vars.SLE)],id.vars = "Alt")
head(q.dat.SLE.cat)
q.dat.SLE.cat=merge(q.dat.SLE.cat,cats.source,"variable")
SLE.SalEnv_count2=reshape2::dcast(q.dat.SLE.cat,Alt+flow.cat~source,value.var = "value",sum)
SLE.SalEnv_count2$flow.cat=factor(SLE.SalEnv_count2$flow.cat,levels=c("low","opt","high","dam","high2"))


SLE.SalEnv_FWO=reshape2::dcast(SLE.SalEnv_count2,Alt~flow.cat,value.var = "LOK",sum)
SLE.SalEnv_FWO=SLE.SalEnv_FWO[match(alts.sort,SLE.SalEnv_FWO$Alt),]
for(i in 2:ncol(SLE.SalEnv_FWO)){
  tmp=data.frame(val=((SLE.SalEnv_FWO[,i]-SLE.SalEnv_FWO[1,i])/SLE.SalEnv_FWO[1,i])*100)
  colnames(tmp)=paste0(names(SLE.SalEnv_FWO)[i],".FWO")
  SLE.SalEnv_FWO=cbind(SLE.SalEnv_FWO,tmp)
}
subset(SLE.SalEnv_FWO,Alt%in%c("NA25","CC","CCR1"))

cols=viridis::magma(2)
cat.val=c("low","opt","high","dam","high2")
SLE.labs=c("Low (<150 cfs)","Optimum (150 - 1400 cfs)","Stress (1400 - 1700 cfs)","Damaging (>1700 cfs)","Extreme (>4000 cfs)")
# png(filename=paste0(plot.path,"Iteration3_TechEval/SLE_RECOVER_SalEnv_LOKBasin2.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
# png(filename=paste0(plot.path,"Iteration3_TechEval/SLE_RECOVER_SalEnv_LOKBasin.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:5),5,1,byrow=T))
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,2,1,0.5),lwd=0.5);

ymax=c(300,1500,500,800,300)
yval=ymax/2

for(i in 1:5){
  ylim.val=c(0,ymax[i]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i]);ymin=seq(ylim.val[1],ylim.val[2],yval[i])
  tmp=subset(SLE.SalEnv_count2,flow.cat==cat.val[i])
  tmp=tmp[match(alts.sort,tmp$Alt),]
  tmp=t(tmp[,c("LOK","basin")])
  x=barplot(tmp,col=adjustcolor(cols,0.5),
            ylim=ylim.val,space=c(0,0),xaxs="i",
            axes=F,ann=F,names.arg = rep(NA,ncol(tmp)))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i!=5){axis_fun(1,x,x,NA)}else{axis_fun(1,x,x,alts.sort,las=2,line=-0.25,cex=0.8)}
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i],cex=0.7)
  if(rowSums(tmp)[2]==0){
    text(x,colSums(tmp),round(colSums(tmp),0),font=2,col="black",pos=3,cex=0.5,offset=0.25)
  }else{
    text(x,colSums(tmp),round(colSums(tmp),0),font=2,col="black",pos=3,cex=0.5,offset=0.25)
    text(x,colSums(tmp),tmp[2,],font=2,col="black",pos=1,cex=0.5,offset=0.25);#Basin
    text(x,tmp[1,],tmp[1,],font=2,col="white",pos=1,cex=0.5,offset=0.25);#LOK
  }
  
  if(i==1){
    # mtext(side=3,adj=0,line=-1.25," CRE")
    mtext(side=3,adj=1,"St Lucie")
    
    legend("topleft",legend=c("Lake","Basin"),
           pch=c(22),lwd=c(0.1,0.1),lty=0,
           pt.bg=adjustcolor(cols,0.5),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
    
  }
}
mtext(side=2,line=0.5,outer=T,"Count of 14-Day Periods")
mtext(side=1,line=5,outer=T,"Alternative")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_TechEval/SLE_RECOVER_SalEnv_LOKBasin_FWO2.png"),width=5,height=4.5,units="in",res=200,type="windows",bg="white")
# png(filename=paste0(plot.path,"Iteration3_TechEval/SLE_RECOVER_SalEnv_LOKBasin_FWO.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:5),5,1,byrow=T))
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,2,1,0.5),lwd=0.5);

for(i in 1:5){
  tmp=subset(SLE.SalEnv_count2,flow.cat==cat.val[i])
  tmp=tmp[match(alts.sort,tmp$Alt),]
  tmp$TCount=rowSums(tmp[,c("basin","LOK")])
  tmp$basin.FWO=with(tmp,((basin-basin[1])/basin[1])*100)
  tmp$basin.FWO=with(tmp,ifelse(is.nan(basin.FWO)==T,0,basin.FWO))
  tmp$LOK.FWO=with(tmp,(LOK-LOK[1])/LOK[1])*100
  tmp$FWO.diff=with(tmp,(TCount-TCount[1])/TCount[1])*100
  tmp$Alt.plot=1:nrow(tmp)
  
  cols=viridis::magma(3)
  
  #rng.val=range(tmp[,c("LOK.FWO","basin.FWO","FWO.diff")],na.rm=T)
  # ylim.val=c(round(floor(rng.val[1]+rng.val[1]*0.25),-1),ceiling(rng.val[2]+rng.val[2]*0.25))
  ymin=c(0,-100,-100,-100,-100)
  ymax=c(100,20,50,10,100)
  yval=c(50,50,50,50,50)
  
  ylim.val=c(ymin[i],ymax[i]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i]);ymin=seq(ylim.val[1],ylim.val[2],yval[i])
  xlim.val=c(1,nrow(tmp));xmaj=seq(xlim.val[1],xlim.val[2],1);xmin=seq(xlim.val[1],xlim.val[2],1)
  
  shift.val=0.25
  plot(FWO.diff~Alt.plot,tmp,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
  abline(h=ymaj,v=c(0,xmaj)+0.5,lty=1,col=adjustcolor("grey",0.5))
  abline(h=0)
  with(tmp,segments(Alt.plot-shift.val,rep(0,nrow(tmp)),Alt.plot-shift.val,LOK.FWO,lwd=2,col=adjustcolor("black",0.5)))
  with(tmp,points(Alt.plot-shift.val,LOK.FWO,pch=21,bg=adjustcolor(cols[1],1),lwd=0.1,cex=1.25))
  with(tmp,segments(Alt.plot,rep(0,nrow(tmp)),Alt.plot,FWO.diff,lwd=2,col=adjustcolor("black",0.5)))
  with(tmp,points(Alt.plot,FWO.diff,pch=21,bg=adjustcolor(cols[2],1),lwd=0.1,cex=1.25))
  with(tmp,segments(Alt.plot+shift.val,rep(0,nrow(tmp)),Alt.plot+shift.val,basin.FWO,lwd=2,col=adjustcolor("black",0.5)))
  with(tmp,points(Alt.plot+shift.val,basin.FWO,pch=21,bg=adjustcolor(cols[3],1),lwd=0.1,cex=1.25))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i!=5){axis_fun(1,xmaj,xmaj,NA)}else{axis_fun(1,xmaj,xmaj,alts.sort,las=2,line=-0.25,cex=0.8)}
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i],cex=0.7)
  if(i==1){
    # mtext(side=3,adj=0,line=-1.25," CRE")
    mtext(side=3,adj=1,"St Lucie")
    
    legend("topleft",legend=c("Lake","Total","Basin"),
           pch=c(21),lwd=c(0.1),lty=0,
           pt.bg=cols,pt.cex=1.5,ncol=3,
           cex=0.75,bty="n",y.intersp=1,
           x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
  #print(rng.val)
}
mtext(side=2,line=0.5,outer=T,"Average Percent Difference to FWO")
mtext(side=1,line=5,outer=T,"Alternative")
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

ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500"),summarise,count.event=N.obs(sum.CRE.Q6500))
ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500.LOK"),summarise,count.event=N.obs(sum.CRE.Q6500.LOK))

# rslt.CREHighQ=reshape2::dcast(extremeQ_consec,sum.CRE.Q6500~Alt,value.var = "sum.CRE.Q6500",fun.aggregate = function(x)N.obs(x))
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

cols=c("grey20",wesanderson::wes_palette("Zissou1",length(alts.sort)-1,"continuous"))
rslt.CREHigh.sum=rslt.CREHigh.sum[,c("cat",alts.sort)]
rslt.CREHigh.LOK.sum=rslt.CREHigh.LOK.sum[,c("cat",alts.sort)]
tmp=rslt.CREHigh.LOK.sum[4,]
tmp$cat=5
tmp[,2:ncol(tmp)]=0
rslt.CREHigh.LOK.sum=rbind(rslt.CREHigh.LOK.sum,tmp)


# alts.sort2=alts.sort
# alts.sort2[alts.sort2=="CCsimp4"]="CCsimp4*"
xlabs=c("< 14", "14 - 30","30 - 60","60 - 90","> 90")
# png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_highQ_events.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,4,1,0.25),lwd=0.5);
layout(matrix(c(1:5),5,1,byrow=F))

ylim.max=c(150,20,20,10,4)
for(i in 1:nrow(rslt.CREHigh.sum)){
ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

tmp=t(rslt.CREHigh.sum[i,2:ncol(rslt.CREHigh.sum)])
x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
        ylim=ylim.val,axes=F,
        names.arg = rep(NA,length(tmp)),
        space=c(0),yaxs="i",xaxs="i")
# FWO.thresh=tmp[i,]*(1-seq(0.25,0.75,0.25))
# if(tmp[1]>0){
# abline(h=FWO.thresh,lty=2)
# text(x[1],FWO.thresh,paste0((1-seq(0.25,0.75,0.25))*100,"%"),cex=0.7,pos=3,col="white")
# }
text(x,tmp,tmp,pos=3,offset=0.1)
axis_fun(2,ymaj,ymin,format(ymaj))
if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"))
box(lwd=1)
if(i==1){mtext(side=3,adj=0,"CRE Extreme (> 6500 cfs)")}
}
mtext(side=1,line=6.5,"Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_LOK_highQ_events.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,4,1,0.25),lwd=0.5);
layout(matrix(c(1:5),5,1,byrow=F))

ylim.max=c(150,20,20,10,4)
for(i in 1:nrow(rslt.CREHigh.LOK.sum)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.CREHigh.LOK.sum[i,2:ncol(rslt.CREHigh.LOK.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  # FWO.thresh=tmp[i,]*(1-seq(0.25,0.75,0.25))
  # if(tmp[1]>0){
  # abline(h=FWO.thresh,lty=2)
  # text(x[1],FWO.thresh,paste0((1-seq(0.25,0.75,0.25))*100,"%"),cex=0.7,pos=3,col="white")
  # }
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==nrow(rslt.CREHigh.LOK.sum)){axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"))
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"CRE Extreme from LOK (> 6500 cfs)")}
}
mtext(side=1,line=6.5,"Alternatives")
dev.off()



rslt.SLEHigh.sum=rslt.SLEHigh.sum[,c("cat",alts.sort)]
rslt.SLEHigh.LOK.sum=rslt.SLEHigh.LOK.sum[,c("cat",alts.sort)]
tmp=rslt.SLEHigh.LOK.sum[1:2,]
tmp$cat=c(4,5)
tmp[,2:ncol(tmp)]=0
rslt.SLEHigh.LOK.sum=rbind(rslt.SLEHigh.LOK.sum,tmp)

# png(filename=paste0(plot.path,"Iteration3_TechEval/SLE_highQ_events.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,4,1,0.25),lwd=0.5);
layout(matrix(c(1:5),5,1,byrow=F))

ylim.max=c(650,20,10,2,10)
for(i in 1:length(xlabs)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.SLEHigh.sum[i,2:ncol(rslt.SLEHigh.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  # FWO.thresh=tmp[i,]*(1-seq(0.25,0.75,0.25))
  # if(tmp[1]>0){
  # abline(h=FWO.thresh,lty=2)
  # text(x[1],FWO.thresh,paste0((1-seq(0.25,0.75,0.25))*100,"%"),cex=0.7,pos=3,col="white")
  # }
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"))
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"SLE Extreme (> 4000 cfs)")}
}
mtext(side=1,line=6.5,"Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_TechEval/SLE_LOK_highQ_events.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,4,1,0.25),lwd=0.5);
layout(matrix(c(1:5),5,1,byrow=F))

ylim.max=c(650,20,10,2,10)
for(i in 1:length(xlabs)){
  ylim.val=c(0,ylim.max[i]);by.y=ylim.max[i]/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.SLEHigh.LOK.sum[i,2:ncol(rslt.SLEHigh.LOK.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  # FWO.thresh=tmp[i,]*(1-seq(0.25,0.75,0.25))
  # if(tmp[1]>0){
  # abline(h=FWO.thresh,lty=2)
  # text(x[1],FWO.thresh,paste0((1-seq(0.25,0.75,0.25))*100,"%"),cex=0.7,pos=3,col="white")
  # }
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"))
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"SLE Extreme from LOK (> 4000 cfs)")}
}
mtext(side=1,line=6.5,"Alternatives")
dev.off()


##
tmp=subset(q.dat.xtab,Alt==alts.sort[1])

tmp=ecdf_fun(subset(q.dat.xtab,Alt==alts.sort[1]&CRE.high3==1)$S79.14d)
tmp3=ecdf_fun(subset(q.dat.xtab,Alt==alts.sort[3]&CRE.high3==1)$S79.14d)

plot(proportion~value,tmp)
with(tmp3,lines(proportion~value,col="red"))
hist(subset(q.dat.xtab,CRE.high3==1)$S79.14d,breaks = seq(6000,22000,500))
axis_fun(1,seq(6000,22000,1000),seq(6000,22000,500),seq(6000,22000,1000))
hist(subset(q.dat.xtab,Alt==alts.sort[3]&CRE.high3==1)$S79.14d,breaks = seq(6000,22000,500))

# png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_highQ_hist.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.75,1,0.25,0.75),oma=c(3,4,1,0.25),lwd=0.5);
layout(matrix(c(1:30),6,5,byrow=F))

ylim.val=c(0,500);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(6500,22000);by.x=6000;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
bk.vals=seq(xlim.val[1],xlim.val[2],by.x/10)
for(i in 1:length(alts.sort)){
  tmp=subset(q.dat.xtab,Alt==alts.sort[i]&CRE.high3==1)$S79.14d
  mean.FWO=mean(subset(q.dat.xtab,Alt==alts.sort[1]&CRE.high3==1)$S79.14d,na.rm=T)
  hist(tmp,breaks = bk.vals,axes=F,ann=F,ylim=ylim.val,xlim=xlim.val,yaxs="i",xaxs="i")
  if(i%in%seq(6,30,6)){axis_fun(1,xmaj,xmin,xmaj,line=-0.75,cex=0.75,maj.tcl = -0.5, min.tcl = -0.25)}else{axis_fun(1,xmaj,xmin,NA,line=-0.5,maj.tcl = -0.5, min.tcl = -0.25)}
  if(i%in%seq(1,6,1)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  abline(v=mean.FWO,col=adjustcolor(cols[1],0.5),lwd=1.5,lty=2)
  abline(v=mean(tmp,na.rm=T),col=adjustcolor(cols[i],0.5),lwd=1.5,lty=2)
  mtext(side=3,adj=1,line=-1,paste0(alts.sort[i]," "),cex=0.5)
  if(i==1){mtext(side=3,adj=0,"S79 14d Moving Average >6500 cfs")}
}
mtext(side=2,line=2,outer=T,"Frequency")
mtext(side=1,line=1.5,outer=T,"Discharge (cfs)")
dev.off()

## Flows
q.dat.CY=ddply(q.dat,c("Alt","CY","SITE"),summarise,TFlow.kAcFt=sum(cfs.to.acftd(FLOW),na.rm=T)/1000)
q.dat.CY.mean=reshape2::dcast(q.dat.CY,Alt~SITE,value.var = "TFlow.kAcFt",mean)
q.dat.CY.mean=q.dat.CY.mean[match(alts.sort,q.dat.CY.mean$Alt),]
# write.csv(q.dat.CY.mean,paste0(export.path,"Iteration3/Iter3P1_AnnQ.csv"),row.names = F)


# Flows South -------------------------------------------------------------




# Lake Stage --------------------------------------------------------------

lakeO.stage=data.frame()
for(i in 1:length(alts.iter2)){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts.iter2[i],"/RSMBN_output.dss"))  
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts.iter2[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}

for(i in 1:length(alts)){
  dss_out=opendss(paste0(data.path,"Iteration_3_Tech_Discussion/DSSfiles/",alts[i],"/RSMBN_output.dss"))  
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}

lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,"%Y"))

lakeO.stage$low.stg=with(lakeO.stage,ifelse(STAGE<11,1,0))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$High.stg=with(lakeO.stage,ifelse(STAGE>16,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))
lakeO.stage$month=as.numeric(format(lakeO.stage$Date,"%m"))

# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_totalDays.png"),width=6,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,0.25,1),oma=c(1,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(1000,4000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,5000);by.x=1000;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
days.POS=ddply(lakeO.stage,"Alt",summarise,sum.low=sum(low.stg),sum.vlow=sum(vlow.stg),sum.High=sum(High.stg),sum.vHigh=sum(vHigh.stg))
# days.POS$Alt=factor(days.POS$Alt,level=alts.sort)
days.POS=days.POS[match(alts.sort,days.POS$Alt),]

plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(days.POS,points(sum.High,sum.low,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1,col=ifelse(Alt=="CC","black",adjustcolor(cols,0.5))))
text(days.POS$sum.High,days.POS$sum.low,days.POS$Alt,font=2,col=adjustcolor("black",0.5),cex=0.5,pos=4)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u003E 16 ft NGVD29")
mtext(side=2,line=2.75,"Days < 11 ft NGVD29")
mtext(side=3,adj=0, "Cumulative days over the enitre period of simulation")

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,1000);by.x=500;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# with(days.POS,points(sum.vHigh,sum.vlow,pch=19,col=adjustcolor(cols,0.5),lwd=0.1))
with(days.POS,points(sum.vHigh,sum.vlow,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1,col=ifelse(Alt=="CC","black",adjustcolor(cols,0.5))))
text(days.POS$sum.vHigh,days.POS$sum.vlow,days.POS$Alt,font=2,col=adjustcolor("black",0.5),cex=0.5,pos=4)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u2265 17 ft NGVD29")
mtext(side=2,line=2.75,"Days \u2264 10 ft NGVD29")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_MaySept_totalDays.png"),width=6,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,0.25,1),oma=c(1,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(1000,2500);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,1200);by.x=600;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
days.POS=ddply(subset(lakeO.stage,month%in%seq(5,9,1)),"Alt",summarise,sum.low=sum(low.stg),sum.vlow=sum(vlow.stg),sum.High=sum(High.stg),sum.vHigh=sum(vHigh.stg))
days.POS=days.POS[match(alts.sort,days.POS$Alt),]

plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(days.POS,points(sum.High,sum.low,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1,col=ifelse(Alt=="CC","black",adjustcolor(cols,0.5))))
text(days.POS$sum.High,days.POS$sum.low,days.POS$Alt,font=2,col=adjustcolor("black",0.5),cex=0.5,pos=4)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u003E 16 ft NGVD29")
mtext(side=2,line=2.75,"Days < 11 ft NGVD29")
mtext(side=3,adj=0, "Cumulative days over the enitre period of simulation during May - Sept")

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,100);by.x=50;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# with(days.POS,points(sum.vHigh,sum.vlow,pch=19,col=adjustcolor(cols,0.5),lwd=0.1))
with(days.POS,points(sum.vHigh,sum.vlow,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1,col=ifelse(Alt=="CC","black",adjustcolor(cols,0.5))))
text(days.POS$sum.vHigh,days.POS$sum.vlow,days.POS$Alt,font=2,col=adjustcolor("black",0.5),cex=0.5,pos=4)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u2265 17 ft NGVD29")
mtext(side=2,line=2.75,"Days \u2264 10 ft NGVD29")
dev.off()
# Ecological Env ----------------------------------------------------------
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
# sum(test.sum$env.val!=subset(env.rslt,Alt=="ECBr")$env)

env.rslt$env2=with(env.rslt,ifelse(env==2,0,1))
env.rslt=merge(env.rslt,data.frame(Alt=alts.sort,PlotOffset=rev(seq(2,10,2))),"Alt")
env.rslt$env.plt=with(env.rslt,env2+PlotOffset)
env.rslt$env.f=with(env.rslt,ifelse(env==1,"normal","recovery"))
env.rslt$Alt=factor(env.rslt$Alt,levels=alts.sort)

## Stage Envelope Plot

library(LORECOVER)
lakeO.stage$Data.Value=lakeO.stage$STAGE

norm.lakeO.stage.scr=data.frame()
for(i in 1:length(alts.sort)){
  tmp=subset(lakeO.stage,Alt==alts.sort[i])[,c("Date","Data.Value")]
  rslt=LORECOVER::norm_env(tmp)
  rslt$Alt=alts.sort[i]
  norm.lakeO.stage.scr=rbind(norm.lakeO.stage.scr,rslt)
  print(i)
}
# subset(norm.lakeO.stage.scr,Alt=="CC"&Date==date.fun("2016-12-31"))
norm.lakeO.stage.scr=rename(norm.lakeO.stage.scr,c("penalty"="norm.score"))
norm.lakeO.stage.scr$WY=WY(norm.lakeO.stage.scr$Date)
# ddply(norm.lakeO.stage.scr,c("Alt"),summarise,TScore=sum(abs(norm.score),na.rm=T))

rec.lakeO.stage.scr=data.frame()
for(i in 1:length(alts.sort)){
  tmp=subset(lakeO.stage,Alt==alts.sort[i])[,c("Date","Data.Value")]
  rslt=LORECOVER::rec_env(tmp)
  rslt$Alt=alts.sort[i]
  rec.lakeO.stage.scr=rbind(rec.lakeO.stage.scr,rslt)
  print(i)
}
rec.lakeO.stage.scr=rename(rec.lakeO.stage.scr,c("penalty"="rec.score"))
rec.lakeO.stage.scr$WY=WY(rec.lakeO.stage.scr$Date)
# ddply(rec.lakeO.stage.scr,c("Alt"),summarise,TScore=sum(abs(rec.score),na.rm=T))

lakeO.stage.scr=merge(norm.lakeO.stage.scr,rec.lakeO.stage.scr[,c("Date","Alt","rec.score")],c("Date","Alt"))
head(lakeO.stage.scr)
lakeO.stage.scr$CY=as.numeric(format(lakeO.stage.scr$Date,"%Y"))
lakeO.stage.scr$WY=WY(lakeO.stage.scr$Date)
lakeO.stage.scr=lakeO.stage.scr[order(lakeO.stage.scr$Alt,lakeO.stage.scr$Date),]

head(env.rslt)
vars=c("Alt","CY","env")
lakeO.stage.scr=merge(lakeO.stage.scr,env.rslt[,vars],c("Alt","CY"))
lakeO.stage.scr$score=with(lakeO.stage.scr,ifelse(env==1,norm.score,rec.score))
lakeO.stage.scr$Alt_CY=with(lakeO.stage.scr,paste(Alt,CY,sep="_"))
lakeO.stage.scr$cum.abs.pen=with(lakeO.stage.scr,ave(score,Alt_CY,FUN=function(x)cumsum(abs(x))))
lakeO.stage.scr$Alt=factor(lakeO.stage.scr$Alt,levels=alts.sort)

boxplot(cum.abs.pen~Alt,lakeO.stage.scr,outline=F)
stg.scr.sum=ddply(lakeO.stage.scr,"Alt",summarise,mean.val=mean(cum.abs.pen,na.rm=T))
stg.scr.sum$FWO.perdiff=with(stg.scr.sum,((mean.val-mean.val[1])/mean.val[1])*100)
stg.scr.sum

ddply(lakeO.stage.scr,"Alt",summarise,pen_above=sum(norm.score[score>0],na.rm=T))

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
env.pen.sum$total.pen_FWO=with(env.pen.sum,((total.pen-total.pen[1])/total.pen[1])*100)

lakeO.stage.scr$month=as.numeric(format(lakeO.stage.scr$Date,"%m"))
env.pen.sum.maysept=ddply(subset(lakeO.stage.scr,month%in%seq(5,9,1)),"Alt",summarise,
                          N.val=N.obs(score),
                          pen_above=sum(score[score>0],na.rm=T),
                          pen_below=sum(abs(score)[score<0],na.rm=T),
                          per_below=(sum(score<0)/N.obs(score))*100,
                          per0=(sum(score==0,na.rm=T)/N.obs(score))*100,
                          per_above=(sum(score>0)/N.obs(score))*100)
env.pen.sum.maysept$total.pen=rowSums(env.pen.sum.maysept[,c("pen_above","pen_below")])

# plot(env.pen.sum$total.pen_FWO,ylim=c(0,75))

# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_EnvScore_FWO_AllYrs.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,50);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(7,2,0.75,1),lwd=0.5);

x=barplot(env.pen.sum$total.pen_FWO,space=0,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(env.pen.sum$total.pen_FWO,space=0,ylim=ylim.val,col=adjustcolor("dodgerblue1",0.5),axes=F,ann=F,add=T)
with(env.pen.sum,text(x,total.pen_FWO,round(total.pen_FWO,0),pos=3,cex=0.8,offset = 0.1))
axis_fun(1,x,x,alts.sort,line=-0.25,las=3,cex=0.8)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Percent Difference to FWO")
mtext(side=1,line=6.5,"Alternative")
mtext(side=3,adj=0,"Total Penalty Scores - All Years")
dev.off()

cols.IMC=c(rgb(238,232,170,maxColorValue = 255),rgb(143,188,143,maxColorValue = 255))
# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_EnvScore_AllYrs.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,55000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(7,2,0.75,1),lwd=0.5);
# layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(t(env.pen.sum[,c("pen_below","pen_above")]),beside=F,space=0,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(t(env.pen.sum[,c("pen_below","pen_above")]),beside=F,space=0,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)),add=T)
with(env.pen.sum,text(x,pen_below/2,round(pen_below,0),srt=90,cex=0.8))
with(env.pen.sum,text(x,pen_below+abs((pen_below-total.pen)/2),round(pen_above,0),srt=90,cex=0.8))
with(env.pen.sum,text(x,total.pen+total.pen*0.1,round(total.pen,0),srt=90,cex=0.8))
axis_fun(1,x,x,alts.sort,line=-0.25,las=3,cex=0.8)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Stage Envelope Penalty Score")
mtext(side=1,line=6.5,"Alternative")
mtext(side=3,adj=0,"Penalty Scores - All Years")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_EnvScore_MaySep_AllYrs.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,30000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(7,2,0.75,1),lwd=0.5);
# layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(t(env.pen.sum.maysept[,c("pen_below","pen_above")]),beside=F,space=0,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(t(env.pen.sum.maysept[,c("pen_below","pen_above")]),beside=F,space=0,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)),add=T)
with(env.pen.sum.maysept,text(x,pen_below/2,round(pen_below,0),srt=90,cex=0.8))
with(env.pen.sum.maysept,text(x,pen_below+abs((pen_below-total.pen)/2),round(pen_above,0),srt=90,cex=0.8))
with(env.pen.sum.maysept,text(x,total.pen+total.pen*0.15,round(total.pen,0),srt=90,cex=0.8))
axis_fun(1,x,x,alts.sort,line=-0.25,las=3,cex=0.8)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Stage Envelope Penalty Score")
mtext(side=1,line=6.5,"Alternative")
mtext(side=3,adj=0,"Penalty Scores - May - Sep All Years")
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

tmp=subset(rslt.stg17,Alt=="CCR1")
nrow(tmp[tmp$sum.stg17!=0,])
range(tmp[tmp$sum.stg17!=0,"sum.stg17"])
hist(tmp[tmp$sum.stg17!=0,"sum.stg17"])
barplot(ddply(subset(rslt.stg17,Alt=="CCR1"&sum.stg17!=0),"cat",summarise,N.event=sum(count.event))$N.event)

tmp=subset(rslt.stg17,Alt=="CC")
nrow(tmp[tmp$sum.stg17!=0,])
range(tmp[tmp$sum.stg17!=0,"sum.stg17"])
hist(tmp[tmp$sum.stg17!=0,"sum.stg17"])
barplot(ddply(subset(rslt.stg17,Alt=="CC"&sum.stg17!=0),"cat",summarise,N.event=sum(count.event))$N.event)

tmp=ddply(subset(rslt.stg17,is.na(cat)==F),c('Alt','cat'),summarise,count=sum(count.event,na.rm=T))
ddply(tmp,"cat",summarise,max.val=max(count))
plot(count~cat,tmp,log="y")


rslt.stg16.sum=rslt.stg16.sum[,c("cat",alts.sort)]
xlabs=c("< 15", "15 - 30","30 - 60","60 - 90","90 - 180", "> 180")
# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_highstg17_events.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,4,1,0.25),lwd=0.5);
layout(matrix(c(1:4),4,1,byrow=F))

# ylim.max=c(150,20,20,10,4)
for(i in 1:4){
  ylim.val=c(0,20);by.y=10/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.stg17.sum[i,2:ncol(rslt.stg17.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  # FWO.thresh=tmp[i,]*(1-seq(0.25,0.75,0.25))
  # if(tmp[1]>0){
  # abline(h=FWO.thresh,lty=2)
  # text(x[1],FWO.thresh,paste0((1-seq(0.25,0.75,0.25))*100,"%"),cex=0.7,pos=3,col="white")
  # }
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==4){axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"))
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"LOK Daily Stage \u2265 17 Ft NGVD")}
}
mtext(side=1,line=6.5,"Alternatives")
dev.off()


rslt.stg16.sum=rslt.stg16.sum[,c("cat",alts.sort)]
xlabs=c("< 15", "15 - 30","30 - 60","60 - 90","90 - 180", "> 180")
# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_highstg16_events.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(7,4,1,0.25),lwd=0.5);
layout(matrix(c(1:6),6,1,byrow=F))

# ylim.max=c(150,20,20,10,4)
for(i in 1:length(xlabs)){
  ylim.val=c(0,20);by.y=10/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  
  tmp=t(rslt.stg16.sum[i,2:ncol(rslt.stg17.sum)])
  x=barplot(tmp,beside=T,col=adjustcolor(cols,0.5),
            ylim=ylim.val,axes=F,
            names.arg = rep(NA,length(tmp)),
            space=c(0),yaxs="i",xaxs="i")
  # FWO.thresh=tmp[i,]*(1-seq(0.25,0.75,0.25))
  # if(tmp[1]>0){
  # abline(h=FWO.thresh,lty=2)
  # text(x[1],FWO.thresh,paste0((1-seq(0.25,0.75,0.25))*100,"%"),cex=0.7,pos=3,col="white")
  # }
  text(x,tmp,tmp,pos=3,offset=0.1)
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(i==length(xlabs)){axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)}else{axis_fun(1,x,x,NA)}
  mtext(side=2,line=2.5,paste0("# of Events\n",xlabs[i]," days"))
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,"LOK Daily Stage > 16 Ft NGVD")}
}
mtext(side=1,line=6.5,"Alternatives")
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
ann.peak=ddply(lakeO.stage,c("CY","Alt"),summarise,max.stg=max(STAGE,na.rm=T))

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

# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_recurrance_events.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(7,2,1,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=F))

ylim.val=c(0,25);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(recur.alts$min17.gumbel,col=NA,border=NA,
          ylim=ylim.val,axes=F,ann=F,
          names.arg = rep(NA,length(recur.alts$min16.LP3)),
          space=c(0),yaxs="i",xaxs="i")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(recur.alts$min17.gumbel,col=adjustcolor(cols,0.5),
          ylim=ylim.val,axes=F,ann=F,
          names.arg = rep(NA,length(recur.alts$min16.LP3)),
          space=c(0),yaxs="i",xaxs="i",add=T)
points(x,recur.alts$min17.LP3,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1)
axis_fun(1,x,x,NA,las=2,cex=0.8,line=-0.25)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"Annual Max Stage \u2265 17 Ft NGVD")
legend("topleft",legend=c("Log-Pearson Type 3","Gumbel"),
       pch=c(21,22),lwd=c(0.1,0.1),lty=0,
       pt.bg=adjustcolor("grey",0.5),
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " Distribution")

ylim.val=c(0,10);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(recur.alts$min16.gumbel,col=NA,border=NA,
          ylim=ylim.val,axes=F,ann=F,
          names.arg = rep(NA,length(recur.alts$min16.LP3)),
          space=c(0),yaxs="i",xaxs="i")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(recur.alts$min16.gumbel,col=adjustcolor(cols,0.5),
          ylim=ylim.val,axes=F,ann=F,
          names.arg = rep(NA,length(recur.alts$min16.LP3)),
          space=c(0),yaxs="i",xaxs="i",add=T)

points(x,recur.alts$min16.LP3,pch=21,bg=adjustcolor(cols,0.5),lwd=0.1)
axis_fun(1,x,x,alts.sort2,las=2,cex=0.8,line=-0.25)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"Annual Max Stage \u2265 16 Ft NGVD")
mtext(side=1,line=6.5,"Alternatives")
mtext(side=2,outer=T,"Return Period (years)")
dev.off()

# png(filename=paste0(plot.path,"Iteration3_TechEval/LOK_recurrance_curves.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,1.5,1,0.5),oma=c(2,3,1.5,0.25));
layout(matrix(c(1:3),3,1,byrow=F))

ylim.val=c(12,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,30);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

tmp=subset(ann.peak,Alt==alts.sort[1])
recur.tmp=recur.fun(tmp$max.stg)
plot(dat.val~emp.rec.tim,recur.tmp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i",xaxs="i")
abline(v=xmaj,h=ymaj,lty=1,col=adjustcolor("grey",0.5))
abline(v=xmin,h=ymin,lty=2,col=adjustcolor("grey",0.25))
with(recur.fun(subset(ann.peak,Alt==alts.sort[1])$max.stg),points(emp.rec.tim,dat.val,pch=21,bg=adjustcolor("grey",0.5),col=adjustcolor("black",0.5),lwd=0.1,cex=1))
with(recur.fun(subset(ann.peak,Alt==alts.sort[1])$max.stg),lines(LP3,dat.val,col=adjustcolor("dodgerblue1",0.75),lwd=2))
with(recur.fun(subset(ann.peak,Alt==alts.sort[1])$max.stg),lines(gumbel,dat.val,col=adjustcolor("indianred1",0.75),lwd=2))
abline(h=17,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,line=1,"Lake Okeechobee")
mtext(side=3,adj=0,cex=0.75,"LOSOM Iteration 3 - Phase 1",col="grey50")
mtext(side=3,line=-1.25,adj=0,paste(" Alt:",alts.sort[1]))
legend("bottomright",legend=c("Empirical Dist. (Observed Data)",
                              "Theoretical Dist. (Log-Pearsons Type 3)",
                              "Theoretical Dist. (Gumbel)"),
       pch=c(21,NA,NA),lty=c(0,1,1),lwd=c(0.1,2,2),
       col=adjustcolor(c("black","dodgerblue1","indianred1"),0.75),
       pt.bg=c(adjustcolor("grey",0.5),rep(NA,3)),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

tmp=subset(ann.peak,Alt==alts.sort[2])
recur.tmp=recur.fun(tmp$max.stg)
plot(dat.val~emp.rec.tim,recur.tmp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i",xaxs="i")
abline(v=xmaj,h=ymaj,lty=1,col=adjustcolor("grey",0.5))
abline(v=xmin,h=ymin,lty=2,col=adjustcolor("grey",0.25))
with(recur.fun(subset(ann.peak,Alt==alts.sort[2])$max.stg),points(emp.rec.tim,dat.val,pch=21,bg=adjustcolor("grey",0.5),col=adjustcolor("black",0.5),lwd=0.1,cex=1))
with(recur.fun(subset(ann.peak,Alt==alts.sort[2])$max.stg),lines(LP3,dat.val,col=adjustcolor("dodgerblue1",0.75),lwd=2))
with(recur.fun(subset(ann.peak,Alt==alts.sort[2])$max.stg),lines(gumbel,dat.val,col=adjustcolor("indianred1",0.75),lwd=2))
abline(h=17,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,line=-1.25,adj=0,paste(" Alt:",alts.sort[2]))

tmp=subset(ann.peak,Alt==alts.sort[20])
recur.tmp=recur.fun(tmp$max.stg)
plot(dat.val~emp.rec.tim,recur.tmp,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i",xaxs="i")
abline(v=xmaj,h=ymaj,lty=1,col=adjustcolor("grey",0.5))
abline(v=xmin,h=ymin,lty=2,col=adjustcolor("grey",0.25))
with(recur.fun(subset(ann.peak,Alt==alts.sort[20])$max.stg),points(emp.rec.tim,dat.val,pch=21,bg=adjustcolor("grey",0.5),col=adjustcolor("black",0.5),lwd=0.1,cex=1))
with(recur.fun(subset(ann.peak,Alt==alts.sort[20])$max.stg),lines(LP3,dat.val,col=adjustcolor("dodgerblue1",0.75),lwd=2))
with(recur.fun(subset(ann.peak,Alt==alts.sort[20])$max.stg),lines(gumbel,dat.val,col=adjustcolor("indianred1",0.75),lwd=2))
abline(h=17,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,line=-1.25,adj=0,paste(" Alt:",alts.sort[20]))

mtext(side=2,outer=T,line=1,"Max Annual Stage (ft, NGVD29)")
mtext(side=1,line=2,"Return Period (years)")
dev.off()

# Flood Control -----------------------------------------------------------
RSM.sites=c("S351_QFC","S351_FC_SHIFT2_ENVTARG","S354_QFC","S354_FC_SHIFT2_ENVTARG",
            "S77_QFC","S308_QFC","C10A_QFC","S351","S354")
regq.dat=data.frame()

regq.dat=data.frame()
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
    regq.dat=rbind(tmp,regq.dat)
    print(i)
  }
}

n.alts=length(alts)
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3_Tech_Discussion/DSSfiles/",alts[j],"/RSMBN_output.dss"))  
  
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


# head(regq.dat,20)
# regq.dat=regq.dat[order(regq.dat$Alt,regq.dat$SITE,regq.dat$Date),]
# regq.dat$WY=WY(regq.dat$Date)
regq.dat$CY=as.numeric(format(regq.dat$Date,"%Y"))
regq.dat$month=as.numeric(format(regq.dat$Date,"%m"))

RSM.sites.region=data.frame(SITE=RSM.sites,Region=c(rep("WCAs",4),"Cal",'StL',"LWLagoon"))



##

q.dat$hydro.season=with(q.dat,FL.Hydroseason(q.dat$Date))
q.dat$bloom.period=with(q.dat,ifelse(SITE%in%c("S77","S78","S79"),
                                     ifelse(as.numeric(format(Date,"%m"))%in%c(6:9),"bloom","no.bloom"),
                                     ifelse(as.numeric(format(Date,"%m"))%in%c(5:9),"bloom","no.bloom")))


est.allPOS.sum=ddply(q.dat,c("SITE","CY","Alt"),summarise,TQ=sum(cfs.to.acftd(FLOW),na.rm=T))
est.allPOS.sum=ddply(est.allPOS.sum,c("SITE","Alt"),summarise,Avg.TQ.kacft=mean(TQ/1000))
# write.csv(est.allPOS.sum,paste0(export.path,"Iteration2/w_SR35/POS_eststruct_sum.csv"),row.names = F)

q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+CY~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
head(q.dat.xtab)
q.dat.xtab$FlowSouth=rowSums(q.dat.xtab[,c("S351","S354")],na.rm=T)