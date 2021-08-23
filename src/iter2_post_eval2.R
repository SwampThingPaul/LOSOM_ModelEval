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
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
alts=alts[!alts%in%c("_Batch_Results","Northern_Estuaries","Iteration2_STL_Flows_13Jul2021.xlsx")]
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2","SR3.5")

cols=c("grey50","grey80",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-3,"continuous")),"deeppink")
cols=cols[alts.sort%in%c("NA25","ECBr","CC","OPT")]
cols=c("grey50","grey80","#E6C019","darkorchid1")

# Discharge ---------------------------------------------------------------
RSM.sites=c("S79","S80","S80_QPFCSOURCE_LAKE","S79_QPFCSOURCE_LAKE",
            "S77","S308","S77_QFC","S308_QFC","S308BF",
            "TMC2EST","S48","S49","NSF2EST","S2","S3","S4BP")
q.dat=data.frame()
alts=c("NA25","ECBr","CC","OPT")
alts.sort=c("NA25","ECBr","CC","OPT")
n.alts=length(alts.sort)
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

tmp=subset(SalEnv_count.melt,Alt%in%c("NA25","ECBr","CC","OPT")&cat%in%c("high","dam"))
tmp$cat_source=with(tmp,paste(cat,source,sep="_"))

tmp2=reshape2::dcast(tmp,region+Alt~cat_source,value.var = "value",mean)
tmp3=subset(tmp2,region=="CRE")
tmp3=merge(tmp3,subset(mean.ann.Q,Alt%in%c("NA25","ECBr","CC","OPT"))[,c("Alt",'S77.kacft')],"Alt")
tmp3=rename(tmp3,c("S77.kacft"="RegFlow"))
tmp3=tmp3[match(tmp3$Alt,c("NA25","ECBr","CC","OPT")),]
tmp3$PerFWO_dam_basin=with(tmp3,(dam_basin-dam_basin[1])/dam_basin[1]*100)
tmp3$PerFWO_dam_LOK=with(tmp3,(dam_LOK-dam_LOK[1])/dam_LOK[1]*100)
tmp3$PerFWO_high_basin=with(tmp3,(high_basin-high_basin[1])/high_basin[1]*100)
tmp3$PerFWO_high_LOK=with(tmp3,(high_LOK-high_LOK[1])/high_LOK[1]*100)
tmp3$PerFWO_RegFlow=with(tmp3,(RegFlow-RegFlow[1])/RegFlow[1]*100)

tmp4=subset(tmp2,region=="SLE")
tmp4=merge(tmp4,subset(mean.ann.Q,Alt%in%c("NA25","ECBr","CC","OPT"))[,c("Alt",'S308.kacft')],"Alt")
tmp4=rename(tmp4,c("S308.kacft"="RegFlow"))
tmp4=tmp4[match(tmp4$Alt,c("NA25","ECBr","CC","OPT")),]
tmp4$PerFWO_dam_basin=with(tmp4,(dam_basin-dam_basin[1])/dam_basin[1]*100)
tmp4$PerFWO_dam_LOK=with(tmp4,(dam_LOK-dam_LOK[1])/dam_LOK[1]*100)
tmp4$PerFWO_high_basin=with(tmp4,(high_basin-high_basin[1])/high_basin[1]*100)
tmp4$PerFWO_high_LOK=with(tmp4,(high_LOK-high_LOK[1])/high_LOK[1]*100)
tmp4$PerFWO_RegFlow=with(tmp4,(RegFlow-RegFlow[1])/RegFlow[1]*100)

vars.sort=c("region","Alt","RegFlow","high_LOK","high_basin","dam_LOK","dam_basin",
            paste("PerFWO",c("RegFlow","high_LOK","high_basin","dam_LOK","dam_basin"),sep="_"))
tb.dat=rbind(tmp3[,vars.sort],tmp4[,vars.sort])
tb.dat[tb.dat==0]=NA
# write.csv(tb.dat,paste0(export.path,"post_iter2_estsum_Iter2Opt.csv"),row.names = F)

tb.dat%>%
  flextable()%>%
  colformat_double(j=3,digits=0)%>%
  colformat_double(j=8:12,digits=1,na_str = "---")%>%
  merge_v(j=1)%>%
  fix_border_issues()%>%
  valign(j=1,valign="top")%>%
  vline(j=7)%>%
  hline(i=4)%>%
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
##### RECOVER plots
vars.CRE=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high3.count"),sep=".")
# apply(tmp[,vars],2,FUN=function(x)sum(x,na.rm=T))
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars.CRE)],id.vars = "Alt")
CRE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
CRE.SalEnv_count=CRE.SalEnv_count[match(alts.sort,CRE.SalEnv_count$Alt),]
# write.csv(CRE.SalEnv_count,paste0(export.path,"CRE_SalEnv_count_Iter2Opt.csv"),row.names=F)

CRE.SalEnv_count$perFWO.low=with(CRE.SalEnv_count,(CRE.low.count-CRE.low.count[1])/CRE.low.count[1])*100
CRE.SalEnv_count$perFWO.opt=with(CRE.SalEnv_count,(CRE.opt.count-CRE.opt.count[1])/CRE.opt.count[1])*100
CRE.SalEnv_count$perFWO.LOK.stress=with(CRE.SalEnv_count,(CRE.high.LOK.count-CRE.high.LOK.count[1])/CRE.high.LOK.count[1])*100
CRE.SalEnv_count$perFWO.LOK.dam=with(CRE.SalEnv_count,(CRE.dam.LOK.count-CRE.dam.LOK.count[1])/CRE.dam.LOK.count[1])*100
CRE.SalEnv_count$perFWO.ext=with(CRE.SalEnv_count,(CRE.high3.count-CRE.high3.count[1])/CRE.high3.count[1])*100
CRE.SalEnv_count[,c('Alt',"perFWO.low","perFWO.opt","perFWO.LOK.stress","perFWO.LOK.dam","perFWO.ext")]

vars.SLE=paste("SLE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high2.count"),sep=".")
# apply(tmp[,vars],2,FUN=function(x)sum(x,na.rm=T))
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars.SLE)],id.vars = "Alt")
SLE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
SLE.SalEnv_count=SLE.SalEnv_count[match(alts.sort,SLE.SalEnv_count$Alt),]
# write.csv(SLE.SalEnv_count,paste0(export.path,"SLE_SalEnv_count_Iter2Opt.csv"),row.names=F)

SLE.SalEnv_count$perFWO.low=with(SLE.SalEnv_count,(SLE.low.count-SLE.low.count[1])/SLE.low.count[1])*100
SLE.SalEnv_count$perFWO.opt=with(SLE.SalEnv_count,(SLE.opt.count-SLE.opt.count[1])/SLE.opt.count[1])*100
SLE.SalEnv_count$perFWO.LOK.stress=with(SLE.SalEnv_count,(SLE.high.LOK.count-SLE.high.LOK.count[1])/SLE.high.LOK.count[1])*100
SLE.SalEnv_count$perFWO.LOK.dam=with(SLE.SalEnv_count,(SLE.dam.LOK.count-SLE.dam.LOK.count[1])/SLE.dam.LOK.count[1])*100
SLE.SalEnv_count$perFWO.ext=with(SLE.SalEnv_count,(SLE.high2.count-SLE.high2.count[1])/SLE.high2.count[1])*100
SLE.SalEnv_count[,c('Alt',"perFWO.low","perFWO.opt","perFWO.LOK.stress","perFWO.LOK.dam","perFWO.ext")]

CRE.SalEnv_count2=CRE.SalEnv_count[,c("Alt",vars.CRE)]
SLE.SalEnv_count2=SLE.SalEnv_count[,c("Alt",vars.SLE)]
CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
SLE.labs=c("Low Flow\n(<150 cfs)","Optimum\n(150 - 1400 cfs)","Stress From LOK\n(1400 - 1700 cfs)","Damaging From LOK\n(>1700 cfs)","Extreme\n(>4000 cfs)")
# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_RECOVER_SalEnv_total_Iter2Opt.png"),width=7,height=4.25,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,2,1),lwd=0.5);

ymax=c(1000,1000,400,300,80)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(CRE.SalEnv_count2[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count2[,i],round(CRE.SalEnv_count2[,i],0),font=2,col="black",pos=1,cex=0.5,offset=0.25)
  if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ymax=c(200,1000,200,200,200)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(SLE.SalEnv_count2[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count2[,i],round(SLE.SalEnv_count2[,i],0),font=2,col="black",pos=1,cex=0.5,offset=0.25)
  if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"Count of 14-Day Periods")

dev.off()



CRE.SalEnv_fwo=CRE.SalEnv_count[,c('Alt',"perFWO.low","perFWO.opt","perFWO.LOK.stress","perFWO.LOK.dam","perFWO.ext")]
SLE.SalEnv_fwo=SLE.SalEnv_count[,c('Alt',"perFWO.low","perFWO.opt","perFWO.LOK.stress","perFWO.LOK.dam","perFWO.ext")]
# CRE.labs=c("Low Flow (<750 cfs)","Optimum (750 - 2100 cfs)","Stress (2100 - 2600 cfs)","Damaging (>2600 cfs)")
# SLE.labs=c("Low Flow (<150 cfs)","Optimum (150 - 1400 cfs)","Stress (1400 - 1700 cfs)","Damaging (>1700 cfs)")
# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_RECOVER_SalEnv_perDiff_Iter2Opt.png"),width=7,height=4.25,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,2,1),lwd=0.5);

ylim.val=c(-80,80);by.y=40;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y)
for(i in 2:6){
  x=barplot(CRE.SalEnv_fwo[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  abline(h=0,lwd=1)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_fwo[,i],format(round(CRE.SalEnv_fwo[,i],1),nsmall=1),
       font=2,pos=ifelse(CRE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.1)
  # if(i%in%c(2,4,5)){
  # text(x,CRE.SalEnv_fwo[,i],format(round(CRE.SalEnv_fwo[,i],1),nsmall=1),
  #      font=2,pos=ifelse(CRE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.1,
  #      col=ifelse(CRE.SalEnv_fwo[,i]<0,"forestgreen",ifelse(CRE.SalEnv_fwo[,i]>0,"red","black")))}
  # if(i==3){
  #   text(x,CRE.SalEnv_fwo[,i],format(round(CRE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(CRE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.1,
  #        col=ifelse(CRE.SalEnv_fwo[,i]>0,"forestgreen",ifelse(CRE.SalEnv_fwo[,i]<0,"red","black")))}
  if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ylim.val=c(-100,80);by.y=40;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y)
for(i in 2:6){
  x=barplot(SLE.SalEnv_fwo[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  abline(h=0,lwd=1)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_fwo[,i],format(round(SLE.SalEnv_fwo[,i],1),nsmall=1),
       font=2,pos=ifelse(SLE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.1)
  # if(i%in%c(4,5)){
  #   text(x,SLE.SalEnv_fwo[,i],format(round(SLE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(SLE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.25,
  #        col=ifelse(SLE.SalEnv_fwo[,i]<0,"forestgreen",ifelse(SLE.SalEnv_fwo[,i]>0,"red","black")))}
  # if(i%in%c(2,3)){
  #   text(x,SLE.SalEnv_fwo[,i],format(round(SLE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(SLE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.25,
  #        col=ifelse(SLE.SalEnv_fwo[,i]>0,"forestgreen",ifelse(SLE.SalEnv_fwo[,i]<0,"red","black")))}
  if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.5,outer=T,"Average Percent Difference to FWO")
dev.off()

## 
q.dat.xtab$QLT457=with(q.dat.xtab,ifelse(S79<457,1,0))
q.dat.xtab$Q457_750=with(q.dat.xtab,ifelse(S79>=457&S79<750,1,0))
q.dat.xtab$Q_Opt=with(q.dat.xtab,ifelse(S79>=750&S79<2100,1,0))
q.dat.xtab$Q_Stress=with(q.dat.xtab,ifelse(S79>=2100&S79<2600,1,0))
q.dat.xtab$Q_Dam=with(q.dat.xtab,ifelse(S79>=2600,1,0))
q.dat.xtab$Q2600_4500=with(q.dat.xtab,ifelse(S79>=2600&S79<4500,1,0))
q.dat.xtab$Q4500_6500=with(q.dat.xtab,ifelse(S79>=4500&S79<6500,1,0))
q.dat.xtab$QGT6500=with(q.dat.xtab,ifelse(S79>6500,1,0))
q.dat.xtab$GT2100=with(q.dat.xtab,ifelse(S79>=2100,1,0))

CRE.QCat.POS=ddply(q.dat.xtab,"Alt",summarise,
                   Q.LT457=sum(cfs.to.acftd(S79[QLT457==1])/1000,na.rm=T),
                   Q.Q457_750=sum(cfs.to.acftd(S79[Q457_750==1])/1000,na.rm=T),
                   Q.Q_Opt=sum(cfs.to.acftd(S79[Q_Opt==1])/1000,na.rm=T),
                   Q.Q_Stress=sum(cfs.to.acftd(S79[Q_Stress==1])/1000,na.rm=T),
                   Q.Q2600_4500=sum(cfs.to.acftd(S79[Q2600_4500==1])/1000,na.rm=T),
                   Q.Q4500_6500=sum(cfs.to.acftd(S79[Q4500_6500==1])/1000,na.rm = T),
                   Q.QGT6500=sum(cfs.to.acftd(S79[QGT6500==1])/1000,na.rm=T),
                   Q.QGT2100=sum(cfs.to.acftd(S79[GT2100==1])/1000,na.rm=T))
CRE.QCat.POS=CRE.QCat.POS[match(alts.sort,CRE.QCat.POS$Alt),]
CRE.QCat.POS
# with(subset(q.dat.xtab,Alt=="CC"),sum(cfs.to.acftd(S79[QLT457==1])/1000,na.rm=T))

# Daily Counts ------------------------------------------------------------
# CRE
q.dat.xtab$da.CRE.low=with(q.dat.xtab,ifelse(S79<750,1,0)) # RECOVER Low
q.dat.xtab$da.CRE.opt=with(q.dat.xtab,ifelse(S79>=750&S79<2100,1,0)) # RECOVER Optimum
q.dat.xtab$da.CRE.high=with(q.dat.xtab,ifelse(S79>=2100&S79<2600,1,0)) # RECOVER Stress
q.dat.xtab$da.CRE.high.LOK=with(q.dat.xtab,ifelse(da.CRE.high==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=2100,1,0),0))
q.dat.xtab$da.CRE.high.Basin=with(q.dat.xtab,da.CRE.high-da.CRE.high.LOK)
q.dat.xtab$da.CRE.high3=with(q.dat.xtab,ifelse(S79>=6500,1,0))
q.dat.xtab$da.CRE.dam=with(q.dat.xtab,ifelse(S79>=2600,1,0)) # RECOVER Damaging
q.dat.xtab$da.CRE.dam.LOK=with(q.dat.xtab,ifelse(da.CRE.dam==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=2600,1,0),0))
q.dat.xtab$da.CRE.dam.Basin=with(q.dat.xtab,da.CRE.dam-da.CRE.dam.LOK)

CRE.vars=c( "da.CRE.low", "da.CRE.opt","da.CRE.high","da.CRE.high.LOK","da.CRE.high.Basin",  
            "da.CRE.dam","da.CRE.dam.LOK","da.CRE.dam.Basin","da.CRE.high3")
cre.q.dat.xtab.melt=melt(q.dat.xtab[,c("Alt","Date",CRE.vars)],id.vars=c("Alt","Date"))
cre.q.dat.xtab.melt$region="CRE"
cre.q.dat.xtab.da=reshape2::dcast(cre.q.dat.xtab.melt,region+Alt~variable,value.var="value",sum,na.rm=T)
cre.q.dat.xtab.da=cre.q.dat.xtab.da[match(cre.q.dat.xtab.da$Alt,alts.sort),]

var.names=paste0("PerFWO.",c("low","opt","high","high.LOK","high.Basin","dam","dam.LOK","dam.Basin","high3"))
for(i in 1:length(CRE.vars)){
  tmp=data.frame(val=((cre.q.dat.xtab.da[,CRE.vars[i]]-cre.q.dat.xtab.da[1,CRE.vars[i]])/cre.q.dat.xtab.da[1,CRE.vars[i]])*100)
  colnames(tmp)=var.names[i]
  cre.q.dat.xtab.da=cbind(cre.q.dat.xtab.da,tmp)
}

# SLE
q.dat.xtab$da.SLE.low=with(q.dat.xtab,ifelse(SLE.S80trib<150,1,0)) # RECOVER Low
q.dat.xtab$da.SLE.opt=with(q.dat.xtab,ifelse(SLE.S80trib>=150&SLE.S80trib<1400,1,0)) # RECOVER Optimum
q.dat.xtab$da.SLE.high=with(q.dat.xtab,ifelse(SLE.S80trib>=1400&SLE.S80trib<1700,1,0)) # RECOVER stress
q.dat.xtab$da.SLE.high.LOK=with(q.dat.xtab,ifelse(da.SLE.high==1,ifelse((SLE.S80trib-S80_QPFCSOURCE_LAKE)<=1400,1,0),0))
q.dat.xtab$da.SLE.high.Basin=with(q.dat.xtab,da.SLE.high-da.SLE.high.LOK)
q.dat.xtab$da.SLE.dam=with(q.dat.xtab,ifelse(SLE.S80trib>=1700,1,0))# RECOVER damaging
q.dat.xtab$da.SLE.dam.LOK=with(q.dat.xtab,ifelse(da.SLE.dam==1,ifelse((SLE.S80trib-S80_QPFCSOURCE_LAKE)<=1700,1,0),0))
q.dat.xtab$da.SLE.dam.Basin=with(q.dat.xtab,da.SLE.dam-da.SLE.dam.LOK)
q.dat.xtab$da.SLE.high2=with(q.dat.xtab,ifelse(SLE.S80trib>=4000,1,0))

SLE.vars=c( "da.SLE.low", "da.SLE.opt","da.SLE.high","da.SLE.high.LOK","da.SLE.high.Basin",  
            "da.SLE.dam","da.SLE.dam.LOK","da.SLE.dam.Basin","da.SLE.high2")
SLE.q.dat.xtab.melt=reshape2::melt(q.dat.xtab[,c("Alt","Date",SLE.vars)],id.vars=c("Alt","Date"))
SLE.q.dat.xtab.melt$region="SLE"
SLE.q.dat.xtab.da=reshape2::dcast(SLE.q.dat.xtab.melt,region+Alt~variable,value.var="value",sum,na.rm=T)
SLE.q.dat.xtab.da=SLE.q.dat.xtab.da[match(SLE.q.dat.xtab.da$Alt,alts.sort),]

var.names=paste0("PerFWO.",c("low","opt","high","high.LOK","high.Basin","dam","dam.LOK","dam.Basin","high2"))
for(i in 1:length(SLE.vars)){
  tmp=data.frame(val=((SLE.q.dat.xtab.da[,SLE.vars[i]]-SLE.q.dat.xtab.da[1,SLE.vars[i]])/SLE.q.dat.xtab.da[1,SLE.vars[i]])*100)
  colnames(tmp)=var.names[i]
  SLE.q.dat.xtab.da=cbind(SLE.q.dat.xtab.da,tmp)
}

CRE.vars=paste("da.CRE",c("low", "opt","high.LOK","dam.LOK","high3"),sep=".")
SLE.vars=paste("da.SLE",c("low", "opt","high.LOK","dam.LOK","high2"),sep=".")

CRE.SalEnv_count.da=cre.q.dat.xtab.da[,c("Alt",CRE.vars)]
SLE.SalEnv_count.da=SLE.q.dat.xtab.da[,c("Alt",SLE.vars)]

CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
SLE.labs=c("Low Flow\n(<150 cfs)","Optimum\n(150 - 1400 cfs)","Stress From LOK\n(1400 - 1700 cfs)","Damaging From LOK\n(>1700 cfs)","Extreme\n(>4000 cfs)")
# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_RECOVER_SalEnv_total_daily_Iter2Opt.png"),width=7,height=4.25,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2.5,1,1),oma=c(2,2,2,1),lwd=0.5);

ymax=c(11000,12000,500,2500,1000)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(CRE.SalEnv_count.da[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count.da[,i],round(CRE.SalEnv_count.da[,i],0),font=2,col="black",pos=1,cex=0.5,offset=0.25)
  if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ymax=c(5000,20000,500,2500,2500)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(SLE.SalEnv_count.da[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count.da[,i],round(SLE.SalEnv_count.da[,i],0),font=2,col="black",pos=ifelse(SLE.SalEnv_count.da[,i]<100,3,1),cex=0.5,offset=0.25)
  if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"Count of Days")
mtext(side=1,outer=T,line=1,adj=1,"Based on daily data (CRE: S79; SLE: S80+Tribs)",cex=0.5,col="grey")
dev.off()

CRE.vars=paste0("PerFWO.",c("low","opt","high.LOK","dam.LOK","high3"))
CRE.SalEnv_count.da2=cre.q.dat.xtab.da[,c("Alt",CRE.vars)]
SLE.vars=paste0("PerFWO.",c("low","opt","high.LOK","dam.LOK","high2"))
SLE.SalEnv_count.da2=SLE.q.dat.xtab.da[,c("Alt",SLE.vars)]

# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_RECOVER_SalEnv_perDiff_daily_Iter2Opt.png"),width=7,height=4.25,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,2,1),lwd=0.5);

ylim.val=c(-100,100);by.y=50;ymaj=seq(-100,ylim.val[2],by.y);ymin=seq(-100,ylim.val[2],by.y)
for(i in 2:6){
  x=barplot(CRE.SalEnv_count.da2[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  abline(h=0,lwd=1)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count.da2[,i],format(round(CRE.SalEnv_count.da2[,i],1),nsmall=1),
       font=2,pos=ifelse(CRE.SalEnv_count.da2[,i]<0,1,3),cex=0.5,offset=0.1)
  # if(i%in%c(2,4,5)){
  # text(x,CRE.SalEnv_fwo[,i],format(round(CRE.SalEnv_fwo[,i],1),nsmall=1),
  #      font=2,pos=ifelse(CRE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.1,
  #      col=ifelse(CRE.SalEnv_fwo[,i]<0,"forestgreen",ifelse(CRE.SalEnv_fwo[,i]>0,"red","black")))}
  # if(i==3){
  #   text(x,CRE.SalEnv_fwo[,i],format(round(CRE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(CRE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.1,
  #        col=ifelse(CRE.SalEnv_fwo[,i]>0,"forestgreen",ifelse(CRE.SalEnv_fwo[,i]<0,"red","black")))}
  if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ylim.val=c(-110,100);by.y=50;ymaj=seq(-100,ylim.val[2],by.y);ymin=seq(-100,ylim.val[2],by.y)
for(i in 2:6){
  x=barplot(SLE.SalEnv_count.da2[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  abline(h=0,lwd=1)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count.da2[,i],format(round(SLE.SalEnv_count.da2[,i],1),nsmall=1),
       font=2,pos=ifelse(SLE.SalEnv_count.da2[,i]<0,1,3),cex=0.5,offset=0.1)
  # if(i%in%c(4,5)){
  #   text(x,SLE.SalEnv_fwo[,i],format(round(SLE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(SLE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.25,
  #        col=ifelse(SLE.SalEnv_fwo[,i]<0,"forestgreen",ifelse(SLE.SalEnv_fwo[,i]>0,"red","black")))}
  # if(i%in%c(2,3)){
  #   text(x,SLE.SalEnv_fwo[,i],format(round(SLE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(SLE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.25,
  #        col=ifelse(SLE.SalEnv_fwo[,i]>0,"forestgreen",ifelse(SLE.SalEnv_fwo[,i]<0,"red","black")))}
  if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.5,outer=T,"Average Percent Difference to FWO")
dev.off()


var.names=c("region", "Alt", "low", "opt", "high","high.LOK", "high.Basin", "dam", "dam.LOK", 
            "dam.Basin", "extreme", "PerFWO.low", "PerFWO.opt", 
            "PerFWO.high", "PerFWO.high.LOK", "PerFWO.high.Basin", "PerFWO.dam", 
            "PerFWO.dam.LOK", "PerFWO.dam.Basin", "PerFWO.ext")

CRE.da.count=cre.q.dat.xtab.da
colnames(CRE.da.count)=var.names

SLE.da.count=SLE.q.dat.xtab.da
colnames(SLE.da.count)=var.names

sal.env.daily=rbind(CRE.da.count,SLE.da.count)
# write.csv(sal.env.daily,paste0(export.path,"SLE_SalEnv_count_da_Iter2Opt.csv"),row.names=F)


# Monthly Average ---------------------------------------------------------
q.dat.xtab$month=as.numeric(format(q.dat.xtab$Date,"%m"))
q.dat.xtab.mon=reshape2::dcast(q.dat,Alt+CY+month~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))

S80trib=ddply(q.dat.xtab,c("Alt","CY","month"),summarise,SLE.S80trib=mean(SLE.S80trib,na.rm=T))
q.dat.xtab.mon=merge(q.dat.xtab.mon,S80trib,c("Alt","CY","month"))
q.dat.xtab.mon=q.dat.xtab.mon[order(q.dat.xtab.mon$Alt,q.dat.xtab.mon$CY,q.dat.xtab.mon$month),]

q.dat.xtab.mon$mon.CRE.low=with(q.dat.xtab.mon,ifelse(S79<750,1,0)) # RECOVER Low
q.dat.xtab.mon$mon.CRE.opt=with(q.dat.xtab.mon,ifelse(S79>=750&S79<2100,1,0)) # RECOVER Optimum
q.dat.xtab.mon$mon.CRE.high=with(q.dat.xtab.mon,ifelse(S79>=2100&S79<2600,1,0)) # RECOVER Stress
q.dat.xtab.mon$mon.CRE.high.LOK=with(q.dat.xtab.mon,ifelse(mon.CRE.high==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=2100,1,0),0))
q.dat.xtab.mon$mon.CRE.high.Basin=with(q.dat.xtab.mon,mon.CRE.high-mon.CRE.high.LOK)
q.dat.xtab.mon$mon.CRE.high3=with(q.dat.xtab.mon,ifelse(S79>=6500,1,0))
q.dat.xtab.mon$mon.CRE.dam=with(q.dat.xtab.mon,ifelse(S79>=2600,1,0)) # RECOVER Damaging
q.dat.xtab.mon$mon.CRE.dam.LOK=with(q.dat.xtab.mon,ifelse(mon.CRE.dam==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=2600,1,0),0))
q.dat.xtab.mon$mon.CRE.dam.Basin=with(q.dat.xtab.mon,mon.CRE.dam-mon.CRE.dam.LOK)

CRE.vars=c( "mon.CRE.low", "mon.CRE.opt","mon.CRE.high","mon.CRE.high.LOK","mon.CRE.high.Basin",  
            "mon.CRE.dam","mon.CRE.dam.LOK","mon.CRE.dam.Basin","mon.CRE.high3")
cre.q.dat.xtab.melt=melt(q.dat.xtab.mon[,c("Alt","CY","month",CRE.vars)],id.vars=c("Alt","CY","month"))
cre.q.dat.xtab.melt$region="CRE"
cre.q.dat.xtab.mon=reshape2::dcast(cre.q.dat.xtab.melt,region+Alt~variable,value.var="value",sum,na.rm=T)
cre.q.dat.xtab.mon=cre.q.dat.xtab.mon[match(cre.q.dat.xtab.mon$Alt,alts.sort),]

var.names=paste0("PerFWO.",c("low","opt","high","high.LOK","high.Basin","dam","dam.LOK","dam.Basin","high3"))
for(i in 1:length(CRE.vars)){
  tmp=data.frame(val=((cre.q.dat.xtab.mon[,CRE.vars[i]]-cre.q.dat.xtab.mon[1,CRE.vars[i]])/cre.q.dat.xtab.mon[1,CRE.vars[i]])*100)
  colnames(tmp)=var.names[i]
  cre.q.dat.xtab.mon=cbind(cre.q.dat.xtab.mon,tmp)
}

# SLE
q.dat.xtab.mon$mon.SLE.low=with(q.dat.xtab.mon,ifelse(SLE.S80trib<150,1,0)) # RECOVER Low
q.dat.xtab.mon$mon.SLE.opt=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=150&SLE.S80trib<1400,1,0)) # RECOVER Optimum
q.dat.xtab.mon$mon.SLE.high=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=1400&SLE.S80trib<1700,1,0)) # RECOVER stress
q.dat.xtab.mon$mon.SLE.high.LOK=with(q.dat.xtab.mon,ifelse(mon.SLE.high==1,ifelse((SLE.S80trib-S80_QPFCSOURCE_LAKE)<=1400,1,0),0))
q.dat.xtab.mon$mon.SLE.high.Basin=with(q.dat.xtab.mon,mon.SLE.high-mon.SLE.high.LOK)
q.dat.xtab.mon$mon.SLE.dam=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=1700,1,0))# RECOVER damaging
q.dat.xtab.mon$mon.SLE.dam.LOK=with(q.dat.xtab.mon,ifelse(mon.SLE.dam==1,ifelse((SLE.S80trib-S80_QPFCSOURCE_LAKE)<=1700,1,0),0))
q.dat.xtab.mon$mon.SLE.dam.Basin=with(q.dat.xtab.mon,mon.SLE.dam-mon.SLE.dam.LOK)
q.dat.xtab.mon$mon.SLE.high2=with(q.dat.xtab.mon,ifelse(SLE.S80trib>=4000,1,0))

SLE.vars=c( "mon.SLE.low", "mon.SLE.opt","mon.SLE.high","mon.SLE.high.LOK","mon.SLE.high.Basin",  
            "mon.SLE.dam","mon.SLE.dam.LOK","mon.SLE.dam.Basin","mon.SLE.high2")
SLE.q.dat.xtab.melt=reshape2::melt(q.dat.xtab.mon[,c("Alt","CY","month",SLE.vars)],id.vars=c("Alt","CY","month"))
SLE.q.dat.xtab.melt$region="SLE"
SLE.q.dat.xtab.mon=reshape2::dcast(SLE.q.dat.xtab.melt,region+Alt~variable,value.var="value",sum,na.rm=T)
SLE.q.dat.xtab.mon=SLE.q.dat.xtab.mon[match(SLE.q.dat.xtab.mon$Alt,alts.sort),]

var.names=paste0("PerFWO.",c("low","opt","high","high.LOK","high.Basin","dam","dam.LOK","dam.Basin","high2"))
for(i in 1:length(SLE.vars)){
  tmp=data.frame(val=((SLE.q.dat.xtab.mon[,SLE.vars[i]]-SLE.q.dat.xtab.mon[1,SLE.vars[i]])/SLE.q.dat.xtab.mon[1,SLE.vars[i]])*100)
  colnames(tmp)=var.names[i]
  SLE.q.dat.xtab.mon=cbind(SLE.q.dat.xtab.mon,tmp)
}

CRE.vars=paste("mon.CRE",c("low", "opt","high.LOK","dam.LOK","high3"),sep=".")
SLE.vars=paste("mon.SLE",c("low", "opt","high.LOK","dam.LOK","high2"),sep=".")

CRE.SalEnv_count.mon=cre.q.dat.xtab.mon[,c("Alt",CRE.vars)]
SLE.SalEnv_count.mon=SLE.q.dat.xtab.mon[,c("Alt",SLE.vars)]

CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
SLE.labs=c("Low Flow\n(<150 cfs)","Optimum\n(150 - 1400 cfs)","Stress From LOK\n(1400 - 1700 cfs)","Damaging From LOK\n(>1700 cfs)","Extreme\n(>4000 cfs)")
# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_RECOVER_SalEnv_total_month_Iter2Opt.png"),width=7,height=4.25,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2.5,1,1),oma=c(2,2,2,1),lwd=0.5);

ymax=c(300,400,100,100,30)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(CRE.SalEnv_count.mon[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count.mon[,i],round(CRE.SalEnv_count.mon[,i],0),font=2,col="black",pos=1,cex=0.5,offset=0.25)
  if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ymax=c(50,400,40,50,60)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(SLE.SalEnv_count.mon[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count.mon[,i],round(SLE.SalEnv_count.mon[,i],0),font=2,col="black",pos=ifelse(SLE.SalEnv_count.mon[,i]<100,3,1),cex=0.5,offset=0.25)
  if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0,outer=T,"Count of Months")
mtext(side=1,outer=T,line=1,adj=1,"Based on monthly mean data (CRE: S79; SLE: S80+Tribs)",cex=0.5,col="grey")

dev.off()

CRE.vars=paste0("PerFWO.",c("low","opt","high.LOK","dam.LOK","high3"))
CRE.SalEnv_count.mon2=cre.q.dat.xtab.mon[,c("Alt",CRE.vars)]
SLE.vars=paste0("PerFWO.",c("low","opt","high.LOK","dam.LOK","high2"))
SLE.SalEnv_count.mon2=SLE.q.dat.xtab.mon[,c("Alt",SLE.vars)]

# png(filename=paste0(plot.path,"Post-Iteration_2/CRE_RECOVER_SalEnv_perDiff_mon_Iter2Opt.png"),width=7,height=4.25,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(2,2,1,1),oma=c(2,2,2,1),lwd=0.5);

ylim.val=c(-100,200);by.y=50;ymaj=seq(-100,ylim.val[2],by.y);ymin=seq(-100,ylim.val[2],by.y)
for(i in 2:6){
  x=barplot(CRE.SalEnv_count.mon2[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  abline(h=0,lwd=1)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count.mon2[,i],format(round(CRE.SalEnv_count.mon2[,i],1),nsmall=1),
       font=2,pos=ifelse(CRE.SalEnv_count.mon2[,i]<0,1,3),cex=0.5,offset=0.1)
  # if(i%in%c(2,4,5)){
  # text(x,CRE.SalEnv_fwo[,i],format(round(CRE.SalEnv_fwo[,i],1),nsmall=1),
  #      font=2,pos=ifelse(CRE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.1,
  #      col=ifelse(CRE.SalEnv_fwo[,i]<0,"forestgreen",ifelse(CRE.SalEnv_fwo[,i]>0,"red","black")))}
  # if(i==3){
  #   text(x,CRE.SalEnv_fwo[,i],format(round(CRE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(CRE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.1,
  #        col=ifelse(CRE.SalEnv_fwo[,i]>0,"forestgreen",ifelse(CRE.SalEnv_fwo[,i]<0,"red","black")))}
  if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
}
mtext(side=4,line=0.5,"Caloosahatchee")

ylim.val=c(-110,100);by.y=50;ymaj=seq(-100,ylim.val[2],by.y);ymin=seq(-100,ylim.val[2],by.y)
for(i in 2:6){
  x=barplot(SLE.SalEnv_count.mon2[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  abline(h=0,lwd=1)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,SLE.labs[i-1],cex=0.7)
  text(x,SLE.SalEnv_count.mon2[,i],format(round(SLE.SalEnv_count.mon2[,i],1),nsmall=1),
       font=2,pos=ifelse(SLE.SalEnv_count.mon2[,i]<0,1,3),cex=0.5,offset=0.1)
  # if(i%in%c(4,5)){
  #   text(x,SLE.SalEnv_fwo[,i],format(round(SLE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(SLE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.25,
  #        col=ifelse(SLE.SalEnv_fwo[,i]<0,"forestgreen",ifelse(SLE.SalEnv_fwo[,i]>0,"red","black")))}
  # if(i%in%c(2,3)){
  #   text(x,SLE.SalEnv_fwo[,i],format(round(SLE.SalEnv_fwo[,i],1),nsmall=1),
  #        font=2,pos=ifelse(SLE.SalEnv_fwo[,i]<0,1,3),cex=0.5,offset=0.25,
  #        col=ifelse(SLE.SalEnv_fwo[,i]>0,"forestgreen",ifelse(SLE.SalEnv_fwo[,i]<0,"red","black")))}
  if(i==2){mtext(side=3,adj=0,line=-1.25," SLE")}
}
mtext(side=4,line=0.5,"St Lucie")
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.5,outer=T,"Average Percent Difference to FWO")
dev.off()

var.names=c("region", "Alt", "low", "opt", "high","high.LOK", "high.Basin", "dam", "dam.LOK", 
            "dam.Basin", "extreme", "PerFWO.low", "PerFWO.opt", 
            "PerFWO.high", "PerFWO.high.LOK", "PerFWO.high.Basin", "PerFWO.dam", 
            "PerFWO.dam.LOK", "PerFWO.dam.Basin", "PerFWO.ext")

CRE.mon.count=cre.q.dat.xtab.mon
colnames(CRE.mon.count)=var.names

SLE.mon.count=SLE.q.dat.xtab.mon
colnames(SLE.mon.count)=var.names

sal.env.month=rbind(CRE.mon.count,SLE.mon.count)
# write.csv(sal.env.month,paste0(export.path,"SLE_SalEnv_count_mon_Iter2Opt.csv"),row.names=F)


##### Lake Discharges
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


# png(filename=paste0(plot.path,"Post-Iteration_2/Lakedischarge_Annualmean_Iter2Opt.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
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
mtext(side=3,adj=0,"Mean Annual Discharge\n\u22651400 cfs at S-80",cex=0.75)
mtext(side=3,line=-1.25,adj=0," SLE")
text(x,SLE.GT1400_annual.mean$mean.val,round(SLE.GT1400_annual.mean$mean.val,0),col="black",pos=1,cex=1)
mtext(side=3,adj=1,"CY1965 - 2016",cex=0.75)
mtext(side=1,line=2.5,"Alternative")
dev.off()


CRE.LT2100_annual=ddply(q.dat.xtab,c("CY","Alt"),summarise,Q.Lake.LT2100=sum(cfs.to.acftd(S79_QPFCSOURCE_LAKE[S79_LT2100==1])/1000,na.rm=T))
CRE.LT2100_annual.mean=ddply(CRE.LT2100_annual,"Alt",summarise,mean.val=mean(Q.Lake.LT2100))
CRE.LT2100_annual.mean=CRE.LT2100_annual.mean[match(alts.sort,CRE.LT2100_annual.mean$Alt),]
CRE.LT2100_annual.mean

SLE.LT1400_annual=ddply(q.dat.xtab,c("CY","Alt"),summarise,Q.Lake.LT1400=sum(cfs.to.acftd(S80_QPFCSOURCE_LAKE[S80_LT1400==1])/1000,na.rm=T))
SLE.LT1400_annual.mean=ddply(SLE.LT1400_annual,"Alt",summarise,mean.val=mean(Q.Lake.LT1400))
SLE.LT1400_annual.mean=SLE.LT1400_annual.mean[match(alts.sort,SLE.LT1400_annual.mean$Alt),]
SLE.LT1400_annual.mean

# png(filename=paste0(plot.path,"Post-Iteration_2/Lakedischarge_LTStress_Annualmean_Iter2Opt.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:2),1,2,byrow=T))
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);

ylim.val=c(0,300);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(CRE.LT2100_annual.mean$mean.val,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(CRE.LT2100_annual.mean$mean.val,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"Mean Annual Discharge\n<2100 cfs at S-79",cex=0.75)
text(x,CRE.LT2100_annual.mean$mean.val,round(CRE.LT2100_annual.mean$mean.val,0),col="black",pos=3,cex=1)
# mtext(side=3,adj=1,"CY1965 - 2016",cex=0.75)
mtext(side=1,line=2.5,"Alternative")
mtext(side=2,line=2.5,"Lake Discharge\n(x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=3,line=-1.25,adj=0," CRE")
# mtext(side=1,line=3,adj=1,"Flow Tag: S79_QPFCSOURCE_LAKE",cex=0.5,col=adjustcolor("black",0.5),font=3)

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(SLE.LT1400_annual.mean$mean.val,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(SLE.LT1400_annual.mean$mean.val,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,cex=0.8,las=2)
box(lwd=1)
mtext(side=3,adj=0,"Mean Annual Discharge\n<1400 cfs at S-80",cex=0.75)
mtext(side=3,line=-1.25,adj=0," SLE")
text(x,SLE.LT1400_annual.mean$mean.val,round(SLE.LT1400_annual.mean$mean.val,0),col="black",pos=3,cex=1)
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
# png(filename=paste0(plot.path,"Post-Iteration_2/AvgFloodControl_Iter2Opt.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),heights=c(1,0.4))

x=barplot(t(tmp),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,4))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp),beside=F,col=cols2,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,4),add=T)
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




# Load and Flow -----------------------------------------------------------
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

wq.dat.xtab=reshape2::dcast(wq.dat,Station.ID+Date.EST~param,value.var="HalfMDL",mean)
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

# POS WQ data --------------------------------------------------------------
month.mean=ddply(subset(wq.dat.xtab,Station.ID%in%wq.sites[c(1:2,4:6)]),c("Station.ID","month"),summarise,
                 mean.TP=mean(TP,na.rm=T),SD.TP=sd(TP,na.rm=T),N.TP=N.obs(TP),
                 mean.TN=mean(TN,na.rm=T),SD.TN=sd(TN,na.rm=T),N.TN=N.obs(TN))


# write.csv(month.mean,paste0(export.path,"S77S308_monthlymean_Iter2Opt.csv"),row.names = F)
S77.WQ.sim=data.frame(Date.EST=seq(date.fun("1965-01-15"),date.fun("2020-12-31"),"1 month"))
S77.WQ.sim$WY=WY(S77.WQ.sim$Date.EST)
S77.WQ.sim$month=as.numeric(format(S77.WQ.sim$Date.EST,'%m'))
S77.WQ.sim=merge(S77.WQ.sim,subset(month.mean,Station.ID=="S77"),"month")
S77.WQ.sim=S77.WQ.sim[order(S77.WQ.sim$Date.EST),]
head(S77.WQ.sim)

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

S308.WQ.sim=data.frame(Date.EST=seq(date.fun("1965-01-15"),date.fun("2020-12-31"),"1 month"))
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

# S77 ---------------------------------------------------------------------
q.dat.xtab$WY=WY(q.dat.xtab$Date)
vars=c("Alt","Date",'WY',"S77","S77_QFC")
S77.q.dat.xtab=q.dat.xtab[,vars]
S77.q.dat.xtab$preReg=with(S77.q.dat.xtab,ifelse(S77==0,0,round((S77_QFC/S77)*100,2)))
range(S77.q.dat.xtab$preReg,na.rm=T)

head(S77.WQ.sim)
vars=c('Date.EST',"sim.TP","sim.TN")
S77.q.dat.xtab=merge(S77.q.dat.xtab,S77.WQ.sim[,vars],by.x="Date",by.y="Date.EST",all.x=T)
S77.q.dat.xtab=S77.q.dat.xtab[order(S77.q.dat.xtab$Alt,S77.q.dat.xtab$Date),]

S77.q.dat.xtab$sim.TP.inter=with(S77.q.dat.xtab,ave(sim.TP,Alt,FUN=function(x) dat.interp(x)))
S77.q.dat.xtab$sim.TN.inter=with(S77.q.dat.xtab,ave(sim.TN,Alt,FUN=function(x) dat.interp(x)))
# plot(sim.TP~Date,subset(S77.q.dat.xtab,Alt=="AA"))
# with(subset(S77.q.dat.xtab,Alt=="AA"),lines(Date,sim.TP.inter))
S77.q.dat.xtab$TP.load=with(S77.q.dat.xtab,Load.Calc.kg(S77,sim.TP.inter))
S77.q.dat.xtab$TN.load=with(S77.q.dat.xtab,Load.Calc.kg(S77,sim.TN.inter))

## 
S77.Load.WY=ddply(subset(S77.q.dat.xtab,WY%in%WYs),c("Alt","WY"),summarise,
                  TFlow=sum(cfs.to.acftd(S77),na.rm=T),
                  TPLoad=sum(TP.load,na.rm=T),
                  TNLoad=sum(TN.load,na.rm=T))
S77.Load.WY$S77.TNFWM=with(S77.Load.WY,(TNLoad/(TFlow*1.233e6))*1e6)
S77.Load.WY$S77.TPFWM=with(S77.Load.WY,(TPLoad/(TFlow*1.233e6))*1e9)
S77.Load.WY$Alt=factor(S77.Load.WY$Alt,levels=alts.sort)

boxplot(TPLoad~Alt,S77.Load.WY)
boxplot(TNLoad~Alt,S77.Load.WY)

boxplot(S77.TNFWM~Alt,S77.Load.WY)
boxplot(S77.TPFWM~Alt,S77.Load.WY)


S77.nut.mod.sum=ddply(S77.Load.WY,"Alt",summarise,
                      mean.Q=mean(TFlow/1000,na.rm=T),
                      mean.TP.load=mean(TPLoad),mean.TN.load=mean(TNLoad),
                      mean.TP.FWM=mean(S77.TPFWM),mean.TN.FWM=mean(S77.TNFWM))
S77.nut.mod.sum$Alt=factor(S77.nut.mod.sum$Alt,levels=alts.sort)
S77.nut.mod.sum=S77.nut.mod.sum[match(S77.nut.mod.sum$Alt,alts.sort),]

S77.nut.mod.sum$Q.FWO.diff=with(S77.nut.mod.sum,(mean.Q-mean.Q[1])/mean.Q[1])*100
S77.nut.mod.sum$TP.FWO.diff=with(S77.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
S77.nut.mod.sum$TN.FWO.diff=with(S77.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100


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

boxplot(TPLoad~Alt,S308.Load.WY)
boxplot(TNLoad~Alt,S308.Load.WY)

boxplot(S308.TNFWM~Alt,S308.Load.WY)
boxplot(S308.TPFWM~Alt,S308.Load.WY)


S308.nut.mod.sum=ddply(S308.Load.WY,"Alt",summarise,
                       mean.Q=mean(TFlow/1000,na.rm=T),
                       mean.TP.load=mean(TPLoad),mean.TN.load=mean(TNLoad),
                       mean.TP.FWM=mean(S308.TPFWM,na.rm=T),mean.TN.FWM=mean(S308.TNFWM,na.rm=T))
S308.nut.mod.sum$Alt=factor(S308.nut.mod.sum$Alt,levels=alts.sort)
S308.nut.mod.sum=S308.nut.mod.sum[match(alts.sort,S308.nut.mod.sum$Alt),]

S308.nut.mod.sum$Q.FWO.diff=with(S308.nut.mod.sum,(mean.Q-mean.Q[1])/mean.Q[1])*100
S308.nut.mod.sum$TP.FWO.diff=with(S308.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
S308.nut.mod.sum$TN.FWO.diff=with(S308.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100


# png(filename=paste0(plot.path,"Post-Iteration_2/S77S308_Flow_Load_Iter2Opt.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:6),3,2,byrow=F))
par(family="serif",mar=c(2,1.5,0.25,1),oma=c(4,3,1.75,0.25),lwd=0.5);

ylim.val=c(-15,15);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(S77.nut.mod.sum$Q.FWO.diff,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(S77.nut.mod.sum$Q.FWO.diff,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
with(S77.nut.mod.sum,text(x,Q.FWO.diff,format(round(Q.FWO.diff,1),nsmall=0),font=2,cex=1.25,
                          pos=ifelse(Q.FWO.diff<0,3,1),offset=0.25))
abline(h=0,lwd=1)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA,cex=0.8,las=2);box(lwd=1)
mtext(side=3,adj=0,"Discharge",font=2,cex=0.75)
mtext(side=3, adj=0,"S-77",line=0.8)

x=barplot(S77.nut.mod.sum$TP.FWO.diff,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(S77.nut.mod.sum$TP.FWO.diff,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
with(S77.nut.mod.sum,text(x,TP.FWO.diff,format(round(TP.FWO.diff,1),nsmall=0),font=2,
                          pos=ifelse(TP.FWO.diff<0,3,1),offset=0.25,cex=1.25,
                          col=ifelse(TP.FWO.diff<0,"forestgreen",ifelse(TP.FWO.diff>0,"red","black"))))
abline(h=0,lwd=1)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA,cex=0.8,las=2);box(lwd=1)
mtext(side=3,adj=0,"TP Load",font=2,cex=0.75)

x=barplot(S77.nut.mod.sum$TN.FWO.diff,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(S77.nut.mod.sum$TN.FWO.diff,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
with(S77.nut.mod.sum,text(x,TN.FWO.diff,format(round(TN.FWO.diff,1),nsmall=0),font=2,
                          pos=ifelse(TN.FWO.diff<0,3,1),offset=0.25,cex=1.25,
                          col=ifelse(TN.FWO.diff<0,"forestgreen",ifelse(TN.FWO.diff>0,"red","black"))))

abline(h=0,lwd=1)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,cex=0.8,las=2);box(lwd=1)
mtext(side=3,adj=0,"TN Load",font=2,cex=0.75)
mtext(side=1,line=3,'Model Alternative')

ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(S308.nut.mod.sum$Q.FWO.diff,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(S308.nut.mod.sum$Q.FWO.diff,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
with(S308.nut.mod.sum,text(x,Q.FWO.diff,format(round(Q.FWO.diff,1),nsmall=0),font=2,cex=1.25,
                           pos=ifelse(Q.FWO.diff<0,3,1),offset=0.25))
abline(h=0,lwd=1)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA,cex=0.8,las=2);box(lwd=1)
mtext(side=3,adj=1,"Discharge",font=2,cex=0.75)
mtext(side=3, adj=1,"S-308",line=0.8)

x=barplot(S308.nut.mod.sum$TP.FWO.diff,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(S308.nut.mod.sum$TP.FWO.diff,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
with(S308.nut.mod.sum,text(x,TP.FWO.diff,format(round(TP.FWO.diff,1),nsmall=0),font=2,
                          pos=ifelse(TP.FWO.diff<0,3,1),offset=0.25,cex=1.25,
                          col=ifelse(TP.FWO.diff<0,"forestgreen",ifelse(TP.FWO.diff>0,"red","black"))))
abline(h=0,lwd=1)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA,cex=0.8,las=2);box(lwd=1)
mtext(side=3,adj=1,"TP Load",font=2,cex=0.75)

x=barplot(S308.nut.mod.sum$TN.FWO.diff,col=NA,border=NA,ylim=ylim.val,space=0,axes=F,ann=F)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(S308.nut.mod.sum$TN.FWO.diff,col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F,add=T)
with(S308.nut.mod.sum,text(x,TN.FWO.diff,format(round(TN.FWO.diff,1),nsmall=0),font=2,
                          pos=ifelse(TN.FWO.diff<0,3,1),offset=0.25,cex=1.25,
                          col=ifelse(TN.FWO.diff<0,"forestgreen",ifelse(TN.FWO.diff>0,"red","black"))))
abline(h=0,lwd=1)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,cex=0.8,las=2);box(lwd=1)
mtext(side=3,adj=1,"TN Load",font=2,cex=0.75)
mtext(side=1,line=3,'Model Alternative')
mtext(side=2,line=1.5,outer=T,"Average Percent Difference to FWO")
mtext(side=1,line=2.5,outer=T,adj=0,"FL WY 1966 - 2016",col="grey",font=3,cex=0.75)
dev.off()



# Lake Backpumping --------------------------------------------------------
q.dat.xtab$WY=WY(q.dat.xtab$Date)
# S2 ----------------------------------------------------------------------
vars=c("Alt","Date",'WY',"S2")
S2.q.dat.xtab=subset(q.dat.xtab,WY%in%WYs)[,vars]

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
S3.q.dat.xtab=subset(q.dat.xtab,WY%in%WYs)[,vars]

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
S4.q.dat.xtab=subset(q.dat.xtab,WY%in%WYs)[,vars]

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
vars=c("Area","Alt","mean.Q","mean.TP.load", "mean.TN.load","Q.FWO.diff","TP.FWO.diff", "TN.FWO.diff")
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

inflows=c("S65E", "FEC", "TOTAL_ISTOK", 
          "S77BF", "S4BP", "S3", "S2", "C12ABP", "C12BP", "C10BP", "C4ABP", 
          "S236", "P5WPS", "S308BF", "TCNSQ", "S154", "S135")

lok.wb.inflow=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(inflows)){
    paths=paste0("/RSMBN/",inflows[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=inflows[i]
    tmp$Alt=alts[j]
    lok.wb.inflow=rbind(tmp,lok.wb.inflow)
    print(i)
  }
}

lok.wb.xtab=reshape2::dcast(lok.wb.inflow,Alt+Date~SITE,value.var = "FLOW",mean)

lok.wb.xtab$TInflow=rowSums(lok.wb.xtab[,inflows],na.rm=T)
lok.wb.xtab$TEAA=rowSums(lok.wb.xtab[,c("S4BP", "S3", "S2")],na.rm=T)
lok.wb.xtab$CY=as.numeric(format(lok.wb.xtab$Date,"%Y"))
lok.wb.xtab$WY=WY(lok.wb.xtab$Date)

lok.wb.xtab.CY=ddply(lok.wb.xtab,c("Alt","CY"),summarise,TFlow.kAcFt=sum(cfs.to.acftd(TInflow)/1000,na.rm=T))
lok.inflow.sum=ddply(lok.wb.xtab.CY,"Alt",summarise,mean.val=mean(TFlow.kAcFt))
lok.inflow.sum=lok.inflow.sum[match(lok.inflow.sum$Alt,alts.sort),]

lok.wb.WY=ddply(subset(lok.wb.xtab,WY%in%WYs),c("Alt","WY"),summarise,
                Inflow=sum(TInflow,na.rm=T),
                EAA.BF=sum(TEAA),
                S77.BF=sum(S77BF),
                S308.BF=sum(S308BF))
lok.wb.WY$S77_per=with(lok.wb.WY,(S77.BF/Inflow)*100)
lok.wb.WY$S308_per=with(lok.wb.WY,(S308.BF/Inflow)*100)
lok.wb.WY$EAA_per=with(lok.wb.WY,(EAA.BF/Inflow)*100)

mean.LakeWB=ddply(lok.wb.WY,"Alt",summarise,
                  mean.Inflow=mean(Inflow),
                  mean.EAA.BF=mean(EAA.BF),
                  mean.S308.BF=mean(S308.BF),
                  mean.S77.BF=mean(S77.BF))
mean.LakeWB$S77_per=with(mean.LakeWB,(mean.S77.BF/mean.Inflow)*100)
mean.LakeWB$S308_per=with(mean.LakeWB,(mean.S308.BF/mean.Inflow)*100)
mean.LakeWB$EAA_per=with(mean.LakeWB,(mean.EAA.BF/mean.Inflow)*100)
mean.LakeWB=mean.LakeWB[match(mean.LakeWB$Alt,alts.sort),]

sum.WB=rbind(data.frame(Area="S77",Alt=mean.LakeWB$Alt,PerWB=mean.LakeWB$S77_per),
             data.frame(Area="S308",Alt=mean.LakeWB$Alt,PerWB=mean.LakeWB$S308_per),
             data.frame(Area="EAA",Alt=mean.LakeWB$Alt,PerWB=mean.LakeWB$EAA_per))

sort.vals=c("S77_NA25", "S77_ECBr", "S77_CC","S77_OPT", "S308_NA25", "S308_ECBr", 
            "S308_CC",'S308_OPT', "EAA_NA25", "EAA_ECBr", "EAA_CC","EAA_OPT")
all.nut.mod.sum=merge(all.nut.mod.sum,sum.WB,c("Area","Alt"),all.x=T)

vars=c("Area", "Alt", "PerWB", "mean.flow", "mean.TP.load", "mean.TN.load", 
       "Q.BF.FWO.diff", "TP.FWO.diff", "TN.FWO.diff")
all.nut.mod.sum=all.nut.mod.sum[,vars]
all.nut.mod.sum=all.nut.mod.sum[match(paste(all.nut.mod.sum$Area,all.nut.mod.sum$Alt,sep="_"),sort.vals),]

# write.csv(all.nut.mod.sum,paste0(export.path,"post_iter2_backpump_QLoad_Iter2Opt.csv"),row.names = F)

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
  hline(i=c(4,8))%>%
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
  set_caption(caption="Average annual load and average percent change relative to FWO (NA25) over the simulation period or record between May 1965 and April 2016 for back flow/pumping from S77, S308 and EAA (S2, S3 and S4) to Lake Okeechobee.")# %>%print("docx")
