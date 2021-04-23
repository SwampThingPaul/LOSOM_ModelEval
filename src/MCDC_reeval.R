## 
## LOSOM
## MCDA
## Iteration 1 alternative evaluation
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
library(Hmisc)
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
alts=list.files(paste0(data.path,"Iteration_1/Model_Output/"))
# alts=alts[!(alts%in%c("SPEF","SPAS","SPLC"))];# removed the Everglades Foundation, Audubon and Lake Commnuities alteratives
n.alts=length(alts)
alts.sort=c("LSM25B", "LSMECB","ABNE", "ECRE", "ELOK","ESFL", "ESLE", "WAS","WRDC","WRDS","NAV","REC","SPAS","SPEF","SPLC")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

# Lake Stage --------------------------------------------------------------
lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_1/Model_Output/",alts[i],"/RSMBN/RSMBN_output.dss"))  
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(rownames(tmp))
  tmp$Alt=alts[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}

# RECOVER Stage Envelope --------------------------------------------------
## 
# library(LORECOVER)
# 
# head(lakeO.stage)
# lakeO.stage$Data.Value=lakeO.stage$STAGE
# 
# norm.lakeO.stage.scr=data.frame()
# for(i in 1:n.alts){
#   tmp=subset(lakeO.stage,Alt==alts[i])
#   rslt=norm_env(tmp)
#   rslt$Alt=alts[i]
#   norm.lakeO.stage.scr=rbind(norm.lakeO.stage.scr,rslt)
#   print(i)
# }
# norm.lakeO.stage.scr=rename(norm.lakeO.stage.scr,c("score"="norm.score"))
# norm.lakeO.stage.scr$WY=WY(norm.lakeO.stage.scr$Date)
# norm.lakeO.stage.scr$CY=as.numeric(format(norm.lakeO.stage.scr$Date,"%Y"))
# 
# ## MFL
# norm.lakeO.stage.scr$LT11=with(norm.lakeO.stage.scr,ifelse(Data.Value<11,1,0))
# norm.lakeO.stage.scr$LT11.80=with(norm.lakeO.stage.scr,ave(LT11,Alt,FUN=function(x) c(rep(NA,79),rollapply(x,width=80,FUN=function(x)sum(x,na.rm=T)))))
# 
# ddply(norm.lakeO.stage.scr,c("Alt","CY"),summarise,N.val=sum(LT11.80==80,na.rm=T))


RSM.sites=c("S77","S78","S79","S80","S308","S351","S352","S354","S351_FC_SHIFT2_ENVTARG","S354_FC_SHIFT2_ENVTARG","S351_QFC","S352_QFC","S354_QFC","G376G379G381","G335","G436","S77_QFC","S308_QFC")
q.dat=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_1/Model_Output/",alts[j],"/RSMBN/RSMBN_output.dss"))  
  
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

q.dat.xtab=reshape2::dcast(q.dat,Alt+Date~SITE,value.var="FLOW",mean)
q.dat.xtab$hydro.season=FL.Hydroseason(q.dat.xtab$Date)
q.dat.xtab$hydro.season2=with(q.dat.xtab,ifelse(as.numeric(format(Date,"%m"))%in%seq(6,10,1),"A_Wet","B_Dry"))

q.dat.xtab$S79.Q14=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S79.Q30=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,29),rollapply(x,width=30,FUN=function(x)mean(x,na.rm=T)))))
# q.dat.xtab$CRE.opt=with(q.dat.xtab,ifelse(S79.Q14>=750&S79.Q14<2100,1,0))


## WCA3A 
RSM.stg.sites=c("WCA3A_3A-3","WCA3A_3A-2")
RSM.stg.sites=data.frame(SITE=RSM.stg.sites,Station=c("CA3_63","CA3_62"))
wca.stg.dat=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_1/Model_Output/",alts[j],"/RSMGL/RSMGL_output.dss"))  
  
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
wca.stg.dat.xtab=reshape2::dcast(wca.stg.dat,Date+Alt~Station,value.var="STAGE",mean)
wca.stg.dat.xtab$mean_6263=rowMeans(wca.stg.dat.xtab[,c("CA3_62","CA3_63")],na.rm=T)

wca.stg.dat.xtab$low.close=with(wca.stg.dat.xtab,ifelse(mean_6263<9.3,1,0))
wca.stg.dat.xtab$high.close=with(wca.stg.dat.xtab,ifelse(mean_6263>11.6,1,0))

# CRE ---------------------------------------------------------------------
## Current MCDA metric
q.dat.xtab2=q.dat.xtab

# q.dat.xtab2$LT457=with(q.dat.xtab2,ifelse(S79<457,1,0))
q.dat.xtab2$LT457=with(q.dat.xtab2,ifelse(S79<457,1,0))
q.dat.xtab2$GT6500=with(q.dat.xtab2,ifelse(S79>6500,1,0))
q.dat.xtab2$RecOpt=with(q.dat.xtab2,ifelse(S79.Q14>=750&S79<2100,1,0))
q.dat.xtab2$Rng_45006500=with(q.dat.xtab2,ifelse(S79>=4500&S79<6500,1,0))
q.dat.xtab2$Rng_26004500=with(q.dat.xtab2,ifelse(S79>=2600&S79<4500,1,0))

CE.vals=ddply(q.dat.xtab2,"Alt",summarise,
      per.LT457=sum(LT457,na.rm=T)/N.obs(LT457)*100,
      per.GT6500=sum(GT6500)/N.obs(GT6500)*100,
      per.RecOpt=sum(RecOpt,na.rm=T)/N.obs(RecOpt)*100,
      per.Rng_45006500=sum(Rng_45006500)/N.obs(Rng_45006500)*100,
      per.Rng_26004500=sum(Rng_26004500)/N.obs(Rng_26004500)*100)

CE.vals$per.LT457.RS=with(CE.vals,1-(per.LT457/max(per.LT457)))# *0.25
CE.vals$per.GT6500.RS=with(CE.vals,1-(per.GT6500/max(per.GT6500)))# *0.25
CE.vals$per.RecOpt.RS=with(CE.vals,per.RecOpt/max(per.RecOpt))# *0.20
CE.vals$per.Rng_45006500.RS=with(CE.vals,1-(per.Rng_45006500/max(per.Rng_45006500)))# *0.2
CE.vals$per.Rng_26004500.RS=with(CE.vals,1-(per.Rng_26004500/max(per.Rng_26004500)))# *0.1
# CE.vals$Score=rowSums(CE.vals[,7:11])

wts=c(0.25,0.25,0.2,0.2,0.1)
CE.vals$Score=apply(CE.vals[,7:11],1,FUN=function(x) wtd.mean(x,wts))

alts.sort=c("LSM25B", "LSMECB","ABNE", "ECRE", "ELOK","ESFL", "ESLE", "WAS","WRDC","WRDS","NAV","REC","SPAS","SPEF","SPLC")
#c("LSM25B", "LSMECB")
#alts.sort=c("ELOK","ECRE", "ESLE","ESFL","NAV","REC", "WAS","ABNE","SPAS","SPLC","SPEF","WRDS","WRDC")
CE.vals2=CE.vals[match(alts.sort,CE.vals$Alt),c("Alt","Score")]
CE.vals2$Score2=round(CE.vals2$Score/max(CE.vals2$Score),2)
CE.vals2
# CE.vals2%>%flextable()%>%print(preview="docx")

## New MCDA metric
q.dat.xtab3=q.dat.xtab

q.dat.xtab3$LT457=with(q.dat.xtab3,ifelse(S79.Q30<457,1,0))
q.dat.xtab3$RecOpt=with(q.dat.xtab3,ifelse(S79.Q14>=750&S79<2100,1,0))
q.dat.xtab3$GT2600=with(q.dat.xtab3,ifelse(S79>2600,1,0))
# q.dat.xtab3$LT457=with(q.dat.xtab3,ifelse(S79<457,1,0))
# q.dat.xtab3$RecOpt=with(q.dat.xtab3,ifelse(S79>=750&S79<2100,1,0))
# q.dat.xtab3$GT2600=with(q.dat.xtab3,ifelse(S79>2600,1,0))

CE.vals=ddply(q.dat.xtab3,"Alt",summarise,
              per.LT457=sum(LT457,na.rm=T)/N.obs(LT457)*100,
              per.RecOpt=sum(RecOpt,na.rm=T)/N.obs(RecOpt)*100,
              per.GT2600=sum(GT2600)/N.obs(GT2600)*100)

CE.vals$per.LT457.RS=with(CE.vals,1-(per.LT457/max(per.LT457)))
CE.vals$per.RecOpt.RS=with(CE.vals,per.RecOpt/max(per.RecOpt))
CE.vals$per.GT2600.RS=with(CE.vals,1-(per.GT2600/max(per.GT2600)))

wts=c(0.25,0.5,0.25)
CE.vals$Score=apply(CE.vals[,5:7],1,FUN=function(x) wtd.mean(x,wts))

#c("LSM25B", "LSMECB")
# alts.sort=c("ELOK","ECRE", "ESLE","ESFL","NAV","REC", "WAS","ABNE","SPAS","SPLC","SPEF","WRDS","WRDC")
CE.vals3=CE.vals[match(alts.sort,CE.vals$Alt),c("Alt","Score")]
CE.vals3$Score2=round(CE.vals3$Score/max(CE.vals3$Score),2)
CE.vals3
# CE.vals2%>%flextable()%>%print(preview="docx")

CE.vals3=CE.vals3[,c("Alt","Score2")]
colnames(CE.vals3)<-c("Alt","Alt.score")

merge(CE.vals2[,c("Alt","Score2")],CE.vals3,"Alt")#%>%write.csv(paste0(export.path,"CE.MCDA.csv"),row.names = F)

# South Florida Ecology ---------------------------------------------------
head(q.dat.xtab)
q.dat.xtab$S351S354=rowSums(q.dat.xtab[,c("S351","S354")],na.rm=T)
q.dat.xtab$S351S354_QFC=rowSums(q.dat.xtab[,c("S351_QFC","S354_QFC")],na.rm=T)
q.dat.xtab$S351S354_FC=rowSums(q.dat.xtab[,c("S351_FC_SHIFT2_ENVTARG","S354_FC_SHIFT2_ENVTARG")],na.rm=T)
q.dat.xtab$STA2_3_4=rowSums(q.dat.xtab[,c("G335","G436","G376G379G381")],na.rm=T)
q.dat.xtab$CY=as.numeric(format(q.dat.xtab$Date,"%Y"))

ESFL.vals=ddply(q.dat.xtab,c("CY","Alt"),summarise,dry.Q.south=sum(ifelse(hydro.season=="B_Dry",cfs.to.acftd(S351S354_FC),NA),na.rm=T),
                                 wet.Q.south=sum(ifelse(hydro.season=="A_Wet",cfs.to.acftd(S351S354_FC),NA),na.rm=T),
                                 STA.Q=sum(cfs.to.acftd(STA2_3_4)))
ESFL.vals=ddply(ESFL.vals,"Alt",summarise,dry.Q.south=mean(dry.Q.south/1000,na.rm=T),
                wet.Q.south=mean(wet.Q.south/1000,na.rm=T),
                STA.Q=mean(STA.Q/1000))
ESFL.vals
ESFL.vals$dry.Q.south.RS=with(ESFL.vals,(dry.Q.south/max(dry.Q.south)))
ESFL.vals$wet.Q.south.RS=with(ESFL.vals,(wet.Q.south/max(wet.Q.south)))
ESFL.vals$STA.Q.rs=with(ESFL.vals,(STA.Q/max(STA.Q)))
ESFL.vals.alt=ESFL.vals

wts=c(0.5,0.25,0.25)
ESFL.vals$Score=apply(ESFL.vals[,5:7],1,FUN=function(x) Hmisc::wtd.mean(x,wts))

ESFL.vals2=ESFL.vals[match(alts.sort,ESFL.vals$Alt),c("Alt","Score")]
ESFL.vals2$Score2=round(ESFL.vals2$Score/max(ESFL.vals2$Score),1)
ESFL.vals2

## New MCDA metric
wca3.sum=ddply(wca.stg.dat.xtab,c("Alt"),summarise,N.low=sum(low.close),N.high=sum(high.close))
ESFL.vals.alt=merge(ESFL.vals.alt,wca3.sum,"Alt")
ESFL.vals.alt$N.low.RS=with(ESFL.vals.alt,1-(N.low/max(N.low)))
ESFL.vals.alt$N.high.RS=with(ESFL.vals.alt,1-(N.high/max(N.high)))

wts=c(0.4,0.10,0.10,0.20,0.20)
sum(wts)
vars=c("dry.Q.south.RS","wet.Q.south.RS","STA.Q.rs","N.low.RS","N.high.RS")
ESFL.vals.alt$Score=apply(ESFL.vals.alt[,vars],1,FUN=function(x) Hmisc::wtd.mean(x,wts))
ESFL.vals.alt2=ESFL.vals.alt[match(alts.sort,ESFL.vals.alt$Alt),c("Alt","Score")]
ESFL.vals.alt2$Score2=round(ESFL.vals.alt2$Score/max(ESFL.vals.alt2$Score),1)
ESFL.vals.alt2

ESFL.vals.alt2=ESFL.vals.alt2[,c("Alt","Score2")]
colnames(ESFL.vals.alt2)<-c("Alt","Alt.score")

merge(ESFL.vals2[,c("Alt","Score2")],ESFL.vals.alt2,"Alt")#%>%write.csv(paste0(export.path,"SFL.MCDA.csv"),row.names = F)

# HAB risk ----------------------------------------------------------------
alts.sort=c("LSM25B", "LSMECB","ABNE", "ECRE", "ELOK","ESFL", "ESLE", "WAS","WRDC","WRDS","NAV","REC","SPAS","SPEF","SPLC")
q.dat$month=as.numeric(format(q.dat$Date,"%m"))
q.dat$CY=as.numeric(format(q.dat$Date,"%Y"))
# q.dat$sumr.period=with(q.dat,ifelse(month%in%seq(5,8,1),1,0))
## Adjusted summer period
q.dat$sumr.period=with(q.dat,ifelse(SITE%in%c("S308","S308_QFC","S80")&month%in%seq(5,8,1),1,
                                    ifelse(SITE%in%c("S77","S77_QFC","S79")&month%in%seq(6,8,1),1,0)))


sum.flows=reshape::cast(subset(q.dat,sumr.period==1),SITE+CY~Alt,value="FLOW",fun.aggregate=function(x)sum(cfs.to.acftd(x),na.rm=T))
sum.flows=sum.flows[,c("SITE","CY",alts.sort)]

# head(subset(sum.flows,SITE=="S308_QFC"))%>%
#   flextable()%>%
#   colformat_double(j=2,digits=0,na_str="---",big.mark = "")%>%
#   colformat_double(j=3:17,digits=2,na_str="---",big.mark = "")%>%
#   fontsize(size=9,part="body")%>%
#   fontsize(size=10,part="header")%>%
#   padding(padding=0.5,part="all")%>%
#   font(fontname="Times New Roman",part="all")#%>%print(preview="docx")

hab.risk.fun=function(Alt,FWO){
  ifelse(Alt==0,1,
         ifelse((Alt-FWO)>=0,0,
                ifelse((Alt-FWO)<0,0.5,NA)))

}
with(subset(sum.flows,SITE=="S308_QFC"),hab.risk.fun(ABNE,LSM25B))

for(i in 1:length(alts.sort)){
  tmp=data.frame(val=hab.risk.fun(sum.flows[,alts.sort[i]],sum.flows[,"LSM25B"]))
  val.name=paste0(alts.sort[i],".risk")
  colnames(tmp)<-val.name
  sum.flows=cbind(sum.flows,tmp)
}
head(subset(sum.flows,SITE=="S308_QFC")[,c("SITE","CY","ABNE.risk")])

vars=c("SITE","CY",paste0(alts.sort,".risk"))
sum.flows.melt=reshape::melt(sum.flows[,vars],id.vars=vars[1:2])
# sum.flows.melt=merge(sum.flows.melt,data.frame(value=c(0,0.5,1),class=c("HIGH","MODERATE","LOW")),"value")
sum.flows.melt=merge(sum.flows.melt,data.frame(variable=paste0(alts.sort,".risk"),Alt=alts.sort),"variable")

sum.tble=cast(sum.flows.melt,Alt~SITE,value="value",sum,na.rm=T)[,c("Alt","S308","S308_QFC","S80","S77","S77_QFC","S79")]
sum.tble[match(alts.sort,sum.tble$Alt),]%>%
  flextable()%>%
  colformat_double(j=2:7,digits=1,na_str="---",big.mark = "")%>%
  fontsize(size=9,part="body")%>%
  fontsize(size=10,part="header")%>%
  padding(padding=0.5,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%autofit()#%>%print(preview="docx")

sum.tble$S308_QFC.RS=with(sum.tble,S308_QFC/max(S308_QFC))
sum.tble$S80.RS=with(sum.tble,S80/max(S80))
sum.tble$S77_QFC.RS=with(sum.tble,S77_QFC/max(S77_QFC))
sum.tble$S79.RS=with(sum.tble,S79/max(S79))


sum.tble[,c("Alt","S308_QFC.RS","S77_QFC.RS")]
sum.tble[,c("Alt","S80.RS","S79.RS")]

wts=c(0.5,0.5)
vars=c("S308_QFC.RS","S77_QFC.RS")
sum.tble$Score.QFC=apply(sum.tble[,vars],1,FUN=function(x) Hmisc::wtd.mean(x,wts))
sum.tble$Score.QFC.RS=with(sum.tble,Score.QFC/max(Score.QFC))

vars=c("S80.RS","S79.RS")
sum.tble$Score.est=apply(sum.tble[,vars],1,FUN=function(x) Hmisc::wtd.mean(x,wts))
sum.tble$Score.est.RS=with(sum.tble,Score.est/max(Score.est))

# sum.tble[,c("Alt","Score.QFC.RS","Score.est.RS")]#%>%write.csv(paste0(export.path,"HAB.MCDA.csv"),row.names = F)
sum.tble[,c("Alt","S308_QFC.RS","S77_QFC.RS","S80.RS","S79.RS")]#%>%write.csv(paste0(export.path,"HAB.MCDA.csv"),row.names = F)
