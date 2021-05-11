## 
## LOSOM
##
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

# Discharge ---------------------------------------------------------------
RSM.sites=c("S351","S352","S354")
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

q.dat$WY=WY(q.dat$Date)
q.dat=subset(q.dat,WY%in%WYs);# Full Florida WY (MAy - April) 

EAA.q=ddply(subset(q.dat,SITE%in%c("S354","S352","S351")),c("Alt","Date","WY"),summarise,TFlow.cfs=sum(FLOW,na.rm=T))

EAA.q.WY=ddply(EAA.q,c("Alt","WY"),summarise,TFlow=sum(cfs.to.acftd(TFlow.cfs)))

mean(subset(EAA.q.WY,Alt=="LSM25B")$TFlow)/1000
mean(subset(EAA.q.WY,Alt=="LSMECB")$TFlow)/1000

# Observed flow data ------------------------------------------------------
dates=date.fun(c("1978-05-01","2020-04-30"))

# Discharge ---------------------------------------------------------------
flow.dbkeys=read.xlsx("C:/Julian_LaCie/_GitHub/LakeO_Sediment/Data/discharge/LakeO_DBKEYS_V3.xlsx",sheet=1)

flow.dat=data.frame()
for(i in 1:nrow(flow.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],flow.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(flow.dbkeys$DBKEY[i])
  flow.dat=rbind(tmp,flow.dat)
  print(paste(i,": ",flow.dbkeys$DBKEY[i]))
}
# write.csv(flow.dat,paste0(data.path,"discharge/WY1979_2020_dailyQ.csv"),row.names=F)
vars=c("DBKEY","STRUCT","ALIAS","Priority","Basin","Inflow_direction","Outflow","WQSite")
flow.data=merge(flow.dat,flow.dbkeys[,vars],"DBKEY")

flow.data$WY=WY(flow.data$Date)
flow.data$Date.EST=date.fun(flow.data$Date)
flow.xtab=data.frame(cast(flow.data,Date.EST+WY+STRUCT+ALIAS+Inflow_direction+Outflow+Basin+WQSite~Priority,value="Data.Value",fun.aggregate=function(x) ifelse(sum(x,na.rm=T)==0,NA,sum(x,na.rm=T))))

flow.xtab$fflow.cfs=with(flow.xtab,ifelse(is.na(P1),P2,P1));#if only two priorities
flow.xtab$fflow.cfs=with(flow.xtab,fflow.cfs*Inflow_direction)#all inflows are positive and all negative values are outflow
flow.xtab$direct=with(flow.xtab,ifelse(fflow.cfs<0,"Outflow","Inflow"))
# flow.xtab$month=as.numeric(format(flow.xtab$Date,"%m"))
# flow.xtab$CY=as.numeric(format(flow.xtab$Date,"%Y"))

flow.xtab2=cast(flow.xtab,STRUCT+ALIAS+Basin+WQSite+WY+Date.EST~direct,value="fflow.cfs",fun.aggregate=function(x) sum(abs(x),na.rm=T))

subset(flow.dbkeys,Basin=="S"&Outflow==1)
Q.s.sites=c("S352","S2","S3","S351_TEMP","S352_TEMP","S354_TEMP")
# south.out=ddply(subset(flow.mon.sum,Basin=="S"&WY%in%seq(1979,2020,1)),"WY",summarise,Tflow=sum(cfs.to.acftd(Outflow),na.rm=T))
# south.out=cast(subset(flow.xtab2,ALIAS%in%Q.s.sites),WY~ALIAS,value="Outflow",fun.aggregate = function(x)sum(cfs.to.acftd(x),na.rm=T))
south.out=ddply(subset(flow.xtab2,ALIAS%in%Q.s.sites),"WY",summarise,TFlow=sum(cfs.to.acftd(Outflow),na.rm=T))
south.out=merge(south.out,data.frame(WY=2016:2020,WY5mean=mean(subset(south.out,WY%in%seq(2016,2020,1))$TFlow)),all.x=T)
south.out=merge(south.out,data.frame(WY=2008:2020,LORS08mean=mean(subset(south.out,WY%in%seq(2008,2020,1))$TFlow)),all.x=T)

mean(subset(south.out,WY%in%seq(2016,2020,1))$TFlow)/1000
mean(subset(south.out,WY%in%seq(2008,2020,1))$TFlow)/1000

##
south.out_POR=subset(south.out,WY%in%c(1979:2020))
south.out_POR$Alt="Obs.7920"
head(south.out_POR[,c("Alt","WY",'TFlow')])

south.out_LORS08=subset(south.out,WY%in%c(2008:2020))
south.out_LORS08$Alt="Obs.LORS08"
head(south.out_LORS08[,c("Alt","WY",'TFlow')])

head(EAA.q.WY)
tmp=rbind(subset(EAA.q.WY,Alt%in%c("LSM25B","LSMECB")),
                 south.out_POR[,c("Alt","WY",'TFlow')],
                 south.out_LORS08[,c("Alt","WY",'TFlow')])

ddply(tmp,"Alt",summarise,mean.val=mean(TFlow/1000),sd.val=sd(TFlow/1000))%>%
  flextable()%>%autofit%>%print(preview="docx")


cols=c("grey","grey",adjustcolor(wesanderson::wes_palette("Zissou1",2,"continuous"),0.5))
# png(filename=paste0(plot.path,"LOSOM_LakeO_QSouth.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
# layout(matrix(1:2,2,1))
par(family="serif",mar=c(1,2.25,0.5,0.5),oma=c(3,2.5,0.75,0.1));

ylim.val=c(0,15e5);by.y=2.5e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("LSM25B","LSMECB","Observed\nWY1979-2020","Observerd\nWY2008-2020")
boxplot(TFlow~Alt,tmp,ylim=ylim.val,outline=F,axes=F,ann=F,col=cols,yaxs="i")
axis_fun(1,1:4,1:4,xlabs,line=-0.75,padj=1)
axis_fun(2,ymaj,ymin,ymaj/1000)
abline(v=2.5)
box(lwd=1)
mtext(side=3,adj=0,"EAA (S-351 + S-352 + S-354)")
mtext(side=2,line=2.75,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
axis(1,line=1.75,at=1.5,label="LOSOM",lty=0)
axis(1,line=1.75,at=3.5,label="Observed",lty=0)
dev.off()

#### 
## Verification/Troubleshooting Q South
# Q.s.site2=data.frame(SITE=c("S352","S351",'S354'),DBKEY=c(91510,91508,91513))
# Q.south=data.frame()
# for(i in 1:nrow(Q.s.site2)){
# tmp=DBHYDRO_daily(dates[1],dates[2],Q.s.site2$DBKEY[i])
# Q.south=rbind(Q.south,tmp)
# }
# Q.south$WY=WY(Q.south$Date)
# Q.south=merge(Q.south,Q.s.site2,"DBKEY")
# Q.south$Data.Value[Q.south$Data.Value<0]<-NA
# 
# tail(subset(Q.south,DBKEY==91508))
# tail(subset(Q.south,DBKEY==91513))
# tail(subset(Q.south,DBKEY==91510))
# 
# tmp2=cast(Q.south,WY~SITE,value="Data.Value",fun.aggregate = function(x)sum(cfs.to.acftd(x),na.rm=T))
# 
# test=DBHYDRO_daily(dates[1],dates[2],"15018")
# test$WY=WY(test$Date)
# 
# plot(Data.Value~Date,test)
# test$Data.Value[test$Data.Value<0]<-NA
# 
# ddply(test,"WY",summarise,TFlow=sum(cfs.to.acftd(Data.Value),na.rm=T))
