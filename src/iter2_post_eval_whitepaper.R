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

library(lmom)
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


# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
alts=alts[!alts%in%c("_Batch_Results","Northern_Estuaries","Iteration2_STL_Flows_13Jul2021.xlsx")]
#n.alts=length(alts)
n.alts=length(alts[!(alts%in%c("OPT","OPT-S2"))])
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2","SR3.5")

cols=c("grey50","grey80",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-3,"continuous")),"deeppink")
cols=cols[alts.sort%in%c("NA25","ECBr","CC")]
# cols=c("grey50","grey80","#E6C019","darkorchid1","darkorchid3")

alts=c("NA25","ECBr","CC")
alts.sort=c("NA25","ECBr","CC")
n.alts=length(alts.sort)
# Discharge ---------------------------------------------------------------
RSM.sites=c("S79","S80","S80_QPFCSOURCE_LAKE","S79_QPFCSOURCE_LAKE",
            "S77","S308","S77_QFC","S308_QFC","S308BF","S77BF",
            "TMC2EST","S48","S49","NSF2EST","S2","S3","S4BP","C10A","C10A_QFC","S155A","S155A_QFC")

q.dat=data.frame()
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
q.dat.xtab$CRE.high3.LOK.count=0
q.dat.xtab$CRE.high3.basin.count=0
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


tmp=data.frame(Date=seq(date.fun("1965-01-01"),date.fun("2016-12-31"),"1 day"))
# tmp$D14.period=as.numeric(format(tmp$Date,'%j'))%/%15L+1L
tmp$D14.period=(1:nrow(tmp))%/%14L+1L
# nrow(tmp)
tmp[13:15,]

q.dat.xtab=merge(q.dat.xtab,tmp,"Date")
# unique(q.dat.xtab$D14.period)
# length(subset(q.dat.xtab,D14.period==1)$D14.period)

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
    tmp$CRE.high3.LOK.count[i]=with(tmp,
                                    ifelse(CRE.high3.count[i]==1,
                                           ifelse((S79.14d[i]-S79_QPFCSOURCE_LAKE.14d[i])<=6500,1,0),0))
    tmp$CRE.high3.basin.count[i]=with(tmp,CRE.high3.count[i]-CRE.high3.LOK.count[i])
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

recover.consec=ddply(q.dat.xtab2,c("Alt","D14.period"),summarise,SLE.opt.count=sum(SLE.opt.count,na.rm=T))
range(recover.consec$SLE.opt.count)
subset(recover.consec,CRE.high3.count>1)


vars.CRE=paste("CRE",c("low.count","opt.count","high.LOK.count","dam.LOK.count","high3.count"),sep=".")
# apply(tmp[,vars],2,FUN=function(x)sum(x,na.rm=T))
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars.CRE)],id.vars = "Alt")
CRE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
CRE.SalEnv_count=CRE.SalEnv_count[match(alts.sort,CRE.SalEnv_count$Alt),]

CRE.SalEnv_count$perFWO.low=with(CRE.SalEnv_count,(CRE.low.count-CRE.low.count[1])/CRE.low.count[1])*100
CRE.SalEnv_count$perFWO.opt=with(CRE.SalEnv_count,(CRE.opt.count-CRE.opt.count[1])/CRE.opt.count[1])*100
CRE.SalEnv_count$perFWO.LOK.stress=with(CRE.SalEnv_count,(CRE.high.LOK.count-CRE.high.LOK.count[1])/CRE.high.LOK.count[1])*100
CRE.SalEnv_count$perFWO.LOK.dam=with(CRE.SalEnv_count,(CRE.dam.LOK.count-CRE.dam.LOK.count[1])/CRE.dam.LOK.count[1])*100
CRE.SalEnv_count$perFWO.ext=with(CRE.SalEnv_count,(CRE.high3.count-CRE.high3.count[1])/CRE.high3.count[1])*100
CRE.SalEnv_count[,c('Alt',"perFWO.low","perFWO.opt","perFWO.LOK.stress","perFWO.LOK.dam","perFWO.ext")]

CRE.SalEnv_count2=subset(CRE.SalEnv_count,Alt%in%c("NA25","CC"))

CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
cols=c("grey50","#E6C019")
# png(filename=paste0(plot.path,"WhitePaper/CRE_RECOVER_SalEnv.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:10),2,5,byrow=T))
par(family="serif",mar=c(1,2,1,1),oma=c(2,2,2,1),lwd=0.5);

ymax=c(800,800,300,200,80)
yval=ymax/2
for(i in 2:6){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(CRE.SalEnv_count2[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,NA)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count2[,i],round(CRE.SalEnv_count2[,i],0),font=2,col="black",pos=1,cex=0.75,offset=0.25)
  if(i==2){mtext(side=3,adj=0,line=-1.25," CRE")}
  if(i==2){mtext(side=2,line=2.5,"Count of 14-Day Periods")}
}
# mtext(side=4,line=0.5,"Caloosahatchee")


for(i in 7:11){
  ylim.val=c(-40,80);ymaj=seq(ylim.val[1],ylim.val[2],40);ymin=seq(ylim.val[1],ylim.val[2],40)
  x=barplot(CRE.SalEnv_count2[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  abline(h=0)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,c("NA25","CC"),cex=0.8,las=2)
  box(lwd=1)
  mtext(side=3,adj=0,CRE.labs[i-1],cex=0.7)
  text(x,CRE.SalEnv_count2[,i],round(CRE.SalEnv_count2[,i],1),font=2,col="black",pos=1,cex=0.75,offset=0.25)
  if(i==7){mtext(side=2,line=2.5,"Average % Difference to FWO")}
}
dev.off()

# Daily Counts ------------------------------------------------------------
# CRE
q.dat.xtab$da.CRE.low=with(q.dat.xtab,ifelse(S79<750,1,0)) # RECOVER Low
q.dat.xtab$da.CRE.opt=with(q.dat.xtab,ifelse(S79>=750&S79<2100,1,0)) # RECOVER Optimum
q.dat.xtab$da.CRE.high=with(q.dat.xtab,ifelse(S79>=2100&S79<2600,1,0)) # RECOVER Stress
q.dat.xtab$da.CRE.high.LOK=with(q.dat.xtab,ifelse(da.CRE.high==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=2100,1,0),0))
q.dat.xtab$da.CRE.high.Basin=with(q.dat.xtab,da.CRE.high-da.CRE.high.LOK)
q.dat.xtab$da.CRE.high3=with(q.dat.xtab,ifelse(S79>=6500,1,0))
q.dat.xtab$da.CRE.high3.LOK=with(q.dat.xtab,ifelse(da.CRE.high3==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=6500,1,0),0))
q.dat.xtab$da.CRE.high3.Basin=with(q.dat.xtab,da.CRE.high3-da.CRE.high3.LOK)

q.dat.xtab$da.CRE.dam=with(q.dat.xtab,ifelse(S79>=2600,1,0)) # RECOVER Damaging
q.dat.xtab$da.CRE.dam.LOK=with(q.dat.xtab,ifelse(da.CRE.dam==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=2600,1,0),0))
q.dat.xtab$da.CRE.dam.Basin=with(q.dat.xtab,da.CRE.dam-da.CRE.dam.LOK)
q.dat.xtab$da.CRE.stressdam=with(q.dat.xtab,ifelse(S79>=2100,1,0)) # RECOVER Stress
q.dat.xtab$da.CRE.stressdam.LOK=with(q.dat.xtab,ifelse(da.CRE.stressdam==1,ifelse((S79-S79_QPFCSOURCE_LAKE)<=2100,1,0),0))

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


# cre.stressdam.sum=dcast(q.dat.xtab,CY~Alt,value.var="da.CRE.stressdam.LOK",sum,na.rm=T)
# cre.stressdam.sum$FWO.per=with(cre.stressdam.sum,ifelse(NA25==0,0,(CC-NA25)/NA25))*100
# cre.stressdam.sum$CC.plot=with(cre.stressdam.sum,ifelse(CC>0&FWO.per>0,1,NA))
# plot(CC.plot~CY,cre.stressdam.sum,ylim=c(0,2))
# with(cre.stressdam.sum,segments(CY,rep(0,length(CY)),CY,CC.plot))
     
cre.dam.sum=ddply(subset(q.dat.xtab,Alt%in%c("CC","NA25")),c("Alt","CY"),summarise,N.val=sum(da.CRE.stressdam.LOK))
cre.dam.sum$dam.cat=with(cre.dam.sum,ifelse(N.val<15,1,
                                            ifelse(N.val>=15&N.val<30,2,
                                                   ifelse(N.val>=30&N.val<60,3,
                                                          ifelse(N.val>=60&N.val<90,4,
                                                                 ifelse(N.val>=90,5,0))))))

cre.dam.sum$dam.cat=as.character(cre.dam.sum$dam.cat)
cols.rmp=colorRampPalette(c("forestgreen","yellow","indianred1"))(5)
cols.rmp=adjustcolor(colorRampPalette(c("red","grey","blue"))(5),0.5)

cols=c("1"=cols.rmp[1],"2"=cols.rmp[2],"3"=cols.rmp[3],"4"=cols.rmp[4],"5"=cols.rmp[5])
CRE.DAM=ggplot(cre.dam.sum, aes(x = CY, y = Alt, fill = dam.cat)) +
  geom_tile(aes(group = dam.cat), colour = 'black')+
  # geom_text(aes(label=opt.outcome),size=2.5,colour="grey50",family="serif")+
  # scale_x_reverse(expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(1965,2016,10))+
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = cols,
                    name="Stress & Damaging\nDaily Discharge\nEvents (Days)",
                    breaks=1:5,
                    labels=c("<15","15 - 30","30 - 60","60 - 90",">90")) +
  theme_bw() +
  theme(
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8)
  )+
  labs(title = "S-79 Discharges",
       subtitle = "Simulation Period of Record 1965 - 2016",
       caption = "LOSOM Planning: RSM-BN Iteration 2\nConsecutive and non-consecutive days",
       x="Calendar Year",
       y=" ")
CRE.DAM
#ggsave(paste0(plot.path,"WhitePaper/CRE_da_StressDam_ggplot.png"),CRE.DAM,device="png",height =2.5,width=7,units="in")
cre.dam.sum$dam.cat=with(cre.dam.sum,ifelse(N.val<15,1,
                                            ifelse(N.val>=15&N.val<30,2,
                                                   ifelse(N.val>=30&N.val<60,3,
                                                          ifelse(N.val>=60&N.val<90,4,
                                                                 ifelse(N.val>=90,5,0))))))
cre.dam.sum$Alt.plt=with(cre.dam.sum,ifelse(Alt=="CC",1,2))
cols=adjustcolor(colorRampPalette(c("red","grey","blue"))(5),0.5)
cols=viridis::viridis(5,alpha=0.5)

alts.val=c("NA25",'CC')
xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0.5,2.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"WhitePaper/CRE_da_StressDam.png"),width=7,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,0.5),oma=c(3,2,1,1),lwd=0.1);
layout(matrix(c(1:2),1,2,byrow=F),widths=c(1,0.25))

plot(Alt.plt~CY,cre.dam.sum,xlim=c(xlim.val[1]-0.5,xlim.val[2]+0.5),ylim=ylim.val,type="n",xaxs="i",yaxs="i",axes=F,ann=F)
for(i in 1965:2016){
  for(j in 1:2){
  tmp=subset(cre.dam.sum,Alt==alts.val[j]&CY==i)
  rect(tmp$CY-0.5,tmp$Alt.plt+0.5,tmp$CY+0.5,tmp$Alt.plt-0.5,col=cols[tmp$dam.cat])
  }
}
axis_fun(2,1:2,1:2,rev(alts.val))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
mtext(side=3,adj=0,line=1,"S-79 Discharges")
mtext(side=3,adj=0,"Simulation Period of Record 1965 - 2016",cex=0.75,col="grey50")
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=2,line=3,"Model Alternative")
mtext(side=1,outer=T,adj=1,line=1.75,"LOSOM Planning: RSM-BN Iteration 2\nConsecutive and non-consecutive days",cex=0.75)

plot(0:1,0:1,ann=F,axes=F,type="n")
labs=c("<15","15 - 30","30 - 60","60 - 90",">90")
n=5
y.top.s=0.75
y.bot.e=0
segs=seq(y.bot.e,y.top.s,(y.top.s-y.bot.e)/n)
ybot.vals=head(segs,-1)
ytop.vals=tail(segs,-1)
rect(
  0,
  ybot.vals,
  0.25,
  ytop.vals,
  col=rev(cols)
)
text(x=0.25, y = ybot.vals+diff(segs)/2, labels = rev(labs),cex=0.75,pos=4)
text(x=0.125,y=0.75,adj=0,pos=3,labels="Stress & Damaging\nDaily Discharge\nEvents (Days)",xpd=NA,cex=0.75)
dev.off()


## Return Frequency (Flood frequency)
ann.maxQ=ddply(q.dat.xtab,c("Alt","CY"),summarise,maxQ.S79=max(S79,na.rm=T),maxQ.S79Lake=max(S79_QPFCSOURCE_LAKE,na.rm=T))
tmp.CC=recur.fun(subset(ann.maxQ,Alt=="CC")$maxQ.S79)
tmp.dry.CC=recur.fun(subset(ann.maxQ,Alt=="CC"&CY<=1994)$maxQ.S79)
tmp.wet.CC=recur.fun(subset(ann.maxQ,Alt=="CC"&CY>1994)$maxQ.S79)

plot(dat.val~emp.rec.tim,tmp.CC)
with(tmp.CC,lines(LP3,dat.val,col=adjustcolor("dodgerblue1",0.75),lwd=2))
with(tmp.dry.CC,lines(LP3,dat.val,col=adjustcolor("indianred1",0.75),lwd=2,lty=2))
with(tmp.wet.CC,lines(LP3,dat.val,col=adjustcolor("palegreen3",0.75),lwd=2,lty=2))
abline(h=6500)

subset(tmp,dat.val>=6500)
subset(tmp.dry.CC,dat.val>=6500)
subset(tmp.wet.CC,dat.val>=6500)


tmp.NA25=recur.fun(subset(ann.maxQ,Alt=="NA25")$maxQ.S79)
plot(dat.val~emp.rec.tim,tmp.CC)
with(tmp.NA25,points(emp.rec.tim,dat.val,pch=21,bg="grey"))
with(tmp.NA25,lines(LP3,dat.val,col=adjustcolor("grey",0.75),lwd=2))
with(tmp.CC,points(emp.rec.tim,dat.val,pch=21,bg="dodgerblue1"))
with(tmp.CC,lines(LP3,dat.val,col=adjustcolor("dodgerblue1",0.75),lwd=2))

subset(tmp.NA25,dat.val>=6500)
subset(tmp.CC,dat.val>=6500)

q.dat.xtab$month=as.numeric(format(q.dat.xtab$Date,"%m"))
tmp=ddply(q.dat.xtab,c("Alt","CY","month"),summarise,max.q=max(S79))
test=recur.fun(subset(tmp,Alt=="CC")$max.q)
test$emp.rec.tim.yr=test$emp.rec.tim/12
test$LP3.yr=test$LP3/12
tail(subset(test,dat.val>=6500))
plot(dat.val~emp.rec.tim.yr,test)
with(test,lines(LP3.yr,dat.val,col=adjustcolor("dodgerblue1",0.75),lwd=2))



q.dat.xtab$climate.period=with(q.dat.xtab,ifelse(CY<=1994,"dry","wet"))
CRE.extreme.sum=ddply(q.dat.xtab,c("Alt","CY","climate.period"),summarise,high3.all=sum(da.CRE.high3),high3.LOK=sum(da.CRE.high3.LOK),high3.basin=sum(da.CRE.high3.Basin))
CRE.extreme.sum$ext.all.cat=with(CRE.extreme.sum,ifelse(high3.all<15,1,
                                            ifelse(high3.all>=15&high3.all<30,2,
                                                   ifelse(high3.all>=30&high3.all<60,3,
                                                          ifelse(high3.all>=60&high3.all<90,4,
                                                                 ifelse(high3.all>=90,5,0))))))

CRE.extreme.sum$Alt.plt=with(CRE.extreme.sum,ifelse(Alt=="CC",1,2))
alts.val=c("NA25",'CC')
xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0.5,2.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"WhitePaper/CRE_da_Ext.png"),width=7,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,0.5),oma=c(3,2,1,1),lwd=0.1);
layout(matrix(c(1:2),1,2,byrow=F),widths=c(1,0.25))

plot(Alt.plt~CY,CRE.extreme.sum,xlim=c(xlim.val[1]-0.5,xlim.val[2]+0.5),ylim=ylim.val,type="n",xaxs="i",yaxs="i",axes=F,ann=F)
for(i in 1965:2016){
  for(j in 1:2){
    tmp=subset(CRE.extreme.sum,Alt==alts.val[j]&CY==i)
    rect(tmp$CY-0.5,tmp$Alt.plt+0.5,tmp$CY+0.5,tmp$Alt.plt-0.5,col=cols[tmp$ext.all.cat])
  }
}
axis_fun(2,1:2,1:2,rev(alts.val))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5);box(lwd=1)
mtext(side=3,adj=0,line=1,"S-79 Discharges")
mtext(side=3,adj=0,"Simulation Period of Record 1965 - 2016",cex=0.75,col="grey50")
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=2,line=3,"Model Alternative")
mtext(side=1,outer=T,adj=1,line=1.75,"LOSOM Planning: RSM-BN Iteration 2\nConsecutive and non-consecutive days",cex=0.75)

plot(0:1,0:1,ann=F,axes=F,type="n")
labs=c("<15","15 - 30","30 - 60","60 - 90",">90")
n=5
y.top.s=0.75
y.bot.e=0
segs=seq(y.bot.e,y.top.s,(y.top.s-y.bot.e)/n)
ybot.vals=head(segs,-1)
ytop.vals=tail(segs,-1)
rect(
  0,
  ybot.vals,
  0.25,
  ytop.vals,
  col=rev(cols)
)
text(x=0.25, y = ybot.vals+diff(segs)/2, labels = rev(labs),cex=0.75,pos=4)
text(x=0.125,y=0.75,adj=0,pos=3,labels="Stress & Damaging\nDaily Discharge\nEvents (Days)",xpd=NA,cex=0.75)
dev.off()


plot(high3.all~CY,subset(CRE.extreme.sum,Alt=="CC"),type="l")
with(subset(CRE.extreme.sum,Alt=="NA25"),lines(CY,high3.all,col="red"))

subset(CRE.extreme.sum,Alt=="CC")
boxplot(high3.all~climate.period,CRE.extreme.sum,outline=F)

# Lake O Stage ------------------------------------------------------------

lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/RSMBN_output.dss"))  
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

lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,"%Y"))
## All Data
hist(lakeO.stage$STAGE)
(length(subset(lakeO.stage,Alt=="CC"&STAGE>17)$STAGE)/nrow(lakeO.stage))*100


ann.peak=ddply(lakeO.stage,c("CY","Alt"),summarise,max.stg=max(STAGE,na.rm=T))
ann.peak$GT17=with(ann.peak,ifelse(round(max.stg,1)>=16.9,1,0))
subset(ann.peak,Alt=="CC"&GT17==1)
subset(ann.peak,Alt=="CC")
subset(ann.peak,Alt=="NA25"&GT17==1)


xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,1.1);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"WhitePaper/LOK_GT17_timeline.png"),width=7,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.5,1.25,0.5),oma=c(3,1,1,1),lwd=0.1);

plot(GT17~CY,ann.peak,xlim=c(xlim.val[1],xlim.val[2]+1),ylim=ylim.val,type="n",xaxs="i",yaxs="i",axes=F,ann=F)
with(subset(ann.peak,Alt=="CC"),segments(CY,0,CY,GT17,lwd=5,lend=2,col="grey"))
# with(subset(ann.peak,Alt=="NA25"),segments(CY,0,CY,GT17,lwd=5,lend=2,col="red"))
# with(subset(ann.peak,Alt=="ECBr"),segments(CY,0,CY,GT17,lwd=5,lend=2,col="blue"))
lines(c(1965,2016),c(0.5,0.5),col="green",lwd=5,lend=2)

tmp=subset(ann.peak,Alt=="CC"&GT17==1)
for(i in 1:nrow(tmp)){
  with(tmp[i,],lines(c(CY,CY+4),c(0.5,0.5),col="red",lwd=5,lend=2))  
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
box(lwd=1)
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=3,adj=0,line=1,"Lake Okeechobee")
mtext(side=3,adj=0,cex=0.75,"Annual maximum stage \u2265 17Ft NVGD29*",col="grey50")
mtext(side=1,adj=1,line=1.75,outer=T,"LOSOM Iteration 2: Alternative CC\nRecovery years (red) identified.",cex=0.8)
dev.off()

## added >=16.9 like Paul Grey's analysis for comparison

## https://rpubs.com/cassiorampinelli/528388
library(lmom)

#Sorting maxima by decreasing order
sorted.maximas<-sort(round(subset(ann.peak,Alt=="CC")$max.stg,1),decreasing=T)

#Computing the empirical probabilities
p<-(c(1:length(sorted.maximas)))/(length(sorted.maximas)+1)

#Computing the recurrence time
tr<-1/p

#Estimating the parameters for Gumbel distribution
fit<-samlmu(subset(ann.peak,Alt=="CC")$max.stg)
para<-pelgum(fit)
para
# library(dgumbel)
# 1/pgumbel(17,para[1],para[2])

#Estimating the parameters for Log Pearson type 3 distribution
para3<-pelpe3(fit)
para3

#Plotting empirical recurrence time and discharges
plot(tr,sorted.maximas,xlab="Recurrence Time (years)",ylab="Stage (Ft)",ylim=c(10,20),xlim=c(0,100))
grid()
abline(h=17)

#Fitting recurrence time employing Gumbel distribution
y<-c(sorted.maximas)
gumbel.accum<-cdfgum(y,para)
fitted.tr<-1/(1-gumbel.accum)
lines(fitted.tr,y,col="blue",lty=2)
data.frame(fitted.tr,y)



recur.all.CC=recur.fun(subset(ann.peak,Alt=="CC")$max.stg)
recur.dry.CC=recur.fun(subset(ann.peak,Alt=="CC"&CY<=1994)$max.stg)
recur.wet.CC=recur.fun(subset(ann.peak,Alt=="CC"&CY>1994)$max.stg)

subset(recur.all.CC,dat.val>=17)
subset(recur.dry.CC,dat.val>=17)
subset(recur.wet.CC,dat.val>=17)

# png(filename=paste0(plot.path,"WhitePaper/StgRecurrance_AppA.png"),width=5.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,0.5),oma=c(1,1,1,0.25));

ylim.val=c(12,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,30);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(dat.val~emp.rec.tim,recur.all.CC,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",yaxs="i",xaxs="i")
abline(v=xmaj,h=ymaj,lty=1,col=adjustcolor("grey",0.5))
abline(v=xmin,h=ymin,lty=2,col=adjustcolor("grey",0.25))
with(recur.all.CC,points(emp.rec.tim,dat.val,pch=21,bg=adjustcolor("grey",0.5),col=adjustcolor("black",0.5),lwd=0.1,cex=1))
with(recur.all.CC,lines(LP3,dat.val,col=adjustcolor("dodgerblue1",0.75),lwd=2))
with(recur.dry.CC,lines(LP3,dat.val,col=adjustcolor("indianred1",0.75),lwd=2,lty=2))
with(recur.wet.CC,lines(LP3,dat.val,col=adjustcolor("palegreen3",0.75),lwd=2,lty=2))
abline(h=17,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.25,"Return Period (years)")
mtext(side=2,line=2,"Max Annual Stage (ft, NGVD29)")
mtext(side=3,adj=0,line=1,"Lake Okeechobee")
mtext(side=3,adj=0,cex=0.75,"LOSOM Iteration 2: Alternative CC",col="grey50")

legend("bottomright",legend=c("Empirical Dist. (Observed Data)",
                              "Theoretical Dist. (All Data; 1965 - 2016)",
                              "Theoretical Dist. (Dry Phase; 1965 - 1994)",
                              "Theoretical Dist. (Wet Phase; 1995 - 2016)"),
       pch=c(21,NA,NA,NA),lty=c(0,1,2,2),lwd=c(0.1,2,2,2),
              col=adjustcolor(c("black","dodgerblue1","indianred1","palegreen3"),0.75),
              pt.bg=c(adjustcolor("grey",0.5),rep(NA,3)),
              pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

## https://stats.stackexchange.com/a/71279
dgumbel <- function(x,mu,s){exp((mu - x)/s - exp((mu - x)/s))/s}
pgumbel <- function(q,mu,s){exp(-exp(-((q - mu)/s)))}
qgumbel <- function(p, mu, s){mu-s*log(-log(p))}

library(fitdistrplus)
plotdist(subset(ann.peak,Alt=="CC")$max.stg)

gumbel.fit.CC <- fitdist(subset(ann.peak,Alt=="CC")$max.stg, "gumbel", start=list(mu=5, s=5), method="mle")

summary(gumbel.fit.CC)
gofstat(gumbel.fit.CC, discrete=FALSE) 
plot(gumbel.fit.CC)

pgumbel(92,86.15,2.65)
1/pgumbel(17,14.459180,1.525164)

## Emp and Theoretical CDFs
h=hist(subset(ann.peak,Alt=="CC")$max.stg)
s=sort(subset(ann.peak,Alt=="CC")$max.stg)
obsp=ppoints(s)

plot(s,obsp)
xmin <- h$breaks[1]
xmax <- h$breaks[length(h$breaks)]
sfin <- seq(xmin, xmax, by = (xmax - xmin)/100)
para=list(mu=gumbel.fit.CC$estimate[1], s=gumbel.fit.CC$estimate[2])
pdistname="pgumbel"
theopfin <- do.call(pdistname, c(list(sfin), as.list(para)))
lines(sfin, theopfin, lty = 1, col = "red")

test=data.frame(stg=sfin,theop.gumbel=theopfin,recur=1/(1-theopfin))

plot(1/(1-obsp),s,ylim=c(10,20),xlim=c(0,30))
with(test,lines(recur,stg))

###
pgumbel2=function(z,u,s,ep){
  exp(-(1+ep*((z-u)/s)^-1/ep))
}
1/pgumbel2(92,86.15,2.65,-0.03)


# https://www.dataanalysisclassroom.com/lesson60/
# library(extRemes)
# 
# fatigue=read.table("http://www.dataanalysisclassroom.com/wp-content/uploads/2018/04/cycles_fatigue.txt",header=F)
# temperature_data=read.csv("https://www.dataanalysisclassroom.com/wp-content/uploads/2018/03/cp_temperature.csv",header=T)
# 
# fatigue_cycles = as.matrix(fatigue)
# 
# fit = fevd(fatigue_cycles,type="GEV")
# summary(fit)









# Climate - AMO -----------------------------------------------------------
# AMO data https://psl.noaa.gov/data/timeseries/AMO/
vars=c('year',month.abb)
row.count=length(seq(1856,2021,1))
noaa.amo.path="https://psl.noaa.gov/data/correlation/amon.us.long.data"

# AMO.dat=read.table("https://psl.noaa.gov/data/correlation/amon.us.long.data",header=F,skip=1,col.names=vars,nrows=row.count,na.string="-99.990")
AMO.dat=read.table("https://psl.noaa.gov/data/correlation/amon.sm.long.data",header=F,skip=1,col.names=vars,nrows=row.count,na.string="-99.990")
AMO.dat.melt=melt(AMO.dat,id.vars="year")
AMO.dat.melt=merge(AMO.dat.melt,data.frame(variable=month.abb,month=1:12))
AMO.dat.melt$Date.mon=with(AMO.dat.melt,date.fun(paste(year,month,"01",sep="-")))
AMO.dat.melt=AMO.dat.melt[order(AMO.dat.melt$Date.mon),c("Date.mon","value")]
AMO.dat.melt$warm=with(AMO.dat.melt,ifelse(value>0,value,0))
AMO.dat.melt$dry=with(AMO.dat.melt,ifelse(value<0,value,0))
AMO.dat.melt$ma=with(AMO.dat.melt,c(rep(NA,120),zoo::rollapply(value,width=121,FUN=function(x)mean(x,na.rm=T))))
head(AMO.dat.melt)
tail(AMO.dat.melt)

# png(filename=paste0(plot.path,"WhitePaper/Kaplan_AMO.png"),width=6.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(1,2,1,0.25));

ylim.val=c(-0.3,0.2);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1861-01-01","2016-12-01"));xmaj=seq(xlim.val[1],xlim.val[2],"20 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ymaj[4]=0

plot(value~Date.mon,AMO.dat.melt,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,xaxs="i")
abline(v=xmaj,h=ymaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(AMO.dat.melt,is.na(value)==F),shaded.range(Date.mon,rep(0,length(Date.mon)),ifelse(value>0,value,0),"indianred1",lty=1))
with(subset(AMO.dat.melt,is.na(value)==F),shaded.range(Date.mon,ifelse(value<0,value,0),rep(0,length(Date.mon)),"dodgerblue1",lty=1))
abline(h=0)
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
abline(v=date.fun(c("1965-01-01","2016-12-31")))
# text(date.fun(date.fun("1965-01-01")+diff(date.fun(c("1965-01-01","2016-12-31")))/2),ylim.val[2],"RSM P.O.S.",font=2,cex=0.75)
text(date.fun(date.fun("1965-01-01")+diff(date.fun(c("1965-01-01","2016-12-31")))/2),ylim.val[2],"Planning Period",font=2,cex=0.75)
mtext(side=3,adj=0,"AMO Index - smoothed")

mtext(side=2,line=2.5,"Observed AMO Index")
mtext(side=1,line=2,"Date (Year)")
mtext(side=1,outer=T,adj=1,"Kaplan SST dataset\nN Atlantic temp (0 to 70N)\nData Source: NOAA",cex=0.75)
dev.off()
