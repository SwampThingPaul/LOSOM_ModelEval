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
cols=c("grey50","grey80","#E6C019","darkorchid1","darkorchid3")

alts=c("NA25","ECBr","CC","OPT","OPT-S2")
alts.sort=c("NA25","ECBr","CC","OPT","OPT-S2")
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


# CRE MFL -----------------------------------------------------------------
q.dat.xtab$mfl.exceed=with(q.dat.xtab,ifelse(is.na(S79.30d)==T,0,ifelse(S79.30d<457,1,0)))
CRE.mfl.rslt=data.frame()
q.dat1.xtab.mfl=data.frame()
for(j in 1:n.alts){
  
  tmp=subset(q.dat.xtab,Alt==alts.sort[j])
  ## Adapted from mflst_cre_v2.py
  for(i in 2:nrow(tmp)){
    if(tmp$mfl.exceed[i-1]==1&tmp$mfl.exceed[i]==0){
      tmp$exceed_end[i-1]=1 #found the last exceedance dates 
    }else{
      tmp$exceed_end[i-1]=0
    }
  }
  
  # subset(tmp,exceed_end==1)
  
  tmp$countdown=0
  tmp$exceed2=NA
  counts=0
  exc_n=0
  for(i in 30:nrow(tmp)){
    # rest counts
    if(tmp$mfl.exceed[i-1]==0&tmp$mfl.exceed[i]==1){
      counts=1
    }
    
    if(tmp$exceed_end[i]==1){
      if(tmp$countdown[i-1]<1){
        tmp$countdown[i]=365
      }else{
        tmp$countdown[i]=tmp$countdown[i-1]-1
        if(tmp$countdown[i]==0 & tmp$mfl.exceed[i]==1){
          tmp$countdown[i]=365
        }
      }
    }else{
      tmp$countdown[i]=tmp$countdown[i-1]-1
      counts=counts+1
      
      if(counts>366 & tmp$mfl.exceed[i]==1){
        tmp$countdown[i]=365
        counts=0
      }
      if(tmp$countdown[i]==0 & tmp$mfl.exceed[i]==1){
        tmp$countdown[i]=365
      }
    }
    
    #identify yearly violations
    if(tmp$countdown[i]<0){
      if(tmp$mfl.exceed[i]==1){
        if(tmp$mfl.exceed[i-1]!=1){
          tmp$exceed2[i]=1
          exc_n=exc_n+1}else{
            tmp$exceed2[i]=0
          }
      }else{tmp$exceed2[i]=0}
    }else{
      if(tmp$countdown[i]==365 & tmp$exceed_end[i]==0){
        tmp$exceed2[i]==1
        exc_n=exc_n+1
      }else{
        tmp$exceed2[i]=0
      }
    }
  }
  
  
  counts
  exc_n
  
  CRE.mfl.rslt=rbind(CRE.mfl.rslt,data.frame(Alt=alts.sort[j],N.exceed=exc_n))
  q.dat1.xtab.mfl=rbind(q.dat1.xtab.mfl,tmp)
  print(j)
}

q.dat1.xtab.mfl$plot_exc=with(q.dat1.xtab.mfl,ifelse(countdown<0&mfl.exceed==1,S79.30d,NA))
q.dat1.xtab.mfl$plot_exc365=with(q.dat1.xtab.mfl,ifelse(countdown>0&mfl.exceed==1,S79.30d,NA))

# i=5
# # png(filename=paste0(plot.path,"Post-Iteration_2/CRE_MFL_Alt_",alts.sort[i],".png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
# layout(matrix(1:2,2,1),heights=c(1,0.2))
# par(family="serif",mar=c(1,3,0.75,1),oma=c(1.5,1,2.5,0.5));
# 
# ylim.val=c(0,15000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# xlim.val=date.fun(c("1965-01-01","2017-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
# 
# plot(S79.30d~Date,q.dat1.xtab.mfl,type="n",yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F)
# abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
# abline(h=457,col="brown",lty=2)
# with(subset(q.dat1.xtab.mfl,Alt==alts.sort[i]),lines(Date,S79.30d,col="blue",lty=1.5))
# with(subset(q.dat1.xtab.mfl,Alt==alts.sort[i]),lines(Date,plot_exc,col="orange",lty=1.5))
# with(subset(q.dat1.xtab.mfl,Alt==alts.sort[i]),lines(Date,plot_exc365,col="grey",lty=1.5))
# axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
# axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=2,line=3,"Discharge (cfs)")
# mtext(side=1,line=1.5,"Year")
# mtext(side=3,paste0("MFL Recovery Water Body - Caloosahatchee River 30 Day Averege Flow at S79\n",
#                     alts.sort[i]," : ",subset(CRE.mfl.rslt,Alt==alts.sort[i])$N.exceed,
#                     " exceedance in 52 years of simualtion"))
# plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
# legend(0.5,-0.5,legend=c("30-day Moving Average","MFL Criteria (457 cfs)","Exceedance","Exceedance w/in 365 Days"),
#        pch=NA,
#        lty=c(1,2,1,1),lwd=2,
#        col=c("blue","brown","orange","grey"),
#        pt.bg=NA,
#        pt.cex=1.5,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
# dev.off()


CRE.mfl.rslt$NA25_perchange=with(CRE.mfl.rslt,round(((N.exceed-N.exceed[1])/N.exceed[1])*100,2))

# png(filename=paste0(plot.path,"Post-Iteration_2/CREMFL_Iter2Opt.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,50);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(2,2,0.5,0.25),oma=c(1,2,0.75,1),lwd=0.5);

x=barplot(CRE.mfl.rslt$N.exceed,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,length(alts.sort)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(CRE.mfl.rslt$N.exceed,beside=F,ylim=ylim.val,col=adjustcolor(cols,0.5),axes=F,ann=F,names.arg=rep(NA,length(alts.sort)),add=T)
text(x,CRE.mfl.rslt$N.exceed,CRE.mfl.rslt$N.exceed,pos=3)
text(x,CRE.mfl.rslt$N.exceed,paste0(CRE.mfl.rslt$NA25_perchange,"%"),pos=1,font=3)
axis_fun(1,x,x,alts.sort,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"MFL Exceedances")
mtext(side=1,line=1.75,"Alternative")
mtext(side=3,adj=0,"Caloosahatchee MFL")
mtext(side=3,adj=1,"CY 1965 - 2016")
dev.off()
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

tmp=subset(SalEnv_count.melt,Alt%in%alts.sort&cat%in%c("high","dam"))
tmp$cat_source=with(tmp,paste(cat,source,sep="_"))

tmp2=reshape2::dcast(tmp,region+Alt~cat_source,value.var = "value",mean)
tmp3=subset(tmp2,region=="CRE")
tmp3=merge(tmp3,subset(mean.ann.Q,Alt%in%alts.sort)[,c("Alt",'S77.kacft')],"Alt")
tmp3=rename(tmp3,c("S77.kacft"="RegFlow"))
tmp3=tmp3[match(tmp3$Alt,alts.sort),]
tmp3$PerFWO_dam_basin=with(tmp3,(dam_basin-dam_basin[1])/dam_basin[1]*100)
tmp3$PerFWO_dam_LOK=with(tmp3,(dam_LOK-dam_LOK[1])/dam_LOK[1]*100)
tmp3$PerFWO_high_basin=with(tmp3,(high_basin-high_basin[1])/high_basin[1]*100)
tmp3$PerFWO_high_LOK=with(tmp3,(high_LOK-high_LOK[1])/high_LOK[1]*100)
tmp3$PerFWO_RegFlow=with(tmp3,(RegFlow-RegFlow[1])/RegFlow[1]*100)

tmp4=subset(tmp2,region=="SLE")
tmp4=merge(tmp4,subset(mean.ann.Q,Alt%in%alts.sort)[,c("Alt",'S308.kacft')],"Alt")
tmp4=rename(tmp4,c("S308.kacft"="RegFlow"))
tmp4=tmp4[match(tmp4$Alt,alts.sort),]
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


##### Consecutive days 
# q.dat.xtab$da.CRE.high3
# q.dat.xtab$da.SLE.high2
extremeQ_consec=data.frame()
for(j in 1:length(alts.sort)){
  tmp=subset(q.dat.xtab,Alt==alts.sort[j])
  tmp$CRE.Q6500=0
  tmp$SLE.Q4000=0
  for(i in 2:nrow(tmp)){
    tmp$CRE.Q6500[i]=with(tmp,ifelse(da.CRE.high3[i-1]==0&da.CRE.high3[i]>0,1,
                                 ifelse(da.CRE.high3[i-1]>0&da.CRE.high3[i]>0,1,0)))
    
  }
  for(i in 2:nrow(tmp)){
    tmp$SLE.Q4000[i]=with(tmp,ifelse(da.SLE.high2[i-1]==0&da.SLE.high2[i]>0,1,
                                     ifelse(da.SLE.high2[i-1]>0&da.SLE.high2[i]>0,1,0)))
    
  }
  
  CRE.highQ=consec.startend(tmp$CRE.Q6500>0)
  tmp$sum.CRE.Q6500=0
  for(i in 1:length(CRE.highQ$ends)){
    tmp[CRE.highQ$ends[i],]$sum.CRE.Q6500=with(tmp[c(CRE.highQ$starts[i]:CRE.highQ$ends[i]),],sum(CRE.Q6500,na.rm=T))
  }
  
  SLE.highQ=consec.startend(tmp$SLE.Q4000>0)
  tmp$sum.SLE.Q4000=0
  for(i in 1:length(SLE.highQ$ends)){
    tmp[SLE.highQ$ends[i],]$sum.SLE.Q4000=with(tmp[c(SLE.highQ$starts[i]:SLE.highQ$ends[i]),],sum(SLE.Q4000,na.rm=T))
  }
  
  extremeQ_consec=rbind(tmp,extremeQ_consec)
  print(j)
}
extremeQ_consec

rslt.CREHighQ=reshape2::dcast(extremeQ_consec,sum.CRE.Q6500~Alt,value.var = "sum.CRE.Q6500",fun.aggregate = function(x)N.obs(x))
rslt.CREHighQ=ddply(extremeQ_consec,c("Alt","sum.CRE.Q6500"),summarise,count.event=N.obs(sum.CRE.Q6500))
rslt.CREHighQ$cat=with(rslt.CREHighQ,ifelse(sum.CRE.Q6500>0&sum.CRE.Q6500<14,1,
                                      ifelse(sum.CRE.Q6500>=14&sum.CRE.Q6500<30,2,
                                             ifelse(sum.CRE.Q6500>=30&sum.CRE.Q6500<60,3,
                                                    ifelse(sum.CRE.Q6500>=60&sum.CRE.Q6500<90,4,
                                                           ifelse(sum.CRE.Q6500>=90,5,NA))))))
rslt.SLEHighQ=reshape2::dcast(extremeQ_consec,sum.SLE.Q4000~Alt,value.var = "sum.SLE.Q4000",fun.aggregate = function(x)N.obs(x))
rslt.SLEHighQ=ddply(extremeQ_consec,c("Alt","sum.SLE.Q4000"),summarise,count.event=N.obs(sum.SLE.Q4000))
rslt.SLEHighQ$cat=with(rslt.SLEHighQ,ifelse(sum.SLE.Q4000>0&sum.SLE.Q4000<14,1,
                                            ifelse(sum.SLE.Q4000>=14&sum.SLE.Q4000<30,2,
                                                   ifelse(sum.SLE.Q4000>=30&sum.SLE.Q4000<60,3,
                                                          ifelse(sum.SLE.Q4000>=60&sum.SLE.Q4000<90,4,
                                                                 ifelse(sum.SLE.Q4000>=90,5,NA))))))


rslt.CREHigh.sum=reshape2::dcast(subset(rslt.CREHighQ,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.SLEHigh.sum=reshape2::dcast(subset(rslt.SLEHighQ,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)
# make filler values for 5th category
tmp=rslt.SLEHigh.sum[4,]
tmp[,2:ncol(tmp)]<-0
tmp$cat=5
rslt.SLEHigh.sum=rbind(rslt.SLEHigh.sum,tmp)

# png(filename=paste0(plot.path,"Post-Iteration_2/Est_highQ_events_Iter2Opt.png"),width=6.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,2,0.25),lwd=0.5);
layout(matrix(c(1:10),5,2,byrow=F))

ylim.val=c(0,250);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("< 14", "14 - 30","30 - 60","60 - 90","> 90")
for(i in 1:length(alts.sort)){
  x=barplot(rslt.CREHigh.sum[,alts.sort[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.CREHigh.sum)),space=c(0),yaxs="i",xaxs="i")
  text(x,rslt.CREHigh.sum[,alts.sort[i]],rslt.CREHigh.sum[,alts.sort[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(alts.sort)){axis_fun(1,x,x,xlabs,line=-0.5)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  mtext(side=3,line=-1.25,adj=0,paste0(" Alt: ",alts.sort[i]),font=2,cex=0.75)
  if(i==1){mtext(side=3,adj=0,"CRE Extreme (> 6500 cfs)")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")
mtext(side=2,line=0.5,outer=T,"Number of Events")

ylim.val=c(0,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:length(alts.sort)){
  x=barplot(rslt.SLEHigh.sum[,alts.sort[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.SLEHigh.sum)),space=c(0),yaxs="i",xaxs="i")
  text(x,rslt.SLEHigh.sum[,alts.sort[i]],rslt.SLEHigh.sum[,alts.sort[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(alts.sort)){axis_fun(1,x,x,xlabs,line=-0.5)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  #mtext(side=3,line=-1.25,adj=0,paste0(" Alt: ",alts.sort[i]),font=2)
  if(i==1){mtext(side=3,adj=0,"SLE Extreme (> 4000 cfs)")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")

dev.off()



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

x=barplot(t(tmp),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp),beside=F,col=cols2,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,n.alts),add=T)
with(regq.dat.CY.mean,text(x,WCAs/2,round(WCAs,0),cex=0.75,col="white"))
with(regq.dat.CY.mean,text(x,WCAs+(((Cal+WCAs)-WCAs)/2),round(regq.dat.CY.mean$Cal,0),cex=0.75))
with(regq.dat.CY.mean,text(x,(WCAs+Cal)+(((Cal+WCAs+StL)-(Cal+WCAs))/2),round(regq.dat.CY.mean$StL,0),cex=0.75))
with(regq.dat.CY.mean,text(x,Cal+WCAs+StL+LWLagoon,round(regq.dat.CY.mean$LWLagoon,0),pos=3,offset=0.1,cex=0.75))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,alts.sort,line=-0.25,las=2);box(lwd=1)
mtext(side=2,line=3,"Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=3,"Alternatives")

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
vars=c("Alt","Date",'WY',"S77","S77BF","S77_QFC")
S77.q.dat.xtab=q.dat.xtab[,vars]
S77.q.dat.xtab$preReg=with(S77.q.dat.xtab,ifelse(S77==0,0,round((S77_QFC/S77)*100,2)))
range(S77.q.dat.xtab$preReg,na.rm=T)

head(S77.WQ.sim)
vars=c('Date.EST',"sim.TP","sim.TN")
S77.q.dat.xtab=merge(S77.q.dat.xtab,S77.WQ.sim[,vars],by.x="Date",by.y="Date.EST",all.x=T)
S77.q.dat.xtab=S77.q.dat.xtab[order(S77.q.dat.xtab$Alt,S77.q.dat.xtab$Date),]

S77.q.dat.xtab=S77.q.dat.xtab[order(S77.q.dat.xtab$Alt,S77.q.dat.xtab$Date),]
S77.q.dat.xtab$sim.TP.inter=with(S77.q.dat.xtab,ave(sim.TP,Alt,FUN=function(x) dat.interp(x)))
S77.q.dat.xtab$sim.TN.inter=with(S77.q.dat.xtab,ave(sim.TN,Alt,FUN=function(x) dat.interp(x)))
# plot(sim.TP~Date,subset(S77.q.dat.xtab,Alt=="AA"))
# with(subset(S77.q.dat.xtab,Alt=="AA"),lines(Date,sim.TP.inter))
S77.q.dat.xtab$TP.load=with(S77.q.dat.xtab,Load.Calc.kg(S77,sim.TP.inter))
S77.q.dat.xtab$TN.load=with(S77.q.dat.xtab,Load.Calc.kg(S77,sim.TN.inter))
S77.q.dat.xtab$S77BF.TP.load=with(S77.q.dat.xtab,Load.Calc.kg(S77BF,sim.TP.inter))
S77.q.dat.xtab$S77BF.TN.load=with(S77.q.dat.xtab,Load.Calc.kg(S77BF,sim.TN.inter))

## 
S77.Load.WY=ddply(subset(S77.q.dat.xtab,WY%in%WYs),c("Alt","WY"),summarise,
                  TFlow=sum(cfs.to.acftd(S77),na.rm=T),
                  TPLoad=sum(TP.load,na.rm=T),
                  TNLoad=sum(TN.load,na.rm=T),
                  TFlow.BF=sum(cfs.to.acftd(S77BF),na.rm=T),
                  TPLoad.S77BF=sum(S77BF.TP.load,na.rm=T),
                  TNLoad.S77BF=sum(S77BF.TN.load,na.rm=T))
S77.Load.WY$S77.TNFWM=with(S77.Load.WY,(TNLoad/(TFlow*1.233e6))*1e6)
S77.Load.WY$S77.TPFWM=with(S77.Load.WY,(TPLoad/(TFlow*1.233e6))*1e9)
S77.Load.WY$Alt=factor(S77.Load.WY$Alt,levels=alts.sort)

boxplot(TPLoad~Alt,S77.Load.WY)
boxplot(TNLoad~Alt,S77.Load.WY)

boxplot(S77.TNFWM~Alt,S77.Load.WY)
boxplot(S77.TPFWM~Alt,S77.Load.WY)


S77.nut.mod.sum=ddply(S77.Load.WY,"Alt",summarise,
                      mean.Q=mean(TFlow/1000,na.rm=T),
                      mean.Q.BF=mean(TFlow.BF/1000,na.rm=T),
                      mean.TP.load=mean(TPLoad),mean.TN.load=mean(TNLoad),
                      mean.TP.FWM=mean(S77.TPFWM),mean.TN.FWM=mean(S77.TNFWM),
                      mean.TP.load.BF=mean(TPLoad.S77BF),mean.TN.load.BF=mean(TNLoad.S77BF))
# write.csv(S77.nut.mod.sum,paste0(export.path,"S77_load.csv"),row.names = F)
S77.nut.mod.sum$Alt=factor(S77.nut.mod.sum$Alt,levels=alts.sort)

S77.nut.mod.sum$Q.FWO.diff=with(S77.nut.mod.sum,(mean.Q-mean.Q[1])/mean.Q[1])*100
S77.nut.mod.sum$TP.FWO.diff=with(S77.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
S77.nut.mod.sum$TN.FWO.diff=with(S77.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100
S77.nut.mod.sum$Q.BF.FWO.diff=with(S77.nut.mod.sum,(mean.Q.BF-mean.Q.BF[1])/mean.Q.BF[1])*100
S77.nut.mod.sum$TP.FWO.BF.diff=with(S77.nut.mod.sum,(mean.TP.load.BF-mean.TP.load.BF[1])/mean.TP.load.BF[1])*100
S77.nut.mod.sum$TN.FWO.BF.diff=with(S77.nut.mod.sum,(mean.TN.load.BF-mean.TN.load.BF[1])/mean.TN.load.BF[1])*100


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

S308.nut.mod.sum=ddply(S308.Load.WY,"Alt",summarise,
                       mean.Q=mean(TFlow/1000),mean.Q.BF=mean(TFlow.BF/1000),
                       mean.TP.load=mean(TPLoad),mean.TN.load=mean(TNLoad),
                       mean.TP.FWM=mean(S308.TPFWM,na.rm=T),mean.TN.FWM=mean(S308.TNFWM,na.rm=T),
                       mean.TP.load.BF=mean(TPLoad.BF),mean.TN.load.BF=mean(TNLoad.BF))
S308.nut.mod.sum$Alt=factor(S308.nut.mod.sum$Alt,levels=alts.sort)

S308.nut.mod.sum$Q.FWO.diff=with(S308.nut.mod.sum,(mean.Q-mean.Q[1])/mean.Q[1])*100
S308.nut.mod.sum$Q.BF.FWO.diff=with(S308.nut.mod.sum,(mean.Q.BF-mean.Q.BF[1])/mean.Q.BF[1])*100
S308.nut.mod.sum$TP.FWO.diff=with(S308.nut.mod.sum,(mean.TP.load-mean.TP.load[1])/mean.TP.load[1])*100
S308.nut.mod.sum$TN.FWO.diff=with(S308.nut.mod.sum,(mean.TN.load-mean.TN.load[1])/mean.TN.load[1])*100
S308.nut.mod.sum$TP.FWO.BF.diff=with(S308.nut.mod.sum,(mean.TP.load.BF-mean.TP.load.BF[1])/mean.TP.load.BF[1])*100
S308.nut.mod.sum$TN.FWO.BF.diff=with(S308.nut.mod.sum,(mean.TN.load.BF-mean.TN.load.BF[1])/mean.TN.load.BF[1])*100

S308.nut.mod.sum$TP.FWM.FWO.diff=with(S308.nut.mod.sum,(mean.TP.FWM-mean.TP.FWM[1])/mean.TP.FWM[1])*100
S308.nut.mod.sum$TN.FWM.FWO.diff=with(S308.nut.mod.sum,(mean.TN.FWM-mean.TN.FWM[1])/mean.TN.FWM[1])*100


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
vars=c("Area","Alt","mean.Q.BF","mean.TP.load.BF", "mean.TN.load.BF","Q.BF.FWO.diff","TP.FWO.BF.diff", "TN.FWO.BF.diff")
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


# LOK Water Budget --------------------------------------------------------
## MDS LKTFPL C10ABK HpmDelta?
outflows=c("NELKSH_WS_QWS","NLKSH_WS_QWS","ISTOK_WS_QWS","S77","S4_WS",
           "C10A","C12A","C12","C12A","S352","S352","C4A","C3","S354",
           "BRIGHTON_WS","S308")

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

sort.vals=c("S77_NA25", "S77_ECBr", "S77_CC","S77_OPT","S77_OPT-S2", "S308_NA25", "S308_ECBr", 
            "S308_CC",'S308_OPT',"S308_OPT-S2", "EAA_NA25", "EAA_ECBr", "EAA_CC","EAA_OPT","EAA_OPT-S2")
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
  hline(i=c(5,10))%>%
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


# LOK Stage ---------------------------------------------------------------

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

lakeO.stage$month=as.numeric(format(lakeO.stage$Date,'%m'))
lakeO.stage$day=as.numeric(format(lakeO.stage$Date,'%d'))
lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,'%Y'))
lakeO.stage$WY=WY(lakeO.stage$Date)
lakeO.stage$low.stg=with(lakeO.stage,ifelse(STAGE<11,1,0))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$High.stg=with(lakeO.stage,ifelse(STAGE>16,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))

# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_totalDays_Iter2Opt.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,3,0.25,1),oma=c(1,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,4000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,5000);by.x=1000;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
days.POS=ddply(lakeO.stage,"Alt",summarise,sum.low=sum(low.stg),sum.vlow=sum(vlow.stg),sum.High=sum(High.stg),sum.vHigh=sum(vHigh.stg))
days.POS$Alt=factor(days.POS$Alt,level=alts.sort)
plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(days.POS,points(sum.High,sum.low,pch=19,col=adjustcolor("dodgerblue1",0.5),lwd=0.1))
text(days.POS$sum.High,days.POS$sum.low,days.POS$Alt,font=2,col=adjustcolor("black",0.75),cex=0.75)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u003E 16 ft NGVD29")
mtext(side=2,line=2.75,"Days < 11 ft NGVD29")
mtext(side=3,adj=0, "Cumulative days over the enitre period of simulation")

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,1500);by.x=500;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

plot(sum.low~sum.High,days.POS,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(days.POS,points(sum.vHigh,sum.vlow,pch=19,col=adjustcolor("dodgerblue1",0.5),lwd=0.1))
text(days.POS$sum.vHigh,days.POS$sum.vlow,days.POS$Alt,font=2,col=adjustcolor("black",0.75),cex=0.75)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Days \u2265 17 ft NGVD29")
mtext(side=2,line=2.75,"Days \u2264 10 ft NGVD29")
dev.off()

# Dam Safety --------------------------------------------------------------
LORS.zoneA=data.frame(Date=date.fun(c("2008-01-01","2008-04-01","2008-06-01",
                                      "2008-09-15","2008-11-01","2008-12-31")),
                      ZoneA=c(17.25,17.25,16.00,16.50,17.25,17.25))
LORS.zoneA=merge(data.frame(Date=seq(date.fun("2008-01-01"),
                                     date.fun("2008-12-31"),'1 days')),
      LORS.zoneA,"Date",all.x=T)
LORS.zoneA$ZoneA=na.approx(LORS.zoneA$ZoneA)
LORS.zoneA$month=as.numeric(format(LORS.zoneA$Date,'%m'))
LORS.zoneA$day=as.numeric(format(LORS.zoneA$Date,'%d'))

lakeO.stage=merge(lakeO.stage,LORS.zoneA[c("month","day","ZoneA")],c("month","day"),all.x=T)
lakeO.stage=lakeO.stage[order(lakeO.stage$Alt,lakeO.stage$Date),]

ddply(subset(lakeO.stage,CY%in%seq(1965,2005,1)),
      "Alt",summarise,
      N_GT1725=sum(STAGE>17.25,na.rm=T), #537 days
      N_ZoneA=sum(STAGE>ZoneA,na.rm=T), #830 days
      N_GT18=sum(STAGE>18,na.rm=T)) #20 days 


# Consecutive  -------------------------------------------------------------
highstg_consec=data.frame()
for(j in 1:n.alts){
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


# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_highstg_events_Iter2Opt.png"),width=6.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,2,0.25),lwd=0.5);
layout(matrix(c(1:10),5,2,byrow=F))

ylim.val=c(0,20);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("< 15", "15 - 30","30 - 60","60 - 90","90 - 180", "> 180")
for(i in 1:length(alts.sort)){
x=barplot(rslt.stg16.sum[,alts.sort[i]],beside=T,ylim=ylim.val,
        col=adjustcolor(cols[i],0.5),ann=F,axes=F,
        names.arg = rep(NA,nrow(rslt.stg16.sum)),space=c(0,0),yaxs="i",xaxs="i")
text(x,rslt.stg16.sum[,alts.sort[i]],rslt.stg16.sum[,alts.sort[i]],pos=3)
axis_fun(2,ymaj,ymin,ymaj)
if(i==length(alts.sort)){axis_fun(1,x,x,xlabs,line=-0.5)}else{axis_fun(1,x,x,NA)}
box(lwd=1)
mtext(side=3,line=-1.25,adj=0,paste0(" Alt: ",alts.sort[i]),font=2,cex=0.75)
if(i==1){mtext(side=3,adj=0,"Daily Stage > 16 Ft NGVD")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")
mtext(side=2,line=0.5,outer=T,"Number of Events")

ylim.val=c(0,20);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("< 15", "15 - 30","30 - 60","60 - 90")
for(i in 1:length(alts.sort)){
  x=barplot(rslt.stg17.sum[,alts.sort[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.stg17.sum)),space=c(0),yaxs="i",xaxs="i")
  text(x,rslt.stg17.sum[,alts.sort[i]],rslt.stg17.sum[,alts.sort[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(alts.sort)){axis_fun(1,x,x,xlabs,line=-0.5)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  #mtext(side=3,line=-1.25,adj=0,paste0(" Alt: ",alts.sort[i]),font=2)
  if(i==1){mtext(side=3,adj=0,"Daily Stage \u2265 17 Ft NGVD")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")

dev.off()

lowstg_consec=data.frame()
for(j in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts.sort[j])
  tmp$stg11=0
  tmp$stg10=0
  for(i in 2:nrow(tmp)){
    # tmp$consec[i]=with(tmp,ifelse(Q_GT2100[i-1]>0&Q_GT2100[i]>0,1,0))
    tmp$stg11[i]=with(tmp,ifelse(low.stg[i-1]==0&low.stg[i]>0,1,
                                 ifelse(low.stg[i-1]>0&low.stg[i]>0,1,0)))
    tmp$stg10[i]=with(tmp,ifelse(vlow.stg[i-1]==0&vlow.stg[i]>0,1,
                                 ifelse(vlow.stg[i-1]>0&vlow.stg[i]>0,1,0)))
    
  }
  
 lowstg=consec.startend(tmp$stg11>0)
  tmp$sum.stg11=0
  for(i in 1:length(lowstg$ends)){
    tmp[lowstg$ends[i],]$sum.stg11=with(tmp[c(lowstg$starts[i]:lowstg$ends[i]),],sum(stg11,na.rm=T))
  }
  
  vlowstg=consec.startend(tmp$stg10>0)
  tmp$sum.stg10=0
  for(i in 1:length(vlowstg$ends)){
    tmp[vlowstg$ends[i],]$sum.stg10=with(tmp[c(vlowstg$starts[i]:vlowstg$ends[i]),],sum(stg10,na.rm=T))
  }
  
  lowstg_consec=rbind(tmp,lowstg_consec)
  print(j)
}
rslt.stg11=reshape2::dcast(lowstg_consec,sum.stg11~Alt,value.var = "sum.stg11",fun.aggregate = function(x)N.obs(x))

rslt.stg11=ddply(lowstg_consec,c("Alt","sum.stg11"),summarise,count.event=N.obs(sum.stg11))
max(rslt.stg11$sum.stg11)
rslt.stg11$cat=with(rslt.stg11,ifelse(sum.stg11>0&sum.stg11<30,1,
                                      ifelse(sum.stg11>=30&sum.stg11<90,2,
                                             ifelse(sum.stg11>=90&sum.stg11<180,3,
                                                    ifelse(sum.stg11>=180&sum.stg11<365,4,
                                                           ifelse(sum.stg11>=365,5,NA))))))
rslt.stg10=ddply(lowstg_consec,c("Alt","sum.stg10"),summarise,count.event=N.obs(sum.stg10))
max(rslt.stg10$sum.stg10)
rslt.stg10$cat=with(rslt.stg10,ifelse(sum.stg10>0&sum.stg10<30,1,
                                      ifelse(sum.stg10>=30&sum.stg10<90,2,
                                             ifelse(sum.stg10>=90&sum.stg10<180,3,
                                                    ifelse(sum.stg10>=180&sum.stg10<365,4,
                                                           ifelse(sum.stg10>=365,5,NA))))))

rslt.stg11.sum=reshape2::dcast(subset(rslt.stg11,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.stg10.sum=reshape2::dcast(subset(rslt.stg10,is.na(cat)==F),cat~Alt,value.var="count.event",sum,na.rm=T)

# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_lowstg_events_Iter2Opt.png"),width=6.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,2,0.25),lwd=0.5);
layout(matrix(c(1:10),5,2,byrow=F))

ylim.val=c(0,15);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("< 30", "30 - 90","90 - 180","180 - 365","> 365")
for(i in 1:length(alts.sort)){
  x=barplot(rslt.stg11.sum[,alts.sort[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.stg11.sum)),space=c(0),yaxs="i",xaxs="i")
  text(x,rslt.stg11.sum[,alts.sort[i]],rslt.stg11.sum[,alts.sort[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(alts.sort)){axis_fun(1,x,x,xlabs,line=-0.5)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  mtext(side=3,line=-1.25,adj=0,paste0(" Alt: ",alts.sort[i]),font=2,cex=0.75)
  if(i==1){mtext(side=3,adj=0,"Daily Stage < 11 Ft NGVD")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")
mtext(side=2,line=0.5,outer=T,"Number of Events")

for(i in 1:length(alts.sort)){
  x=barplot(rslt.stg10.sum[,alts.sort[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.stg10.sum)),space=c(0),yaxs="i",xaxs="i")
  text(x,rslt.stg10.sum[,alts.sort[i]],rslt.stg10.sum[,alts.sort[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(alts.sort)){axis_fun(1,x,x,xlabs,line=-0.5)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  #mtext(side=3,line=-1.25,adj=0,paste0(" Alt: ",alts.sort[i]),font=2)
  if(i==1){mtext(side=3,adj=0,"Daily Stage \u2264 10 Ft NGVD")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")
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
# sum(test.sum$env.val!=subset(env.rslt,Alt=="ECBr")$env)

env.rslt$env2=with(env.rslt,ifelse(env==2,0,1))
env.rslt=merge(env.rslt,data.frame(Alt=alts.sort,PlotOffset=rev(seq(2,10,2))),"Alt")
env.rslt$env.plt=with(env.rslt,env2+PlotOffset)
env.rslt$env.f=with(env.rslt,ifelse(env==1,"normal","recovery"))
env.rslt$Alt=factor(env.rslt$Alt,levels=alts.sort)

#env.count=reshape2::dcast(env.rslt,Alt~env.f,value.var = "env",function(x) N.obs(x))
env.count=ddply(env.rslt,c("Alt","env.f"),summarise,N.val=N.obs(env.f))
env.count$axs.val=11:2

# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_Env_Iter2Opt.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1.75,1),oma=c(2,2,1,2));

ylim.val=c(1.5,11.5)
xlim.val=c(1965,2016);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(env2~CY,env.rslt,type="n",axes=F,ann=F,xlim=xlim.val,xaxs="i",yaxs="i",ylim=ylim.val)
abline(v=c(xmaj,xmin),h=c(2:19),lty=c(1,3),lwd=0.5,col=c("black","grey"))
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

##
library(LORECOVER)
# dat=data.frame(Date=date.fun(c("2016-12-30","2016-12-31")),Data.Value=c(15.7671746873643,15.7087025511184))
# norm_env(dat)
# head(lakeO.stage)
lakeO.stage$Data.Value=lakeO.stage$STAGE

# tmp=subset(lakeO.stage,Alt=="CC"&Date==date.fun("2016-12-31"))
# tmp
# norm_env(tmp)

norm.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts[i])[,c("Date","Data.Value")]
  rslt=LORECOVER::norm_env(tmp)
  rslt$Alt=alts[i]
  norm.lakeO.stage.scr=rbind(norm.lakeO.stage.scr,rslt)
  print(i)
}
subset(norm.lakeO.stage.scr,Alt=="CC"&Date==date.fun("2016-12-31"))

norm.lakeO.stage.scr=rename(norm.lakeO.stage.scr,c("penalty"="norm.score"))
norm.lakeO.stage.scr$WY=WY(norm.lakeO.stage.scr$Date)
# write.csv(subset(norm.lakeO.stage.scr,Alt=='CC'),paste0(export.path,"CC_LakeStage_Check.csv"),row.names = F)
ddply(norm.lakeO.stage.scr,c("Alt"),summarise,TScore=sum(abs(norm.score),na.rm=T))

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

ddply(rec.lakeO.stage.scr,c("Alt"),summarise,TScore=sum(abs(rec.score),na.rm=T))

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

lakeO.stage.scr$month=as.numeric(format(lakeO.stage.scr$Date,"%m"))
env.pen.sum.maysept=ddply(subset(lakeO.stage.scr,month%in%seq(5,9,1)),"Alt",summarise,
                  N.val=N.obs(score),
                  pen_above=sum(score[score>0],na.rm=T),
                  pen_below=sum(abs(score)[score<0],na.rm=T),
                  per_below=(sum(score<0)/N.obs(score))*100,
                  per0=(sum(score==0,na.rm=T)/N.obs(score))*100,
                  per_above=(sum(score>0)/N.obs(score))*100)
env.pen.sum.maysept$total.pen=rowSums(env.pen.sum.maysept[,c("pen_above","pen_below")])

# write.csv(env.pen.sum,paste0(export.path,"LOMetrics_Iter2Opt.csv"),row.names=F)
# png(filename=paste0(plot.path,"Post-Iteration_2/LakeO_EnvScore_BWA_Iter2Opt.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,60);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,1,0.5,0.25),oma=c(2,2.5,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.40))

cols.val2=c(rgb(255/255,255/255,0),rgb(143/255,188/255,143/255),rgb(100/255,149/255,237/255))
x=barplot(t(env.pen.sum[,c("per_below","per0","per_above")]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,v=x[2,],lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(env.pen.sum[,c("per_below","per0","per_above")]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=cols.val2,add=T,xaxt="n")
with(env.pen.sum,text(x[1,],per_below/2,paste0(round(per_below,0),"%"),cex=0.5))
with(env.pen.sum,text(x[2,],per0/2,paste0(round(per0,0),"%"),cex=0.5))
with(env.pen.sum,text(x[3,],per_above/2,paste0(round(per_above,0),"%"),cex=0.5))
x.val=x[2,] #x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,alts.sort,line=-0.5,cex=0.8)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Alternative")
mtext(side=2,line=2,"Percent",cex=1)
mtext(side=3,adj=0,"Lake Okeechobee")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("% time Below","% time Within","% time Above"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols.val2,
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Stage Envelope")
dev.off()
env.pen.sum.plns=subset(env.pen.sum,Alt%in%alts.sort[alts.sort!="NA25"])

# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_EnvScore_BWA_FWO_Iter2Opt.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(-50,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(t(env.pen.sum.plns[,c("FWO_PerBelow","FWO_PerWith","FWO_PerAbove")]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,v=x[2,],lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(env.pen.sum.plns[,c("FWO_PerBelow","FWO_PerWith","FWO_PerAbove")]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("dodgerblue1","forestgreen","indianred1"),add=T,xaxt="n")
x.val=x[2,] #x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,alts.sort[alts.sort!="NA25"],line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Plan Name")
mtext(side=2,line=2,"Average Percent Difference to FWO",cex=1)
mtext(side=3,adj=0,"Lake Okeechobee")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("Time Below","Time Within","Time Above"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c("dodgerblue1","forestgreen","indianred1"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Stage Envelope")
dev.off()

cols.IMC=c(rgb(238,232,170,maxColorValue = 255),rgb(143,188,143,maxColorValue = 255))
# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_EnvScore_AllYrs_Iter2Opt.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
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

# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_EnvScore_MaySep_Iter2Opt.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,20000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(t(env.pen.sum.maysept[,c("pen_below","pen_above")]),beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(t(env.pen.sum.maysept[,c("pen_below","pen_above")]),beside=F,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,nrow(env.pen.sum)),add=T)
with(env.pen.sum.maysept,text(x,pen_below/2,round(pen_below,0)))
with(env.pen.sum.maysept,text(x,pen_below+abs((pen_below-total.pen)/2),round(pen_above,0)))
with(env.pen.sum.maysept,text(x,total.pen,round(total.pen,0),pos=3))
axis_fun(1,x,x,alts.sort,line=-0.5)
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


# LOK MFL -----------------------------------------------------------------
# write.csv(subset(lakeO.stage,Alt=="OPT")[,c("Alt","Date","STAGE")],paste0(export.path,"LakeStg_OPT.csv"),row.names = F)
#18month window
find_endofdayR=function(date){
  # adapted from mflst_LO_v3.py from IMC
  start_year=as.numeric(format(date,'%Y'))
  start_month=as.numeric(format(date,'%m'))
  start_day=as.numeric(format(date,'%d'))
  
  # if(start_month>0 & start_month < 11){
  #   end_day=30
  #   end_month=5
  #   end_year=start_year+1
  # }else if(start_month==11){
  #   end_day=start_day
  #   end_month=5
  #   end_year=start_year+2
  # }else{
  #   end_day=30
  #   end_month=5
  #   end_year=start_year+2
  # }
  end_day=ifelse(start_month==11,start_day,30)
  end_month=5
  end_year=ifelse(start_month<11,start_year+1,start_year+2)
  
  end_date=as.Date(paste(end_year,end_month,end_day,sep="-"))
  return(end_date)
}
library(lubridate)

LOK.mfl.rslt=data.frame()
lakeO.stage.mfl=data.frame()

mfl_criteria1=11
mfl_criteria2=80
for(j in 1:n.alts){

tmp=subset(lakeO.stage,Alt==alts.sort[j])
tmp$exceed=with(tmp,ifelse(STAGE<mfl_criteria1,1,0))

tmp$exceed_end=NA
tmp$exc_sum=0
tmp$exc_data2=0
tmp$exc_data3=0
tmp$exc_end_data=0
tmp$exc_reset_data=0

end_jd=NA
# exc_ct=NA
exc_n=0
tmp_date=NA
tmp_sum2=NA

for(i in 1:nrow(tmp)){
  if(tmp$exceed[i]==1){
    date.val=date.fun(tmp$Date[i]+ddays(1))
    end_jd=date.fun(find_endofdayR(date.val)-ddays(1))
    
    ## find the checking window
    tmp_date=seq(date.val,end_jd,"1 days")
    tmp_len=length(tmp_date);tmp_len# to match python len(...) function
    # range(tmp_date)
    tmp_idx=which(tmp$Date%in%tmp_date)
    
    ### get total exceedence days and index or days when stage <11.ft  within the last 18 months
    tmp_idx2=which(tmp$exceed[tmp_idx]==1)
    
    tmp_sum2=length(tmp_idx2)
    tmp$exc_sum[i]=tmp_sum2
    
    if(tmp_sum2>mfl_criteria2){
      tmp$exc_data2[tmp_idx[tmp_idx2[mfl_criteria2]]]=1
    }
  } 
}
# plot(exc_data2,type="l")
# plot(tmp$exc_data2,type="l")
# sum(exc_data2==1,na.rm=T)
# sum(tmp$exc_sum>0,na.rm=T)

for(i in 2:nrow(tmp)){
  
  if(tmp$exc_data2[i-1]==1&tmp$exc_data2[i]==0){
    tmp$exc_end_data[i-1]=1
  }
}

# plot(exc_end_data,type="l")
# plot(tmp$exc_end_data,type="l")
# sum(exc_end_data==1)
# which(exc_end_data==1)

counts=0
for(i in 2:nrow(tmp)){
  if(tmp$exc_data2[i-1]==0&tmp$exc_data2[i]==1){
    # print(i)
    counts=1
  }
  if(tmp$exc_end_data[i]==1){
   if(tmp$exc_reset_data[i-1]<1){
     tmp$exc_reset_data[i]=365
    }else{
      tmp$exc_reset_data[i]=tmp$exc_reset_data[i-1]-1
      if(tmp$exc_reset_data[i]==0&tmp$exc_data2[i]==1){
        tmp$exc_reset_data[i]=365
      }
    }
  }else{
    tmp$exc_reset_data[i]=tmp$exc_reset_data[i-1]-1
    counts=counts+1
    
    if(counts>366&tmp$exc_data2[i]==1){
      tmp$exc_reset_data[i]=365
      counts=0
    }
    if(tmp$exc_reset_data[i]==0&tmp$exc_data2[i]==1){
      tmp$exc_reset_data[i]==365
    }
  }
  
  # identify yearly violation
  if(tmp$exc_reset_data[i]<0){
    if(tmp$exc_data2[i]==1){
      if(tmp$exc_data2[i-1]!=1){
        tmp$exc_data3[i]=1
        exc_n=exc_n+1
      }else{
        tmp$exc_data3[i]=0
      }
    }else{
      tmp$exc_data3[i]=0
    }
  }else{
    if(tmp$exc_reset_data[i]==365&tmp$exc_end_data[i]==0){
      tmp$exc_data3[i]=1
      exc_n=exc_n+1
      
    }else{
      tmp$exc_data3[i]=0
    }
  }
}
# counts
# exc_n

LOK.mfl.rslt=rbind(LOK.mfl.rslt,data.frame(Alt=alts.sort[j],N.exceed=exc_n))
lakeO.stage.mfl=rbind(lakeO.stage.mfl,tmp)

print(j)
}

LOK.mfl.rslt

lakeO.stage.mfl$plot_exc=with(lakeO.stage.mfl,ifelse(exc_reset_data<0&exc_data2==1,STAGE,NA))
lakeO.stage.mfl$plot_exc365=with(lakeO.stage.mfl,ifelse(exc_reset_data>0&exc_data2==1,STAGE,NA))

# i=5
# for(i in 1:n.alts){
# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_MFL_Alt_",alts.sort[i],".png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
# layout(matrix(1:2,2,1),heights=c(1,0.2))
# par(family="serif",mar=c(1,3,0.75,1),oma=c(1.5,1,2.5,0.5));
# 
# ylim.val=c(8,20);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# xlim.val=date.fun(c("1965-01-01","2017-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
# 
# plot(STAGE~Date,lakeO.stage.mfl,type="n",yaxs="i",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F)
# abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
# abline(h=mfl_criteria1,col="brown",lty=2)
# with(subset(lakeO.stage.mfl,Alt==alts.sort[i]),lines(Date,STAGE,col="blue",lty=1.5))
# with(subset(lakeO.stage.mfl,Alt==alts.sort[i]),lines(Date,plot_exc,col="orange",lty=1.5))
# with(subset(lakeO.stage.mfl,Alt==alts.sort[i]),lines(Date,plot_exc365,col="grey",lty=1.5))
# axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
# axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=2,line=2.5,"Stage Elevation (Ft, NGVD29)")
# mtext(side=1,line=1.5,"Year")
# mtext(side=3,paste0("MFL Recovery Water Body - Minimum Flows and Levels for Lake Okeechobee\n",
#                     alts.sort[i]," : ",subset(LOK.mfl.rslt,Alt==alts.sort[i])$N.exceed,
#                     " exceedance in 52 years of simualtion"))
# 
# plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
# legend(0.5,-0.5,legend=c("Stage Elevation","MFL Criteria (11.0 Ft, NGVD29)","Exceedance","Exceedance w/in 365 Days"),
#        pch=NA,
#        lty=c(1,2,1,1),lwd=2,
#        col=c("blue","brown","orange","grey"),
#        pt.bg=NA,
#        pt.cex=1.5,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
# dev.off()
# }

LOK.mfl.rslt$NA25_perchange=with(LOK.mfl.rslt,round(((N.exceed-N.exceed[1])/N.exceed[1])*100,2))

# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_MFL_Iter2Opt.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,12);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(2,2,0.5,0.25),oma=c(1,2,0.75,1),lwd=0.5);

x=barplot(LOK.mfl.rslt$N.exceed,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,length(alts.sort)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(LOK.mfl.rslt$N.exceed,beside=F,ylim=ylim.val,col=adjustcolor(cols,0.5),axes=F,ann=F,names.arg=rep(NA,length(alts.sort)),add=T)
text(x,LOK.mfl.rslt$N.exceed,LOK.mfl.rslt$N.exceed,pos=3)
text(x,LOK.mfl.rslt$N.exceed,paste0(LOK.mfl.rslt$NA25_perchange,"%"),pos=1,font=3)
axis_fun(1,x,x,alts.sort,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"MFL Exceedances")
mtext(side=1,line=1.75,"Alternative")
mtext(side=3,adj=0,"Lake Okeechobee MFL")
mtext(side=3,adj=1,"CY 1965 - 2016")
dev.off()

# LWL ---------------------------------------------------------------------
# test
# LWL.test=data.frame()
# dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/CC/RSMBN_output.dss"))  
# RSM.sites=c("C10A_QFC","S155A")
# for(i in 1:length(RSM.sites)){
#   paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
#   tmp=data.frame(getFullTSC(dss_out,paths))
#   tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
#   rownames(tmp)<-NULL
#   tmp$SITE=RSM.sites[i]
#   tmp$Alt="CC"
#   LWL.test=rbind(tmp,LWL.test)
#   print(i)
# }
# LWL.test$source="RSMBN"
# 
# dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/CC/RSMGL_output.dss"))  
# paths="/RSMGL/S155A/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/"
# tmp=data.frame(getFullTSC(dss_out,paths))
# tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
# rownames(tmp)<-NULL
# tmp$SITE="S155A"
# tmp$Alt="CC"
# tmp$source="RSMGL"
# LWL.test=rbind(tmp,LWL.test)
# 
# LWL.test$SITE2=with(LWL.test,ifelse(SITE=="S155A",paste(SITE,substr(source,4,5),sep="_"),SITE))
# 
# LWL.test.xtab=reshape2::dcast(LWL.test,Alt+Date~SITE2,value.var = "FLOW",mean)
# plot(S155A_BN~S155A_GL,LWL.test.xtab);abline(0,1);abline(lm(S155A_BN~S155A_GL,LWL.test.xtab),col="red")
# lm(S155A_BN~S155A_GL,LWL.test.xtab)
# plot(S155A_GL~S155A_BN,LWL.test.xtab);abline(0,1);abline(lm(S155A_GL~S155A_BN,LWL.test.xtab),col="red")
# lm(S155A_GL~S155A_BN,LWL.test.xtab)
# 
# ##
# LWL.test.xtab$month=as.numeric(format(LWL.test.xtab$Date,"%m"))
# LWL.test.xtab$CY=as.numeric(format(LWL.test.xtab$Date,"%Y"))
# LWL.test.xtab$LOSOM.hydroseason=with(LWL.test.xtab,ifelse(month%in%c(6:10),"wet","dry"))
# LWL.test.xtab$LOK_S155A=apply(LWL.test.xtab[,c("C10A_QFC","S155A_GL")],1,min,na.rm=T)
# LWL.test.xtab$Basin_S155A=with(LWL.test.xtab,S155A_GL-LOK_S155A)
# 
# LWL.test.melt=reshape2::melt(LWL.test.xtab[,c("Alt","CY","LOSOM.hydroseason","LOK_S155A","Basin_S155A")],id.vars=c("Alt","CY","LOSOM.hydroseason"))
# LWL.test.melt$count.QGT0=with(LWL.test.melt,ifelse(value>0,1,0))
# 
# LWL.total.CY=ddply(LWL.test.melt,c("Alt","CY","LOSOM.hydroseason","variable"),summarise,
#                    TFlow.kacft=sum(cfs.to.acftd(value)/1000,na.rm=T))
# reshape2::dcast(LWL.total.CY,variable~LOSOM.hydroseason,value.var="TFlow.kacft",mean)
# reshape2::dcast(LWL.test.melt,variable~LOSOM.hydroseason,value.var="count.QGT0",sum)
# 
# ## Using just RSMBN source
# LWL.test.xtab$LOK_S155A=apply(LWL.test.xtab[,c("C10A_QFC","S155A_BN")],1,min,na.rm=T)
# LWL.test.xtab$Basin_S155A=with(LWL.test.xtab,S155A_BN-LOK_S155A)
# 
# LWL.test.melt=reshape2::melt(LWL.test.xtab[,c("Alt","CY","LOSOM.hydroseason","LOK_S155A","Basin_S155A")],id.vars=c("Alt","CY","LOSOM.hydroseason"))
# LWL.test.melt$count.QGT0=with(LWL.test.melt,ifelse(value>0,1,0))
# 
# LWL.total.CY=ddply(LWL.test.melt,c("Alt","CY","LOSOM.hydroseason","variable"),summarise,
#                    TFlow.kacft=sum(cfs.to.acftd(value)/1000,na.rm=T))
# reshape2::dcast(LWL.total.CY,variable~LOSOM.hydroseason,value.var="TFlow.kacft",mean)
# reshape2::dcast(LWL.test.melt,variable~LOSOM.hydroseason,value.var="count.QGT0",sum)

######
ann.Q=ddply(q.dat.xtab,c("Alt","CY"),summarise,C10AQ=sum(cfs.to.acftd(C10A)))
mean.ann.Q=ddply(ann.Q,"Alt",summarise,C10A.kacft=mean(C10AQ/1000))
mean.ann.Q=mean.ann.Q[match(mean.ann.Q$Alt,alts.sort),]
mean.ann.Q$FWO=with(mean.ann.Q,(C10A.kacft-C10A.kacft[1])/C10A.kacft[1])*100
mean.ann.Q$FWO[1]=NA

# png(filename=paste0(plot.path,"Post-Iteration_2/LWL_TotalQ_Iter2Opt.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=1);

x=barplot(mean.ann.Q$C10A.kacft,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,length(alts.sort)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(mean.ann.Q$C10A.kacft,beside=F,ylim=ylim.val,col=adjustcolor(cols,0.5),axes=F,ann=F,names.arg=rep(NA,length(alts.sort)),add=T)
text(x[2:5,],mean.ann.Q$C10A.kacft[2:5],paste(round(mean.ann.Q$FWO[2:5],1),"%"),pos=3)
text(x,mean.ann.Q$C10A.kacft,round(mean.ann.Q$C10A.kacft,1),pos=1,font=2)
axis_fun(1,x,x,alts.sort,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Discharge (x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=1,line=1.75,"Alternative")
mtext(side=3,adj=0,"Lake Worth Lagoon (C10A)")
mtext(side=3,adj=1,"CY 1965-2016")
dev.off()

## 
LWL.q.dat=q.dat.xtab[,c("Date","Alt","C10A_QFC","S155A")]
LWL.q.dat$month=as.numeric(format(LWL.q.dat$Date,"%m"))
LWL.q.dat$CY=as.numeric(format(LWL.q.dat$Date,"%Y"))
LWL.q.dat$LOSOM.hydroseason=with(LWL.q.dat,ifelse(month%in%c(6:10),"A_wet","B_dry"))
LWL.q.dat$LOK_S155A=apply(LWL.q.dat[,c("C10A_QFC","S155A")],1,min,na.rm=T)
LWL.q.dat$Basin_S155A=with(LWL.q.dat,S155A-LOK_S155A)

LWL.q.dat.melt=reshape2::melt(LWL.q.dat[,c("Alt","Date","CY","LOSOM.hydroseason","LOK_S155A","Basin_S155A")],
                             id.vars=c("Alt","Date","CY","LOSOM.hydroseason"))
LWL.q.dat.melt$count.QGT0=with(LWL.q.dat.melt,ifelse(value>0,1,0))

LWL.total.CY=ddply(LWL.q.dat.melt,c("Alt","CY","LOSOM.hydroseason","variable"),summarise,
                   TFlow.kacft=sum(cfs.to.acftd(value)/1000,na.rm=T))
LWL.total.CY.mean=reshape2::dcast(LWL.total.CY,LOSOM.hydroseason+variable~Alt,value.var="TFlow.kacft",mean)
vars=c("LOSOM.hydroseason","variable",alts.sort)
LWL.total.CY.mean=LWL.total.CY.mean[,vars]

LWL.total.CY.count=reshape2::dcast(subset(LWL.q.dat.melt,variable=="LOK_S155A"),LOSOM.hydroseason+variable~Alt,value.var="count.QGT0",sum)
LWL.total.CY.count=LWL.total.CY.count[,vars]

cols.IMC=c(rgb(100,149,237,maxColorValue = 255),
           rgb(64,224,208,maxColorValue = 255),
           rgb(238,232,170,maxColorValue = 255),
           rgb(189,183,107,maxColorValue = 255))
cols.IMC.txt=c(rgb(46,46,255,maxColorValue = 255),
               rgb(194,188,119,maxColorValue = 255))
# png(filename=paste0(plot.path,"Post-Iteration_2/LWL_Iter2Opt.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(as.matrix(LWL.total.CY.mean[,3:7]),beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,length(alts.sort)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(as.matrix(LWL.total.CY.mean[,3:7]),beside=F,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,length(alts.sort)),add=T)
text(x,LWL.total.CY.mean[1,alts.sort]/2,round(LWL.total.CY.mean[1,alts.sort]),xpd=NA)
text(x,LWL.total.CY.mean[1,alts.sort]+colSums(LWL.total.CY.mean[1:2,alts.sort])/2,round(LWL.total.CY.mean[2,alts.sort]),xpd=NA)
text(x,colSums(LWL.total.CY.mean[1:2,alts.sort])+(colSums(LWL.total.CY.mean[1:3,alts.sort])-colSums(LWL.total.CY.mean[1:2,alts.sort]))/2,round(LWL.total.CY.mean[3,alts.sort]),xpd=NA)
text(x,colSums(LWL.total.CY.mean[1:3,alts.sort])+(colSums(LWL.total.CY.mean[1:4,alts.sort])-colSums(LWL.total.CY.mean[1:3,alts.sort]))/2,round(LWL.total.CY.mean[4,alts.sort]),xpd=NA)
text(x,colSums(LWL.total.CY.mean[1:4,alts.sort]),LWL.total.CY.count[1,alts.sort],col=cols.IMC.txt[1],pos=3,font=2)
text(x,colSums(LWL.total.CY.mean[1:4,alts.sort])+8,LWL.total.CY.count[2,alts.sort],col=cols.IMC.txt[2],pos=3,font=2)
axis_fun(1,x,x,alts.sort,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Discharge (x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=1,line=1.75,"Alternative")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
mtext(side=3,line=-3,"Lake Worth Lagoon\nEstuary Flows (S155A)\nSimulation Period of Record\n(1965-2016)")
legend(0.5,0.5,legend=c("Wet Season - LOK","Wet Season - Basin","Dry Season - LOK","Dry Season - Basin","Wet Season - Days LOK Flow >0\n(Jun-Oct)","Dry Season - Days LOK Flow >0\n(Nov-May)"),
       pch=c(22,22,22,22,NA,NA),
       lty=0,lwd=0.01,
       text.col=c("black","black","black","black",cols.IMC.txt),
       col="black",
       pt.bg=c(cols.IMC,NA,NA),
       pt.cex=2,ncol=1,cex=0.7,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=1,line=-1,adj=1,"Estimated using RSMBN\noutput, may not reflect\nexisting published values.",cex=0.75,col="red",font=2)
dev.off()



# Regulation Schedules ----------------------------------------------------
zones=c(paste("LOK",paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),sep="-"),
        paste("LOK",paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_"),sep="_"))
zone.alias=data.frame(zone=zones,
                      zone2=c(paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_")))
reg.sch=data.frame()
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
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

# reg.sch2=ddply(reg.sch,c("Alt","zone","DOY"),summarise,stg=mean(STAGE))

# png(filename=paste0(plot.path,"Post-Iteration_2/CC_OPT_REGSCH.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,1))

xlim.val.date=date.fun(c("1965-01-01","1965-12-31"));xmaj.dat=seq(xlim.val.date[1],xlim.val.date[2],"2 month");xmin.dat=seq(xlim.val.date[1],xlim.val.date[2],"1 month")
xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.dat,"%j"));xmin=as.numeric(format(xmin.dat,"%j"));
ylim.val=c(10,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/0.5)

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
mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Alternative CC")

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="OPT"),lines(DOY,ZONE_A,col="red",lwd=2))
with(subset(reg.sch2,Alt=="OPT"),lines(DOY,ZONE_B,col="deepskyblue2",lwd=2))
with(subset(reg.sch2,Alt=="OPT"),lines(DOY,ZONE_C,col="orange",lwd=2))
with(subset(reg.sch2,Alt=="OPT"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))
with(subset(reg.sch2,Alt=="OPT"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="OPT"&DOY==90),text(85,ZONE_B,"Zone B",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="OPT"&DOY==105),text(105,ZONE_C,"Zone C",pos=3,col="orange",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="OPT"&DOY==30),text(30,LOWSM_15_LEVEL+(ZONE_C-LOWSM_15_LEVEL)/2,"Zone D",col="grey",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="OPT"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Date (Month)")
mtext(side=3,adj=0,"Alternative OPT")
dev.off()

##
head(lakeO.stage)
reg.sch2=reshape2::dcast(reg.sch,Alt+Date~zone2,value.var = "STAGE",mean)

vars=c("Alt","Date","STAGE")
lakeO.stage.reg=merge(lakeO.stage[,vars],
                      reg.sch2,c("Alt","Date"))

lakeO.stage.reg$ZONE_D1=with(lakeO.stage.reg,ifelse(Alt%in%c("OPT","OPT-S2"),LOWSM_15_LEVEL,ZONE_D1))



zone.freq=ddply(lakeO.stage.reg,"Alt",summarise,
      GT.zoneA=sum(STAGE>ZONE_A),
      zoneB=sum(ifelse(STAGE<ZONE_A&STAGE>ZONE_B,1,0)),
      zoneC=sum(ifelse(STAGE<ZONE_B&STAGE>ZONE_C,1,0)),
      zoneD=sum(ifelse(STAGE<ZONE_C&STAGE>ZONE_D1,1,0)),
      LT.zoneD=sum(STAGE<ZONE_D1))
zone.freq[match(alts.sort,zone.freq$Alt),]


zone.freq=ddply(lakeO.stage.reg,"Alt",summarise,
                above.zoneD=sum(STAGE>ZONE_C),
                zoneD=sum(ifelse(STAGE<ZONE_C&STAGE>ZONE_D1,1,0)),
                below.zoneD=sum(STAGE<ZONE_D1))
zone.freq=zone.freq[match(alts.sort,zone.freq$Alt),]
zone.freq[,2:4]=(zone.freq[,2:4]/18993)*100

# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_Stage_RegSch_Iter2Opt.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

# cols=c("orange","grey","purple")
cols=wesanderson::wes_palette("Zissou1", 3, type = "continuous")
zone.freq2=t(zone.freq[,2:4])[3:1,]
x=barplot(zone.freq2,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,length(alts.sort)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(zone.freq2,beside=F,ylim=ylim.val,col=adjustcolor(cols,0.5),axes=F,ann=F,names.arg=rep(NA,length(alts.sort)),add=T)
text(x,zone.freq2[1,]/2,format(round(zone.freq2[1,],1)),xpd=NA,font=2)
text(x,zone.freq2[1,]+zone.freq2[2,]/2,format(round(zone.freq2[2,],1)),xpd=NA,font=2)
text(x,colSums(zone.freq2[1:2,])+(zone.freq2[3,])/2,format(round(zone.freq2[3,],1)),xpd=NA,font=2)
axis_fun(1,x,x,alts.sort)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Percent of Days\nDuring Period of Simulation")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("Above Zone D","Within Zone D","Below Zone D"),
       pch=c(22,22,22),
       lty=0,lwd=0.01,
       col="black",
       pt.bg=adjustcolor(rev(cols),0.5),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=1,line=-1,adj=1,"Simulation Period of Record\n(1965-2016; 18993 days)",cex=0.75)
dev.off()



zone.freq=subset(zone.freq,!(Alt%in%c("OPT","OPT-S2")))
# png(filename=paste0(plot.path,"Post-Iteration_2/LOK_Stage_RegSch.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

# cols=c("orange","grey","purple")
cols=wesanderson::wes_palette("Zissou1", 3, type = "continuous")
zone.freq2=t(zone.freq[,2:4])[3:1,]
x=barplot(zone.freq2,beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,3))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(zone.freq2,beside=F,ylim=ylim.val,col=adjustcolor(cols,0.5),axes=F,ann=F,names.arg=rep(NA,3),add=T)
text(x,zone.freq2[1,]/2,format(round(zone.freq2[1,],1)),xpd=NA,font=2)
text(x,zone.freq2[1,]+zone.freq2[2,]/2,format(round(zone.freq2[2,],1)),xpd=NA,font=2)
text(x,colSums(zone.freq2[1:2,])+(zone.freq2[3,])/2,format(round(zone.freq2[3,],1)),xpd=NA,font=2)
axis_fun(1,x,x,alts.sort[!(alts.sort%in%c("OPT","OPT-S2"))])
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Percent of Days\nDuring Period of Simulation")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("Above Zone D","Within Zone D","Below Zone D"),
       pch=c(22,22,22),
       lty=0,lwd=0.01,
       col="black",
       pt.bg=adjustcolor(rev(cols),0.5),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=1,line=-1,adj=1,"Simulation Period of Record\n(1965-2016; 18993 days)",cex=0.75)
dev.off()
