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

# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
alts=alts[!alts%in%c("_Batch_Results","Northern_Estuaries")]
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

# Lake Stage --------------------------------------------------------------
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
lakeO.stage1=lakeO.stage
range(lakeO.stage$Date)
# write.csv(subset(lakeO.stage,Alt=="NA25"),paste0(export.path,"LakeStg_NA25.csv"),row.names = F)
# write.csv(subset(lakeO.stage,Alt=="ECBr"),paste0(export.path,"LakeStg_ECBr.csv"),row.names = F)
# # Sanity Check
# plot(STAGE~Date,lakeO.stage,type="n")
# with(subset(lakeO.stage,Alt==alts[1]),lines(Date,STAGE,col="red"))
# with(subset(lakeO.stage,Alt==alts[2]),lines(Date,STAGE,col="green"))
# with(subset(lakeO.stage,Alt==alts[3]),lines(Date,STAGE,col="blue"))
# 
# plot(value~proportion,ecdf_fun(subset(lakeO.stage,Alt=="LSM25B")$STAGE),type="n")
# with(ecdf_fun(subset(lakeO.stage,Alt=="LSM25B")$STAGE),lines(1-proportion,value,col="purple"))
# with(ecdf_fun(subset(lakeO.stage,Alt=="ECRE")$STAGE),lines(1-proportion,value,col="red"))
# with(ecdf_fun(subset(lakeO.stage,Alt=="ESLE")$STAGE),lines(1-proportion,value,col="lightblue"))
# with(ecdf_fun(subset(lakeO.stage,Alt=="ABNE")$STAGE),lines(1-proportion,value,col="green"))

## Experimenting with wavelet analysis
# library(WaveletComp)
# # http://www.hs-stat.com/projects/WaveletComp/WaveletComp_guided_tour.pdf
# test=subset(lakeO.stage1,Alt=='NA25')
# wave1=analyze.wavelet(test,"STAGE",
#                       loess.span = 0,
#                       make.pval = TRUE, n.sim = 10)
# 
# 
# test2=subset(lakeO.stage1,Alt=='CC')
# wave2=analyze.wavelet(test2,"STAGE",
#                       loess.span = 0,
#                       make.pval = TRUE, n.sim = 10)
# 
# layout(matrix(1:2,2,1))
# par(family="serif",mar=c(3,3,0.25,1),oma=c(1,2,1,0.25));
# wt.image(wave1,graphics.reset = FALSE)
# wt.image(wave2,graphics.reset = FALSE)
# 
# wt.avg(wave1)
# wt.avg(wave2)
# 
# my.rec <- reconstruct(wave1)
# my.rec <- reconstruct(wave2)

# LOK MFL -----------------------------------------------------------------
## Date functions
# cal2jd=function(date){
#   year=as.numeric(format(date,"%Y"))
#   month=as.numeric(format(date,"%m"))
#   day=as.numeric(format(date,"%d"))
#   
#   a = (14 - month)/12
#   y = year + 4800 - a
#   m = month + 12*a - 3
#   julday=day + ((153*m + 2)/5) + 365*y + y/4 - y/100 + y/400 - 32045
#  return(julday) 
# }
# # cal2jd(as.Date("1965-01-01"))
# jd2cal=function(julday){
#   a = as.integer(julday + 32044)
#   b = (4*a + 3)%/%146097
#   c = a - (146097*b)%/%4
#   d = (4*c + 3)%/%1461
#   e = c - (1461*d)%/%4
#   m = (5*e + 2)%/%153
#   day = (e + 1 - (153*m + 2)%/%5)
#   month = m + 3 - 12*(m%/%10)
#   year = 100*b + d - 4800 + m%/%10
#   date.val=as.Date(paste(year,month,day,sep="-"))
#   return(date.val)
# }
# cal2jd(as.Date("2016-01-31"))
# jd2cal(2457420)
# 
# 
# astrolibR::jdcnv(1965,01,31,0)
# astrolibR::daycnv(2438792)

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

## Not a direct 18 months
## (from 40E-8.221(1))
## The eighteen month period shall be initiated following the first day 
## Lake Okeechobee falls below 11 feet NGVD, and shall not include more than 
## one wet season, defined as May 31st through October 31st of any given
## calendar year.
### 
library(lubridate)
mfl_criteria1=11
mfl_criteria2=80
test=subset(lakeO.stage1,Alt=="NA25")
test$exceed=with(test,ifelse(STAGE<mfl_criteria1,1,0))
test$exceed_win80d=with(test,c(rep(NA,79),rollapply(exceed,width=80,FUN=function(x)sum(x,na.rm=T))))
subset(test,exceed_win80d>=80)

test$exceed_end=NA
test$exc_sum=NA

end_jd=NA
exc_data2=rep(0,nrow(test))
exc_data3=rep(0,nrow(test))
exc_end_data=rep(0,nrow(test))
exc_reset_data=rep(0,nrow(test))
exc_ct=NA
exc_n=NA
tmp_date=NA
tmp_sum2=NA
i=6026
i=6376
test[i,]

for(i in 1:nrow(test)){
  if(test$exceed[i]==1){
    date.val=date.fun(test$Date[i])#+ddays(1))
    end_jd=date.fun(find_endofdayR(date.val)-ddays(1))
    
    ## find the checking window
    tmp_date=seq(date.val,end_jd,"1 days")
    tmp_len=length(tmp_date);tmp_len# to match python len(...) function
    # range(tmp_date)
    tmp_idx=which(test$Date%in%tmp_date)
    
    ### get total exceedence days and index or days when stage <11.ft  within the last 18 months
    tmp_idx2=which(test$exceed[tmp_idx]==1)
    
    tmp_sum2=length(tmp_idx2)-1
    test$exc_sum[i]=tmp_sum2
  
    if(tmp_sum2>mfl_criteria2){
      exc_data2[tmp_idx[tmp_idx2[mfl_criteria2-1]]]=1
    }
  } 
}
plot(exc_data2,type="l")
sum(exc_data2==1,na.rm=T)
sum(test$exc_sum>0,na.rm=T)


for(i in 2:nrow(test)){
  
  if(exc_data2[i-1]==1&exc_data2[i]==0){
    exc_end_data[i-1]=1
  }
}
plot(exc_end_data,type="l")
sum(exc_end_data==1)
which(exc_end_data==1)

counts=0
for(i in 2:nrow(test)){
  if(exc_data2[i-1]==0&exc_data2[i]==1){
    counts=1
  }
  if(exc_end_data[i]==1){
    if(exc_reset_data[i-1]<1){
      exc_reset_data[i]=365
    }else{
      exc_reset_data[i]=exc_reset_data[i-1]-1
      if(exc_reset_data[i]==0&exc_data2[i]==1){
        exc_reset_data[i]=365
      }
    }
  }else{
    exc_reset_data[i]=exc_reset_data[i-1]-1
    counts=counts+1
    
    if(counts>366&exc_data2[i]==1){
      exc_reset_data[i]=365
      counts=0
    }
    if(exc_reset_data[i]==0&exc_data2[i]==1){
      exc_reset_data[i]==365
    }
  }
  # identify yearly violation
  if(exc_reset_data[i]<0){
    if(exc_reset_data[i]==1){
      if(exc_data2[i-1]!=1){
        exc_data3[i]=1
        exc_n=exc_n+1
      }else{
        exc_data3[i]=0
      }
    }else{
      exc_data3[i]=0
    }
  }else{
    if(exc_reset_data[i]==365&exc_end_data[i]==0){
      exc_data3[i]=1
      exc_n=exc_n+1
    }else{
      exc_data3[i]=0
    }
  }
}
counts
sum(exc_data3==1)
which(exc_data3==1)

###

lakeO.stage$WY=WY(lakeO.stage$Date)
lakeO.stage$low.stg=with(lakeO.stage,ifelse(STAGE<=11,1,0))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$High.stg=with(lakeO.stage,ifelse(STAGE>16,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))

# png(filename=paste0(plot.path,"Iteration_2/LO_totalDays.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
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
mtext(side=2,line=2.75,"Days \u2264 11 ft NGVD29")
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

days.POS=days.POS[match(alts.sort,days.POS$Alt),]
days.POS$vlow.perdiff=with(days.POS,((sum.vlow-sum.vlow[1])/sum.vlow[1])*100)
days.POS$vHigh.perdiff=with(days.POS,((sum.vHigh-sum.vHigh[1])/sum.vHigh[1])*100)


lakeO.stage.low=ddply(subset(lakeO.stage,WY%in%WYs),c("WY","Alt"),summarise,freq=sum(vlow.stg,na.rm=T),N.val=N.obs(vlow.stg))
lakeO.stage.low$Alt=factor(lakeO.stage.low$Alt,level=alts.sort)
lakeO.stage.low.sum=ddply(lakeO.stage.low,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
lakeO.stage.low.sum=lakeO.stage.low.sum[match(alts.sort,lakeO.stage.low.sum$Alt),]

lakeO.stage.high=ddply(subset(lakeO.stage,WY%in%WYs),c("WY","Alt"),summarise,freq=sum(vHigh.stg,na.rm=T),N.val=N.obs(vHigh.stg))
lakeO.stage.high$Alt=factor(lakeO.stage.high$Alt,level=alts.sort)
lakeO.stage.high.sum=ddply(lakeO.stage.high,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
lakeO.stage.high.sum=lakeO.stage.high.sum[match(alts.sort,lakeO.stage.high.sum$Alt),]

lakeO.stage.freq.sum=merge(lakeO.stage.low.sum[,c("Alt","mean.freq")],
lakeO.stage.high.sum[,c("Alt","mean.freq")],"Alt")
colnames(lakeO.stage.freq.sum)<-c("Alt","freqLow","freqHigh")
lakeO.stage.freq.sum=lakeO.stage.freq.sum[match(alts.sort,lakeO.stage.freq.sum$Alt),]
lakeO.stage.freq.sum$freqLow.perdiff=with(lakeO.stage.freq.sum,((freqLow-freqLow[1])/freqLow[1])*100)
lakeO.stage.freq.sum$freqHigh.perdiff=with(lakeO.stage.freq.sum,((freqHigh-freqHigh[1])/freqHigh[1])*100)

# write.csv(lakeO.stage.freq.sum,paste0(export.path,"highlow_meanfreq_rslt.csv"),row.names = F)

# png(filename=paste0(plot.path,"Iteration_2/LO_iter1_freq_bxp.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.25,1),oma=c(4,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,lakeO.stage.high,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,lakeO.stage.high.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(lakeO.stage.high,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(lakeO.stage.high.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(High Stage)\n(Days Yr\u207B\u00B9)")
mtext(side=3,adj=0,"Lake Okeechobee")

ylim.val=c(0,25);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,lakeO.stage.low,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,lakeO.stage.low.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(lakeO.stage.low,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(lakeO.stage.low.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Low Stage)\n(Days Yr\u207B\u00B9)")
mtext(side=1,line=4,'Model Alternative')
dev.off()

# Stage duration curves

# png(filename=paste0(plot.path,"Iteration_2/LO_StageDuration.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(8,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[1])$STAGE),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[2])$STAGE),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  # with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-lwr.CI,value,col=adjustcolor(cols[i],0.5),lwd=1,lty=2))
  # with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-upr.CI,value,col=adjustcolor(cols[i],0.5),lwd=1,lty=2))
  legend("bottomleft",legend=c(alts.sort[c(1,2,i)]),
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"Lake Okeechobee")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/LO_StageDuration2.png"),width=7.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:6,1,6,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(8,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[1])$STAGE),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[2])$STAGE),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  # with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-lwr.CI,value,col=adjustcolor(cols[i],0.5),lwd=1,lty=2))
  # with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE),lines(1-upr.CI,value,col=adjustcolor(cols[i],0.5),lwd=1,lty=2))
  legend("bottomleft",legend=c(alts.sort[c(1,2,i)]),
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  axis_fun(1,xmaj,xmin,format(xmaj))
  if(i==3){mtext(side=3, adj=0,"Lake Okeechobee")}
  if(i==3){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()

# https://stackoverflow.com/a/26104829
# decdf <- function(x, baseline, treatment)  (ecdf(baseline)(x)) - (ecdf(treatment)(x))
# treat.a=subset(lakeO.stage,Alt==alts.sort[1])$STAGE
# treat.b=subset(lakeO.stage,Alt==alts.sort[3])$STAGE
# test=decdf(x,subset(lakeO.stage,Alt==alts.sort[1])$STAGE,subset(lakeO.stage,Alt==alts.sort[3])$STAGE)
# curve(decdf(x,treat.a,treat.b), from=min(treat.a,treat.b), to=max(treat.a,treat.b))
# png(filename=paste0(plot.path,"Iteration_2/LO_StageDuration_Diff.png"),width=7.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:6,1,6,byrow=T))

xlim.val=c(8,18);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(-0.01,0.2);by.y=0.05;ymaj=seq(max(ylim.val[1],0),ylim.val[2],by.y);ymin=seq(max(ylim.val[1],0),ylim.val[2],by.y/2)

for(i in 3:n.alts){
  x.val=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[1])$STAGE)
  x.val$value=round(x.val$value,4)
  y.val=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE)
  y.val$value=round(y.val$value,4)
  tmp=merge(x.val,y.val,"value")
  tmp$diff.val=with(tmp,proportion.x-proportion.y)
  
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  abline(h=0)
  with(tmp,lines(diff.val~value,col=cols[i],lwd=2))
  axis_fun(1,xmaj,xmin,format(xmaj))
  text(xlim.val[1],ylim.val[2],paste0("FWO - ",alts.sort[i]),pos=4,font=2)
  
  if(i==3){mtext(side=3, adj=0,"Lake Okeechobee")}
  if(i==3){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  if(i==3){mtext(side=2,line=3,"Difference in SDC")}
}
# mtext(side=2,line=1.75,outer=T,"Difference in SDC")
mtext(side=1,line=1,outer=T,"Stage Elevation (Ft, NGVD29)")
dev.off()

test=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[1])$STAGE)
max(subset(test,max(proportion)>0.40)$value)
max(subset(test,min(proportion)<0.40)$proportion)

subset(test,round(proportion,2)==0.70)

SDC_seg=ddply(lakeO.stage,"Alt",summarise,
      SDC_point_10=min(subset(ecdf_fun(STAGE),(1-proportion)<0.10)$value),
      SDC_point_20=min(subset(ecdf_fun(STAGE),1-proportion<0.20)$value),
      SDC_point_30=min(subset(ecdf_fun(STAGE),1-proportion<0.30)$value),
      SDC_point_40=min(subset(ecdf_fun(STAGE),1-proportion<0.40)$value),
      SDC_point_50=min(subset(ecdf_fun(STAGE),1-proportion<0.50)$value),
      SDC_point_60=min(subset(ecdf_fun(STAGE),1-proportion<0.60)$value),
      SDC_point_70=min(subset(ecdf_fun(STAGE),1-proportion<0.70)$value),
      SDC_point_80=min(subset(ecdf_fun(STAGE),1-proportion<0.80)$value),
      SDC_point_90=min(subset(ecdf_fun(STAGE),1-proportion<0.90)$value))
SDC_seg=SDC_seg[match(alts.sort,SDC_seg$Alt),]
SDC_seg[,2:10]=round(SDC_seg[,2:10],2)
SDC_seg%>%
flextable()%>%
  colformat_double(j=2:10,digits=2,na_str="---")%>%
  fontsize(size=13,part="body")%>%
  fontsize(size=14,part="header")%>%
  font(fontname="Times New Roman",part="all")%>%
  align(j=2:10,part="all",align="center")%>%
  bold(part="header")%>%
  padding(padding=1,part="all")%>%
  set_header_labels("SDC_point_10"="10% Pt", 
                    "SDC_point_20"="20% Pt", 
                    "SDC_point_30"="30% Pt", 
                    "SDC_point_40"="40% Pt", 
                    "SDC_point_50"="50% Pt", 
                    "SDC_point_60"="60% Pt", 
                    "SDC_point_70"="70% Pt", 
                    "SDC_point_80"="80% Pt", 
                    "SDC_point_90"="90% Pt")%>%
  autofit()#%>%print(preview="pptx")

par(family="serif",mar=c(1,1.5,0.1,0.1),oma=c(3,3.5,0.75,0.5));
layout(matrix(1:72,8,8))

tmp.comp=data.frame()
for(i in 1:8){
  for(j in 1:8){
    alt.comp1=subset(SDC_seg,Alt==alts.sort[i])[,2:10]
    alt.comp2=subset(SDC_seg,Alt==alts.sort[j])[,2:10]
    tmp=t(alt.comp1-alt.comp2)
    tmp.comp=rbind(tmp.comp,data.frame(prop=seq(0.1,0.9,0.1),
               Alt1=alts.sort[i],
               Alt2=alts.sort[j],
               value=as.numeric(tmp)))
  }
}
tmp.comp

# png(filename=paste0(plot.path,"Iteration_2/LO_StageDuration_diff.png"),width=7.5,height=5.25,units="in",res=200,type="windows",bg="white")
xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x)
ylim.val=c(-1,1);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(1,1.5,0.1,0.1),oma=c(3,2.5,1,1.5));
layout(matrix(1:49,7,7,byrow = T))
for(i in 1:7){
  if(i!=1){for(k in 1:(i-1)){plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)}}
  
  alts2=alts.sort[-1:-i]
  for(j in 1:length(alts2)){
    plot(value~prop,subset(tmp.comp,Alt1==alts.sort[i]&Alt2==alts2[j]),ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
    
    abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
    abline(h=0)
    with(subset(tmp.comp,Alt1==alts.sort[i]&Alt2==alts2[j]),points(prop,value,pch=21,bg="red",lwd=0.1))
    if(j==1){axis_fun(1,xmaj,xmin,format(xmaj),cex=0.8,line=-0.75)}else{axis_fun(1,xmaj,xmin,NA,cex=0.8,line=-0.5)}
    if(j==1){axis_fun(2,ymaj,ymin,format(ymaj),cex=0.8)}else{axis_fun(2,ymaj,ymin,NA,cex=0.8)}
    box(lwd=1)
    if(i==1){mtext(side=3,alts2[j])}
    if(j==length(alts2)){mtext(side=4,line=0.25,alts.sort[i])}
    }
}
mtext(side=2,outer=T,"Difference in SDC (Ft)")
mtext(side=1,outer=T,"Proportion of Time")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/LO_StageDuration_segments.png"),width=7.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1.05);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],0.1)
ylim.val=c(8,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(j in 1:n.alts){
test=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[j])$STAGE)
test$proportion=1-test$proportion
plot(value~proportion,test,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(test,lines(proportion,value,col=adjustcolor(cols[j],0.5),lwd=2))

prop.val=seq(0.1,0.9,0.1)
for(i in 1:length(prop.val)){
  tmp=min(subset(test,proportion<prop.val[i])$value)
  segments(prop.val[i],0,prop.val[i],tmp,col=adjustcolor("red",0.25))
  points(prop.val[i],tmp,pch=21,bg=adjustcolor("red",0.25),col=adjustcolor("black",0.25))
  text(prop.val[i],tmp,pos=4,format(round(tmp,2),nsmall=2),cex=0.75)
}
if(j%in%c(1:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
if(j%in%c(1,5)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
if(j==1){mtext(side=3, adj=0,"Lake Okeechobee")}
if(j==4){mtext(side=3, adj=1,"CY 1965 - 2016")}
mtext(side=3, adj=1,line=-1.25,paste0(alts.sort[j]," "))
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()

###
x.val=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[1])$STAGE)
x.val$value=round(x.val$value,4)
y.val=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[3])$STAGE)
y.val$value=round(y.val$value,4)
tmp=merge(x.val,y.val,"value")
tmp$diff.val=with(tmp,proportion.x-proportion.y)
plot(diff.val~value,tmp)

test=decdf(x,subset(lakeO.stage,Alt==alts.sort[1])$STAGE,subset(lakeO.stage,Alt==alts.sort[3])$STAGE)
plot(test)


highlow.ecdf=data.frame()
for(i in 1:n.alts){
  tmp.ecdf=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[i])$STAGE)
  tmp.ecdf$proportion=1-tmp.ecdf$proportion
  
  # plot(value~proportion,tmp.ecdf)
  # lines(c(-1,max(subset(tmp.ecdf,value>=17)$proportion)),y=c(17,17),col="Red")
  # lines(c(max(subset(tmp.ecdf,value>=17)$proportion),max(subset(tmp.ecdf,value>=17)$proportion)),
  #       y=c(0,17),col="red")
  # lines(c(-1,max(subset(tmp.ecdf,value>=10)$proportion)),y=c(10,10),col="Red")
  # lines(c(max(subset(tmp.ecdf,value>=10)$proportion),max(subset(tmp.ecdf,value>=10)$proportion)),
  #       y=c(0,10),col="red")

  high.val=max(subset(tmp.ecdf,value>=17)$proportion)
  low.val=max(subset(tmp.ecdf,value>=10)$proportion)
  tmp=data.frame(Alt=alts.sort[i],high.prop=high.val,low.prop=low.val)
  highlow.ecdf=rbind(tmp,highlow.ecdf)
}
highlow.ecdf

highlow.ecdf=highlow.ecdf[match(alts.sort,highlow.ecdf$Alt),]
highlow.ecdf$high.prop[is.infinite(highlow.ecdf$high.prop)==T]<-NA
# write.csv(highlow.ecdf,paste0(export.path,"iter2_highlow_ecdf_rslt.csv"),row.names = F)

# RECOVER Stage Envelope --------------------------------------------------
## 
AprSep=seq(date.fun("1965-04-15"),date.fun("1965-09-15"),"1 days")
MayAug=seq(date.fun("1965-05-01"),date.fun("1965-09-01"),"1 days")

stg.env=lakeO.stage1
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
# write.csv(subset(stg.env.CY,Alt=='ECBr'),paste0(export.path,"ECBr_stgenv.csv"),row.names = F)

## Checking Stage Envelope excel worksheet
# test=openxlsx::read.xlsx(paste0(wd,"/_resources/IMC_Code/StageEnvelopeWorksheet_ECBr_11June2021.xlsx"),sheet=2,startRow = 10)
# test$Date=date.fun(openxlsx::convertToDate(test$Date))
# test=test[1:18993,c(1,3,7)]
# colnames(test)=c("Date","STAGE","Envelope")
# test$STAGE=as.numeric(test$STAGE)
# test$CY=as.numeric(format(test$Date,"%Y"))
# test$month_day=as.character(format(test$Date,"%m_%d"))
# test$AugDec.stg=with(test,ifelse(as.numeric(format(Date,"%m"))%in%c(8:12),STAGE,NA))
# test$JuneJuly.stg=with(test,ifelse(as.numeric(format(Date,"%m"))%in%c(6,7),STAGE,NA))
# test$AprSep.stg=with(test,ifelse(month_day%in%as.character(format(AprSep,"%m_%d")),STAGE,NA))
# test$MayAug.stg=with(test,ifelse(month_day%in%as.character(format(MayAug,"%m_%d")),STAGE,NA))
# 
# test.sum=ddply(subset(test,CY%in%seq(1965,2016,1)),c("CY"),summarise,
#       max.stg=max(STAGE,na.rm=T),
#       AugDec.max=max(AugDec.stg,na.rm=T),
#       JuneJuly_13=sum(JuneJuly.stg>13,na.rm=T),
#       MayAug_11.5=sum(MayAug.stg<11.5,na.rm=T),
#       AprSep_12=sum(AprSep.stg<12,na.rm=T),
#       env.val=min(Envelope,na.rm=T),
#       env.val.max=max(Envelope,na.rm=T))
# 
# plot(test.sum$max.stg~subset(stg.env.CY,Alt=='ECBr')$max.stg);abline(0,1)
# 
# sum(test.sum$max.stg==subset(stg.env.CY,Alt=='ECBr')$max.stg)
# sum(test.sum$AugDec.max==subset(stg.env.CY,Alt=='ECBr')$AugDec.max)
# sum(test.sum$JuneJuly_13==subset(stg.env.CY,Alt=='ECBr')$JuneJuly_13)
# sum(test.sum$MayAug_11.5==subset(stg.env.CY,Alt=='ECBr')$MayAug_11.5)
# sum(test.sum$AprSep_12==subset(stg.env.CY,Alt=='ECBr')$AprSep_12)
# #plot(test.sum$AprSep_12~subset(stg.env.CY,Alt=='ECBr')$AprSep_12);abline(0,1)
# 
# test.sum$env.val2=1
# for(j in 2:nrow(tmp)){
#   test.sum$env.val2[j]=with(test.sum,
#                             ifelse(max.stg[j-1]>17|JuneJuly_13[j-1]>=30,2,
#                                    ifelse(AugDec.max[j-1]<=16&(MayAug_11.5[j-1]>=60|AprSep_12[j-1]>=90),1,env.val2[j-1])))
# }
# subset(test.sum,env.val!=env.val2)
# sum(test.sum$env.val!=test.sum$env.val2)
# plot(test.sum$env.val~test.sum$env.val2)
# write.csv(test.sum,paste(export.path,"stage_envelope_verify.csv"),row.names = F)
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
env.rslt=merge(env.rslt,data.frame(Alt=alts.sort,PlotOffset=rev(seq(2,16,2))),"Alt")
env.rslt$env.plt=with(env.rslt,env2+PlotOffset)
env.rslt$env.f=with(env.rslt,ifelse(env==1,"normal","recovery"))

reshape2::dcast(env.rslt,Alt~env.f,value.var = "env",function(x) N.obs(x))



# png(filename=paste0(plot.path,"Iteration_2/LakeO_Env.png"),width=8,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1.75,0.5),oma=c(2,2,1,0.25));

ylim.val=c(2,17)
xlim.val=c(1965,2016);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(env2~CY,env.rslt,type="n",axes=F,ann=F,xlim=xlim.val,xaxs="i",ylim=ylim.val)
abline(v=c(xmaj,xmin),lty=c(1,3),lwd=0.5,col=c("black","grey"))
for(i in 1:length(alts.sort)){
  with(subset(env.rslt,Alt==alts.sort[i]),lines(env.plt~CY,type="s",col=cols[i],lwd=2.5))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.8,cex=0.6)
axis_fun(2,seq(3,17,2),seq(3,17,2),rev(alts.sort))
box(lwd=1)
mtext(side=3,adj=1,"Upper Step = Normal Envelope\nLower Step = Recovery Envelope",font=3)
mtext(side=1,line=1.25,"Calendar Year")
dev.off()

## ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Lake_Okeechobee/lok_stage_envelope.pdf
##
library(LORECOVER)

head(lakeO.stage)
lakeO.stage1$Data.Value=lakeO.stage$STAGE

norm.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage1,Alt==alts[i])
  rslt=norm_env(tmp)
  rslt$Alt=alts[i]
  norm.lakeO.stage.scr=rbind(norm.lakeO.stage.scr,rslt)
  print(i)
}
norm.lakeO.stage.scr=rename(norm.lakeO.stage.scr,c("penalty"="norm.score"))
norm.lakeO.stage.scr$WY=WY(norm.lakeO.stage.scr$Date)
# norm.lakeO.stage.scr=subset(norm.lakeO.stage.scr,WY%in%seq(1966,2016,1));# Full Florida WY (MAy - April) 
# lakeO.stage.scr$cum.abs.pen=with(lakeO.stage.scr,ave(score,Alt_CY,FUN=function(x)cumsum(abs(x))))

head(norm.lakeO.stage.scr)
norm.lakeO.stage.scr.WY=ddply(norm.lakeO.stage.scr,c("Alt","WY"),summarise,TScore=sum(abs(norm.score),na.rm=T))
norm.lakeO.stage.scr.WY$Alt=factor(norm.lakeO.stage.scr.WY$Alt,levels=alts.sort)
norm.lakeO.stage.scr.WY.sum=ddply(norm.lakeO.stage.scr.WY,"Alt",summarise,mean.val=mean(TScore))
norm.lakeO.stage.scr.WY.sum$Alt=factor(norm.lakeO.stage.scr.WY.sum$Alt,levels=alts.sort)
norm.lakeO.stage.scr.WY.sum$FWO.diff=with(norm.lakeO.stage.scr.WY.sum,(mean.val-mean.val[1])/mean.val[1])*100

boxplot(TScore~Alt,norm.lakeO.stage.scr.WY,outline=F)

rec.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage1,Alt==alts[i])
  rslt=rec_env(tmp)
  rslt$Alt=alts[i]
  rec.lakeO.stage.scr=rbind(rec.lakeO.stage.scr,rslt)
  print(i)
}
rec.lakeO.stage.scr=rename(rec.lakeO.stage.scr,c("penalty"="rec.score"))
rec.lakeO.stage.scr$WY=WY(rec.lakeO.stage.scr$Date)
# rec.lakeO.stage.scr=subset(rec.lakeO.stage.scr,WY%in%seq(1966,2016,1));# Full Florida WY (MAy - April) 

rec.lakeO.stage.scr.WY=ddply(rec.lakeO.stage.scr,c("Alt","WY"),summarise,TScore=sum(abs(rec.score),na.rm=T))
rec.lakeO.stage.scr.WY$Alt=factor(rec.lakeO.stage.scr.WY$Alt,levels=alts.sort)
rec.lakeO.stage.scr.WY.sum=ddply(rec.lakeO.stage.scr.WY,"Alt",summarise,mean.val=mean(TScore))
rec.lakeO.stage.scr.WY.sum$Alt=factor(rec.lakeO.stage.scr.WY.sum$Alt,levels=alts.sort)
rec.lakeO.stage.scr.WY.sum$FWO.diff=with(rec.lakeO.stage.scr.WY.sum,(mean.val-mean.val[1])/mean.val[1])*100

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

env.pen.sum=ddply(lakeO.stage.scr,"Alt",summarise,
                  N.val=N.obs(score),
      pen_above=sum(score[score>0],na.rm=T),
      pen_below=sum(abs(score)[score<0],na.rm=T),
      per_below=(sum(score<0)/N.obs(score))*100,
      per0=(sum(score==0,na.rm=T)/N.obs(score))*100,
      per_above=(sum(score>0)/N.obs(score))*100)
env.pen.sum=env.pen.sum[match(alts.sort,env.pen.sum$Alt),]
env.pen.sum$FWO_PerBelow=with(env.pen.sum,(per_below-per_below[1])/per_below[1])*100
env.pen.sum$FWO_PerWith=with(env.pen.sum,(per0-per0[1])/per0[1])*100
env.pen.sum$FWO_PerAbove=with(env.pen.sum,(per_above-per_above[1])/per_above[1])*100
# write.csv(env.pen.sum,paste0(export.path,"Iteration2/iter2_LOMetrics.csv"),row.names=F)
# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_BWA.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,60);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(1,1,0.5,0.25),oma=c(2,2.5,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

cols.val2=c(rgb(255/255,255/255,0),rgb(143/255,188/255,143/255),rgb(100/255,149/255,237/255))
x=barplot(t(env.pen.sum[,c("per_below","per0","per_above")]),beside=T,
        ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,v=x[2,],lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(env.pen.sum[,c("per_below","per0","per_above")]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=cols.val2,add=T,xaxt="n")
x.val=x[2,] #x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,alts.sort,line=-0.5,cex=0.90)
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
env.pen.sum.plns=subset(env.pen.sum,Alt%in%alts.sort[3:8])

# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_BWA_FWO.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
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
axis_fun(1,x.val,x.val,alts.sort[3:8],line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Plan Name")
mtext(side=2,line=2,"Average Percent Difference to FWO",cex=1)
mtext(side=3,adj=0,"Lake Okeechobee")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("Below (PM38)","Within (PM39)","Above (PM40)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c("dodgerblue1","forestgreen","indianred1"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Stage Envelope")

dev.off()

env.pen.sum=ddply(subset(lakeO.stage.scr,WY%in%WYs),"Alt",summarise,
                  pen_above=sum(score>0,na.rm=T),
                  pen_below=sum(score<0,na.rm=T),
                  per_below=(pen_below/N.obs(score))*100,
                  per0=(sum(score==0,na.rm=T)/N.obs(score))*100,
                  per_above=(pen_above/N.obs(score))*100)
env.pen.sum=env.pen.sum[match(alts.sort,env.pen.sum$Alt),]
env.pen.sum$FWO_PerBelow=with(env.pen.sum,(per_below-per_below[1])/per_below[1])*100
env.pen.sum$FWO_PerWith=with(env.pen.sum,(per0-per0[1])/per0[1])*100
env.pen.sum$FWO_PerAbove=with(env.pen.sum,(per_above-per_above[1])/per_above[1])*100

env.pen.sum.plns=subset(env.pen.sum,Alt%in%alts.sort[3:8])
# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_BWA_FWO2.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
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
axis_fun(1,x.val,x.val,alts.sort[3:8],line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Plan Name")
mtext(side=2,line=2,"Average Percent Difference to FWO",cex=1)
mtext(side=3,adj=0,"Lake Okeechobee")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("Below (PM38)","Within (PM39)","Above (PM40)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c("dodgerblue1","forestgreen","indianred1"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Stage Envelope")

dev.off()

# stg.scr.sum=ddply(subset(lakeO.stage.scr,WY%in%WYs),"Alt",summarise,mean.val=mean(sum(abs(score)),na.rm=T))
# stg.scr.sum$FWO.perdiff=with(stg.scr.sum,((mean.val-mean.val[1])/mean.val[1])*100)

lakeO.stage.scr.WY=ddply(subset(lakeO.stage.scr,WY%in%WYs),c("Alt","WY"),summarise,cum.pen=sum(abs(score),na.rm=T))
stg.scr.sum=ddply(lakeO.stage.scr.WY,"Alt",summarise,mean.val=mean(cum.pen,na.rm=T))
stg.scr.sum$FWO.perdiff=with(stg.scr.sum,((mean.val-mean.val[1])/mean.val[1])*100)
# write.csv(stg.scr.sum,paste0(export.path,"stg_score_perdiff.csv"),row.names=F)

# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_all_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

boxplot(cum.pen~Alt,subset(lakeO.stage.scr.WY,WY%in%WYs),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,stg.scr.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(lakeO.stage.scr.WY,WY%in%WYs&Alt==alts.sort[1])$cum.pen),lty=2,col="black")
abline(h=mean(subset(lakeO.stage.scr.WY,WY%in%WYs&Alt==alts.sort[1])$cum.pen),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
# axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"Lake Okeechobee")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Stage Envelope\nAnnual Score (unitless)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_all_sum.png"),width=3.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));

xlim.val=c(-25,50);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(stg.scr.sum$FWO.perdiff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(stg.scr.sum,segments(FWO.perdiff,1:8,rep(0,8),1:8))
with(stg.scr.sum,points(FWO.perdiff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,norm.lakeO.stage.scr.WY.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Stage Envelope")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()
## 
# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_bxp.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
# par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,4,1,0.25));
par(family="serif",mar=c(1,3,0.25,1),oma=c(4,2,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TScore~Alt,norm.lakeO.stage.scr.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,norm.lakeO.stage.scr.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(norm.lakeO.stage.scr.WY,Alt==alts.sort[1])$TScore),lty=2,col="black")
abline(h=subset(norm.lakeO.stage.scr.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
# axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"Lake Okeechobee")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Normal Lake Envelope\nAnnual Score (unitless)")

ylim.val=c(0,2500);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TScore~Alt,rec.lakeO.stage.scr.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,rec.lakeO.stage.scr.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(rec.lakeO.stage.scr.WY,Alt==alts.sort[1])$TScore),lty=2,col="black")
abline(h=subset(rec.lakeO.stage.scr.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"Recovery Lake Envelope\nAnnual Score (unitless)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/LakeO_EnvScore_sum.png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-25,50);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(norm.lakeO.stage.scr.WY.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(norm.lakeO.stage.scr.WY.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(norm.lakeO.stage.scr.WY.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,norm.lakeO.stage.scr.WY.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Normal Envelope")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')

xlim.val=c(-25,50);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(rec.lakeO.stage.scr.WY.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(rec.lakeO.stage.scr.WY.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(rec.lakeO.stage.scr.WY.sum,points(FWO.diff,1:8,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:8,1:8,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Recovery Envelope")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
dev.off()

# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S78","S79","S80","S308","S351","S352","S354","S77_QFC",
            "S308_QFC","S80_QPFCSOURCE_LAKE","S79_QPFCSOURCE_LAKE","S79_QFC","S80_QFC","TMC2EST","S48","S49","NSF2EST")
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

q.dat$Alt_SITE=paste(q.dat$Alt,q.dat$SITE,sep="_")
q.dat$Q.14=with(q.dat,ave(FLOW,Alt_SITE,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))

q.dat$CRE.low=with(q.dat,ifelse(SITE=="S79"&Q.14<750,1,0))
q.dat$CRE.dam=with(q.dat,ifelse(SITE=="S79"&Q.14>2600,1,0))
q.dat$CRE.opt=with(q.dat,ifelse(SITE=="S79"&Q.14>=750&Q.14<2100,1,0))
q.dat$CRE.stress=with(q.dat,ifelse(SITE=="S79"&Q.14>=2100&Q.14<=2600,1,0))

q.dat$SLE.low=with(q.dat,ifelse(SITE=="S80"&Q.14<150,1,0))
q.dat$SLE.dam=with(q.dat,ifelse(SITE=="S80"&Q.14>1700,1,0))
q.dat$SLE.opt=with(q.dat,ifelse(SITE=="S80"&Q.14>=150&Q.14<1400,1,0))
q.dat$SLE.stress=with(q.dat,ifelse(SITE=="S80"&Q.14>=1400&Q.14<1700,1,0))

q.dat$hydro.season=with(q.dat,FL.Hydroseason(q.dat$Date))
q.dat$bloom.period=with(q.dat,ifelse(SITE%in%c("S77","S78","S79"),
                                     ifelse(as.numeric(format(Date,"%m"))%in%c(6:9),"bloom","no.bloom"),
                                     ifelse(as.numeric(format(Date,"%m"))%in%c(5:9),"bloom","no.bloom")))

q.dat1=q.dat
q.dat=subset(q.dat,WY%in%WYs);# Full Florida WY (MAy - April) 

ddply(q.dat1,"Alt",summarise,stress.count=sum(CRE.stress,na.rm=T))
ddply(subset(q.dat1,SITE=="S79"),"Alt",summarise,stress.Q=sum(Q.14[CRE.stress==1],na.rm=T)/1000)

est.allPOS.sum=ddply(q.dat1,c("SITE","CY","Alt"),summarise,TQ=sum(cfs.to.acftd(FLOW),na.rm=T))
est.allPOS.sum=ddply(est.allPOS.sum,c("SITE","Alt"),summarise,Avg.TQ.kacft=mean(TQ/1000))
# write.csv(est.allPOS.sum,paste0(export.path,"POS_eststruct_sum.csv"),row.names = F)


q.dat.xtab=reshape2::dcast(q.dat,Alt+Date+WY~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
head(q.dat.xtab)
q.dat.xtab$FlowSouth=rowSums(q.dat.xtab[,c("S351","S354")],na.rm=T)
q.dat.xtab$S79.14d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S79.30d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,29),rollapply(x,width=30,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80.14d=with(q.dat.xtab,ave(S80,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))

q.dat.xtab$CRE.low=with(q.dat.xtab,ifelse(S79.14d<750,1,0))
q.dat.xtab$CRE.dam=with(q.dat.xtab,ifelse(S79.14d>2600,1,0))
q.dat.xtab$CRE.opt=with(q.dat.xtab,ifelse(S79.14d>=750&S79.14d<2100,1,0))
q.dat.xtab$CRE.stress=with(q.dat.xtab,ifelse(S79.14d>=2100&S79.14d<=2600,1,0))

q.dat.xtab$SLE.low=with(q.dat.xtab,ifelse(S80.14d<150,1,0))
q.dat.xtab$SLE.dam=with(q.dat.xtab,ifelse(S80.14d>1700,1,0))
q.dat.xtab$SLE.opt=with(q.dat.xtab,ifelse(S80.14d>=150&S80.14d<1400,1,0))
q.dat.xtab$SLE.stress=with(q.dat.xtab,ifelse(S80.14d>=1400&S80.14d<=1700,1,0))


q.dat1.xtab=reshape2::dcast(q.dat1,Alt+Date+CY~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
# q.dat1.xtab$Alt_Yr=with(q.dat1.xtab,paste(Alt,CY,sep="_"))
q.dat1.xtab$S79.30d=with(q.dat1.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,29),rollapply(x,width=30,FUN=function(x)mean(x,na.rm=T)))))
q.dat1.xtab$exceed=with(q.dat1.xtab,ifelse(is.na(S79.30d)==T,0,ifelse(S79.30d<457,1,0)))
###

## CRE Team flow categories analysis
q.dat1.xtab$S79.14d=with(q.dat1.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat1.xtab$Alt=factor(q.dat1.xtab$Alt,levels=alts.sort)
# based on 14-day
q.dat1.xtab$QLT457=with(q.dat1.xtab,ifelse(S79.14d<457,1,0))
q.dat1.xtab$Q457_750=with(q.dat1.xtab,ifelse(S79.14d>=457&S79.14d<750,1,0))
q.dat1.xtab$Q_Opt=with(q.dat1.xtab,ifelse(S79.14d>=750&S79.14d<2100,1,0))
q.dat1.xtab$Q_Stress=with(q.dat1.xtab,ifelse(S79.14d>=2100&S79.14d<2600,1,0))
q.dat1.xtab$Q_Dam=with(q.dat1.xtab,ifelse(S79.14d>=2600,1,0))
q.dat1.xtab$Q2600_4500=with(q.dat1.xtab,ifelse(S79.14d>=2600&S79.14d<4500,1,0))
q.dat1.xtab$Q4500_6500=with(q.dat1.xtab,ifelse(S79.14d>=4500&S79.14d<6500,1,0))
q.dat1.xtab$QGT6500=with(q.dat1.xtab,ifelse(S79.14d>6500,1,0))

CRE.QCat.POS.sum=ddply(q.dat1.xtab,"Alt",summarise,
                       N.LT457=sum(QLT457,na.rm=T),
                       N.Q457_750=sum(Q457_750,na.rm=T),
                       N.Q_Opt=sum(Q_Opt,na.rm=T),
                       N.Q_Stress=sum(Q_Stress,na.rm=T),
                       N.Q_Dam=sum(Q_Dam,na.rm=T),
                       N.Q2600_4500=sum(Q2600_4500,na.rm=T),
                       N.Q4500_6500=sum(Q4500_6500,na.rm = T),
                       N.QGT6500=sum(QGT6500,na.rm=T),
                       N.total=N.obs(Alt))
CRE.QCat.POS.sum
CRE.QCat.POS.sum=merge(CRE.QCat.POS.sum,
                       reshape2::dcast(est.allPOS.sum,Alt~SITE,value.var="Avg.TQ.kacft",sum)[,c("Alt","S77_QFC","S79",'S77')],"Alt")
# write.csv(CRE.QCat.POS.sum,paste0(export.path,"Iteration2/iter2_CREQ_metrics.csv"),row.names=F)

q.dat1.xtab$QLT457=with(q.dat1.xtab,ifelse(S79<457,1,0))
q.dat1.xtab$Q457_750=with(q.dat1.xtab,ifelse(S79>=457&S79<750,1,0))
q.dat1.xtab$Q_Opt=with(q.dat1.xtab,ifelse(S79>=750&S79<2100,1,0))
q.dat1.xtab$Q_Stress=with(q.dat1.xtab,ifelse(S79>=2100&S79<2600,1,0))
q.dat1.xtab$Q_Dam=with(q.dat1.xtab,ifelse(S79>=2600,1,0))
q.dat1.xtab$Q2600_4500=with(q.dat1.xtab,ifelse(S79>=2600&S79<4500,1,0))
q.dat1.xtab$Q4500_6500=with(q.dat1.xtab,ifelse(S79>=4500&S79<6500,1,0))
q.dat1.xtab$QGT6500=with(q.dat1.xtab,ifelse(S79>6500,1,0))
q.dat1.xtab$month=as.numeric(format(q.dat1.xtab$Date,"%m"))
q.dat1.xtab$AMO_period=with(q.dat1.xtab,ifelse(CY%in%seq(1965,1994,1),"dry_cold","wet_warm"))

# range(q.dat1.xtab$Date)
CRE.QCat.POS.sum=ddply(q.dat1.xtab,"Alt",summarise,
      N.LT457=sum(QLT457,na.rm=T),
      N.Q457_750=sum(Q457_750,na.rm=T),
      N.Q_Opt=sum(Q_Opt,na.rm=T),
      N.Q_Stress=sum(Q_Stress,na.rm=T),
      N.Q2600_4500=sum(Q2600_4500,na.rm=T),
      N.Q4500_6500=sum(Q4500_6500,na.rm = T),
      N.QGT6500=sum(QGT6500,na.rm=T),
      N.total=N.obs(Alt))
CRE.QCat.POS.sum


CRE.QCat.POS=ddply(q.dat1.xtab,"Alt",summarise,
                       Q.LT457=sum(cfs.to.acftd(S79[QLT457==1])/1000,na.rm=T),
                       Q.Q457_750=sum(cfs.to.acftd(S79[Q457_750==1])/1000,na.rm=T),
                       Q.Q_Opt=sum(cfs.to.acftd(S79[Q_Opt==1])/1000,na.rm=T),
                       Q.Q_Stress=sum(cfs.to.acftd(S79[Q_Stress==1])/1000,na.rm=T),
                       Q.Q2600_4500=sum(cfs.to.acftd(S79[Q2600_4500==1])/1000,na.rm=T),
                       Q.Q4500_6500=sum(cfs.to.acftd(S79[Q4500_6500==1])/1000,na.rm = T),
                       Q.QGT6500=sum(cfs.to.acftd(S79[QGT6500==1])/1000,na.rm=T))

apply(CRE.QCat.POS[,2:8],2,max,na.rm=T)
cols2=c("grey50","black",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))
labs=c("<457","457 - 750","750 - 2100","2100 - 2600","2600 - 4500","4500 - 6500",">6500")
# png(filename=paste0(plot.path,"Iteration_2/S79Q_cat_total.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:8),2,4,byrow=T))
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);

ymax=c(5000,8000,25000,5000,26000,16000,20000)
yval=ymax/2
for(i in 2:8){
if(i==2){
ylim.val=c(10,5000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
x=barplot(CRE.QCat.POS[,i],col=adjustcolor(cols2,0.5),log="y",ylim=ylim.val,space=0,axes=F,ann=F)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,NA);box(lwd=1)
mtext(side=3,adj=0,paste(labs[i-1],"cfs"),cex=0.75)
text(x,CRE.QCat.POS[,i],round(CRE.QCat.POS[,i],0),font=2,col="black",pos=1,cex=0.4)
}else{
  ylim.val=c(0,ymax[i-1]);by.y=yval[i-1];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  x=barplot(CRE.QCat.POS[,i],col=adjustcolor(cols2,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(5:8)){axis_fun(1,x,x,alts.sort,cex=0.8,las=2)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  mtext(side=3,adj=0,paste(labs[i-1],"cfs"),cex=0.75)
  text(x,CRE.QCat.POS[,i],round(CRE.QCat.POS[,i],0),font=2,col="black",pos=1,cex=0.4)
}
}
plot(0:1,0:1,ann=F,axes=F,type="n")
text(1,0.15,"S79 Daily Discharge\n \nPeriod of Simulation\n CY1965 - 2016.",adj=1)
mtext(side=1,line=0.5,outer=T,"Alternative")
mtext(side=2,line=1.5,outer=T,"Total Discharge (x1000 Ac-Ft d\u207B\u00B9)")
dev.off()


library(fmsb)
# png(filename=paste0(plot.path,"Iteration_2/S79Q_radar.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:8),2,4,byrow=T))
par(family="serif",mar = c(1, 1, 1, 1),oma=c(1,1,1,1),xpd=NA)

for(i in 3:8){
alts.val=c("ECBr","NA25",alts.sort[i])
col.vals=cols2[alts.sort%in%alts.val]
tmp=rbind(rep(max(apply(subset(CRE.QCat.POS.sum,Alt%in%alts.val)[,2:8],1,max,na.rm=T)),7),
          rep(min(apply(subset(CRE.QCat.POS.sum,Alt%in%alts.val)[,2:8],1,min,na.rm=T)),7),
          subset(CRE.QCat.POS.sum,Alt%in%alts.val)[,2:8])
subset(CRE.QCat.POS.sum,Alt%in%alts.val)[,1:8]

labs=c("<457","457 -\n750","750 -\n2100\n(Opt)","2100 -\n2600\n(Stress)","2600 -\n4500","4500 -\n6500",">6500")
radarchart(tmp,
           pcol=col.vals,
           plwd=1.25,pty=NA,
           plty=c(2,1,1),
           cglcol = "grey", cglty = 1, cglwd = 0.8,
           axislabcol = "grey",
           vlabels=labs,vlcex=0.75)
mtext(side=3,adj=0,line=-2,alts.sort[i],font=2)
if(i==3){mtext(side=3,adj=0,"Count of S79 Daily Flow")}
# if(i==6){mtext(side=3,adj=1,"Period of Sim: CY 1965 - 2016")}
# box(lwd=1)
# legend("bottom",legend=alts.val,horiz=T,
#       bty="n",pch=20,pt.cex=2,col=col.vals)
}
plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0.5,legend=alts.sort,
        pch=NA,pt.cex=2,lty=c(2,1,rep(1,6)),col=cols2,
        bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title="Alternatives",title.adj = 0)
plot(0:1,0:1,ann=F,axes=F,type="n")
text(1,0,"Period of Simulation\n CY1965 - 2016.",adj=1)
dev.off()


#### SLE version
q.dat1.xtab$S80.14d=with(q.dat1.xtab,ave(S80,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat1.xtab$S80_QFC.14d=with(q.dat1.xtab,ave(S80_QFC,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat1.xtab$Alt=factor(q.dat1.xtab$Alt,levels=alts.sort)
# based on 14-day
q.dat1.xtab$QLT150=with(q.dat1.xtab,ifelse(S80.14d<150,1,0))
q.dat1.xtab$Q_Opt_SLE=with(q.dat1.xtab,ifelse(S80.14d>=150&S80.14d<1400,1,0))
q.dat1.xtab$Q_Stress_SLE=with(q.dat1.xtab,ifelse(S80.14d>=1400&S80.14d<1700,1,0))
q.dat1.xtab$Q_Dam_SLE=with(q.dat1.xtab,ifelse(S80.14d>=1700,1,0))

SLE.QCat.POS.sum=ddply(q.dat1.xtab,"Alt",summarise,
                       N.LT150=sum(QLT150,na.rm=T),
                       N.Q_Opt_SLE=sum(Q_Opt_SLE,na.rm=T),
                       N.Q_Stress_SLE=sum(Q_Stress_SLE,na.rm=T),
                       N.Q_Dam_SLE=sum(Q_Dam_SLE,na.rm=T),
                       N.total=N.obs(Alt))
SLE.QCat.POS.sum
SLE.QCat.POS.sum=merge(SLE.QCat.POS.sum,
                       reshape2::dcast(est.allPOS.sum,Alt~SITE,value.var="Avg.TQ.kacft",sum)[,c("Alt","S308_QFC","S308",'S80')],"Alt")
# write.csv(SLE.QCat.POS.sum,paste0(export.path,"Iteration2/iter2_SLEQ_metrics.csv"),row.names=F)


## Monthly average flow
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

## RECOVER plots
q.dat1.xtab$S77_QFC.14d=with(q.dat1.xtab,ave(S77_QFC,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat1.xtab$S79_QFC.14d=with(q.dat1.xtab,ave(S79_QFC,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat1.xtab$CRE.low=with(q.dat1.xtab,ifelse(S79.14d<750,1,0)) # RECOVER Low
q.dat1.xtab$CRE.low1=with(q.dat1.xtab,ifelse(S79.14d<457,1,0))
q.dat1.xtab$CRE.low2=with(q.dat1.xtab,ifelse(S79.14d>=457&S79.14d<750,1,0))
q.dat1.xtab$CRE.opt=with(q.dat1.xtab,ifelse(S79.14d>=750&S79.14d<2100,1,0)) # RECOVER Optimum
q.dat1.xtab$CRE.high=with(q.dat1.xtab,ifelse(S79.14d>=2100&S79.14d<2600,1,0)) # RECOVER Stress
q.dat1.xtab$CRE.high1=with(q.dat1.xtab,ifelse(S79.14d>=2600&S79.14d<4500,1,0))
q.dat1.xtab$CRE.high2=with(q.dat1.xtab,ifelse(S79.14d>=4500&S79.14d<6500,1,0))
q.dat1.xtab$CRE.high3=with(q.dat1.xtab,ifelse(S79.14d>=6500,1,0))
q.dat1.xtab$CRE.dam=with(q.dat1.xtab,ifelse(S79.14d>=2600,1,0)) # RECOVER Damaging


q.dat1.xtab$SLE.S80trib=rowSums(q.dat1.xtab[,c("S80","TMC2EST","S48","S49","NSF2EST")],na.rm=T)
q.dat1.xtab$SLE.S80trib.14d=with(q.dat1.xtab,ave(SLE.S80trib,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat1.xtab$SLE.low=with(q.dat1.xtab,ifelse(SLE.S80trib.14d<150,1,0)) # RECOVER Low
q.dat1.xtab$SLE.opt=with(q.dat1.xtab,ifelse(SLE.S80trib.14d>=150&SLE.S80trib.14d<1400,1,0)) # RECOVER Optimum
q.dat1.xtab$SLE.high=with(q.dat1.xtab,ifelse(SLE.S80trib.14d>=1400&SLE.S80trib.14d<1700,1,0)) # RECOVER stress
q.dat1.xtab$SLE.dam=with(q.dat1.xtab,ifelse(SLE.S80trib.14d>=1700,1,0)) # RECOVER damaging
q.dat1.xtab$SLE.high1=with(q.dat1.xtab,ifelse(SLE.S80trib.14d>=1700&SLE.S80trib.14d<4000,1,0))
q.dat1.xtab$SLE.high2=with(q.dat1.xtab,ifelse(SLE.S80trib.14d>=4000,1,0))


head(q.dat1.xtab)

q.dat1.xtab$consec.CRE.low=0

q.dat1.xtab$CRE.low.count=0
q.dat1.xtab$CRE.low1.count=0
q.dat1.xtab$CRE.low2.count=0
q.dat1.xtab$CRE.opt.count=0
q.dat1.xtab$CRE.high.count=0
q.dat1.xtab$CRE.high1.count=0
q.dat1.xtab$CRE.high2.count=0
q.dat1.xtab$CRE.high3.count=0
q.dat1.xtab$CRE.high.LOK.count=0
q.dat1.xtab$CRE.high.basin.count=0
q.dat1.xtab$CRE.dam.count=0
q.dat1.xtab$CRE.dam.LOK.count=0
q.dat1.xtab$CRE.dam.basin.count=0

q.dat1.xtab$SLE.low.count=0
q.dat1.xtab$SLE.opt.count=0
q.dat1.xtab$SLE.high.count=0
q.dat1.xtab$SLE.high1.count=0
q.dat1.xtab$SLE.high2.count=0
q.dat1.xtab$SLE.high.LOK.count=0
q.dat1.xtab$SLE.high.basin.count=0
q.dat1.xtab$SLE.dam.count=0
q.dat1.xtab$SLE.dam.LOK.count=0
q.dat1.xtab$SLE.dam.basin.count=0


q.dat1.xtab2=data.frame()

for(j in 1:length(alts.sort)){
tmp=subset(q.dat1.xtab,Alt==alts.sort[j])
for(i in 14:nrow(tmp)){
  ## CRE
  tmp$CRE.low.count[i]=with(tmp,ifelse(CRE.low[i]==1&sum(CRE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
  tmp$CRE.low1.count[i]=with(tmp,ifelse(CRE.low1[i]==1&sum(CRE.low1.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
  tmp$CRE.low2.count[i]=with(tmp,ifelse(CRE.low2[i]==1&sum(CRE.low2.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
  tmp$CRE.opt.count[i]=with(tmp,ifelse(CRE.opt[i]==1&sum(CRE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$CRE.high.count[i]=with(tmp,ifelse(CRE.high[i]==1&sum(CRE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$CRE.high1.count[i]=with(tmp,ifelse(CRE.high1[i]==1&sum(CRE.high1.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$CRE.high2.count[i]=with(tmp,ifelse(CRE.high2[i]==1&sum(CRE.high2.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$CRE.high3.count[i]=with(tmp,ifelse(CRE.high3[i]==1&sum(CRE.high3.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$CRE.high.LOK.count[i]=with(tmp,
                                 ifelse(CRE.high.count[i]==1,
                                          ifelse((S79.14d[i]-S77_QFC.14d[i])<=2100,1,0),0))
  tmp$CRE.high.basin.count[i]=with(tmp,CRE.high.count[i]-CRE.high.LOK.count[i])
  tmp$CRE.dam.count[i]=with(tmp,ifelse(CRE.dam[i]==1&sum(CRE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$CRE.dam.LOK.count[i]=with(tmp,
                                 ifelse(CRE.dam.count[i]==1,
                                        ifelse((S79.14d[i]-S77_QFC.14d[i])<=2600,1,0),0))
  tmp$CRE.dam.basin.count[i]=with(tmp,CRE.dam.count[i]-CRE.dam.LOK.count[i])
  ## SLE
  tmp$SLE.low.count[i]=with(tmp,ifelse(SLE.low[i]==1&sum(SLE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
  tmp$SLE.opt.count[i]=with(tmp,ifelse(SLE.opt[i]==1&sum(SLE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
  tmp$SLE.high.count[i]=with(tmp,ifelse(SLE.high[i]==1&sum(SLE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$SLE.high.LOK.count[i]=with(tmp,
                                 ifelse(SLE.high.count[i]==1,
                                        ifelse((SLE.S80trib.14d[i]-S80_QFC.14d[i])<=1400,1,0),0))
  tmp$SLE.high.basin.count[i]=with(tmp,SLE.high.count[i]-SLE.high.LOK.count[i])
  tmp$SLE.dam.count[i]=with(tmp,ifelse(SLE.dam[i]==1&sum(SLE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
  tmp$SLE.dam.LOK.count[i]=with(tmp,
                                ifelse(SLE.dam.count[i]==1,
                                       ifelse((SLE.S80trib.14d[i]-S80_QFC.14d[i])<=1700,1,0),0))
  tmp$SLE.dam.basin.count[i]=with(tmp,SLE.dam.count[i]-SLE.dam.LOK.count[i])
  tmp$SLE.high1.count[i]=with(tmp,ifelse(SLE.high1[i]==1&sum(SLE.high1.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$SLE.high2.count[i]=with(tmp,ifelse(SLE.high2[i]==1&sum(SLE.high2.count[(i-13):(i-1)],na.rm=T)==0,1,0))
}
q.dat1.xtab2=rbind(q.dat1.xtab2,tmp)
print(j)
}

vars=c("CRE.low.count","CRE.opt.count", 
       "CRE.high.basin.count", "CRE.high.LOK.count", 
       "CRE.dam.basin.count","CRE.dam.LOK.count",
       "CRE.low1.count","CRE.low2.count","CRE.high.count",
       "CRE.high1.count","CRE.high2.count","CRE.high3.count")
# apply(tmp[,vars],2,FUN=function(x)sum(x,na.rm=T))
tmp=reshape2::melt(q.dat1.xtab2[,c("Alt",vars)],id.vars = "Alt")
reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)

vars=c("SLE.low.count", "SLE.opt.count",
       "SLE.high.LOK.count", "SLE.high.basin.count", 
       "SLE.dam.LOK.count", "SLE.dam.basin.count", 
       "SLE.dam.count","SLE.high.count",
       "SLE.high1.count", "SLE.high2.count")
tmp=reshape2::melt(q.dat1.xtab2[,c("Alt",vars)],id.vars = "Alt")
reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)

# tmp$CRE.low.count=with(tmp,ave(CRE.low,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)sum(x,na.rm=T)))))
# tmp$CRE.low.count=with(tmp,ifelse(CRE.low==1&CRE.low.count==0,1,0))
# sum(tmp$CRE.low.count,na.rm=T)
# 
# for(i in 2:nrow(tmp)){
#   # tmp$consec[i]=with(tmp,ifelse(Q_GT2100[i-1]>0&Q_GT2100[i]>0,1,0))
#   tmp$consec.CRE.low[i]=with(tmp,ifelse(CRE.low[i-1]==0&CRE.low[i]>0,1,
#                                          ifelse(CRE.low[i-1]>0&CRE.low[i]>0,1,0)))
# }
# consec.low=consec.startend(tmp$consec.CRE.low>0)
# tmp$sum.CRE.low=0
# for(i in 1:length(consec.low$ends)){
#   tmp[consec.low$ends[i],]$sum.CRE.low=with(tmp[c(consec.low$starts[i]:consec.low$ends[i]),],sum(consec.CRE.low,na.rm=T))
# }
# sum(tmp$sum.CRE.low>=14)
##
##

consec_2600=consec.startend(tmp$consec.2600.mon>0)
  
q.dat1.xtab.mon=ddply(q.dat1.xtab,c("Alt","CY","month"),summarise,
                      Q.S79=mean(S79),
                      Q_GT2100=sum(S79>2100,na.rm=T),
                      Q_GT2600=sum(S79>2600,na.rm=T))
q.dat1.xtab.mon$Q_GT2100.mon=with(q.dat1.xtab.mon,ifelse(Q.S79>2100,1,0))
q.dat1.xtab.mon$Q_GT2600.mon=with(q.dat1.xtab.mon,ifelse(Q.S79>2600,1,0))

q.dat1.xtab.mon2=data.frame()

for(j in 1:8){
tmp=subset(q.dat1.xtab.mon,Alt==alts.sort[j])
tmp$consec.2100.mon=0
tmp$consec.2600.mon=0
for(i in 2:nrow(tmp)){
  # tmp$consec[i]=with(tmp,ifelse(Q_GT2100[i-1]>0&Q_GT2100[i]>0,1,0))
  tmp$consec.2100.mon[i]=with(tmp,ifelse(Q_GT2100.mon[i-1]==0&Q_GT2100.mon[i]>0,1,
                                           ifelse(Q_GT2100.mon[i-1]>0&Q_GT2100.mon[i]>0,1,0)))
  tmp$consec.2600.mon[i]=with(tmp,ifelse(Q_GT2600.mon[i-1]==0&Q_GT2600.mon[i]>0,1,
                                          ifelse(Q_GT2600.mon[i-1]>0&Q_GT2600.mon[i]>0,1,0)))
  
}

consec_2100=consec.startend(tmp$consec.2100.mon>0)
tmp$sum.2100=0
for(i in 1:length(consec_2100$ends)){
  tmp[consec_2100$ends[i],]$sum.2100=with(tmp[c(consec_2100$starts[i]:consec_2100$ends[i]),],sum(consec.2100.mon))
}
consec_2600=consec.startend(tmp$consec.2600.mon>0)
tmp$sum.2600=0
for(i in 1:length(consec_2600$ends)){
  tmp[consec_2600$ends[i],]$sum.2600=with(tmp[c(consec_2600$starts[i]:consec_2600$ends[i]),],sum(consec.2600.mon,na.rm=T))
}

q.dat1.xtab.mon2=rbind(tmp,q.dat1.xtab.mon2)
}

rslt.2100=reshape2::dcast(q.dat1.xtab.mon2,sum.2100~Alt,value.var = "sum.2100",fun.aggregate = function(x)N.obs(x))
rslt.2100=merge(rslt.2100,data.frame(sum.2100=1:28),all.y=T)
rslt.2100[is.na(rslt.2100)]<-0; # fill in NA values
# png(filename=paste0(plot.path,"Iteration_2/S79Q_Consec2100.png"),width=5.5,height=6.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,40);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
layout(matrix(c(1:8),8,1,byrow=F))
par(family="serif",mar=c(1,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);

for(i in 2:9){
x=barplot(t(rslt.2100[,i]),beside=T,ylim=ylim.val,
          col=cols[i-1],ann=F,axes=F,
          names.arg = rep(NA,nrow(rslt.2100)),space=c(0,0),yaxs="i",xaxs="i")
# text(x[1],rslt.2100[1,i],rslt.2100[1,i],pos=1,cex=0.75)
# text(x[2:28],rslt.2100[2:28,i],rslt.2100[2:28,i],pos=3,cex=0.75)
text(x,rslt.2100[,i],rslt.2100[,i],pos=3,cex=0.75)
if(i==9){axis_fun(1,x,x,rslt.2100$sum.2100,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=1,line=-1,paste0("Alt ", alts.sort[i-1]," "),cex=0.75)
if(i==2){mtext(side=3,adj=1,"CY 1965 - 2016")}
}
mtext(side=2,line=0.5,outer=T,"Freq. Monthly Mean S79 Discharge > 2100 cfs")
mtext(side=1,line=2,"Consecutive Months")
dev.off()

rslt.2600=reshape2::dcast(q.dat1.xtab.mon2,sum.2600~Alt,value.var = "sum.2600",fun.aggregate = function(x)N.obs(x))
rslt.2600=merge(rslt.2600,data.frame(sum.2600=1:28),all.y=T)
rslt.2600[is.na(rslt.2600)]<-0; # fill in NA values
# png(filename=paste0(plot.path,"Iteration_2/S79Q_Consec2600.png"),width=5.5,height=6.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,45);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
layout(matrix(c(1:8),8,1,byrow=F))
par(family="serif",mar=c(1,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);

for(i in 2:9){
  x=barplot(t(rslt.2600[,i]),beside=T,ylim=ylim.val,
            col=cols[i-1],ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.2600)),space=c(0,0),yaxs="i",xaxs="i")
  # text(x[1],rslt.2600[1,i],rslt.2600[1,i],pos=1,cex=0.75)
  # text(x[2:28],rslt.2600[2:28,i],rslt.2600[2:28,i],pos=3,cex=0.75)
  text(x,rslt.2600[,i],rslt.2600[,i],pos=3,cex=0.75)
  if(i==9){axis_fun(1,x,x,rslt.2600$sum.2600,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
  axis_fun(2,ymaj,ymin,ymaj)
  box(lwd=1)
  mtext(side=3,adj=1,line=-1,paste0("Alt ", alts.sort[i-1]," "),cex=0.75)
  if(i==2){mtext(side=3,adj=1,"CY 1965 - 2016")}
}
mtext(side=2,line=0.5,outer=T,"Freq. Monthly Mean S79 Discharge > 2600 cfs")
mtext(side=1,line=2,"Consecutive Months")
dev.off()

rslt.2100.melt=reshape2::melt(rslt.2100,id.vars="sum.2100")
rslt.2100.melt$cumsum.freq=with(rslt.2100.melt,ave(value,variable,FUN=function(x)cumsum(x)))
rslt.2100.melt

lty.val=c(1,2,1,1,1,1,1,1)
layout(matrix(c(1:6),1,6,byrow=F))
par(family="serif",mar=c(1,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);

xlim.val=c(0,30);by.x=12;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,75);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 3:8){
  plot(value~sum.2100,rslt.2100.melt,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n",xaxs="i",yaxs="i")
  abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
  with(subset(rslt.2100.melt,variable==alts.sort[1]),lines(sum.2100,cumsum.freq,lty=lty.val[1],col=cols[1],lwd=2))
  with(subset(rslt.2100.melt,variable==alts.sort[2]),lines(sum.2100,cumsum.freq,lty=lty.val[2],col=cols[2],lwd=2))
  with(subset(rslt.2100.melt,variable==alts.sort[i]),lines(sum.2100,cumsum.freq,lty=lty.val[i],col=cols[i],lwd=2))
  axis_fun(1,xmaj,xmin,xmaj)
  if(i==3){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  if(i==3){mtext(side=2,line=2,"Cum Freq. Monthly Mean S79 Q > 2100 cfs")}
  legend("bottomleft",legend=c("NA25","ECBr",alts.sort[i]),
         lty=c(1,2,1),lwd=c(2),col=c(cols[1:2],cols[i]),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  
}
mtext(side=1,line=1,outer=T,"Consecutive Months")

# AMO analysis (S79) ------------------------------------------------------
head(q.dat1.xtab)
q.dat1.xtab$S79.AF=cfs.to.acftd(q.dat1.xtab$S79)
amo.mean=dcast(q.dat1.xtab,Alt~AMO_period,value.var = "S79.AF",mean)

# png(filename=paste0(plot.path,"Iteration_2/S79Q_AMO_TS.png"),width=6,height=7,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,50000);by.y=25000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1965-01-01","2016-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

par(family="serif",mar=c(2,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);
layout(matrix(c(1:8),8,1,byrow=F))

for(i in 1:8){
plot(S79.AF~Date,q.dat1.xtab,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n",xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(q.dat1.xtab,Alt==alts.sort[i]),shaded.range(Date,rep(1,length(Date)),S79.AF,bg="black",col="grey",lty=1))
# with(subset(q.dat1.xtab,Alt==alts.sort[i]),lines(Date,S79,col=cols[i]))
lines(date.fun(c("1965-01-01","1994-12-31")),rep(subset(amo.mean,Alt==alts.sort[i])$dry_cold,2),col="indianred1",lwd=2)
lines(date.fun(c("1995-01-01","2016-12-31")),rep(subset(amo.mean,Alt==alts.sort[i])$wet_warm,2),col="dodgerblue1",lwd=2)
axis_fun(2,ymaj,ymin,ymaj/1000)
if(i==8){axis_fun(1,xmaj,xmin,format(xmaj,"%Y"));}else{axis_fun(1,xmaj,xmin,NA);}
box(lwd=1)
if(i==1){mtext(side=3,adj=0,"S-79")}
mtext(side=3,adj=1,line=-1.25,alts.sort[i],cex=0.75)
}
mtext(side=2,outer=T,line=1,"Discharge (x1000 Ac-Ft d\u207B\u00B9)")
mtext(side=1,line=2,"Date (Year)")
dev.off()

ylim.val=c(0,15000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1965-01-01","2016-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

# png(filename=paste0(plot.path,"Iteration_2/S79Q_AMO_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);
layout(matrix(c(1:8),2,4,byrow=T))

for(i in 1:8){
boxplot(S79.AF~AMO_period,subset(q.dat1.xtab,Alt==alts.sort[i]),col=adjustcolor(c("indianred1","dodgerblue1"),0.5),outline=F,ann=F,axes=F,ylim=ylim.val)
axis_fun(1,1:2,1:2,c("Dry", "Wet"))
axis_fun(2,ymaj,ymin,ymaj/1000)
box(lwd=1)
mtext(side=3,adj=0,line=-1,paste(" ",alts.sort[i]),cex=0.75,font=2)
if(i==1){mtext(side=3,adj=0,"S-79")}
}
mtext(side=1,outer=T,"AMO Phase")
mtext(side=2,outer=T,line=0.5,"Daily Discharge (x1000 Ac-Ft d\u207B\u00B9)")
dev.off()

CRE.QCat.POS.sum=ddply(q.dat1.xtab,c("Alt","AMO_period"),summarise,
                       N.LT457=sum(QLT457,na.rm=T),
                       N.Q457_750=sum(Q457_750,na.rm=T),
                       N.Q_Opt=sum(Q_Opt,na.rm=T),
                       N.Q_Stress=sum(Q_Stress,na.rm=T),
                       N.Q2600_4500=sum(Q2600_4500,na.rm=T),
                       N.Q4500_6500=sum(Q4500_6500,na.rm = T),
                       N.QGT6500=sum(QGT6500,na.rm=T),
                       N.total=N.obs(Alt))

# png(filename=paste0(plot.path,"Iteration_2/S79Q_AMO_cat_total.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);
layout(matrix(c(1:4,4,4),3,2,byrow=F),widths=c(1,0.25))

ylim.val=c(0,600);by.y=300;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=cast(CRE.QCat.POS.sum,Alt~AMO_period,value="N.Q_Stress",sum)
x=barplot(t(tmp),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp)[1,],t(tmp)[1,],cex=0.7,pos=1)
text(x[2,],t(tmp)[2,],t(tmp)[2,],cex=0.7,col="white",pos=1)
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,NA)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,"2100 - 2600 cfs")

ylim.val=c(0,2000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=cast(CRE.QCat.POS.sum,Alt~AMO_period,value="N.Q2600_4500",sum)
x=barplot(t(tmp),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp)[1,],t(tmp)[1,],cex=0.7,pos=1)
text(x[2,],t(tmp)[2,],t(tmp)[2,],cex=0.7,col="white",pos=1)
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,NA)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,"2600 - 4500 cfs")

ylim.val=c(0,1000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
tmp=cast(CRE.QCat.POS.sum,Alt~AMO_period,value="N.QGT6500",sum)
x=barplot(t(tmp),beside=T,ylim=ylim.val,col=c("khaki","dodgerblue1"),axes=F,ann=F,names.arg = rep(NA,N.obs(alts.sort)))
text(x[1,],t(tmp)[1,],t(tmp)[1,],cex=0.7,pos=1)
text(x[2,],t(tmp)[2,],t(tmp)[2,],cex=0.7,col="white",pos=1)
axis_fun(1,x[1,]+diff(x)/2,x[1,]+diff(x)/2,alts.sort)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=3,adj=0,">6500 cfs")
mtext(side=2,outer=T,line=1,"Count of Events (Days)")
mtext(side=1,line=2.5,"Alternative")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0.5,legend=c("Dry Phase\n(1965 - 1994)","Wet Phase\n(1995 - 2016)"),
       pch=22,pt.cex=2,lty=0,pt.bg=c("khaki","dodgerblue1"),
       bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       ncol=1,title="AMO Period",title.adj = 0)
text(1,0,"S79 Daily Discharge\n \nPeriod of Simulation\n CY1965 - 2016.",adj=1,cex=0.5,xpd=NA)

dev.off()




# CRE MFL -----------------------------------------------------------------
CRE.mfl.rslt=data.frame()
q.dat1.xtab.mfl=data.frame()
for(j in 1:n.alts){

  tmp=subset(q.dat1.xtab,Alt==alts.sort[j])
## Adapted from mflst_cre_v2.py
for(i in 2:nrow(tmp)){
  if(tmp$exceed[i-1]==1&tmp$exceed[i]==0){
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
  if(tmp$exceed[i-1]==0&tmp$exceed[i]==1){
    counts=1
  }
  
  if(tmp$exceed_end[i]==1){
    if(tmp$countdown[i-1]<1){
      tmp$countdown[i]=365
    }else{
      tmp$countdown[i]=tmp$countdown[i-1]-1
      if(tmp$countdown[i]==0 & tmp$exceed[i]==1){
        tmp$countdown[i]=365
      }
    }
  }else{
    tmp$countdown[i]=tmp$countdown[i-1]-1
    counts=counts+1
    
    if(counts>366 & tmp$exceed[i]==1){
      tmp$countdown[i]=365
      counts=0
    }
    if(tmp$countdown[i]==0 & tmp$exceed[i]==1){
      tmp$countdown[i]=365
    }
  }
  
  #identify yearly violations
  if(tmp$countdown[i]<0){
    if(tmp$exceed[i]==1){
      if(tmp$exceed[i-1]!=1){
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

q.dat1.xtab.mfl$plot_exc=with(q.dat1.xtab.mfl,ifelse(countdown<0&exceed==1,S79.30d,NA))
q.dat1.xtab.mfl$plot_exc365=with(q.dat1.xtab.mfl,ifelse(countdown>0&exceed==1,S79.30d,NA))

# for(i in 1:n.alts){
# # png(filename=paste0(plot.path,"Iteration_2/CRE_MFL_Alt_",alts.sort[i],".png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
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
#                     " exceednace in 52 years of simualtion"))
# plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
# legend(0.5,-0.5,legend=c("30-day Moving Average","MFL Criteria (457 cfs)","Exceedance","Exceedance w/in 365 Days"),
#        pch=NA,
#        lty=c(1,2,1,1),lwd=2,
#        col=c("blue","brown","orange","grey"),
#        pt.bg=NA,
#        pt.cex=1.5,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
# dev.off()
# }
###
CRE.mfl.rslt
CRE.mfl.rslt$NA25_perchange=with(CRE.mfl.rslt,round(((N.exceed-N.exceed[1])/N.exceed[1])*100,2))
CRE.mfl.rslt$ECBr_perchange=with(CRE.mfl.rslt,round(((N.exceed-N.exceed[2])/N.exceed[2])*100,2))
CRE.mfl.rslt$plot.y=1:8
# png(filename=paste0(plot.path,"Iteration_2/CRE_MFL_sum.png"),width=6,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(1,5,1,0.25));

xlim.val=c(-30,305);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(plot.y~NA25_perchange,CRE.mfl.rslt,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(CRE.mfl.rslt,segments(NA25_perchange,plot.y,rep(0,8),plot.y))
with(CRE.mfl.rslt,points(NA25_perchange,plot.y,pch=21,bg="dodgerblue1",lwd=0.1))
with(subset(CRE.mfl.rslt,NA25_perchange<0),text(NA25_perchange,plot.y,format(round(NA25_perchange,1),nsmall=1),cex=0.5,pos=2))
with(subset(CRE.mfl.rslt,NA25_perchange>0),text(NA25_perchange,plot.y,format(round(NA25_perchange,1),nsmall=1),cex=0.5,pos=4))
axis_fun(2,1:8,1:8,CRE.mfl.rslt$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Caloosahatchee MFL")
mtext(side=3,adj=1,"CY 1965 - 2016")
mtext(side=1,line=1.5,"Average Percent Difference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()

CRE.mfl.rslt%>%
  flextable()%>%
  set_header_labels("Alt"="Alternative",
                    "N.exceed"="Exceedances",
                    "N.Yrs"="Years Simulated")%>%
  padding(padding=1,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%
  fontsize(size=9,part="body")%>%
  fontsize(size=10,part="header")%>%
  align(j=2:3,align="center",part="all")#%>%print(preview="docx")

head(q.dat1)
q.dat1$month=as.numeric(format(q.dat1$Date,'%m'))

q.month.flowtables=ddply(subset(q.dat1,SITE%in%c("S79","S80")),c("Alt","SITE","CY","month"),summarise,TFlow.cfs=sum(FLOW,na.rm=T))
# write.csv(q.month.flowtables,paste0(export.path,"Iteration2/Estuary_MonQ.csv"),row.names = F)
# PCA ---------------------------------------------------------------------
vars=c("Alt", "Date", "WY", "S308", "S77","S79", "S80", "FlowSouth", "CRE.low", 
  "CRE.dam", "CRE.opt", "SLE.low", "SLE.dam", "SLE.opt")
q.dat.xtab.melt=reshape::melt(q.dat.xtab[,vars],id.vars = vars[1:3])
head(q.dat.xtab.melt)

q.dat.xtab.WY=reshape2::dcast(q.dat.xtab.melt,Alt+WY~variable,value.var = "value",sum,na.rm=T)
lakeO.stage.WY=ddply(lakeO.stage,c("Alt",'WY'),summarise,f.lowstg=sum(low.stg,na.rm=T),f.highstg=sum(High.stg))

WY.Q.stg=merge(q.dat.xtab.WY,lakeO.stage.WY,c("Alt","WY"))

env.pen.sum.WY=ddply(lakeO.stage.scr,c("Alt","WY"),summarise,
                  per_below=(sum(score<0,na.rm=T)/N.obs(score))*100,
                  per0=(sum(score==0,na.rm=T)/N.obs(score))*100,
                  per_above=(sum(score>0,na.rm=T)/N.obs(score))*100)
plot(per0~WY,subset(env.pen.sum.WY,Alt=="NA25"),type="l")
with(subset(env.pen.sum.WY,Alt=="CC"),lines(WY,per0,lty=2,col="red"))
boxplot(per0~Alt,subset(env.pen.sum.WY,WY%in%WYs))

# env.pen.sum.WY=ddply(lakeO.stage.scr,c("Alt","WY"),summarise,TScore=sum(abs(score),na.rm=T))
WY.Q.stg=merge(WY.Q.stg,env.pen.sum.WY[,c("Alt","WY","per0")],c("Alt","WY"),all.x=T)

library(vegan)
library(REdaS)
# KMOS
KMOS(WY.Q.stg[,3:length(names(WY.Q.stg))])

# Bartlett's Test Of Sphericity
bart_spher(WY.Q.stg[,3:length(names(WY.Q.stg))])

## PCA
pca.dat.pca=rda(WY.Q.stg[,3:length(names(WY.Q.stg))],scale = T)

eig <- pca.dat.pca$CA$eig
variance <- eig*100/sum(eig)
cumvar <- cumsum(variance)
eig.pca <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)
eig.pca

# png(filename=paste0(plot.path,"Iteration_2/Q_STG_Scree.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,2,1))
par(family="serif",mar=c(1,2,0.75,1),oma=c(2,1,0.25,0.5));

ylim.val=c(0,8);by.y=2;ymaj=seq(ylim.val[1],100,by.y);ymin=seq(ylim.val[1],100,by.y/2)
x=barplot(eig.pca$eig,ylim=ylim.val,col="grey",yaxt="n")
abline(h=ymaj,lty=3,col="grey")
x=barplot(eig.pca$eig,ylim=ylim.val,col="grey",yaxt="n",add=T)
abline(h=1,lty=2,col="red",lwd=2)
axis_fun(1,line=-0.5,x,x,seq(1,length(x),1),0.7)
axis_fun(2,ymaj,ymin,ymaj,0.75);box(lwd=1)
mtext(side=2,line=1.5,"Eigenvalue")

ylim.val=c(0,120);by.y=25;ymaj=seq(ylim.val[1],100,by.y);ymin=seq(ylim.val[1],100,by.y/2);#set y limit and delineates the major and minor ticks
x=barplot(eig.pca$variance,ylim=ylim.val,col="white",border=0,yaxt="n")# inital plot to get the measurements
abline(h=ymaj,lty=3,col="grey")#makes vertical lines from y axis
x=barplot(eig.pca$variance,ylim=ylim.val,col="grey",yaxt="n",add=T)# the real plot that matters
lines(x,eig.pca$cumvariance,col="indianred1",lwd=2)# adds the cumulative variance for each factor
points(x,eig.pca$cumvariance,pch=21,bg="indianred1",cex=1.25)
abline(h=80,lty=2,col="red",lwd=2)
axis_fun(1,line=-0.5,x,x,seq(1,length(x),1),0.7)
axis_fun(2,ymaj,ymin,ymaj,0.75);box(lwd=1)
mtext(side=1,line=1.5,"Principal Components")
mtext(side=2,line=1.75,"Percentage of Variances")
legend.text=c("Absolute","Cumulative");#helper vaiable for legend
pt.col=c("grey","indianred1")#helper vaiable for legend
legend("topleft",legend=legend.text,pch=c(22,21),pt.bg=pt.col,col=c("black",pt.col[2]),lty=c(0,1),lwd=1.5,pt.cex=1,ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,text.col="white")
legend("topleft",legend=legend.text,pch=c(22,21),pt.bg=pt.col,col="black",lty=0,lwd=0.5,pt.cex=1,ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()

scrs<-scores(pca.dat.pca,display=c("sites","species"),choices=c(1,2,3));

WY.Q.stg$Alt=factor(WY.Q.stg$Alt,levels=alts.sort)

cols2=cols=c("grey50","black",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))
cols.val=cols2[WY.Q.stg$Alt]
pchs=c(21,23,21,21,22,23,24,25)
pch.vals=pchs[WY.Q.stg$Alt]
#ggplot(data.frame(scrs$sites))+geom_point(aes(x=PC1,y=PC2,col=WY.Q.stg$Alt))

labs=rownames(scrs$species)
labs=c(expression(paste("Q"["S308"])),
       expression(paste("Q"["S77"])),
       expression(paste("Q"["S79"])),
       expression(paste("Q"["S80"])),
       expression(paste("Q"["South"])),
       expression(paste("S79"["Low"])),
       expression(paste("S79"["Dam"])),
       expression(paste("S79"["Opt"])),
       expression(paste("S80"["Low"])),
       expression(paste("S80"["Dam"])),
       expression(paste("S80"["Opt"])),
       "Stg <11Ft",
       "Stg >16Ft",
       "% Within")
# png(filename=paste0(plot.path,"Iteration_2/Iter2_PCA_Alts.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,1,2),widths=c(1,0.5))
par(family="serif",mar=c(1,1,0.75,0.5),oma=c(2,3,0.25,0.5));
rscale=0.6
xlim.val=c(-1.5,1.6);by.x=1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-1,1.25);by.y=0.5;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA);
abline(h=0,v=0,lty=3,col="grey");
points(scrs$sites[,c(1,2)],pch=pch.vals,bg=adjustcolor(cols.val,0.25),col=adjustcolor("black",0.25),cex=0.75,lwd=0.5); #plots the points
arrows(0,0,scrs$species[,1]*rscale,scrs$species[,2]*rscale,length = 0.05, angle = 15, code = 2,col="indianred1",lwd=1.5);# makes the arrows
with(scrs,text(species[,1]*rscale,species[,2]*rscale,labels=labs,cex=0.5,font=3))
ordiellipse(pca.dat.pca,WY.Q.stg$Alt,col=adjustcolor(cols,1),lwd=1.5,label=F,lty=c(1,1,3,3,2,4,1,2))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1); #adds x axis ticks
axis_fun(2,ymaj,ymin,format(ymaj),1); #adds y axis ticks
mtext(side=1,line=1.8,paste0("PCA 1 (",round(eig.pca$variance[1],1),"%)"));#adds x axis label with percent variance
mtext(side=2,line=2.75,paste0("PCA 2 (",round(eig.pca$variance[2],1),"%)"));#adds y axis label with percent variance

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.35,0.5,legend=alts.sort,
       pch=NA,
       lty=c(1,1,3,3,2,4,1,2),lwd=2,
       col=adjustcolor(cols,1),
       pt.bg=adjustcolor(cols2,0.5),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Model Alternatives",title.col = "white",text.col = "white")
legend(0.6,0.5,legend=alts.sort,
       pch=pchs,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=adjustcolor(cols2,0.5),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/Iter2_PCA_Alts_nopts.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,1,2),widths=c(1,0.5))
par(family="serif",mar=c(1,1,0.75,0.5),oma=c(2,3,0.25,0.5));
rscale=0.6
xlim.val=c(-1.5,1.6);by.x=1;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2);
ylim.val=c(-1,1.25);by.y=0.5;ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2);
plot(xlim.val,ylim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA);
abline(h=0,v=0,lty=3,col="grey");
# points(scrs$sites[,c(1,2)],pch=pch.vals,bg=adjustcolor(cols.val,0.25),col=adjustcolor("black",0.25),cex=0.75,lwd=0.5); #plots the points
arrows(0,0,scrs$species[,1]*rscale,scrs$species[,2]*rscale,length = 0.05, angle = 15, code = 2,col="indianred1",lwd=1.5);# makes the arrows
with(scrs,text(species[,1]*rscale,species[,2]*rscale,labels=labs,cex=0.5,font=3))
ordiellipse(pca.dat.pca,WY.Q.stg$Alt,col=adjustcolor(cols,1),lwd=1.5,label=F,lty=c(1,1,3,3,2,4,1,2))
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj),1); #adds x axis ticks
axis_fun(2,ymaj,ymin,format(ymaj),1); #adds y axis ticks
mtext(side=1,line=1.8,paste0("PCA 1 (",round(eig.pca$variance[1],1),"%)"));#adds x axis label with percent variance
mtext(side=2,line=2.75,paste0("PCA 2 (",round(eig.pca$variance[2],1),"%)"));#adds y axis label with percent variance

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.35,0.5,legend=alts.sort,
       pch=NA,
       lty=c(1,1,3,3,2,4,1,2),lwd=2,
       col=adjustcolor(cols,1),
       pt.bg=adjustcolor(cols2,0.5),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Model Alternatives",title.col = "white",text.col = "white")
legend(0.6,0.5,legend=alts.sort,
       pch=pchs,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=adjustcolor(cols2,0.5),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Model Alternatives")
dev.off()

# CRE ---------------------------------------------------------------------
S77.sum=ddply(subset(q.dat1,SITE=='S77'),c("CY","Alt"),summarise,TQ=sum(cfs.to.acftd(FLOW),na.rm=T))
S77.sum=ddply(S77.sum,"Alt",summarise,Avg.TQ=mean(TQ/1000))
S77.sum=S77.sum[match(alts.sort,S77.sum$Alt),]

S77.sum$NA25_perchange=with(S77.sum,round(((Avg.TQ-Avg.TQ[1])/Avg.TQ[1])*100,2))
S77.sum$ECBr_perchange=with(S77.sum,round(((Avg.TQ-Avg.TQ[2])/Avg.TQ[2])*100,2))
S77.sum



# Flow duration curves
# png(filename=paste0(plot.path,"Iteration_2/S79_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,20000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S79"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S79"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S79"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  #abline(h=c(750,2100,2600),lty=2,col=adjustcolor("black",0.5))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"CRE (S-79)")}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S77_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,8000);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S77"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S77"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S77"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"C-43 (S-77)")}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

## 
q.dat.cre.low2=ddply(subset(q.dat,SITE=="S79"),c("WY","SITE","Alt"),summarise,freq=sum(CRE.low,na.rm=T),N.val=N.obs(Q.14))
q.dat.cre.low2$Alt=factor(q.dat.cre.low2$Alt,levels=alts.sort)
q.dat.cre.low2.sum=ddply(q.dat.cre.low2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm = T))
q.dat.cre.low2.sum=q.dat.cre.low2.sum[match(alts.sort,q.dat.cre.low2.sum$Alt),]

q.dat.cre.dam2=ddply(subset(q.dat,SITE=="S79"),c("WY","SITE","Alt"),summarise,freq=sum(CRE.dam,na.rm=T),N.val=N.obs(Q.14))
q.dat.cre.dam2$Alt=factor(q.dat.cre.dam2$Alt,levels=alts.sort)
q.dat.cre.dam2.sum=ddply(q.dat.cre.dam2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
q.dat.cre.dam2.sum=q.dat.cre.dam2.sum[match(alts.sort,q.dat.cre.dam2.sum$Alt),]

q.dat.cre.opt2=ddply(subset(q.dat,SITE=="S79"),c("WY","SITE","Alt"),summarise,freq=sum(CRE.opt,na.rm=T),N.val=N.obs(Q.14))
q.dat.cre.opt2$Alt=factor(q.dat.cre.opt2$Alt,levels=alts.sort)
q.dat.cre.opt2.sum=ddply(q.dat.cre.opt2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
q.dat.cre.opt2.sum=q.dat.cre.opt2.sum[match(alts.sort,q.dat.cre.opt2.sum$Alt),]


q.dat.cre.flowcats=ddply(subset(q.dat,SITE=="S79"),c("WY","SITE","Alt"),summarise,
                         opt.freq=sum(CRE.opt,na.rm=T),
                         dam.freq=sum(CRE.dam,na.rm=T),
                         low.freq=sum(CRE.low,na.rm=T),N.val=N.obs(Q.14))
#q.dat.cre.flowcats=ddply(q.dat.cre.flowcats,"Alt",summarise,opt.freq=mean(opt.freq),dam.freq=mean(opt.freq),low.freq=mean(low.freq))

q.dat.cre.flowcats$Alt=factor(q.dat.cre.flowcats$Alt,levels=alts.sort)

cols=c("grey","grey",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

library(ggtern)
CRE.flow.tern=ggtern(q.dat.cre.flowcats,aes(x=dam.freq,y=low.freq,z=opt.freq,label=Alt))+
  geom_point(aes(colour=factor(Alt)),size=2)+
  scale_color_manual(values=cols)+
  geom_confidence_tern(colour="dodgerblue1",alpha=0.5,breaks=c(0.5,0.95),linetype=2)+
  #geom_text(hjust=0, vjust=0,alpha=0.5)+
  labs(title="Frequency of RECOVER Flow catergories (CRE)",
       subtitle=" FLWY 1966 - 2016",
       x="",xarrow="Damaging",
       y="",yarrow="Low",
       z="",zarrow="Optimum",color="Alternatives")+
  theme_bw(base_size=10)+theme_arrowcustomlength(0.1,0.85)+
  theme(panel.spacing=unit(0.5, "lines"),
        text=element_text(family="serif"),
        plot.margin=margin(0.25,1,0.25,1),
        tern.axis.arrow = element_line(color = "red",size=1.75))+
  facet_wrap(~ Alt, ncol = 4)
CRE.flow.tern  

# ggsave(CRE.flow.tern,filename=paste0(plot.path,"Iteration_2/CRE_FlowCat.png"),width=8,height=4,unit="in",device="png")

# png(filename=paste0(plot.path,"Iteration_2/CRE_iter2_freq_bxp.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.25,1),oma=c(4,3,1,0.25));
layout(matrix(1:3,3,1,byrow=T))

ylim.val=c(0,365);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,q.dat.cre.dam2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.cre.dam2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.cre.dam2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.cre.dam2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Damaging Flow)\n(Days Yr\u207B\u00B9)")
mtext(side=3,adj=0,"CRE (S-79)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

boxplot(freq~Alt,q.dat.cre.opt2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.cre.opt2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.cre.opt2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.cre.opt2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Optimum Flow)\n(Days Yr\u207B\u00B9)")

boxplot(freq~Alt,q.dat.cre.low2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.cre.low2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.cre.low2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.cre.low2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Low Flow)\n(Days Yr\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Percent Dry season Q ----------------------------------------------------
cre.q.season=cast(subset(q.dat,SITE=="S79"),WY+Alt~hydro.season,value="FLOW",sum,na.rm=T)
cre.q.season$TFlow=rowSums(cre.q.season[,c("A_Wet","B_Dry")],na.rm=T)
cre.q.season$PerDry=with(cre.q.season,(B_Dry/TFlow)*100)
cre.q.season$Alt=factor(cre.q.season$Alt,levels=alts.sort)

cre.q.season.sum=ddply(cre.q.season,"Alt",summarise,mean.val=mean(PerDry,na.rm=T))
cre.q.season.sum=cre.q.season.sum[match(alts.sort,cre.q.season.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/S79_PercentDryQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerDry~Alt,cre.q.season,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,cre.q.season.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(cre.q.season,Alt==alts.sort[1])$PerDry),lty=2,col="black")
abline(h=subset(cre.q.season.sum,Alt==alts.sort[1])$mean,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Percent Dry Season Discharge\n(May - October)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Basin Contribution ------------------------------------------------------
q.dat$month=as.numeric(format(q.dat$Date,"%m"))
basin.q.season=cast(subset(q.dat,SITE%in%c("S79","S77")),WY+month+Alt~SITE,value="FLOW",sum,na.rm=T)
basin.q.season$Q.C43=with(basin.q.season,ifelse(S79<S77,0,S79-S77))
basin.q.season$Alt=factor(basin.q.season$Alt,levels=alts.sort)

basin.q.season.WY=ddply(basin.q.season,c("WY","Alt"),summarise,TFlow.S79=sum(S79,na.rm=T),TFlow.S77=sum(S77,na.rm=T),TFlow.basin=sum(Q.C43,na.rm=T))
basin.q.season.WY$PerBasin=with(basin.q.season.WY,(TFlow.basin/TFlow.S79)*100)
basin.q.season.WY$PerLake=with(basin.q.season.WY,(TFlow.S77/TFlow.S79)*100)

basin.q.season.WY.sum=ddply(basin.q.season.WY,"Alt",summarise,mean=mean(PerBasin,na.rm=T),mean.lake=mean(PerLake,na.rm=T))
basin.q.season.WY.sum=basin.q.season.WY.sum[match(alts.sort,basin.q.season.WY.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/S79_PercentBasinQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerBasin~Alt,basin.q.season.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,basin.q.season.WY.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(basin.q.season.WY,Alt==alts.sort[1])$PerBasin),lty=2,col="black")
abline(h=subset(basin.q.season.WY.sum,Alt==alts.sort[1])$mean,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Percent S-79 Discharge\nfrom C-43 Basin")

boxplot(PerLake~Alt,basin.q.season.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,basin.q.season.WY.sum$mean.lake,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(basin.q.season.WY,Alt==alts.sort[1])$PerLake),lty=2,col="black")
abline(h=subset(basin.q.season.WY.sum,Alt==alts.sort[1])$mean.lake,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.25,"Percent S-79 Discharge\nfrom Lake")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Bloom period ------------------------------------------------------------
bloom.q.season=cast(subset(q.dat,SITE%in%c("S79","S77")),SITE+WY+Alt~bloom.period,value="FLOW",function(x) sum(cfs.to.acftd(x),na.rm=T))
bloom.q.season$TFlow=rowSums(bloom.q.season[,c("bloom","no.bloom")],na.rm=T)
bloom.q.season$per_bloom=with(bloom.q.season,(bloom/TFlow)*100)
bloom.q.season$Alt=factor(bloom.q.season$Alt,levels=alts.sort)

# bloom.q.season.WY.sum=ddply(bloom.q.season,c("Alt"),summarise,mean_bloom=mean(per_bloom,na.rm=T))
bloom.q.season.WY.sum=cast(bloom.q.season,Alt~SITE,value="per_bloom",function(x)mean(x,na.rm=T))
bloom.q.season.WY.sum=bloom.q.season.WY.sum[match(alts.sort,bloom.q.season.WY.sum$Alt),]

bloom.q.season.WY.sum2=cast(bloom.q.season,Alt~SITE,value="bloom",function(x)mean(x,na.rm=T))
bloom.q.season.WY.sum2=bloom.q.season.WY.sum2[match(alts.sort,bloom.q.season.WY.sum2$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/CRE_PercentBloomQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(per_bloom~Alt,subset(bloom.q.season,SITE=="S77"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum$S77,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S77")$per_bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum,Alt==alts.sort[1])$S77,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"C-43 (S-77)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(per_bloom~Alt,subset(bloom.q.season,SITE=="S79"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum$S79,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S79")$per_bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum,Alt==alts.sort[1])$S79,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=2,line=1.5,"Percent Discharge\nDuring June - Aug",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/CRE_BloomQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,350e3);by.y=100e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(bloom~Alt,subset(bloom.q.season,SITE=="S77"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum2$S77,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S77")$bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum2,Alt==alts.sort[1])$S77,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"C-43 (S-77)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,125e4);by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(bloom~Alt,subset(bloom.q.season,SITE=="S79"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum2$S79,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S79")$bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum2,Alt==alts.sort[1])$S79,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=2,line=1.5,"Total Discharge Volume During June - Aug\n(x10\u00B3 AcFt d\u207B\u00B9)",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()

# total Annual Q ---------------------------------------------------------
S79.q.WY=ddply(subset(q.dat,SITE%in%c("S79")),c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(FLOW)))
S79.q.WY$Alt=factor(S79.q.WY$Alt,levels=alts.sort)
S79.q.WY.sum=ddply(S79.q.WY,"Alt",summarise,mean.val=mean(TFlow.acft),med.val=median(TFlow.acft))
S79.q.WY.sum$Alt=factor(S79.q.WY.sum$Alt,levels=alts.sort)

boxplot(TFlow.acft~Alt,S79.q.WY)

# png(filename=paste0(plot.path,"Iteration_2/S79_WYQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,4e6);by.y=1e6;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,S79.q.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S79.q.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S79.q.WY.sum,Alt==alts.sort[1])$med.val),lty=2,col="black")
abline(h=subset(S79.q.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"CRE (S-79)")
mtext(side=2,line=2.5,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()


CRE.Q.cat=ddply(subset(q.dat,SITE=="S79"),c("Alt","WY"),summarise,N.low=sum(CRE.low,na.rm=T),N.opt=sum(CRE.opt,na.rm=T),N.dam=sum(CRE.dam,na.rm=T))
CRE.Q.cat.mean=ddply(CRE.Q.cat,"Alt",summarise,low=mean(N.low,na.rm=T),opt=mean(N.opt,na.rm=T),dam=mean(N.dam,na.rm=T))
CRE.Q.cat.mean=CRE.Q.cat.mean[match(alts.sort,CRE.Q.cat.mean$Alt),]
CRE.Q.cat.mean$low.perdiff=with(CRE.Q.cat.mean,((low-low[1])/low[1])*100)
CRE.Q.cat.mean$opt.perdiff=with(CRE.Q.cat.mean,((opt-opt[1])/opt[1])*100)
CRE.Q.cat.mean$dam.perdiff=with(CRE.Q.cat.mean,((dam-dam[1])/dam[1])*100)

CRE.Q.cat.mean.melt=reshape::melt(CRE.Q.cat.mean[c("Alt",paste0(c("low","opt","dam"),".perdiff"))],id.vars = "Alt")
layout(matrix(1:8,2,4,byrow=T))
for(i in 3:8){
  barplot(subset(CRE.Q.cat.mean.melt,Alt==alts.sort[i])$value)
}




# SLE ---------------------------------------------------------------------
S308.sum=ddply(subset(q.dat1,SITE=='S308'),c("CY","Alt"),summarise,TQ=sum(cfs.to.acftd(FLOW),na.rm=T))
S308.sum=ddply(S308.sum,"Alt",summarise,Avg.TQ=mean(TQ/1000))
S308.sum=S308.sum[match(alts.sort,S308.sum$Alt),]

S308.sum$NA25_perchange=with(S308.sum,round(((Avg.TQ-Avg.TQ[1])/Avg.TQ[1])*100,2))
S308.sum$ECBr_perchange=with(S308.sum,round(((Avg.TQ-Avg.TQ[2])/Avg.TQ[2])*100,2))
S308.sum

# Flow duration curves
# png(filename=paste0(plot.path,"Iteration_2/S80_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25),xpd=F);
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,7200);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S80"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S80"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S80"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"SLE (S-80)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S308_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,7200);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S308"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S308"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S308"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"C-44 (S-308)")}
  box(lwd=1)
  abline(h=250,lty=2)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

## 
q.dat.sle.low2=ddply(subset(q.dat,SITE=="S80"),c("WY","SITE","Alt"),summarise,freq=sum(SLE.low,na.rm=T),N.val=N.obs(Q.14))
q.dat.sle.low2$Alt=factor(q.dat.sle.low2$Alt,levels=alts.sort)
q.dat.sle.low2.sum=ddply(q.dat.sle.low2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm = T))
q.dat.sle.low2.sum=q.dat.sle.low2.sum[match(alts.sort,q.dat.sle.low2.sum$Alt),]

q.dat.sle.dam2=ddply(subset(q.dat,SITE=="S80"),c("WY","SITE","Alt"),summarise,freq=sum(SLE.dam,na.rm=T),N.val=N.obs(Q.14))
q.dat.sle.dam2$Alt=factor(q.dat.sle.dam2$Alt,levels=alts.sort)
q.dat.sle.dam2.sum=ddply(q.dat.sle.dam2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
q.dat.sle.dam2.sum=q.dat.sle.dam2.sum[match(alts.sort,q.dat.sle.dam2.sum$Alt),]

q.dat.sle.opt2=ddply(subset(q.dat,SITE=="S80"),c("WY","SITE","Alt"),summarise,freq=sum(SLE.opt,na.rm=T),N.val=N.obs(Q.14))
q.dat.sle.opt2$Alt=factor(q.dat.sle.opt2$Alt,levels=alts.sort)
q.dat.sle.opt2.sum=ddply(q.dat.sle.opt2,"Alt",summarise,mean.freq=mean(freq,na.rm=T),sd.freq=sd(freq,na.rm=T))
q.dat.sle.opt2.sum=q.dat.sle.opt2.sum[match(alts.sort,q.dat.sle.opt2.sum$Alt),]

q.dat.sle.flowcats=ddply(subset(q.dat,SITE=="S80"),c("WY","SITE","Alt"),summarise,
                         opt.freq=sum(SLE.opt,na.rm=T),
                         dam.freq=sum(SLE.dam,na.rm=T),
                         low.freq=sum(SLE.low,na.rm=T),N.val=N.obs(Q.14))
#q.dat.cre.flowcats=ddply(q.dat.cre.flowcats,"Alt",summarise,opt.freq=mean(opt.freq),dam.freq=mean(opt.freq),low.freq=mean(low.freq))

q.dat.sle.flowcats$Alt=factor(q.dat.sle.flowcats$Alt,levels=alts.sort)

cols=c("grey","grey",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

SLE.flow.tern=ggtern(q.dat.sle.flowcats,aes(x=dam.freq,y=low.freq,z=opt.freq,label=Alt))+
  geom_point(aes(colour=factor(Alt)),size=2)+
  scale_color_manual(values=cols)+
  geom_confidence_tern(colour="dodgerblue1",alpha=0.5,breaks=c(0.5,0.95),linetype=2)+
  #geom_text(hjust=0, vjust=0,alpha=0.5)+
  labs(title="Frequency of RECOVER Flow catergories (SLE)",
       subtitle="FL WY 1966 - 2016",
       x="",xarrow="Damaging",
       y="",yarrow="Low",
       z="",zarrow="Optimum",color="Alternatives")+
  theme_bw(base_size=10)+theme_arrowcustomlength(0.1,0.85)+
  theme(panel.spacing=unit(0.5, "lines"),
        text=element_text(family="serif"),
        plot.margin=margin(0.25,1,0.25,1),
        tern.axis.arrow = element_line(color = "red",size=1.75))+
  facet_wrap(~ Alt, ncol = 4)
SLE.flow.tern  

# ggsave(SLE.flow.tern,filename=paste0(plot.path,"Iteration_2/SLE_FlowCat.png"), width=8,height=4,unit="in",device="png")


# png(filename=paste0(plot.path,"Iteration_2/SLE_iter2_freq_bxp.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.25,1),oma=c(4,3,1,0.25));
layout(matrix(1:3,3,1,byrow=T))

ylim.val=c(0,130);by.y=60;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,q.dat.sle.dam2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.sle.dam2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.sle.dam2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.sle.dam2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Damaging Flow)\n(Days Yr\u207B\u00B9)")
mtext(side=3,adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,365);by.y=90;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(freq~Alt,q.dat.sle.opt2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.sle.opt2.sum$mean.freq,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.sle.opt2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.sle.opt2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Optimum Flow)\n(Days Yr\u207B\u00B9)")

boxplot(freq~Alt,q.dat.sle.low2,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,q.dat.sle.low2.sum$mean.freq,pch=21,bg="olivedrab2",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.sle.low2,Alt==alts.sort[1])$freq),lty=2)
abline(h=subset(q.dat.sle.low2.sum,Alt==alts.sort[1])$mean.freq,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.5,"\u0192(Low Flow)\n(Days Yr\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Percent Dry season Q ----------------------------------------------------
sle.q.season=cast(subset(q.dat,SITE=="S80"),WY+Alt~hydro.season,value="FLOW",sum,na.rm=T)
sle.q.season$TFlow=rowSums(sle.q.season[,c("A_Wet","B_Dry")],na.rm=T)
sle.q.season$PerDry=with(sle.q.season,(B_Dry/TFlow)*100)
sle.q.season$PerWet=with(sle.q.season,(A_Wet/TFlow)*100)
sle.q.season$Alt=factor(sle.q.season$Alt,levels=alts.sort)

sle.q.season.sum=ddply(sle.q.season,"Alt",summarise,mean.val=mean(PerDry,na.rm=T),mean.wet.val=mean(PerWet,na.rm=T))
sle.q.season.sum=sle.q.season.sum[match(alts.sort,sle.q.season.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/S80_PercentDryQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerDry~Alt,sle.q.season,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.q.season.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(sle.q.season,Alt==alts.sort[1])$PerDry,na.rm=T),lty=2,col="black")
abline(h=subset(sle.q.season.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Percent Dry Season Discharge\n(November - April)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S80_PercentWetQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerWet~Alt,sle.q.season,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,sle.q.season.sum$mean.wet.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(sle.q.season,Alt==alts.sort[1])$PerWet,na.rm=T),lty=2,col="black")
abline(h=subset(sle.q.season.sum,Alt==alts.sort[1])$mean.wet.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Percent Wet Season Discharge\n(May - October)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()
# Basin Contribution ------------------------------------------------------
basin.q.season=cast(subset(q.dat,SITE%in%c("S80","S308")),WY+month+Alt~SITE,value="FLOW",sum,na.rm=T)
basin.q.season$Q.C44=with(basin.q.season,ifelse(S80<S308,0,S80-S308))
basin.q.season$Alt=factor(basin.q.season$Alt,levels=alts.sort)

basin.q.season.WY=ddply(basin.q.season,c("WY","Alt"),summarise,TFlow.S80=sum(S80,na.rm=T),TFlow.S308=sum(S308,na.rm=T),TFlow.basin=sum(Q.C44,na.rm=T))
basin.q.season.WY$PerBasin=with(basin.q.season.WY,(TFlow.basin/TFlow.S80)*100)
basin.q.season.WY$PerLake=with(basin.q.season.WY,(TFlow.S308/TFlow.S80)*100)
basin.q.season.WY$PerLake[is.infinite(basin.q.season.WY$PerLake)==T]<-NA
range(basin.q.season.WY$PerLake,na.rm=T)

basin.q.season.WY.sum=ddply(basin.q.season.WY,"Alt",summarise,mean=mean(PerBasin,na.rm=T),mean.lake=mean(PerLake,na.rm=T))
basin.q.season.WY.sum=basin.q.season.WY.sum[match(alts.sort,basin.q.season.WY.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/S80_PercentBasinQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))
ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(PerBasin~Alt,basin.q.season.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,basin.q.season.WY.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(basin.q.season.WY,Alt==alts.sort[1])$PerBasin),lty=2,col="black")
abline(h=subset(basin.q.season.WY.sum,Alt==alts.sort[1])$mean,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Percent S-80 Discharge\nfrom C-44 Basin")

boxplot(PerLake~Alt,basin.q.season.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,basin.q.season.WY.sum$mean.lake,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(basin.q.season.WY,Alt==alts.sort[1])$PerLake),lty=2,col="black")
abline(h=subset(basin.q.season.WY.sum,Alt==alts.sort[1])$mean.lake,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=2,line=2.25,"Percent S-80 Discharge\nfrom Lake")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# Bloom period ------------------------------------------------------------
bloom.q.season=cast(subset(q.dat,SITE%in%c("S80","S308")),SITE+WY+Alt~bloom.period,value="FLOW",function(x) sum(cfs.to.acftd(x),na.rm=T))
bloom.q.season$TFlow=rowSums(bloom.q.season[,c("bloom","no.bloom")],na.rm=T)
bloom.q.season$per_bloom=with(bloom.q.season,(bloom/TFlow)*100)
bloom.q.season$Alt=factor(bloom.q.season$Alt,levels=alts.sort)

# bloom.q.season.WY.sum=ddply(bloom.q.season,c("Alt"),summarise,mean_bloom=mean(per_bloom,na.rm=T))
bloom.q.season.WY.sum=cast(bloom.q.season,Alt~SITE,value="per_bloom",function(x)mean(x,na.rm=T))
bloom.q.season.WY.sum=bloom.q.season.WY.sum[match(alts.sort,bloom.q.season.WY.sum$Alt),]

bloom.q.season.WY.sum2=cast(bloom.q.season,Alt~SITE,value="bloom",function(x)mean(x,na.rm=T))
bloom.q.season.WY.sum2=bloom.q.season.WY.sum2[match(alts.sort,bloom.q.season.WY.sum2$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/SLE_PercentBloomQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(per_bloom~Alt,subset(bloom.q.season,SITE=="S308"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum$S308,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S308")$per_bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum,Alt==alts.sort[1])$S308,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"C-44 (S-308)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(per_bloom~Alt,subset(bloom.q.season,SITE=="S80"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum$S80,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S80")$per_bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum,Alt==alts.sort[1])$S80,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=2,line=1.5,"Percent Discharge\nDuring May - Aug",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/SLE_BloomQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,200e3);by.y=50e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(bloom~Alt,subset(bloom.q.season,SITE=="S308"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum2$S308,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S308")$bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum2,Alt==alts.sort[1])$S308,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"C-44 (S-308)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,40e4);by.y=10e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(bloom~Alt,subset(bloom.q.season,SITE=="S80"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,bloom.q.season.WY.sum2$S80,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(bloom.q.season,Alt==alts.sort[1]&SITE=="S80")$bloom),lty=2,col="black")
abline(h=subset(bloom.q.season.WY.sum2,Alt==alts.sort[1])$S80,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=2,line=1.5,"Total Discharge Volume During May - Aug\n(x10\u00B3 AcFt d\u207B\u00B9)",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()

# total Annual Q ---------------------------------------------------------
S80.q.WY=ddply(subset(q.dat,SITE%in%c("S80")),c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(FLOW)))
S80.q.WY$Alt=factor(S80.q.WY$Alt,levels=alts.sort)
S80.q.WY.sum=ddply(S80.q.WY,"Alt",summarise,mean.val=mean(TFlow.acft),med.val=median(TFlow.acft))
S80.q.WY.sum$Alt=factor(S80.q.WY.sum$Alt,levels=alts.sort)
S80.q.WY.sum$FWO.perdiff=with(S80.q.WY.sum,((mean.val-mean.val[1])/mean.val[1])*100)

boxplot(TFlow.acft~Alt,S80.q.WY)

# png(filename=paste0(plot.path,"Iteration_2/S80_WYQ_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,1e6);by.y=0.25e6;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,S80.q.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,S80.q.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(S80.q.WY.sum,Alt==alts.sort[1])$med.val),lty=2,col="black")
abline(h=subset(S80.q.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SLE (S-80)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.5,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()


SLE.Q.cat=ddply(subset(q.dat,SITE=="S80"),c("Alt","WY"),summarise,N.low=sum(SLE.low,na.rm=T),N.opt=sum(SLE.opt,na.rm=T),N.dam=sum(SLE.dam,na.rm=T))
SLE.Q.cat.mean=ddply(SLE.Q.cat,"Alt",summarise,low=mean(N.low,na.rm=T),opt=mean(N.opt,na.rm=T),dam=mean(N.dam,na.rm=T))
SLE.Q.cat.mean=SLE.Q.cat.mean[match(alts.sort,SLE.Q.cat.mean$Alt),]
SLE.Q.cat.mean$low.perdiff=with(SLE.Q.cat.mean,((low-low[1])/low[1])*100)
SLE.Q.cat.mean$opt.perdiff=with(SLE.Q.cat.mean,((opt-opt[1])/opt[1])*100)
SLE.Q.cat.mean$dam.perdiff=with(SLE.Q.cat.mean,((dam-dam[1])/dam[1])*100)

SLE.Q.cat.mean.melt=reshape::melt(SLE.Q.cat.mean[,c("Alt",paste0(c("low","opt","dam"),".perdiff"))],id.vars = "Alt")
layout(matrix(1:8,2,4,byrow=T))
for(i in 3:8){
  barplot(subset(SLE.Q.cat.mean.melt,Alt==alts.sort[i])$value)
}

## 
CRE.Q.cat.mean.melt$Est="CRE"
SLE.Q.cat.mean.melt$Est="SLE"

Q.cat.mean=rbind(CRE.Q.cat.mean.melt,SLE.Q.cat.mean.melt)
Q.cat.mean$variable=factor(Q.cat.mean$variable,levels = c("low.perdiff", "opt.perdiff", "dam.perdiff"))
Q.cat.mean$Alt=factor(Q.cat.mean$Alt,levels=alts.sort)

Q.cat.mean2=reshape2::dcast(Q.cat.mean,Alt+variable~Est,value.var="value",mean)
subset(Q.cat.mean2,Alt=="AA")

ylim.val=c(-75,210);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration_2/Estuary_FWOCompare.png"),width=8,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,3,0.75,1),lwd=0.5);
layout(matrix(1:7,1,7,byrow=T),widths=c(1,1,1,1,1,1,0.5))

for(i in 3:8){
pln=alts.sort[i]
x=barplot(t(subset(Q.cat.mean2,Alt==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(subset(Q.cat.mean2,Alt==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("dodgerblue1","indianred1"),add=T,xaxt="n")
x.val=x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,c("Low","Optimal","Dam"),line=-0.5,cex=0.8)
if(i==3){
  axis_fun(2,ymaj,ymin,ymaj)
  mtext(side=2,line=2,"Average Percent\nDifference to FWO",cex=0.8)}else{axis_fun(2,ymaj,ymin,NA)};
box(lwd=1)
mtext(side=3,adj=0,paste0("Alternative ",pln),cex=0.5)

}
mtext(side=1,line=0.75,outer=T,"Discharge Category")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("CRE","SLE"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c("dodgerblue1","indianred1"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Estuary")
dev.off()

# Flow South --------------------------------------------------------------
head(q.dat1)
# q.dat1$month=as.numeric(format(q.dat1$Date,'%m'))
q.dat1$WY=WY(q.dat1$Date)
q.dat1$hydroseason2=with(q.dat1,ifelse(month%in%c(11:12,1:2),"EarlyDry",
                                       ifelse(month%in%c(3:5),"LateDry","Wet")))

# Sanity Check
# ddply(q.dat1,c("month",'hydroseason2'),summarise,N.val=N.obs(FLOW))

FlowSouth.sum.season=reshape2::dcast(subset(q.dat1,SITE%in%c("S351","S354")&WY%in%WYs),Alt+WY~hydroseason2,value.var="FLOW",fun.aggregate = function(x) sum(cfs.to.acftd(x),na.rm=T))
FlowSouth.sum.season$Alt=factor(FlowSouth.sum.season$Alt,levels=alts.sort)
FlowSouth.sum.season.wet=ddply(FlowSouth.sum.season,"Alt",summarise,mean.val=mean(Wet,na.rm=T))
FlowSouth.sum.season.wet$NA25_perchange=with(FlowSouth.sum.season.wet,round(((mean.val-mean.val[1])/mean.val[1])*100,2))
FlowSouth.sum.season.wet$ECBr_perchange=with(FlowSouth.sum.season.wet,round(((mean.val-mean.val[2])/mean.val[2])*100,2))

FlowSouth.sum.season.LateDry=ddply(FlowSouth.sum.season,"Alt",summarise,mean.val=mean(LateDry,na.rm=T))
FlowSouth.sum.season.LateDry$NA25_perchange=with(FlowSouth.sum.season.LateDry,round(((mean.val-mean.val[1])/mean.val[1])*100,2))
FlowSouth.sum.season.LateDry$ECBr_perchange=with(FlowSouth.sum.season.LateDry,round(((mean.val-mean.val[2])/mean.val[2])*100,2))

boxplot(LateDry~Alt,FlowSouth.sum.season)
boxplot(EarlyDry~Alt,FlowSouth.sum.season)

# png(filename=paste0(plot.path,"Iteration_2/FlowSouth_LateDryseason.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,400e3);by.y=100e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

boxplot(LateDry~Alt,FlowSouth.sum.season,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,FlowSouth.sum.season.LateDry$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(FlowSouth.sum.season,Alt==alts.sort[1])$LateDry),lty=2,col="black")
abline(h=mean(subset(FlowSouth.sum.season,Alt==alts.sort[1])$LateDry),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"Flow South (S351 + S354)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Late Dry Season (Mar - May)\nDischarge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/FlowSouth_LateDry_FWOCompare.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,3,0.75,1),lwd=0.5);
ylim.val=c(0,40);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(FlowSouth.sum.season.LateDry$NA25_perchange,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(FlowSouth.sum.season.LateDry$NA25_perchange,
          ylim=ylim.val,axes=F,ann=F,col=cols,xaxt="n",add=T)
axis_fun(1,x,x,alts.sort)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Average Percent\nDifference to FWO",cex=1)
mtext(side=3,adj=0,"Late Dry Season Flows South")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=1,line=2,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/FlowSouth_wetseason.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,320e3);by.y=100e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

boxplot(Wet~Alt,FlowSouth.sum.season,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,FlowSouth.sum.season.wet$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(FlowSouth.sum.season,Alt==alts.sort[1])$Wet),lty=2,col="black")
abline(h=mean(subset(FlowSouth.sum.season,Alt==alts.sort[1])$Wet),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"Flow South (S351 + S354)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Wet Season (June - October)\nDischarge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/FlowSouth_wet_FWOCompare.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,3,0.75,1),lwd=0.5);
ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(FlowSouth.sum.season.wet$NA25_perchange,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(FlowSouth.sum.season.wet$NA25_perchange,
          ylim=ylim.val,axes=F,ann=F,col=cols,xaxt="n",add=T)
axis_fun(1,x,x,alts.sort)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Average Percent\nDifference to FWO",cex=1)
mtext(side=3,adj=0,"Wet Season Flows South")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=1,line=2,"Model Alternatives")
dev.off()

FlowSouth.sum=ddply(subset(q.dat1,SITE%in%c("S351","S354")),c("CY","Alt"),summarise,TQ=sum(cfs.to.acftd(FLOW),na.rm=T))
FlowSouth.sum=ddply(FlowSouth.sum,"Alt",summarise,Avg.TQ=mean(TQ/1000))
FlowSouth.sum=FlowSouth.sum[match(alts.sort,FlowSouth.sum$Alt),]

FlowSouth.sum$NA25_perchange=with(FlowSouth.sum,round(((Avg.TQ-Avg.TQ[1])/Avg.TQ[1])*100,2))
FlowSouth.sum$ECBr_perchange=with(FlowSouth.sum,round(((Avg.TQ-Avg.TQ[2])/Avg.TQ[2])*100,2))
FlowSouth.sum

# png(filename=paste0(plot.path,"Iteration_2/FlowSouth_FWOCompare.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,3,0.75,1),lwd=0.5);
ylim.val=c(0,100);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(FlowSouth.sum$NA25_perchange,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(FlowSouth.sum$NA25_perchange,
        ylim=ylim.val,axes=F,ann=F,col=cols,xaxt="n",add=T)
axis_fun(1,x,x,alts.sort)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Average Percent\nDifference to FWO",cex=1)
mtext(side=3,adj=0,"Flows South")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=1,line=2,"Model Alternatives")
dev.off()

# Flow duration curves
# png(filename=paste0(plot.path,"Iteration_2/S351_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,2400);by.y=800;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S351"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S351"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S351"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"EAA - NNR & Hillsboro Canal (S-351)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S352_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,1250);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S352"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S352"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S352"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"EAA - WPB Canal (S-352)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/S354_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,2000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(q.dat,SITE=="S354"&Alt==alts.sort[1])$FLOW),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S354"&Alt==alts.sort[2])$FLOW),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(q.dat,SITE=="S354"&Alt==alts.sort[i])$FLOW),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"EAA - Miami Canal (S-354)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()

EAA.q=ddply(subset(q.dat,SITE%in%c("S354","S352","S351")),c("Alt","Date","WY"),summarise,TFlow.cfs=sum(FLOW,na.rm=T))
EAA.q2=ddply(subset(q.dat,SITE%in%c("S354","S351")),c("Alt","Date","WY"),summarise,TFlow.cfs=sum(FLOW,na.rm=T))

# png(filename=paste0(plot.path,"Iteration_2/EAA_FlowDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,6000);by.y=2000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(EAA.q,Alt==alts.sort[1])$TFlow.cfs),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(EAA.q,Alt==alts.sort[2])$TFlow.cfs),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(EAA.q,Alt==alts.sort[i])$TFlow.cfs),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("topright",legend=alts.sort[c(1,2,i)],
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"EAA (S-354 + S-351 + S-352)")}
  box(lwd=1)
}
mtext(side=2,line=2.5,outer=T,"Discharge (cfs)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Discharge Volume")
dev.off()


EAA.q.WY=ddply(EAA.q,c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(TFlow.cfs)))
EAA.q.WY$Alt=factor(EAA.q.WY$Alt,levels=alts.sort)
EAA.q.WY.sum=ddply(EAA.q.WY,"Alt",summarise,mean.val=mean(TFlow.acft))
EAA.q.WY.sum=EAA.q.WY.sum[match(alts.sort,EAA.q.WY.sum$Alt),]
EAA.q.WY.sum$FWO.diff=with(EAA.q.WY.sum,(mean.val-mean.val[1])/mean.val[1])*100
# png(filename=paste0(plot.path,"Iteration_2/EAA_discharge_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,10e5);by.y=2.5e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,EAA.q.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,EAA.q.WY.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(EAA.q.WY,Alt==alts.sort[1])$TFlow.acft),lty=2,col="black")
abline(h=subset(EAA.q.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"EAA (S-354 + S-351 + S-352)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/EAA_discharge_sum.png"),width=3,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));

xlim.val=c(-25,50);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(EAA.q.WY.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(EAA.q.WY.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(EAA.q.WY.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,EAA.q.WY.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"EAA Inflow")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()

EAA.q.WY=ddply(EAA.q2,c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(TFlow.cfs)))
EAA.q.WY$Alt=factor(EAA.q.WY$Alt,levels=alts.sort)
EAA.q.WY.sum=ddply(EAA.q.WY,"Alt",summarise,mean.val=mean(TFlow.acft))
EAA.q.WY.sum=EAA.q.WY.sum[match(alts.sort,EAA.q.WY.sum$Alt),]
EAA.q.WY.sum$FWO.diff=with(EAA.q.WY.sum,(mean.val-mean.val[1])/mean.val[1])*100

# png(filename=paste0(plot.path,"Iteration_2/EAA_discharge_bxp2.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,10e5);by.y=2.5e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,EAA.q.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,EAA.q.WY.sum$mean,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(EAA.q.WY,Alt==alts.sort[1])$TFlow.acft),lty=2,col="black")
abline(h=subset(EAA.q.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"EAA (S-354 + S-351)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")
mtext(side=2,line=2.25,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/EAA_discharge_sum2.png"),width=3,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));

xlim.val=c(-25,75);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(EAA.q.WY.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(EAA.q.WY.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(EAA.q.WY.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,EAA.q.WY.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"EAA Inflow")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()
# Southern Everglades -----------------------------------------------------
RSM.stg.sites=c("WCA3A_3A-3","WCA3A_3A-2","WCA3A_3A-4","WCA3A_3A-28","S333_US")
RSM.stg.sites=data.frame(SITE=RSM.stg.sites,Station=c("CA3_63","CA3_62","CA3_64","CA3_65","S333_HW"))
wca.stg.dat=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMGL_output.dss"))  
  
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

unique(wca.stg.dat$Alt)

wca.stg.dat.xtab=data.frame(cast(wca.stg.dat,Date+Alt~Station,value="STAGE",mean))
wca.stg.dat.xtab$mean_6263=rowMeans(wca.stg.dat.xtab[,c("CA3_62","CA3_63")],na.rm=T)
wca.stg.dat.xtab$mean_636465=rowMeans(wca.stg.dat.xtab[,c("CA3_63","CA3_64","CA3_65")],na.rm=T)
wca.stg.dat.xtab$S333_HW.LT9.2=with(wca.stg.dat.xtab,ifelse(S333_HW<9.2,1,0))

wca.stg.dat.xtab$low.close=with(wca.stg.dat.xtab,ifelse(mean_6263<9.3,1,0))
wca.stg.dat.xtab$high.close=with(wca.stg.dat.xtab,ifelse(mean_6263>11.6,1,0))
wca.stg.dat.xtab$LT9.5=with(wca.stg.dat.xtab,ifelse(mean_636465<9.5,1,0))
wca.stg.dat.xtab$Alt=factor(wca.stg.dat.xtab$Alt,levels=alts.sort)


S333HW.sum=ddply(wca.stg.dat.xtab,c("Alt"),summarise,N.LT9.2=sum(S333_HW.LT9.2))
S333HW.sum=S333HW.sum[match(alts.sort,S333HW.sum$Alt),]
S333HW.sum$FWO.diff=with(S333HW.sum,(N.LT9.2-N.LT9.2[1])/N.LT9.2[1])*100

Low.wca3.sum=ddply(wca.stg.dat.xtab,c("Alt"),summarise,N.LT9.5=sum(LT9.5))
Low.wca3.sum=Low.wca3.sum[match(alts.sort,Low.wca3.sum$Alt),]
Low.wca3.sum$FWO.diff=with(Low.wca3.sum,(N.LT9.5-N.LT9.5[1])/N.LT9.5[1])*100

HiLo.close.sum=ddply(wca.stg.dat.xtab,c("Alt"),summarise,N.low=sum(low.close),N.high=sum(high.close))
HiLo.close.sum=HiLo.close.sum[match(alts.sort,HiLo.close.sum$Alt),]
HiLo.close.sum$FWO.Lo.diff=with(HiLo.close.sum,(N.low-N.low[1])/N.low[1])*100
HiLo.close.sum$ECB.Lo.diff=with(HiLo.close.sum,(N.low-N.low[2])/N.low[2])*100
HiLo.close.sum$FWO.Hi.diff=with(HiLo.close.sum,(N.high-N.high[1])/N.high[1])*100
HiLo.close.sum$ECB.Hi.diff=with(HiLo.close.sum,(N.high-N.high[2])/N.high[2])*100

boxplot(mean_6263~Alt,wca.stg.dat.xtab,outline=F)
# Stage duration curves

# png(filename=paste0(plot.path,"Iteration_2/WCA3_6263_StageDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(8,14);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[1])$mean_6263),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[2])$mean_6263),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[i])$mean_6263),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("bottomleft",legend=c(alts.sort[c(1,2,i)]),
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"WCA-3 (3-62 & 3-63 Gauge Avg.)")}
  box(lwd=1)
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/WCA3_636465_StageDuration.png"),width=7.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:8,2,4,byrow=T))

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(7,13);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

for(i in 3:n.alts){
  plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[1])$mean_636465),lines(1-proportion,value,col=cols[1],lty=1,lwd=1.5))
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[2])$mean_636465),lines(1-proportion,value,col=cols[2],lty=2,lwd=1.5))
  with(ecdf_fun(subset(wca.stg.dat.xtab,Alt==alts.sort[i])$mean_636465),lines(1-proportion,value,col=adjustcolor(cols[i],0.5),lwd=2))
  legend("bottomleft",legend=c(alts.sort[c(1,2,i)]),
         lty=c(1,2,1),lwd=c(1.5,1.5,1.5),col=c(cols[1:2],adjustcolor(as.character(cols[i]),0.5)),
         ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  if(i%in%c(3:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  if(i==6){mtext(side=3, adj=1,"FLWY 1966 - 2016")}
  if(i==3){mtext(side=3, adj=0,"WCA-3 (3-63, 3-64 & 3-65 Gauge Avg.)")}
  box(lwd=1)
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()


# png(filename=paste0(plot.path,"Iteration_2/WCA_6263_perdiff.png"),width=5,height=5.25,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,1,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-20,10);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(HiLo.close.sum$FWO.Lo.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(HiLo.close.sum,segments(FWO.Lo.diff,1:8,rep(0,8),1:8))
with(HiLo.close.sum,points(FWO.Lo.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,HiLo.close.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"WCA-3 (3-62 & 3-63 Gauge)",line=1)
mtext(side=3,adj=0,"Freq. Low Water Closure",cex=0.8,col="grey40")
mtext(side=2,line=4,'Model Alternative')

xlim.val=c(-10,20);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(HiLo.close.sum$FWO.Hi.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(HiLo.close.sum,segments(FWO.Hi.diff,1:8,rep(0,8),1:8))
with(HiLo.close.sum,points(FWO.Hi.diff,1:8,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:8,1:8,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=1)
mtext(side=3,adj=0,"Freq. High Water Closure",cex=0.8,col="grey40")
mtext(side=1,outer=T,"Percent Difference to FWO")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/WCA_636465_perdiff.png"),width=3,height=5.25,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,1,1),oma=c(1.5,5,1,0.25));

xlim.val=c(-20,2);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(Low.wca3.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(Low.wca3.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(Low.wca3.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,Low.wca3.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"WCA-3 (3-Gauge Avg.)",line=1)
mtext(side=3,adj=0,"Freq. < 9.5 Ft",cex=0.8,col="grey40")
mtext(side=1,line=1.5,"Percent Difference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/WCA_S333HW_perdiff.png"),width=3,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,0.5,1),oma=c(1.5,5,1,0.25));

xlim.val=c(-30,0);by.x=10;xmaj=c(0,seq(xlim.val[1],xlim.val[2],by.x));xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(S333HW.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(S333HW.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(S333HW.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,S333HW.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Freq. S333HW < 9.2 Ft",cex=1,col="black")
mtext(side=1,line=1.5,"Percent Difference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()

# SRS Appendix A ----------------------------------------------------------
wca.stg.dat.xtab=wca.stg.dat.xtab[order(wca.stg.dat.xtab$Alt,wca.stg.dat.xtab$Date),]
wca.stg.dat.xtab$Stg.inc=with(wca.stg.dat.xtab,ave(mean_636465,Alt,FUN=function(x)c(rep(NA,29),diff(x,lag=29))))
wca.stg.dat.xtab$Stg.Gradient=with(wca.stg.dat.xtab,CA3_63-CA3_65)
wca.stg.dat.xtab$Stage.Anteced.90freq=with(wca.stg.dat.xtab,ave(LT9.5,Alt,FUN=function(x)c(rep(NA,89),rollsum(x,90)/90)))
wca.stg.dat.xtab$decimal.yr=lubridate::decimal_date(wca.stg.dat.xtab$Date)

## Equations from COP
walker.coef=data.frame(Trend=-0.027286178,Stg_Gradient=0.043006751,Freq_90Day=0.303630589,Stage2=0.098476045,Stage_Inc=0.12746041,Stage=-2.375222139,Intercept=70.80455867)

S12ABC.TP.FWM=with(wca.stg.dat.xtab,data.frame(Date=Date,Alt=Alt,
                                             FWM=exp((mean_636465*walker.coef$Stage)+(Stg.Gradient*walker.coef$Stg_Gradient)+((mean_636465^2)*walker.coef$Stage2)+(Stage.Anteced.90freq*walker.coef$Freq_90Day)+(Stg.inc*walker.coef$Stage_Inc)+(2017.75*walker.coef$Trend)+walker.coef$Intercept),WQ.Site="S12ABC"))

walker.coef=data.frame(Trend=-0.015901212,Stg_Gradient=0.336072873,Freq_90Day=0.410513468,Stage2=0.055665333,Stage_Inc=0.07686639,Stage=-1.473865744,Intercept=43.16477753)

S12DS333.TP.FWM=with(wca.stg.dat.xtab,data.frame(Date=Date,Alt=Alt,
                                               FWM=exp((mean_636465*walker.coef$Stage)+(Stg.Gradient*walker.coef$Stg_Gradient)+((mean_636465^2)*walker.coef$Stage2)+(Stage.Anteced.90freq*walker.coef$Freq_90Day)+(Stg.inc*walker.coef$Stage_Inc)+(2017.75*walker.coef$Trend)+walker.coef$Intercept),WQ.Site="S12DS333"))


SRS.WQ.mod=rbind(S12ABC.TP.FWM,S12DS333.TP.FWM)

##Discharge
RSM.sites=c(paste0("S12",c("A","B","C","D")),"S333","S333N","S334","S355A","S355B","S356")
alt.srs=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMGL_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMGL/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    alt.srs=rbind(tmp,alt.srs)
    print(i)
  }
}

alt.srs$Q=cfs.to.acftd(alt.srs$FLOW)
alt.srs$Alt=factor(alt.srs$Alt,levels=alts.sort)
####
acft.to.L=function(x)x*1.233e6
ug.to.kg=function(x)x*1e-9
kg.to.ug=function(x)x/1e-9

flow.xtab=reshape2::dcast(alt.srs,Alt+Date~SITE,value.var="Q",mean)
flow.xtab$FedWY=WY(flow.xtab$Date,"Fed")
flow.xtab=subset(flow.xtab,FedWY%in%seq(1966,2016,1))

flow.xtab$S12ABC=rowSums(flow.xtab[,c("S12A","S12B","S12C")],na.rm=T)
flow.xtab$S12DS333=rowSums(flow.xtab[,c("S12D","S333")],na.rm=T)
flow.xtab$S355AB=rowSums(flow.xtab[,c("S355A","S355B")],na.rm=T)

#pseduo method #2
flow.xtab$S333.adj=with(flow.xtab,S333*(1-ifelse((S333+S333N+S355AB+S356)>0,S334/(S333+S333N+S355AB+S356),0)))
flow.xtab$S333N.adj=with(flow.xtab,S333N*(1-ifelse((S333+S333N+S355AB+S356)>0,S334/(S333+S333N+S355AB+S356),0)))
flow.xtab$S355AB.adj=with(flow.xtab,S355AB*(1-ifelse((S333+S333N+S355AB+S356)>0,S334/(S333+S333N+S355AB+S356),0)))
flow.xtab$S356.adj=with(flow.xtab,S356*(1-ifelse((S333+S333N+S355AB+S356)>0,S334/(S333+S333N+S355AB+S356),0)))

flow.xtab$TFlow.acftd=rowSums(flow.xtab[,c("S12ABC","S12D","S333.adj","S333N.adj","S355AB.adj","S356.adj")],na.rm=T)

S12ABC.TP.FWM$S12ABC.FWM=S12ABC.TP.FWM$FWM
S12DS333.TP.FWM$S12DS333.FWM=S12DS333.TP.FWM$FWM

flow.xtab=merge(flow.xtab,S12ABC.TP.FWM[,c("Date","Alt","S12ABC.FWM")],c("Date","Alt"))
flow.xtab=merge(flow.xtab,S12DS333.TP.FWM[,c("Date","Alt","S12DS333.FWM")],c("Date","Alt"))
flow.xtab$S12A.Load.kg=with(flow.xtab,ug.to.kg(S12ABC.FWM*acft.to.L(S12A)))
flow.xtab$S12B.Load.kg=with(flow.xtab,ug.to.kg(S12ABC.FWM*acft.to.L(S12B)))
flow.xtab$S12C.Load.kg=with(flow.xtab,ug.to.kg(S12ABC.FWM*acft.to.L(S12C)))
flow.xtab$S12D.Load.kg=with(flow.xtab,ug.to.kg(S12DS333.FWM*acft.to.L(S12D)))
flow.xtab$S333.Load.kg=with(flow.xtab,ug.to.kg(S12DS333.FWM*acft.to.L(S333.adj)))
flow.xtab$S333N.Load.kg=with(flow.xtab,ug.to.kg(S12DS333.FWM*acft.to.L(S333N.adj)));
flow.xtab$S355AB.Load.kg=with(flow.xtab,ug.to.kg(8*acft.to.L(S355AB.adj)));#based on walkers analysis
flow.xtab$S356.Load.kg=with(flow.xtab,ug.to.kg(6.1*acft.to.L(S356.adj)));#based on walkers analysis

flow.xtab$TLoad.kg=rowSums(flow.xtab[,c("S12A.Load.kg","S12B.Load.kg","S12C.Load.kg","S12D.Load.kg","S333.Load.kg","S333N.Load.kg","S355AB.Load.kg","S356.Load.kg")],na.rm = T)
flow.xtab$FWM=with(flow.xtab,kg.to.ug(TLoad.kg)/acft.to.L(TFlow.acftd))

flow.xtab$week.num=format(flow.xtab$Date,"%V")
# biweekly.WQ=subset(flow.xtab,week.num%in%seq(1,53,2)&format(Date,"%A")=="Tuesday")

WY.FWM=ddply(flow.xtab,c("Alt","FedWY"),summarise,TFlow=sum(TFlow.acftd,na.rm=T),TLoad=sum(TLoad.kg,na.rm=T))
WY.FWM$FWM=with(WY.FWM,kg.to.ug(TLoad)/acft.to.L(TFlow))
WY.FWM$Alt=factor(WY.FWM$Alt,levels=alts.sort)
# WY.FWM=merge(WY.FWM,ddply(WY.FWM,"Alt",summarise,MeanFWM=mean(FWM,na.rm=T)),"Alt")
# WY.FWM$RF_FWM=with(WY.FWM,9.7/MeanFWM);#Rescale to Fed WY 2017 SRS FWM (method 2)
# WY.FWM$ResFWM=with(WY.FWM,RF_FWM*FWM)

plot(FWM~FedWY,subset(WY.FWM,Alt=="CC"))
with(subset(WY.FWM,Alt=="NA25"),points(FedWY,FWM,pch=21,bg="red"))


WY.FWM.sum=ddply(WY.FWM,"Alt",summarise,mean.val=mean(FWM),median.val=median(FWM))
WY.FWM.sum=WY.FWM.sum[match(alts.sort,WY.FWM.sum$Alt),]
WY.FWM.sum$FWO.diff=with(WY.FWM.sum,(mean.val-mean.val[1])/mean.val[1])*100

# png(filename=paste0(plot.path,"Iteration_2/SRS_FWM_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));

ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(FWM~Alt,WY.FWM,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,WY.FWM.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(WY.FWM.sum,Alt==alts.sort[1])$median.val,lty=2,col="black")
abline(h=subset(WY.FWM.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"SRS (S12s+[(S333s+S355AB+S356)-S334])")
mtext(side=2,line=2.25,"TP FWM (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
mtext(side=3, adj=1,"Fed WY 1966 - 2016")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/SRS_FWM_perdiff.png"),width=3,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,1,1),oma=c(1.5,5,1,0.25));

xlim.val=c(-15,0);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(WY.FWM.sum$FWO.diff,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(WY.FWM.sum,segments(FWO.diff,1:8,rep(0,8),1:8))
with(WY.FWM.sum,points(FWO.diff,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,WY.FWM.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"SRS FWM",cex=1,col="black",line=1)
mtext(side=1,line=2,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
mtext(side=3, adj=0,"Fed WY 1966 - 2016",cex=0.75)
dev.off()



# Discharge to WCA3 -------------------------------------------------------
##Discharge
RSM.sites=c("S8","S150")
wca3.q=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    wca3.q=rbind(tmp,wca3.q)
    print(i)
  }
}

RSM.sites=c(paste0("S11",LETTERS[1:3]))
S11.q=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMGL_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMGL/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    S11.q=rbind(tmp,S11.q)
    print(i)
  }
}

wca3.q=rbind(wca3.q,S11.q)

head(wca3.q)
wca3.q.total=ddply(wca3.q,c("Date","Alt"),summarise,TFlow=sum(FLOW))
wca3.q.total=wca3.q.total[order(wca3.q.total$Alt,wca3.q.total$Date),]
wca3.q.total$WY=WY(wca3.q.total$Date)
wca3.q.total=subset(wca3.q.total,WY%in%WYs);# Full Florida WY (MAy - April) 
wca3.q.total$area="WCA3Inflow"

wca3.inQ.WY=ddply(wca3.q.total,c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(TFlow)))
wca3.inQ.WY$Alt=factor(wca3.inQ.WY$Alt,levels=alts.sort)
wca3.inQ.WY.sum=ddply(wca3.inQ.WY,"Alt",summarise,mean.val=mean(TFlow.acft),median.val=median(TFlow.acft))
wca3.inQ.WY.sum=wca3.inQ.WY.sum[match(alts.sort,wca3.inQ.WY.sum$Alt),]
wca3.inQ.WY.sum$FWO.perdiff=with(wca3.inQ.WY.sum,((mean.val-mean.val[1])/mean.val[1])*100)


srs.sites=c(paste0("S12",c("A","B","C","D")),"S333","S333N")
alt.srs$WY=WY(alt.srs$Date)
SRS.inQ.WY=ddply(subset(alt.srs,SITE%in%srs.sites),c("Alt","WY"),summarise,TFlow.acft=sum(cfs.to.acftd(FLOW)))
SRS.inQ.WY.sum=ddply(SRS.inQ.WY,"Alt",summarise,mean.val=mean(TFlow.acft),median.val=median(TFlow.acft))
SRS.inQ.WY.sum=SRS.inQ.WY.sum[match(alts.sort,SRS.inQ.WY.sum$Alt),]

# png(filename=paste0(plot.path,"Iteration_2/WCA3_WYQ_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.25,0.75,0.5,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,210e4);by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,wca3.inQ.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,wca3.inQ.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(wca3.inQ.WY.sum,Alt==alts.sort[1])$median.val,lty=2,col="black")
abline(h=subset(wca3.inQ.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"WCA-3A Inflow (S-8 + S-150 + S-11s)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,205e4);by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(TFlow.acft~Alt,SRS.inQ.WY,ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,SRS.inQ.WY.sum$mean.val,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=subset(SRS.inQ.WY.sum,Alt==alts.sort[1])$median.val,lty=2,col="black")
abline(h=subset(SRS.inQ.WY.sum,Alt==alts.sort[1])$mean.val,lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"WCA-3A Outflow (S-12s+S-333+S-333N)")

mtext(side=2,outer=T,line=2,"Discharge (x10\u00B3 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()



# -------------------------------------------------------------------------

# CRE
S79.allPOS.sum=subset(est.allPOS.sum,SITE=="S79")
S79.allPOS.sum=S79.allPOS.sum[match(alts.sort,S79.allPOS.sum$Alt),]
S79.allPOS.sum$FWO.perdiff=with(S79.allPOS.sum,((Avg.TQ.kacft-Avg.TQ.kacft[1])/Avg.TQ.kacft[1])*100)
S79.allPOS.sum

subset(Q.cat.mean2,Alt=="EE2")

CRE.mfl.rslt

# Lake 
subset(env.pen.sum,Alt=="EE2")
subset(stg.scr.sum,Alt=="EE2")

# SLE
Q.cat.mean2
subset(Q.cat.mean2,Alt=="EE2")
S80.allPOS.sum=subset(est.allPOS.sum,SITE=="S80")
S80.allPOS.sum=S80.allPOS.sum[match(alts.sort,S80.allPOS.sum$Alt),]
S80.allPOS.sum$FWO.perdiff=with(S80.allPOS.sum,((Avg.TQ.kacft-Avg.TQ.kacft[1])/Avg.TQ.kacft[1])*100)
S80.allPOS.sum

# Flow south
FlowSouth.sum
FlowSouth.sum.season.wet
HiLo.close.sum






### 

RSM.sites=c("S77_QFC","S79_QFC")
q.dat2=data.frame()

for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts[j]
    q.dat2=rbind(tmp,q.dat2)
    print(i)
  }
}

q.dat2$WY=WY(q.dat2$Date)

QFC.sum=ddply(subset(q.dat2,WY%in%WYs),c("Alt","WY",'SITE'),summarise,Q.acft=sum(cfs.to.acftd(FLOW),na.rm=T))
QFC.sum$Alt=factor(QFC.sum$Alt,levels=alts.sort)

QFC.sum2=dcast(QFC.sum,Alt~SITE,value.var="Q.acft",mean)
QFC.sum2$S77.fwo=with(QFC.sum2,(S77_QFC-S77_QFC[1])/S77_QFC[1])*100
QFC.sum2$S79.fwo=with(QFC.sum2,(S79_QFC-S79_QFC[1])/S79_QFC[1])*100

boxplot(Q.acft~Alt,subset(QFC.sum,SITE=="S77_QFC"))
boxplot(Q.acft~Alt,subset(QFC.sum,SITE=="S79_QFC"))

## check >=2100 - <2600 cfs LOK regulatory
plot(FLOW~Date,subset(q.dat2,Alt=="NA25"&SITE=="S77_QFC"),type="l")
abline(h=c(2100,2600),lty=2,col="red")


# png(filename=paste0(plot.path,"Iteration_2/CRE_QFC_bxp.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.75,0.25,1),oma=c(4,3.75,1,0.25));
layout(matrix(1:2,2,1,byrow=T))

ylim.val=c(0,200e4);by.y=50e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(Q.acft~Alt,subset(QFC.sum,SITE=="S77_QFC"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,QFC.sum2$S77_QFC,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(QFC.sum,Alt=="NA25"&SITE=="S77_QFC")$Q.acft),lty=2,col="black")
abline(h=mean(subset(QFC.sum,Alt=="NA25"&SITE=="S77_QFC")$Q.acft),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,NA,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"S-77 (QFC)")
mtext(side=3, adj=1,"FLWY 1966 - 2016")

ylim.val=c(0,400e4);by.y=100e4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(Q.acft~Alt,subset(QFC.sum,SITE=="S79_QFC"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,QFC.sum2$S79_QFC,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(QFC.sum,Alt=="NA25"&SITE=="S79_QFC")$Q.acft),lty=2,col="black")
abline(h=mean(subset(QFC.sum,Alt=="NA25"&SITE=="S79_QFC")$Q.acft),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/10e3)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"S-79 (QFC)")
mtext(side=2,line=2,"Annual Discharge (x10\u00B3 AcFt d\u207B\u00B9)",outer=T)
mtext(side=1,line=3,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/CRE_QFC_perdiff.png"),width=6,height=5.25,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,0.7,1,1),oma=c(2,5,1,0.25));
layout(matrix(1:2,1,2,byrow=T))

xlim.val=c(-30,30);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(QFC.sum2$S79.fwo,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(QFC.sum2,segments(S77.fwo,1:8,rep(0,8),1:8))
with(QFC.sum2,points(S77.fwo,1:8,pch=21,bg="dodgerblue1",lwd=0.1))
axis_fun(2,1:8,1:8,QFC.sum2$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"S-77 (QFC Tag)")
mtext(side=2,line=4,'Model Alternative')

xlim.val=c(-10,20);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(QFC.sum2$S79.fwo,1:8,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(QFC.sum2,segments(S79.fwo,1:8,rep(0,8),1:8))
with(QFC.sum2,points(S79.fwo,1:8,pch=21,bg="indianred1",lwd=0.1))
axis_fun(2,1:8,1:8,NA)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3, adj=1,"FLWY 1966 - 2016",line=1)
mtext(side=3,adj=0,"S-79 (QFC Tag)")
mtext(side=1,outer=T,"Average Percent Difference to FWO")
dev.off()
