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
unique(lakeO.stage$Alt)
range(lakeO.stage$Date)

###

lakeO.stage$WY=WY(lakeO.stage$Date)
lakeO.stage$low.stg=with(lakeO.stage,ifelse(STAGE<=11,1,0))
lakeO.stage$vlow.stg=with(lakeO.stage,ifelse(STAGE<=10,1,0))
lakeO.stage$High.stg=with(lakeO.stage,ifelse(STAGE>16,1,0))
lakeO.stage$vHigh.stg=with(lakeO.stage,ifelse(STAGE>=17,1,0))

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_totalDays.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
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

# Stage duration curves
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_StageDuration.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
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
  if(i%in%c(3:5)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
  if(i==3){mtext(side=3, adj=0,"Lake Okeechobee")}
  if(i%in%c(3,7)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_StageDuration2.png"),width=7.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:7,1,7,byrow=T))

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

## Differnce in SDC 
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_StageDuration_Diff.png"),width=7.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:7,1,7,byrow=T))

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
# write.csv(SDC_seg,paste0(export.path,"Iteration2/w_SR35/iter2_SDC_seg_analysis.csv"),row.names=F)
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

tmp.comp=data.frame()
for(i in 1:9){
  for(j in 1:9){
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
subset(tmp.comp,Alt2=="NA25")

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_StageDuration_diff.png"),width=7.5,height=5.25,units="in",res=200,type="windows",bg="white")
xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x)
ylim.val=c(-1,1);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(1,1.5,0.1,0.1),oma=c(3,2.5,1,1.5));
layout(matrix(1:64,8,8,byrow = T))
for(i in 1:8){
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

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_StageDuration_segments.png"),width=7.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:10,2,5,byrow=T))

xlim.val=c(0,1.07);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],0.1)
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
  text(prop.val[i],tmp,pos=4,format(round(tmp,2),nsmall=2),cex=0.65,offset = 0.25)
}
if(j%in%c(1:4)){axis_fun(1,xmaj,xmin,NA)}else{axis_fun(1,xmaj,xmin,format(xmaj))}
if(j%in%c(1,5)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
if(j==1){mtext(side=3, adj=0,"Lake Okeechobee")}
if(j==5){mtext(side=3, adj=1,"CY 1965 - 2016")}
mtext(side=3, adj=1,line=-1.25,paste0(alts.sort[j]," "))
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
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
env.rslt=merge(env.rslt,data.frame(Alt=alts.sort,PlotOffset=rev(seq(2,18,2))),"Alt")
env.rslt$env.plt=with(env.rslt,env2+PlotOffset)
env.rslt$env.f=with(env.rslt,ifelse(env==1,"normal","recovery"))
env.rslt$Alt=factor(env.rslt$Alt,levels=alts.sort)

#env.count=reshape2::dcast(env.rslt,Alt~env.f,value.var = "env",function(x) N.obs(x))
env.count=ddply(env.rslt,c("Alt","env.f"),summarise,N.val=N.obs(env.f))
env.count$axs.val=19:2

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_Env.png"),width=8,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1.75,1),oma=c(2,2,1,2));

ylim.val=c(1.5,19.5)
xlim.val=c(1965,2016);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
plot(env2~CY,env.rslt,type="n",axes=F,ann=F,xlim=xlim.val,xaxs="i",yaxs="i",ylim=ylim.val)
abline(v=c(xmaj,xmin),h=c(2:19),lty=c(1,3),lwd=0.5,col=c("black","grey"))
abline(h=c(2:19),lty=c(3),lwd=0.5,col=c("grey"))
for(i in 1:length(alts.sort)){
  with(subset(env.rslt,Alt==alts.sort[i]),lines(env.plt~CY,type="s",col=cols[i],lwd=2.5))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5,cex=0.75)
axis_fun(2,seq(3,19,2),seq(3,19,2),rev(alts.sort))
axis_fun(4,env.count$axs.val,env.count$axs.val,env.count$N.val,cex=0.6)
abline(h=seq(3.5,19.5,2))
box(lwd=1)
mtext(side=3,adj=1,"Upper Step = Normal Envelope\nLower Step = Recovery Envelope",font=3)
mtext(side=1,line=1.25,"Calendar Year")
mtext(side=4,line=1.25,"Normal/Recovery Envelop Count")
dev.off()

##
library(LORECOVER)

head(lakeO.stage)
lakeO.stage$Data.Value=lakeO.stage$STAGE

norm.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts[i])
  rslt=norm_env(tmp)
  rslt$Alt=alts[i]
  norm.lakeO.stage.scr=rbind(norm.lakeO.stage.scr,rslt)
  print(i)
}
norm.lakeO.stage.scr=rename(norm.lakeO.stage.scr,c("penalty"="norm.score"))
norm.lakeO.stage.scr$WY=WY(norm.lakeO.stage.scr$Date)

head(norm.lakeO.stage.scr)
norm.lakeO.stage.scr.WY=ddply(norm.lakeO.stage.scr,c("Alt","WY"),summarise,TScore=sum(abs(norm.score),na.rm=T))
norm.lakeO.stage.scr.WY$Alt=factor(norm.lakeO.stage.scr.WY$Alt,levels=alts.sort)
norm.lakeO.stage.scr.WY.sum=ddply(norm.lakeO.stage.scr.WY,"Alt",summarise,mean.val=mean(TScore))
norm.lakeO.stage.scr.WY.sum$Alt=factor(norm.lakeO.stage.scr.WY.sum$Alt,levels=alts.sort)
norm.lakeO.stage.scr.WY.sum$FWO.diff=with(norm.lakeO.stage.scr.WY.sum,(mean.val-mean.val[1])/mean.val[1])*100

boxplot(TScore~Alt,norm.lakeO.stage.scr.WY,outline=F)

rec.lakeO.stage.scr=data.frame()
for(i in 1:n.alts){
  tmp=subset(lakeO.stage,Alt==alts[i])
  rslt=rec_env(tmp)
  rslt$Alt=alts[i]
  rec.lakeO.stage.scr=rbind(rec.lakeO.stage.scr,rslt)
  print(i)
}
rec.lakeO.stage.scr=rename(rec.lakeO.stage.scr,c("penalty"="rec.score"))
rec.lakeO.stage.scr$WY=WY(rec.lakeO.stage.scr$Date)

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
# write.csv(env.pen.sum,paste0(export.path,"Iteration2/w_SR35/iter2_LOMetrics.csv"),row.names=F)
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LakeO_EnvScore_BWA.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
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
env.pen.sum.plns=subset(env.pen.sum,Alt%in%alts.sort[3:9])

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_EnvScore_BWA_FWO.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
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
axis_fun(1,x.val,x.val,alts.sort[3:9],line=-0.5)
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


lakeO.stage.scr.WY=ddply(subset(lakeO.stage.scr,WY%in%WYs),c("Alt","WY"),summarise,cum.pen=sum(abs(score),na.rm=T))
stg.scr.sum=ddply(lakeO.stage.scr.WY,"Alt",summarise,mean.val=mean(cum.pen,na.rm=T))
stg.scr.sum$FWO.perdiff=with(stg.scr.sum,((mean.val-mean.val[1])/mean.val[1])*100)
# write.csv(stg.scr.sum,paste0(export.path,"Iteration2/w_SR35/stg_score_perdiff.csv"),row.names=F)

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_EnvScore_all_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
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

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/LOK_EnvScore_all_sum.png"),width=3.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(2,5,1,0.25));

xlim.val=c(-25,50);by.x=25;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(stg.scr.sum$FWO.perdiff,1:n.alts,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(stg.scr.sum,segments(FWO.perdiff,1:n.alts,rep(0,n.alts),1:n.alts))
with(stg.scr.sum,points(FWO.perdiff,1:n.alts,pch=21,bg="dodgerblue1",lwd=0.1))
with(stg.scr.sum,text(FWO.perdiff,1:n.alts,format(round(FWO.perdiff,1),nsmall=1),pos=ifelse(FWO.perdiff<0,2,4),cex=0.75,offset=0.25))
axis_fun(2,1:n.alts,1:n.alts,norm.lakeO.stage.scr.WY.sum$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.9);box(lwd=1)
mtext(side=3,adj=0,"Stage Envelope")
mtext(side=1,line=2.5,"Average Percent\nDifference to FWO")
mtext(side=2,line=4,'Model Alternative')
dev.off()

# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S78","S79","S80","S308","S351","S352","S354","S77_QFC",
            "S308_QFC","S79_QFC","S80_QFC","TMC2EST","S48","S49","NSF2EST","S80_QPFCSOURCE_LAKE","S308BF","S2","S3","S4BP")
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

# q.dat$Alt_SITE=paste(q.dat$Alt,q.dat$SITE,sep="_")
# q.dat$Q.14=with(q.dat,ave(FLOW,Alt_SITE,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
# 
# q.dat$CRE.low=with(q.dat,ifelse(SITE=="S79"&Q.14<750,1,0))
# q.dat$CRE.dam=with(q.dat,ifelse(SITE=="S79"&Q.14>2600,1,0))
# q.dat$CRE.opt=with(q.dat,ifelse(SITE=="S79"&Q.14>=750&Q.14<2100,1,0))
# q.dat$CRE.stress=with(q.dat,ifelse(SITE=="S79"&Q.14>=2100&Q.14<=2600,1,0))
# 
# q.dat$SLE.low=with(q.dat,ifelse(SITE=="S80"&Q.14<150,1,0))
# q.dat$SLE.dam=with(q.dat,ifelse(SITE=="S80"&Q.14>1700,1,0))
# q.dat$SLE.opt=with(q.dat,ifelse(SITE=="S80"&Q.14>=150&Q.14<1400,1,0))
# q.dat$SLE.stress=with(q.dat,ifelse(SITE=="S80"&Q.14>=1400&Q.14<1700,1,0))

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
q.dat.xtab$S2S3=rowSums(q.dat.xtab[,c("S2","S3")],na.rm=T)
q.dat.xtab$S79.14d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S79.30d=with(q.dat.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,29),rollapply(x,width=30,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80.14d=with(q.dat.xtab,ave(S80,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))

# vars=c("Date","Alt","S79")
# write.csv(q.dat.xtab[,vars],paste0(export.path,"Iteration2/w_SR35/S79Q_iter2_wSR35.csv"))

## MFL exceedance
q.dat.xtab$exceed=with(q.dat.xtab,ifelse(is.na(S79.30d)==T,0,ifelse(S79.30d<457,1,0)))

### Monthly flow volumes
q.dat.mon=dcast(q.dat, Alt+CY+month~SITE,value.var = "FLOW",sum)

## C44 WB
q.dat.mon.SLE=q.dat.mon[,c("Alt","CY","month","S308","S308BF","S80")]
q.dat.mon.SLE$TFlow.out=rowSums(q.dat.mon.SLE[,c("S308BF","S80")])
q.dat.mon.SLE$C44Basin=with(q.dat.mon.SLE,ifelse(TFlow.out<S308,0,TFlow.out-S308))
q.dat.mon.SLE$lake.Q=with(q.dat.mon.SLE,ifelse(TFlow.out==0,S308,ifelse(S308>TFlow.out,S308-TFlow.out,0)))
q.dat.mon.SLE$TFlow.in=rowSums(q.dat.mon.SLE[,c("C44Basin","lake.Q")])

q.dat.mon.SLE$per_S308BF=with(q.dat.mon.SLE,ifelse(TFlow.out==0,0,S308BF/TFlow.out))
q.dat.mon.SLE$per_S80=with(q.dat.mon.SLE,ifelse(TFlow.out==0,0,S80/TFlow.out))

### Annual flow volumes
q.dat.CY=ddply(q.dat,c("CY","Alt",'SITE'),summarise,TFlow.acft=sum(cfs.to.acftd(FLOW),na.rm=T))
# write.csv(q.dat.CY,paste0(export.path,"Iteration2/w_SR35/ann_Q.csv"),row.names = F)
# q.dat.CY.sum=ddply(q.dat.CY,c("Alt",'SITE'),summarise,mean.val=mean(TFlow.acft),sd.val=sd(TFlow.acft))

CRE.Q.sum=dcast(subset(q.dat.CY,SITE%in%c("S79","S77","S77_QFC")),Alt~SITE,value.var="TFlow.acft",mean)
CRE.Q.sum=CRE.Q.sum[match(alts.sort,CRE.Q.sum$Alt),]
CRE.Q.sum

sites.val=c("NSF2EST","S4BP", "S2", "S3", "S308", "S308_QFC", "S308BF", "S351","S352", "S354", "S48", "S49", "S77", "S77_QFC", "S78", "S79","S79_QFC", "S80", "S80_QFC", "S80_QPFCSOURCE_LAKE", "TMC2EST")
group.val=c("NSF2EST","S2S3S4", "S2S3S4", "S2S3S4", "S308", "S308_QFC", "S308BF", "FlowSouth","S352", "FlowSouth", "S48", "S49", "S77", "S77_QFC", "S78", "S79","S79_QFC", "S80", "S80_QFC", "S80_QPFCSOURCE_LAKE", "TMC2EST")
q.dat2=merge(q.dat,data.frame(SITE=sites.val,SITE2=group.val),"SITE")

q.dat.CY2=ddply(q.dat2,c("CY","Alt",'SITE2'),summarise,TFlow.acft=sum(cfs.to.acftd(FLOW),na.rm=T))
q.dat.CY2$Alt=factor(q.dat.CY2$Alt,levels=alts.sort)

Flowsouth.sum=reshape2::dcast(subset(q.dat.CY2,SITE2%in%c("S2S3S4","FlowSouth")),Alt~SITE2,value.var="TFlow.acft",mean)
Flowsouth.sum=Flowsouth.sum[match(alts.sort,Flowsouth.sum$Alt),]
Flowsouth.sum$perFWO.flowsouth=with(Flowsouth.sum,(FlowSouth-FlowSouth[1])/FlowSouth[1])*100
Flowsouth.sum$perFWO.S2S3S4=with(Flowsouth.sum,(S2S3S4-S2S3S4[1])/S2S3S4[1])*100
Flowsouth.sum

Flowsouth.sum_1995=reshape2::dcast(subset(q.dat.CY2,SITE2%in%c("S2S3S4","FlowSouth")&CY>=1995),Alt~SITE2,value.var="TFlow.acft",mean)
Flowsouth.sum_1995=Flowsouth.sum_1995[match(alts.sort,Flowsouth.sum_1995$Alt),]
Flowsouth.sum_1995$perFWO.flowsouth=with(Flowsouth.sum_1995,(FlowSouth-FlowSouth[1])/FlowSouth[1])*100
Flowsouth.sum_1995$perFWO.S2S3S4=with(Flowsouth.sum_1995,(S2S3S4-S2S3S4[1])/S2S3S4[1])*100
Flowsouth.sum_1995

# RECOVER plots -----------------------------------------------------------
## CRE
q.dat.xtab$S77_QFC.14d=with(q.dat.xtab,ave(S77_QFC,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S79_QFC.14d=with(q.dat.xtab,ave(S79_QFC,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$CRE.low=with(q.dat.xtab,ifelse(S79.14d<750,1,0)) # RECOVER Low
q.dat.xtab$CRE.low1=with(q.dat.xtab,ifelse(S79.14d<457,1,0))
q.dat.xtab$CRE.low2=with(q.dat.xtab,ifelse(S79.14d>=457&S79.14d<750,1,0))
q.dat.xtab$CRE.opt=with(q.dat.xtab,ifelse(S79.14d>=750&S79.14d<2100,1,0)) # RECOVER Optimum
q.dat.xtab$CRE.high=with(q.dat.xtab,ifelse(S79.14d>=2100&S79.14d<2600,1,0)) # RECOVER Stress
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
q.dat.xtab$S308_QFC.14d=with(q.dat.xtab,ave(S308_QFC,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S308.14d=with(q.dat.xtab,ave(S308,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80_QFC.14d=with(q.dat.xtab,ave(S80_QFC,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$S80_QPFCSOURCE_LAKE.14d=with(q.dat.xtab,ave(S80_QPFCSOURCE_LAKE,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
q.dat.xtab$SLE.low=with(q.dat.xtab,ifelse(SLE.S80trib.14d<150,1,0)) # RECOVER Low
q.dat.xtab$SLE.opt=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=150&SLE.S80trib.14d<1400,1,0)) # RECOVER Optimum
q.dat.xtab$SLE.high=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1400&SLE.S80trib.14d<1700,1,0)) # RECOVER stress
q.dat.xtab$SLE.dam=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1700,1,0)) # RECOVER damaging
q.dat.xtab$SLE.high1=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=1700&SLE.S80trib.14d<4000,1,0))
q.dat.xtab$SLE.high2=with(q.dat.xtab,ifelse(SLE.S80trib.14d>=4000,1,0))


head(q.dat.xtab)
q.dat.xtab$consec.CRE.low=0

q.dat.xtab$CRE.low.count=0
q.dat.xtab$CRE.low1.count=0
q.dat.xtab$CRE.low2.count=0
q.dat.xtab$CRE.opt.count=0
q.dat.xtab$CRE.high.count=0
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
                                          ifelse((SLE.S80trib.14d[i]-S308.14d[i])<=1400,1,0),0))
    tmp$SLE.high.basin.count[i]=with(tmp,SLE.high.count[i]-SLE.high.LOK.count[i])
    tmp$SLE.dam.count[i]=with(tmp,ifelse(SLE.dam[i]==1&sum(SLE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))  
    tmp$SLE.dam.LOK.count[i]=with(tmp,
                                  ifelse(SLE.dam.count[i]==1,
                                         ifelse((SLE.S80trib.14d[i]-S308.14d[i])<=1700,1,0),0))
    tmp$SLE.dam.basin.count[i]=with(tmp,SLE.dam.count[i]-SLE.dam.LOK.count[i])
    tmp$SLE.high1.count[i]=with(tmp,ifelse(SLE.high1[i]==1&sum(SLE.high1.count[(i-13):(i-1)],na.rm=T)==0,1,0))
    tmp$SLE.high2.count[i]=with(tmp,ifelse(SLE.high2[i]==1&sum(SLE.high2.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  }
  q.dat.xtab2=rbind(q.dat.xtab2,tmp)
  print(j)
}


# CRE ---------------------------------------------------------------------
cumsum_reset <- function(x, reset=NA) {
  # from https://rdrr.io/github/billdenney/bsd.report/src/R/math_helpers.R
  count <- 0
  ret <- rep(NA_integer_, length(x))
  for (i in seq_along(x)) {
    count <- count + x[i]
    if (x[i] %in% reset) {
      count <- 0
    }
    ret[i] <- count
  }
  ret
}
## consecutive counts 
tmp=subset(q.dat.xtab2,Alt==alts.sort[1])
tmp$d14_period=as.numeric(format(tmp$Date,"%j"))%/%15L+1L
head(tmp[,c("CRE.low","CRE.low.count","d14_period")],50L)
test=ddply(tmp,c("CY","d14_period"),summarise,low.sum=sum(CRE.low.count,na.rm=T))
# test$consec=cumsum_reset(test$low.sum,0)

test.consec=consec.startend(test$low.sum>0)
test$sum.low=0
for(i in 1:length(test.consec$ends)){
  test[test.consec$ends[i],]$sum.low=with(test[c(test.consec$starts[i]:test.consec$ends[i]),],sum(low.sum))
}
rslt=ddply(test,"sum.low",summarise,count=N.obs(sum.low))

plot(rep(1,18),subset(rslt,sum.low>0)$count,type="n")
text(rep(1,18),subset(rslt,sum.low>0)$count,subset(rslt,sum.low>0)$sum.low)
### 
vars=c("CRE.low.count","CRE.opt.count", 
       "CRE.high.basin.count", "CRE.high.LOK.count", 
       "CRE.dam.basin.count","CRE.dam.LOK.count",
       "CRE.low1.count","CRE.low2.count","CRE.high.count",
       "CRE.high1.count","CRE.high2.count","CRE.high3.count","CRE.dam.count")
# apply(tmp[,vars],2,FUN=function(x)sum(x,na.rm=T))
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars)],id.vars = "Alt")
CRE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
CRE.SalEnv_count=CRE.SalEnv_count[match(alts.sort,CRE.SalEnv_count$Alt),]

CRE.SalEnv_count$perFWO.opt=with(CRE.SalEnv_count,(CRE.opt.count-CRE.opt.count[1])/CRE.opt.count[1])*100
CRE.SalEnv_count$perFWO.stress=with(CRE.SalEnv_count,(CRE.high.count-CRE.high.count[1])/CRE.high.count[1])*100
CRE.SalEnv_count$perFWO.dam=with(CRE.SalEnv_count,(CRE.dam.count-CRE.dam.count[1])/CRE.dam.count[1])*100
CRE.SalEnv_count$perFWO_2600_4500=with(CRE.SalEnv_count,(CRE.high1.count-CRE.high1.count[1])/CRE.high1.count[1])*100
CRE.SalEnv_count$perFWO_4500_6500=with(CRE.SalEnv_count,(CRE.high2.count-CRE.high2.count[1])/CRE.high2.count[1])*100
CRE.SalEnv_count$perFWO_6500=with(CRE.SalEnv_count,(CRE.high3.count-CRE.high3.count[1])/CRE.high3.count[1])*100
CRE.SalEnv_count[,c('Alt',"perFWO.opt","perFWO.stress","perFWO.dam","perFWO_2600_4500","perFWO_4500_6500","perFWO_6500")]

vars=c("CRE.low1.count","CRE.low2.count","CRE.opt.count","CRE.high.count","CRE.high1.count","CRE.high2.count","CRE.high3.count")

library(fmsb)
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/CRE_RECOVER_radar.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:8),2,4,byrow=T))
par(family="serif",mar = c(1, 1, 1, 1),oma=c(1,1,1,1),xpd=NA)

for(i in 3:9){
  alts.val=c("ECBr","NA25",alts.sort[i])
  col.vals=cols[alts.sort%in%alts.val]
  tmp=rbind(rep(max(apply(CRE.SalEnv_count[,c("Alt",vars)][,-1],1,max,na.rm=T)),7),
            rep(min(apply(CRE.SalEnv_count[,c("Alt",vars)][,-1],1,min,na.rm=T)),7),
            subset(CRE.SalEnv_count[,c("Alt",vars)],Alt%in%alts.val)[,-1])
  # subset(CRE.SalEnv_count,Alt%in%alts.val)[,1:8]
  
  labs=c("<457","457 -\n750","750 -\n2100\n(Opt)","2100 -\n2600\n(Stress)","2600 -\n4500","4500 -\n6500",">6500")
  radarchart(tmp,
             pcol=col.vals,
             plwd=1.5,pty=NA,
             plty=c(2,1,1),
             cglcol = "grey", cglty = 1, cglwd = 0.8,
             axislabcol = "grey",
             vlabels=labs,vlcex=0.75)
  mtext(side=3,adj=0,line=-2,alts.sort[i],font=2)
  if(i==3){mtext(side=3,adj=0,"Count of 14-Day Period")}
  # if(i==6){mtext(side=3,adj=1,"Period of Sim: CY 1965 - 2016")}
  # box(lwd=1)
  # legend("bottom",legend=alts.val,horiz=T,
  #       bty="n",pch=20,pt.cex=2,col=col.vals)
}
plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0.5,legend=alts.sort,
       pch=NA,pt.cex=2,lty=c(2,1,rep(1,7)),col=cols,
       bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title="Alternatives",title.adj = 0)
text(1,-0.1,"Period of Simulation\n CY1965 - 2016.",adj=1,xpd=NA)
dev.off()

CRE.SalEnv_count=CRE.SalEnv_count[,c("Alt",vars)]
labs=c("<457","457 - 750","750 - 2100","2100 - 2600","2600 - 4500","4500 - 6500",">6500")
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/CRE_cat_total.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
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
mtext(side=2,line=0.75,outer=T,"Count of 14-Day periods")
mtext(side=3,adj=0,outer=T,line=1.5,"Caloosahatchee Estuary - Salinity Envelope")
dev.off()

head(q.dat.xtab2)

CRE.QCat.POS=ddply(q.dat.xtab2,"Alt",summarise,
                   Q.LT457=sum(cfs.to.acftd(S79.14d[CRE.low1.count==1])/1000,na.rm=T),
                   Q.Q457_750=sum(cfs.to.acftd(S79.14d[CRE.low2.count==1])/1000,na.rm=T),
                   Q.Q_Opt=sum(cfs.to.acftd(S79.14d[CRE.opt.count==1])/1000,na.rm=T),
                   Q.Q_Stress=sum(cfs.to.acftd(S79.14d[CRE.high.count==1])/1000,na.rm=T),
                   Q.Q2600_4500=sum(cfs.to.acftd(S79.14d[CRE.high1.count==1])/1000,na.rm=T),
                   Q.Q4500_6500=sum(cfs.to.acftd(S79.14d[CRE.high2.count==1])/1000,na.rm = T),
                   Q.QGT6500=sum(cfs.to.acftd(S79.14d[CRE.high3.count==1])/1000,na.rm=T))
CRE.QCat.POS=CRE.QCat.POS[match(alts.sort,CRE.QCat.POS$Alt),]
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/CRE_Qcat_total.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:8),2,4,byrow=T))
par(family="serif",mar=c(2,2,0.25,1),oma=c(2,3,2,0.25),lwd=0.5);

ymax=c(300,600,2500,2500,3000,3000,2000)
yval=ymax/2
for(i in 2:8){
  if(i==2){
    ylim.val=c(10,5000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
    x=barplot(CRE.QCat.POS[,i],col=adjustcolor(cols,0.5),log="y",ylim=ylim.val,space=0,axes=F,ann=F)
    axis_fun(2,ymaj,ymin,ymaj)
    axis_fun(1,x,x,NA);box(lwd=1)
    mtext(side=3,adj=0,paste(labs[i-1],"cfs"),cex=0.75)
    text(x,CRE.QCat.POS[,i],round(CRE.QCat.POS[,i],0),font=2,col="black",pos=1,cex=0.4)
  }else{
    ylim.val=c(0,ymax[i-1]);by.y=yval[i-1];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    x=barplot(CRE.QCat.POS[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
    axis_fun(2,ymaj,ymin,ymaj)
    if(i%in%c(5:8)){axis_fun(1,x,x,alts.sort,cex=0.8,las=2)}else{axis_fun(1,x,x,NA)}
    box(lwd=1)
    mtext(side=3,adj=0,paste(labs[i-1],"cfs"),cex=0.75)
    text(x,CRE.QCat.POS[,i],round(CRE.QCat.POS[,i],0),font=2,col="black",pos=1,cex=0.4)
  }
}
plot(0:1,0:1,ann=F,axes=F,type="n")
text(1,0.15,"S79 14-Day\nAvg Discharge\n \nPeriod of Simulation\n CY1965 - 2016.",adj=1,xpd=NA)
mtext(side=1,line=0.5,outer=T,"Alternative")
mtext(side=2,line=1.5,outer=T,"Total Discharge (x1000 Ac-Ft d\u207B\u00B9)")
dev.off()

# CRE team
# based on 14-day
q.dat1.xtab=reshape2::dcast(q.dat,Alt+Date+month+CY~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
# q.dat1.xtab$S79.14d=with(q.dat.xtab1,ave(S79,Alt,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
# q.dat1.xtab$QLT457=with(q.dat1.xtab,ifelse(S79.14d<457,1,0))
# q.dat1.xtab$Q457_750=with(q.dat1.xtab,ifelse(S79.14d>=457&S79.14d<750,1,0))
# q.dat1.xtab$Q_Opt=with(q.dat1.xtab,ifelse(S79.14d>=750&S79.14d<2100,1,0))
# q.dat1.xtab$Q_Stress=with(q.dat1.xtab,ifelse(S79.14d>=2100&S79.14d<2600,1,0))
# q.dat1.xtab$Q_Dam=with(q.dat1.xtab,ifelse(S79.14d>=2600,1,0))
# q.dat1.xtab$Q2600_4500=with(q.dat1.xtab,ifelse(S79.14d>=2600&S79.14d<4500,1,0))
# q.dat1.xtab$Q4500_6500=with(q.dat1.xtab,ifelse(S79.14d>=4500&S79.14d<6500,1,0))
# q.dat1.xtab$QGT6500=with(q.dat1.xtab,ifelse(S79.14d>6500,1,0))

## Monthly average flow
q.dat1.xtab.mon=ddply(q.dat1.xtab,c("Alt","CY","month"),summarise,
                      Q.S79=mean(S79),
                      Q_GT2100=sum(S79>2100,na.rm=T),
                      Q_GT2600=sum(S79>2600,na.rm=T))
q.dat1.xtab.mon$Q_GT2100.mon=with(q.dat1.xtab.mon,ifelse(Q.S79>2100,1,0))
q.dat1.xtab.mon$Q_GT2600.mon=with(q.dat1.xtab.mon,ifelse(Q.S79>2600,1,0))

q.dat1.xtab.mon2=data.frame()

for(j in 1:9){
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
rslt.2100=rslt.2100[,c("sum.2100",alts.sort)]
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/CRE_S79Q_Consec2100.png"),width=5.5,height=6.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,45);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
layout(matrix(c(1:9),9,1,byrow=F))
par(family="serif",mar=c(1,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);

for(i in 2:10){
  x=barplot(t(rslt.2100[,i]),beside=T,ylim=ylim.val,
            col=cols[i-1],ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.2100)),space=c(0,0),yaxs="i",xaxs="i")
  # text(x[1],rslt.2100[1,i],rslt.2100[1,i],pos=1,cex=0.75)
  # text(x[2:28],rslt.2100[2:28,i],rslt.2100[2:28,i],pos=3,cex=0.75)
  text(x,rslt.2100[,i],rslt.2100[,i],pos=3,cex=0.75)
  if(i==10){axis_fun(1,x,x,rslt.2100$sum.2100,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
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
rslt.2600=rslt.2600[,c("sum.2600",alts.sort)]
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/CRE_S79Q_Consec2600.png"),width=5.5,height=6.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,45);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
layout(matrix(c(1:9),9,1,byrow=F))
par(family="serif",mar=c(1,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);

for(i in 2:10){
  x=barplot(t(rslt.2600[,i]),beside=T,ylim=ylim.val,
            col=cols[i-1],ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.2600)),space=c(0,0),yaxs="i",xaxs="i")
  # text(x[1],rslt.2600[1,i],rslt.2600[1,i],pos=1,cex=0.75)
  # text(x[2:28],rslt.2600[2:28,i],rslt.2600[2:28,i],pos=3,cex=0.75)
  text(x,rslt.2600[,i],rslt.2600[,i],pos=3,cex=0.75)
  if(i==10){axis_fun(1,x,x,rslt.2600$sum.2600,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
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

# CRE MFL -----------------------------------------------------------------
CRE.mfl.rslt=data.frame()
q.dat1.xtab.mfl=data.frame()
for(j in 1:n.alts){
  
  tmp=subset(q.dat.xtab,Alt==alts.sort[j])
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
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/CRE_MFL_Alt_",alts.sort[i],".png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
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
CRE.mfl.rslt$plot.y=1:n.alts
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/CRE_MFL_sum.png"),width=6,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.7,0.25,1),oma=c(1,5,1,0.25));
layout(matrix(1:2,1,2),widths=c(1,0.1))
xlim.val=c(-30,305);by.x=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(plot.y~NA25_perchange,CRE.mfl.rslt,ann=F,axes=F,type="n",xlim=xlim.val)
abline(v=0)
with(CRE.mfl.rslt,segments(NA25_perchange,plot.y,rep(0,n.alts),plot.y))
with(CRE.mfl.rslt,points(NA25_perchange,plot.y,pch=21,bg="dodgerblue1",lwd=0.1))
with(subset(CRE.mfl.rslt,NA25_perchange<0),text(NA25_perchange,plot.y,format(round(NA25_perchange,1),nsmall=1),cex=0.5,pos=2))
with(subset(CRE.mfl.rslt,NA25_perchange>0),text(NA25_perchange,plot.y,format(round(NA25_perchange,1),nsmall=1),cex=0.5,pos=4))
axis_fun(2,1:n.alts,1:n.alts,CRE.mfl.rslt$Alt)
axis_fun(1,xmaj,xmin,xmaj,line=-0.7,cex=0.8);box(lwd=1)
mtext(side=3,adj=0,"Caloosahatchee MFL")
mtext(side=3,adj=1,"CY 1965 - 2016")
mtext(side=1,line=1.5,"Average Percent Difference to FWO")
mtext(side=2,line=4,'Model Alternative')

plot(rep(1,n.alts),1:n.alts,axes=F,ann=F,type="n")
with(CRE.mfl.rslt,text(rep(1,n.alts),plot.y,N.exceed,cex=0.75,xpd=NA))
mtext(side=3,"MFL\nExceedances",cex=0.5)
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



# SLE ---------------------------------------------------------------------
vars=c("SLE.low.count", "SLE.opt.count",
       "SLE.high.LOK.count", "SLE.high.basin.count", 
       "SLE.dam.LOK.count", "SLE.dam.basin.count", 
       "SLE.dam.count","SLE.high.count",
       "SLE.high1.count", "SLE.high2.count")
tmp=reshape2::melt(q.dat.xtab2[,c("Alt",vars)],id.vars = "Alt")
SLE.SalEnv_count=reshape2::dcast(tmp,Alt~variable,value.var = "value",sum)
SLE.SalEnv_count=SLE.SalEnv_count[match(alts.sort,SLE.SalEnv_count$Alt),]

vars=c("SLE.low.count","SLE.opt.count","SLE.high.count","SLE.dam.count","SLE.high1.count","SLE.high2.count")
SLE.SalEnv_count=SLE.SalEnv_count[,c("Alt",vars)]
labs=c("<150","150 - 1400","1400 - 1700","> 1700","1700 - 4000","> 4000")
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/SLE_cat_total.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:6),2,3,byrow=T))
par(family="serif",mar=c(2,2,0.25,1),oma=c(2.5,2,3,0.25),lwd=0.5);

ymax=c(200,1000,400,600,600,200)
yval=ymax/2
for(i in 2:7){
  ylim.val=c(0,ymax[i-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[i-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[i-1])
  x=barplot(SLE.SalEnv_count[,i],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(5:8)){axis_fun(1,x,x,alts.sort,cex=0.8,las=2)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  mtext(side=3,adj=0,paste(labs[i-1],"cfs"),cex=0.75)
  text(x,SLE.SalEnv_count[,i],round(SLE.SalEnv_count[,i],0),font=2,col="black",pos=1,cex=0.4)
}
mtext(side=1,line=3,adj=1,"S80 + Tribs Discharge Period of Simulation CY1965 - 2016.",cex=0.5)
mtext(side=1,line=0.75,outer=T,"Alternative")
mtext(side=2,line=0.75,outer=T,"Count of 14-Day periods")
mtext(side=3,adj=0,outer=T,line=1.5,"St Lucie Estuary - Salinity Envelope")
dev.off()


q.dat1.xtab.mon=ddply(q.dat1.xtab,c("Alt","CY","month"),summarise,
                      Q.S80=mean(S80))
q.dat1.xtab.mon$Q_GT1400.mon=with(q.dat1.xtab.mon,ifelse(Q.S80>1400,1,0))
q.dat1.xtab.mon$Q_GT1700.mon=with(q.dat1.xtab.mon,ifelse(Q.S80>1700,1,0))

q.dat1.xtab.mon2=data.frame()

for(j in 1:9){
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
rslt.2100=rslt.2100[,c("sum.2100",alts.sort)]
# png(filename=paste0(plot.path,"Iteration_2/w_SR35/CRE_S79Q_Consec2100.png"),width=5.5,height=6.5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,45);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
layout(matrix(c(1:9),9,1,byrow=F))
par(family="serif",mar=c(1,2,0.25,1),oma=c(2,2,2,0.25),lwd=0.5);

for(i in 2:10){
  x=barplot(t(rslt.2100[,i]),beside=T,ylim=ylim.val,
            col=cols[i-1],ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.2100)),space=c(0,0),yaxs="i",xaxs="i")
  # text(x[1],rslt.2100[1,i],rslt.2100[1,i],pos=1,cex=0.75)
  # text(x[2:28],rslt.2100[2:28,i],rslt.2100[2:28,i],pos=3,cex=0.75)
  text(x,rslt.2100[,i],rslt.2100[,i],pos=3,cex=0.75)
  if(i==10){axis_fun(1,x,x,rslt.2100$sum.2100,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
  axis_fun(2,ymaj,ymin,ymaj)
  box(lwd=1)
  mtext(side=3,adj=1,line=-1,paste0("Alt ", alts.sort[i-1]," "),cex=0.75)
  if(i==2){mtext(side=3,adj=1,"CY 1965 - 2016")}
}
mtext(side=2,line=0.5,outer=T,"Freq. Monthly Mean S79 Discharge > 2100 cfs")
mtext(side=1,line=2,"Consecutive Months")
dev.off()


# Flow South --------------------------------------------------------------

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/EVER_FlowSouth_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,800e3);by.y=200e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

boxplot(TFlow.acft~Alt,subset(q.dat.CY2,SITE2=="FlowSouth"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,Flowsouth.sum$FlowSouth,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.CY2,SITE2=="FlowSouth"&Alt==alts.sort[1])$TFlow.acft),lty=2,col="black")
abline(h=mean(subset(q.dat.CY2,SITE2=="FlowSouth"&Alt==alts.sort[1])$TFlow.acft),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/1000)
# axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"Flow South (S-351 + S-354)")
mtext(side=3, adj=1,"CY 1965 - 2016")
mtext(side=2,line=2.5,"Annual Discharge (x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()

# png(filename=paste0(plot.path,"Iteration_2/w_SR35/EVER_FlowSouth_BF_bxp.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2.75,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
ylim.val=c(0,200e3);by.y=50e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

boxplot(TFlow.acft~Alt,subset(q.dat.CY2,SITE2=="S2S3S4"),ylim=ylim.val,axes=F,ann=F,outline=F,col=cols)
points(1:n.alts,Flowsouth.sum$S2S3S4,pch=21,bg="springgreen",lwd=0.1,cex=1.25)
abline(h=median(subset(q.dat.CY2,SITE2=="S2S3S4"&Alt==alts.sort[1])$TFlow.acft),lty=2,col="black")
abline(h=mean(subset(q.dat.CY2,SITE2=="S2S3S4"&Alt==alts.sort[1])$TFlow.acft),lty=2,col="springgreen")
axis_fun(2,ymaj,ymin,ymaj/1000)
# axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
axis_fun(1,1:n.alts,1:n.alts,alts.sort,las=2)
abline(v=2.5)
box(lwd=1)
mtext(side=3, adj=0,"Flow South - Backflow (S-2 + S-3 + S-4)")
mtext(side=3, adj=1,"CY 1965 - 2016")
mtext(side=2,line=2.5,"Annual Discharge (x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=1,line=4,"Model Alternatives")
dev.off()