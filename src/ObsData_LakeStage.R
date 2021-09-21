## 
## Lake Okeechobee Regulation Schedule History
##
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
library(zoo)
library(reshape2)
library(plyr)

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


dates.val=data.frame(Date=seq(date.fun("1900-01-01"),date.fun("1900-12-31"),"1 days"))
# 1965 - ALTERNATE REGULATION SCHEDULE
ZoneA.1965=data.frame(Date=date.fun(c("1900-01-01","1900-04-01","1900-06-15","1900-09-01","1900-11-15","1900-12-31")),
                      ZoneA=c(15.5,15.5,14.0,14.0,15.5,15.5))
ZoneA.1965=merge(ZoneA.1965,dates.val,"Date",all.y=T)
ZoneB.1965=data.frame(Date=date.fun(c("1900-01-01","1900-03-15","1900-06-15","1900-09-01","1900-11-15","1900-12-31")),
                      ZoneB=c(15.5,15.5,13.5,14.0,15.5,15.5))
ZoneB.1965=merge(ZoneB.1965,dates.val,"Date",all.y=T)
ZoneC.1965=data.frame(Date=date.fun(c("1900-01-01","1900-01-15","1900-06-15","1900-09-01","1900-11-15","1900-12-31")),
                      ZoneC=c(15.5,15.5,13.0,13.5,15.5,15.5))
ZoneC.1965=merge(ZoneC.1965,dates.val,"Date",all.y=T)
sch1965=merge(merge(ZoneA.1965,ZoneB.1965,"Date"),ZoneC.1965,"Date")
sch1965$ZoneA=na.approx(sch1965$ZoneA)
sch1965$ZoneB=na.approx(sch1965$ZoneB)
sch1965$ZoneC=na.approx(sch1965$ZoneC)

plot(ZoneA~Date,sch1965,ylim=c(12.0,16.0),type="n")
with(sch1965,lines(ZoneA~Date))
with(sch1965,lines(ZoneB~Date))
with(sch1965,lines(ZoneC~Date))


sch1965$month=as.numeric(format(sch1965$Date,"%m"))
sch1965$day=as.numeric(format(sch1965$Date,"%d"))
# vars=c("day", "month", "ZoneA", "ZoneB", "ZoneC")
# xlsx::write.xlsx(sch1965[,vars],
#                  file=paste0(export.path,"LakeO_Schedules.xlsx"),
#                  sheet="1965",col.names=T,row.names = F,append=F)

# 1972 - INTERIM REGULATION SCHEDULE
ZoneA.1972=data.frame(Date=date.fun(c("1900-01-01","1900-04-01","1900-06-15","1900-08-01","1900-10-15","1900-12-31")),
                      ZoneA=c(15.5,15.5,14.0,14.0,15.5,15.5))
ZoneA.1972=merge(ZoneA.1972,dates.val,"Date",all.y=T)
ZoneB.1972=data.frame(Date=date.fun(c("1900-01-01","1900-02-15","1900-05-07","1900-08-22","1900-10-15","1900-12-31")),
                      ZoneB=c(15.5,15.5,14.0,14.0,15.5,15.5))
ZoneB.1972=merge(ZoneB.1972,dates.val,"Date",all.y=T)
sch1972=merge(ZoneA.1972,ZoneB.1972,"Date")

sch1972$ZoneA=na.approx(sch1972$ZoneA)
sch1972$ZoneB=na.approx(sch1972$ZoneB)

plot(ZoneA~Date,sch1972,ylim=c(12.0,16.0),type="n")
with(sch1972,lines(ZoneA~Date))
with(sch1972,lines(ZoneB~Date))

sch1972$month=as.numeric(format(sch1972$Date,"%m"))
sch1972$day=as.numeric(format(sch1972$Date,"%d"))
# vars=c("day", "month", "ZoneA", "ZoneB")
# xlsx::write.xlsx(sch1972[,vars],
#                  file=paste0(export.path,"LakeO_Schedules.xlsx"),
#                  sheet="1972",col.names=T,row.names = F,append=T)


# 1978 - 78 SCHEDULE
ZoneA.1978=data.frame(Date=date.fun(c("1900-01-01","1900-06-01","1900-09-15","1900-10-01","1900-12-31")),
                      ZoneA=c(18.5,16.5,17.5,18.5,18.5))
ZoneA.1978=merge(ZoneA.1978,dates.val,"Date",all.y=T)
ZoneB.1978=data.frame(Date=date.fun(c("1900-01-01","1900-06-01","1900-09-15","1900-10-01","1900-12-31")),
                      ZoneB=c(17.5,15.5,16.5,17.5,17.5))
ZoneB.1978=merge(ZoneB.1978,dates.val,"Date",all.y=T)
sch1978=merge(ZoneA.1978,ZoneB.1978,"Date")
sch1978$ZoneA=na.approx(sch1978$ZoneA)
sch1978$ZoneB=na.approx(sch1978$ZoneB)


plot(ZoneA~Date,ZoneA.1978,ylim=c(12.0,20.0),type="l")
with(sch1978,lines(ZoneA~Date))
with(sch1978,lines(ZoneB~Date))

sch1978$month=as.numeric(format(sch1978$Date,"%m"))
sch1978$day=as.numeric(format(sch1978$Date,"%d"))
# vars=c("day", "month", "ZoneA", "ZoneB")
# xlsx::write.xlsx(sch1978[,vars],
#                  file=paste0(export.path,"LakeO_Schedules.xlsx"),
#                  sheet="1978",col.names=T,row.names = F,append=T)

# Run25 1994
ZoneA.run25=data.frame(Date=date.fun(c("1900-01-01","1900-03-01","1900-06-01","1900-09-15","1900-10-01","1900-12-31")),
                      ZoneA=c(18.5,18.5,17.0,17.5,18.5,18.5))
ZoneB.run25=data.frame(Date=date.fun(c("1900-01-01","1900-03-01","1900-06-01","1900-09-15","1900-10-01","1900-12-01","1900-12-31")),
                       ZoneB=c(17.5,17.25,16.25,16.75,17.75,17.75,17.5))
ZoneC.run25=data.frame(Date=date.fun(c("1900-01-01","1900-06-01","1900-09-15","1900-10-01","1900-12-01","1900-12-31")),
                       ZoneC=c(17.0,15.75,16.25,17.20,17.20,17.0))
ZoneD.L3.run25=data.frame(Date=date.fun(c("1900-01-01","1900-03-15","1900-06-01","1900-09-15","1900-09-28","1900-12-01","1900-12-31")),
                       ZoneD.L3=c(16.75,16.4,15.75,16.25,17.0,17.0,16.75))
ZoneD.L2.run25=data.frame(Date=date.fun(c("1900-01-01","1900-06-01","1900-09-15","1900-09-23","1900-12-01","1900-12-31")),
                          ZoneD.L2=c(16.5,15.75,16.25,16.75,16.75,16.5))
ZoneD.run25=data.frame(Date=date.fun(c("1900-01-01","1900-06-01","1900-09-15","1900-10-01","1900-12-31")),
                          ZoneD=c(16.0,15.5,16.0,16.75,16.0))
ZoneA.run25=merge(ZoneA.run25,dates.val,"Date",all.y=T)
ZoneB.run25=merge(ZoneB.run25,dates.val,"Date",all.y=T)
ZoneC.run25=merge(ZoneC.run25,dates.val,"Date",all.y=T)
ZoneD.L3.run25=merge(ZoneD.L3.run25,dates.val,"Date",all.y=T)
ZoneD.L2.run25=merge(ZoneD.L2.run25,dates.val,"Date",all.y=T)
ZoneD.run25=merge(ZoneD.run25,dates.val,"Date",all.y=T)

sch.run25=merge(merge(merge(merge(merge(ZoneA.run25,ZoneB.run25,"Date"),
                            ZoneC.run25,"Date"),
                      ZoneD.L3.run25,"Date"),
                ZoneD.L2.run25,"Date"),
                ZoneD.run25,"Date")
sch.run25[,2:ncol(sch.run25)]=apply(sch.run25[,2:ncol(sch.run25)],2,FUN=function(x)na.approx(x))

plot(ZoneA~Date,sch.run25,ylim=c(12.0,20.0),type="l")
with(sch.run25,lines(ZoneB~Date))
with(sch.run25,lines(ZoneC~Date))
with(sch.run25,lines(ZoneD.L3~Date,lty=2))
with(sch.run25,lines(ZoneD.L2~Date,lty=2))
with(sch.run25,lines(ZoneD~Date))

sch.run25$month=as.numeric(format(sch.run25$Date,"%m"))
sch.run25$day=as.numeric(format(sch.run25$Date,"%d"))
vars=c("day", "month", "ZoneA", "ZoneB","ZoneC","ZoneD.L3","ZoneD.L2","ZoneD")
# match(names(sch.run25),vars)
# xlsx::write.xlsx(sch.run25[,vars],
#                   file=paste0(export.path,"LakeO_Schedules.xlsx"),
#                   sheet="run25",col.names=T,row.names = F,append=T)


# WSE
ZoneA.WSE=data.frame(Date=date.fun(c("1900-01-01","1900-04-01","1900-05-30","1900-09-15","1900-10-07","1900-12-31")),
                      ZoneA=c(18.5,18.5,17,17.5,18.5,18.5))
ZoneA.WSE=merge(ZoneA.WSE,dates.val,"Date",all.y=T)

ZoneB.WSE=data.frame(Date=date.fun(c("1900-01-01","1900-05-30","1900-09-15","1900-10-07","1900-12-31")),
                     ZoneB=c(17.5,16.5,17.0,17.5,17.5))
ZoneB.WSE=merge(ZoneB.WSE,dates.val,"Date",all.y=T)

ZoneC.WSE=data.frame(Date=date.fun(c("1900-01-01","1900-05-30","1900-09-15","1900-10-07","1900-12-31")),
                     ZoneC=c(17.0,15.5,16.25,17.0,17.0))
ZoneC.WSE=merge(ZoneC.WSE,dates.val,"Date",all.y=T)

ZoneD.WSE=data.frame(Date=date.fun(c("1900-01-01","1900-05-30","1900-09-15","1900-10-07","1900-12-31")),
                     ZoneD=c(14.75,13.5,15.0,15.5,14.5))
ZoneD.WSE=merge(ZoneD.WSE,dates.val,"Date",all.y=T)

sch.wse=merge(merge(merge(ZoneA.WSE,ZoneB.WSE,"Date"),ZoneC.WSE,"Date"),ZoneD.WSE,"Date")
sch.wse$ZoneA=na.approx(sch.wse$ZoneA)
sch.wse$ZoneB=na.approx(sch.wse$ZoneB)
sch.wse$ZoneC=na.approx(sch.wse$ZoneC)
sch.wse$ZoneD=na.approx(sch.wse$ZoneD)

plot(ZoneA~Date,sch.wse,ylim=c(12.0,20.0),type="l")
with(sch.wse,lines(ZoneA~Date))
with(sch.wse,lines(ZoneB~Date))
with(sch.wse,lines(ZoneC~Date))
with(sch.wse,lines(ZoneD~Date))

sch.wse$month=as.numeric(format(sch.wse$Date,"%m"))
sch.wse$day=as.numeric(format(sch.wse$Date,"%d"))
vars=c("day", "month", "ZoneA", "ZoneB","ZoneC","ZoneD")
# match(names(sch.wse),vars)
# xlsx::write.xlsx(sch.wse[,vars],
#                   file=paste0(export.path,"LakeO_Schedules.xlsx"),
#                   sheet="WSE",col.names=T,row.names = F,append=T)


# LORS08
sch.LORS=read.csv("C:/Julian_LaCie/_Github/CRE_Conditions/Data/LORS.csv")
sch.LORS$Date=with(sch.LORS,date.fun(paste("1900",Month,Day,sep="-")))

plot(High~Date,sch.LORS,ylim=c(8.5,20.0),type="n")
with(sch.LORS,lines(High~Date))
with(sch.LORS,lines(Intermediate~Date))
with(sch.LORS,lines(Low~Date))
with(sch.LORS,lines(LowMid~Date,lty=2,col="grey"))
with(sch.LORS,lines(Inter1ft~Date,lty=2))
with(sch.LORS,lines(LowLow~Date,lty=2,col="grey"))
with(sch.LORS,lines(BaseFlow~Date))
with(sch.LORS,lines(BeneficialUse~Date))
with(sch.LORS,lines(WSM~Date,col="grey"))

sch.LORS$month=as.numeric(format(sch.LORS$Date,"%m"))
sch.LORS$day=as.numeric(format(sch.LORS$Date,"%d"))
vars=c("day", "month", "High", "Intermediate", "Inter1ft", "Low", 
       "BaseFlow", "LowLow", "LowMid", "BeneficialUse", "WSM")
# match(names(sch.LORS),vars)
# xlsx::write.xlsx(sch.LORS[,vars],
#                  file=paste0(export.path,"LakeO_Schedules.xlsx"),
#                  sheet="LORS08",col.names=T,row.names = F,append=T)
# 

ylim.val=c(10,19);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1900-01-01","1900-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
txt.cex=0.75
title.cex=0.75
# png(filename=paste0(plot.path,"z_RegSch/RegSch_1965_2008.png"),width=12,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1.5,0.5,0.5),oma=c(1.5,3,2,1));
layout(matrix(c(1:6),1,6,byrow=F))
# layout(matrix(c(1:10),2,5,byrow=F),heights=c(1,0.15))

plot(ZoneA~Date,sch1965,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch1965,lines(ZoneA~Date,lwd=1.25))
with(sch1965,lines(ZoneB~Date,lwd=1.25))
with(sch1965,lines(ZoneC~Date,lwd=1.25))
text(date.fun("1900-07-01"),14.5,"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-06-01"),14.0,"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),13.5,"Zone C",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),12.0,"Zone D",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=2,line=2.25,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"Alternative\nRegulation Schedule (1965)",cex=title.cex)
# plot(0:1,0:1,ann=F,axes=F,type="n")
# text(0.5,0,adj=0.5,cex=0.75,
#     "Central and Southern Florida\nALTERNATIVE REGULATION SCHEDULE -- LAKE OKEECHOBEE\n1965",xpd=NA)
plot(ZoneA~Date,sch1972,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch1972,lines(ZoneA~Date,lwd=1.25))
with(sch1972,lines(ZoneB~Date,lwd=1.25))
text(date.fun("1900-07-01"),14.5,"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-04-15"),14.5,"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-09-15"),14.5,"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),13.5,"Zone C",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=3,adj=0,"Interim\nRegulation Schedule (1972)",cex=title.cex)
# plot(0:1,0:1,ann=F,axes=F,type="n")
# text(0.5,0,adj=0.5,cex=0.75,
#    "Central and Southern Florida\nINTERIM REGULATION SCHEDULE -- LAKE OKEECHOBEE\n1972",xpd=NA)

plot(ZoneA~Date,sch1978,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch1978,lines(ZoneA~Date,lwd=1.25))
with(sch1978,lines(ZoneB~Date,lwd=1.25))
text(date.fun("1900-07-01"),18.0,"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),16.25,"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),15.5,"Zone C",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=3,adj=0,"Interim\nRegulation Schedule (1978)",cex=title.cex)
# plot(0:1,0:1,ann=F,axes=F,type="n")
# text(0.5,0,adj=0.5,cex=0.75,
#    "Central and Southern Florida\nINTERIM REGULATION SCHEDULE -- LAKE OKEECHOBEE\n78 SCHEDULE\n1978",xpd=NA)

plot(ZoneA~Date,sch.run25,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch.run25,lines(ZoneA~Date,lwd=1.25))
with(sch.run25,lines(ZoneB~Date,lwd=1.25))
with(sch.run25,lines(ZoneC~Date,lwd=1.25))
with(sch.run25,lines(ZoneD.L3~Date,lty=2))
with(sch.run25,lines(ZoneD.L2~Date,lty=2))
with(sch.run25,lines(ZoneD~Date,lwd=1.25))
text(date.fun("1900-06-15"),17.5,"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),16.75,"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),16.1,"Zone C",font=4,cex=txt.cex)
# text(date.fun("1900-01-15"),16.75,"Level 3",font=4,cex=txt.cex-txt.cex*0.2,col="grey30")
# text(date.fun("1900-11-15"),17.1,"Level 3",font=4,cex=txt.cex-txt.cex*0.2,col="grey30")
# text(date.fun("1900-01-15"),16.5,"Level 2",font=4,cex=txt.cex-txt.cex*0.2,col="grey30")
# text(date.fun("1900-11-15"),16.9,"Level 2",font=4,cex=txt.cex-txt.cex*0.2,col="grey30")
# text(date.fun("1900-01-15"),16.15,"Level 1",font=4,cex=txt.cex-txt.cex*0.2,col="grey30")
# text(date.fun("1900-11-15"),16.5,"Level 1",font=4,cex=txt.cex-txt.cex*0.2,col="grey30")
text(date.fun("1900-07-15"),15.8,"Zone D",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),15.25,"Zone E",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=3,adj=0,"Run 25 (1994)",cex=title.cex)

plot(ZoneA~Date,sch.wse,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch.wse,lines(ZoneA~Date,lwd=1.25))
with(sch.wse,lines(ZoneB~Date,lwd=1.25))
with(sch.wse,lines(ZoneC~Date,lwd=1.25))
with(sch.wse,lines(ZoneD~Date,lwd=1.25))
text(date.fun("1900-03-15"),18.75,"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),17.5,"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),16.75,"Zone C",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),15,"Zone D",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),13,"Zone E",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=3,adj=0,"WSE (1999)",cex=title.cex)
# mtext(side=3,adj=0,"Water Supply/Environmental (1999)",cex=title.cex)
# plot(0:1,0:1,ann=F,axes=F,type="n")
# text(0.5,0,adj=0.5,cex=0.75,
#     "Central and Southern Florida\nINTERIM REGULATION SCHEDULE -- LAKE OKEECHOBEE\nWater Supply/Environmental\n1999",xpd=NA)

plot(High~Date,sch.LORS,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch.LORS,lines(High~Date,lwd=1.25))
with(sch.LORS,lines(Intermediate~Date,lwd=1.25))
with(sch.LORS,lines(Low~Date,lwd=1.25))
with(sch.LORS,lines(LowMid~Date,lty=2,col="grey40"))
with(sch.LORS,lines(Inter1ft~Date,lty=2))
with(sch.LORS,lines(LowLow~Date,lty=2,col="grey40"))
with(sch.LORS,lines(BaseFlow~Date,lwd=1.25))
with(sch.LORS,lines(BeneficialUse~Date,lwd=1.25))
with(sch.LORS,lines(WSM~Date,col="grey",lwd=1.25))
text(date.fun("1900-07-15"),17.5,"High Lake Management Band",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),17,"High",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),16.25,"Intermediate",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),14.5,"Low",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),13,"Base Flow",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),12,"Beneficial Use",font=4,cex=txt.cex)
text(date.fun("1900-08-01"),10.25,"Water Shortage Management Band",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2,"Month")
mtext(side=3,adj=0,"LORS08 (2008)",cex=title.cex)
# plot(0:1,0:1,ann=F,axes=F,type="n")
# text(0.5,0,adj=0.5,cex=0.75,
#     "Central and Southern Florida\nINTERIM REGULATION SCHEDULE -- LAKE OKEECHOBEE\nLORS08\n2008",xpd=NA)
dev.off()


# Observed data -----------------------------------------------------------
dates=date.fun(c("1960-01-01","2020-12-31"))

stg=DBHYDRO_daily(dates[1],dates[2],"00268")
stg$Date=date.fun(stg$Date)
stg$CY=as.numeric(format(stg$Date,"%Y"))
stg$decade=(stg$CY%/%10)*10
stg$GT17=with(stg,ifelse(Data.Value>=17,1,0))
stg$GT16=with(stg,ifelse(Data.Value>16,1,0))

sch.order=c("Sch7","Sch65", "Sch72", "Sch78", "Run25", "WSE","LORS08" )
sch.yrs=rbind(
data.frame(Date=seq(dates[1],date.fun("1965-05-01"),"1 days"),Sch=sch.order[1]),
data.frame(Date=seq(date.fun("1965-05-01"),date.fun("1972-01-01"),"1 days"),Sch=sch.order[2]),
data.frame(Date=seq(date.fun("1972-01-02"),date.fun("1978-05-10"),"1 days"),Sch=sch.order[3]),
data.frame(Date=seq(date.fun("1978-05-11"),date.fun("1994-12-27"),"1 days"),Sch=sch.order[4]),
data.frame(Date=seq(date.fun("1994-12-28"),date.fun("1999-11-05"),"1 days"),Sch=sch.order[5]),
data.frame(Date=seq(date.fun("1999-11-06"),date.fun("2008-03-01"),"1 days"),Sch=sch.order[6]),
data.frame(Date=seq(date.fun("2008-03-02"),dates[2],"1 days"),Sch=sch.order[7]))
# sch.yrs$Sch=as.factor(sch.yrs$Sch)


stg=merge(stg,sch.yrs,"Date",all.x=T)
stg$Sch=factor(stg$Sch,levels=sch.order)

sum(is.na(stg$Data.Value)==T)

ylim.val=c(8.5,18.6);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1960-01-01","2020-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
# png(filename=paste0(plot.path,"z_RegSch/Observed_LakeO.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1.5,0.5,0.5),oma=c(1.25,2,1,1));

plot(Data.Value~Date,stg,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
# abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(stg,lines(Data.Value~Date,col="indianred1",lwd=1.25))
# with(stg,shaded.range(Date,rep(0,length(Date)),Data.Value,"dodgerblue1",lty=1))
abline(v=date.fun("1965-05-01"))
abline(v=date.fun("1972-01-01"))
abline(v=date.fun("1978-05-10"))
abline(v=date.fun("1994-12-27"))
abline(v=date.fun("1999-11-05"))
abline(v=date.fun("2008-03-01"))
tmp.dates=date.fun(c("1965-05-01","1972-01-01","1978-05-10","1994-12-27","1999-11-05","2008-03-01","2020-12-31"))
tmp.y=ylim.val[2]+0.3# 18.5
tmp.x=tmp.dates[1]+diff(tmp.dates[1:2])/2
text(tmp.x,tmp.y,"Sch 65",xpd=NA)
tmp.x=tmp.dates[2]+diff(tmp.dates[2:3])/2
text(tmp.x,tmp.y,"Sch 72",xpd=NA)
tmp.x=tmp.dates[3]+diff(tmp.dates[3:4])/2
text(tmp.x,tmp.y,"Sch 78",xpd=NA)
tmp.x=tmp.dates[4]+diff(tmp.dates[4:5])/2
text(tmp.x,tmp.y,"Run 25",xpd=NA)
tmp.x=tmp.dates[5]+diff(tmp.dates[5:6])/2
text(tmp.x,tmp.y,"WSE",xpd=NA)
tmp.x=tmp.dates[6]+diff(tmp.dates[6:7])/2
text(tmp.x,tmp.y,"LORS08",xpd=NA)

axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2,"Year")
mtext(side=2,line=2.5,"Stage Elevation (Ft, NGVD29)")
dev.off()

# mapply(function(x, y, col) lines(x, y, col = col), 
#        x = rbind.data.frame(stg$Date, dplyr::lag(stg$Date)), 
#        y = rbind.data.frame(stg$Data.Value, dplyr::lag(stg$Data.Value)), 
#        col = viridis::plasma(length(stg$Data.Value)))# colorRamps::matlab.like2(length(stg$Data.Value)))

ddply(stg,c("decade"),summarise,N.GT17=sum(GT17,na.rm=T),N.GT16=sum(GT16,na.rm=T))
ddply(stg,c("Sch"),summarise,N.GT17=sum(GT17,na.rm=T),N.GT16=sum(GT16,na.rm=T))
CY.sum=ddply(stg,c("CY"),summarise,N.GT17=sum(GT17,na.rm=T),N.GT16=sum(GT16,na.rm=T))

plot(N.GT17~CY,CY.sum,ylim=c(0,365))
with(CY.sum,lines(N.GT17~CY,col="red"))
with(CY.sum,lines(N.GT16~CY,col="goldenrod3"))


# High Stage Freq Analysis ------------------------------------------------
## Based on https://serc.carleton.edu/hydromodules/steps/168362.html

## All Data
ann.peak=ddply(stg,"CY",summarise,max.stg=max(Data.Value,na.rm=T))

n.val = length(ann.peak$max.stg)
r.val = n.val + 1 - rank(ann.peak$max.stg)  # highest Q has rank r = 1
T.val = (n.val + 1)/r.val

# Set up x axis tick positions and labels
Ttick = c(1.001,1.01,1.1,1.5,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,60,70,80,90,100)
xtlab = c(1.001,1.01,1.1,1.5,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,NA,15,NA,NA,NA,NA,20,NA,30,NA,NA,NA,50,NA,NA,NA,NA,100)
y = -log(-log(1 - 1/T.val))
ytick = -log(-log(1 - 1/Ttick))
xmin = min(min(y),min(ytick))
xmax = max(ytick)

KTtick = -(sqrt(6)/pi)*(0.5772 + log(log(Ttick/(Ttick-1))))
stgTtick = mean(ann.peak$max.stg) + KTtick*sd(ann.peak$max.stg) 
nQ = length(ann.peak$max.stg)
se = (sd(ann.peak$max.stg)*sqrt((1+1.14*KTtick + 1.1*KTtick^2)))/sqrt(nQ) 
LB = stgTtick - qt(0.975, nQ - 1)*se
UB = stgTtick + qt(0.975, nQ - 1)*se
max = max(UB)
Stgmax = max(stgTtick)

# Plot peak flow series with Gumbel axis
plot(y, ann.peak$max.stg,
     ylab = expression( "Stage (Ft, NGVD29)" ) ,
     xaxt = "n", xlab = "Return Period, T (year)",
     ylim = c(10, Stgmax),
     xlim = c(xmin, xmax),
     pch = 21, bg = "red"
)  
axis(1, at = ytick, labels = as.character(xtlab))

# Add fitted line and confidence limits
lines(ytick, stgTtick, col = "black", lty=1, lwd=2)  
lines(ytick, LB, col = "blue", lty = 1, lwd=1.5)
lines(ytick, UB, col = "red", lty = 1, lwd=1.5)  

# Draw grid lines
abline(v = ytick, lty = 3, col="light gray")             
abline(h = seq(10, floor(Stgmax), 2), lty = 3,col="light gray") 
par(cex = 1)


## https://rpubs.com/cassiorampinelli/528388
library(lmom)

#Sorting maxima by decreasing order
sorted.maximas<-sort(ann.peak$max.stg,decreasing=T)

#Computing the empirical probabilities
p<-(c(1:length(sorted.maximas)))/(length(sorted.maximas)+1)

#Computing the recurrence time
tr<-1/p

#Estimating the parameters for Gumbel distribution
fit<-samlmu(ann.peak$max.stg)
para<-pelgum(fit)
para

#Estimating the parameters for Log Pearson type 3 distribution
para3<-pelpe3(fit)
para3

plot(1-p,sorted.maximas,ylab="Stage (Ft)",xlab="Cumulative probability",main="")

#Log pearson type 3 fitting
lines(cdfpe3(sorted.maximas,para3),sorted.maximas,col="red")

#Gumbel fitting
lines(cdfgum(sorted.maximas,para),sorted.maximas,col="blue",lty=2)
grid()
#Legend
legend("topleft", legend=c("LP3", "Gumbel"),
       col=c("red", "blue"), lty=1:2, cex=1)

#Plotting empirical recurrence time and discharges
plot(tr,sorted.maximas,xlab="Recurrence Time (years)",ylab="Stage (Ft)",ylim=c(10,20),xlim=c(0,100))
grid()

#Fitting recurrence time employing Gumbel distribution
y<-c(sorted.maximas)
gumbel.accum<-cdfgum(y,para)
fitted.tr<-1/(1-gumbel.accum)
lines(fitted.tr,y,col="blue",lty=2)

#Fitting recurrence time emplyoing Log Pearson 3 distribution
lp3.accum<-cdfpe3(y,para3)
fitted.tr3<-1/(1-lp3.accum)
lines(fitted.tr3,y,col="red")
legend("topleft", legend=c("LP3", "Gumbel"),
       col=c("red", "blue"), lty=1:2, cex=1)



#Sorting maxima by decreasing order
sorted.maximas.pre95<-sort(subset(ann.peak,CY<1995)$max.stg,decreasing=T)

#Computing the empirical probabilities
p<-(c(1:length(sorted.maximas.pre95)))/(length(sorted.maximas.pre95)+1)

#Computing the recurrence time
tr<-1/p

#Estimating the parameters for Gumbel distribution
fit<-samlmu(subset(ann.peak,CY<1995)$max.stg)
para<-pelgum(fit)
para

#Sorting maxima by decreasing order
sorted.maximas.post95<-sort(subset(ann.peak,CY>=1995)$max.stg,decreasing=T)

#Computing the empirical probabilities
p2<-(c(1:length(sorted.maximas.post95)))/(length(sorted.maximas.post95)+1)

#Computing the recurrence time
tr2<-1/p2

#Estimating the parameters for Gumbel distribution
fit2<-samlmu(subset(ann.peak,CY>=1995)$max.stg)
para2<-pelgum(fit2)
para2
#Plotting empirical recurrence time and discharges
plot(tr,sorted.maximas.pre95,xlab="Recurrence Time (years)",ylab="Stage (Ft)",ylim=c(11,19),xlim=c(0,40))
points(tr2,sorted.maximas.post95,pch=19)
grid()
y<-c(sorted.maximas.pre95)
gumbel.accum<-cdfgum(y,para)
fitted.tr<-1/(1-gumbel.accum)
lines(fitted.tr,y,col="blue",lty=2)

y<-c(sorted.maximas.post95)
gumbel.accum<-cdfgum(y,para2)
fitted.tr<-1/(1-gumbel.accum)
lines(fitted.tr,y,col="red",lty=2)


#Sorting maxima by decreasing order
ann.min=ddply(stg,"CY",summarise,min.stg=min(Data.Value,na.rm=T))
sorted.maximas<-sort(ann.min$min.stg,decreasing=T)

#Computing the empirical probabilities
p<-(c(1:length(sorted.maximas)))/(length(sorted.maximas)+1)

#Computing the recurrence time
tr<-1/p

#Estimating the parameters for Gumbel distribution
fit<-samlmu(ann.min$min.stg)
para<-pelgum(fit)
para

#Plotting empirical recurrence time and discharges
plot(tr,sorted.maximas,xlab="Recurrence Time (years)",ylab="Stage (Ft)",ylim=c(8,16),xlim=c(0,40))
grid()
y<-c(sorted.maximas)
gumbel.accum<-cdfgum(y,para)
fitted.tr<-1/(1-gumbel.accum)
lines(fitted.tr,y,col="blue",lty=2)

# Consecutive  -------------------------------------------------------------
highstg_consec=data.frame()

  tmp=stg
  tmp$stg17=0
  tmp$stg16=0
  for(i in 2:nrow(tmp)){
    # tmp$consec[i]=with(tmp,ifelse(Q_GT2100[i-1]>0&Q_GT2100[i]>0,1,0))
    tmp$stg16[i]=with(tmp,ifelse(GT16[i-1]==0&GT16[i]>0,1,
                                 ifelse(GT16[i-1]>0&GT16[i]>0,1,0)))
    tmp$stg17[i]=with(tmp,ifelse(GT17[i-1]==0&GT17[i]>0,1,
                                 ifelse(GT17[i-1]>0&GT17[i]>0,1,0)))
    
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
  
  highstg_consec=tmp

  plot(Data.Value~Date,stg,type="l")
  with(highstg_consec,points(Date,ifelse(stg16==1,16,NA),pch=21,bg="red"))
  with(highstg_consec,points(Date,ifelse(stg17==1,17,NA),pch=21,bg="blue"))
    
ddply(highstg_consec,c("sum.stg16"),summarise,N.val=N.obs(sum.stg16))

rslt.stg16=reshape2::dcast(highstg_consec,sum.stg16~decade,value.var = "sum.stg16",fun.aggregate = function(x)N.obs(x))
rslt.stg16=ddply(highstg_consec,c("decade","sum.stg16"),summarise,count.event=N.obs(sum.stg16))
max(rslt.stg16$sum.stg16)
rslt.stg16$cat=with(rslt.stg16,ifelse(sum.stg16>0&sum.stg16<5,1,
                                      ifelse(sum.stg16>=5&sum.stg16<15,2,
                                      ifelse(sum.stg16>=15&sum.stg16<30,3,
                                             ifelse(sum.stg16>=30&sum.stg16<60,4,
                                                    ifelse(sum.stg16>=60&sum.stg16<90,5,
                                                           ifelse(sum.stg16>=90&sum.stg16<180,6,
                                                                  ifelse(sum.stg16>=180,7,NA))))))))
rslt.stg17=ddply(highstg_consec,c("decade","sum.stg17"),summarise,count.event=N.obs(sum.stg17))
max(rslt.stg17$sum.stg17)
rslt.stg17$cat=with(rslt.stg17,ifelse(sum.stg17>0&sum.stg17<5,1,
                                      ifelse(sum.stg17>=5&sum.stg17<15,2,
                                      ifelse(sum.stg17>=15&sum.stg17<30,3,
                                             ifelse(sum.stg17>=30&sum.stg17<60,4,
                                                    ifelse(sum.stg17>=60&sum.stg17<90,5,
                                                           ifelse(sum.stg17>=90&sum.stg17<180,6,
                                                                  ifelse(sum.stg17>=180,7,NA))))))))


# ddply(subset(rslt.stg16,is.na(cat)==F),c("Alt","cat"),summarise,sum.event=sum(count.event,na.rm=T))
rslt.stg16.sum=reshape2::dcast(subset(rslt.stg16,is.na(cat)==F),cat~decade,value.var="count.event",sum,na.rm=T)

rslt.stg17.sum=reshape2::dcast(subset(rslt.stg17,is.na(cat)==F),cat~decade,value.var="count.event",sum,na.rm=T)
tmp=rslt.stg17.sum[5,]
tmp$cat=7
tmp[,2:ncol(tmp)]=0
rslt.stg17.sum=rbind(rslt.stg17.sum,tmp)


dec.val=as.character(seq(1960,2010,10))
cols=wesanderson::wes_palette("Zissou1",length(dec.val),"continuous")
# png(filename=paste0(plot.path,"z_RegSch/LOK_highstg_events_decade.png"),width=6.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,2,0.25),lwd=0.5);
layout(matrix(c(1:12),6,2,byrow=F))

ylim.val=c(0,6);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("< 5","5 - 15", "15 - 30","30 - 60","60 - 90","90 - 180", "> 180")
for(i in 1:length(dec.val)){
  x=barplot(rslt.stg16.sum[,dec.val[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.stg16.sum)),space=c(0,0),yaxs="i",xaxs="i")
  text(x,rslt.stg16.sum[,dec.val[i]],rslt.stg16.sum[,dec.val[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(dec.val)){axis_fun(1,x,x,xlabs,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  mtext(side=3,line=-1.25,adj=0,paste0(" ",dec.val[i],"s"),font=2,cex=0.75)
  if(i==1){mtext(side=3,adj=0,"Daily Stage > 16 Ft NGVD")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")
mtext(side=2,line=0.5,outer=T,"Number of Events")

for(i in 1:length(dec.val)){
  x=barplot(rslt.stg17.sum[,dec.val[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.stg17.sum)),space=c(0),yaxs="i",xaxs="i")
  text(x,rslt.stg17.sum[,dec.val[i]],rslt.stg17.sum[,dec.val[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(dec.val)){axis_fun(1,x,x,xlabs,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  #mtext(side=3,line=-1.25,adj=0,paste0(" Alt: ",alts.sort[i]),font=2)
  if(i==1){mtext(side=3,adj=0,"Daily Stage \u2265 17 Ft NGVD")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")

dev.off()


# by schedule -------------------------------------------------------------
rslt.stg16=ddply(highstg_consec,c("Sch","sum.stg16"),summarise,count.event=N.obs(sum.stg16))
max(rslt.stg16$sum.stg16)
rslt.stg16$cat=with(rslt.stg16,ifelse(sum.stg16>0&sum.stg16<5,1,
                                      ifelse(sum.stg16>=5&sum.stg16<15,2,
                                             ifelse(sum.stg16>=15&sum.stg16<30,3,
                                                    ifelse(sum.stg16>=30&sum.stg16<60,4,
                                                           ifelse(sum.stg16>=60&sum.stg16<90,5,
                                                                  ifelse(sum.stg16>=90&sum.stg16<180,6,
                                                                         ifelse(sum.stg16>=180,7,NA))))))))
rslt.stg17=ddply(highstg_consec,c("Sch","sum.stg17"),summarise,count.event=N.obs(sum.stg17))
max(rslt.stg17$sum.stg17)
rslt.stg17$cat=with(rslt.stg17,ifelse(sum.stg17>0&sum.stg17<5,1,
                                      ifelse(sum.stg17>=5&sum.stg17<15,2,
                                             ifelse(sum.stg17>=15&sum.stg17<30,3,
                                                    ifelse(sum.stg17>=30&sum.stg17<60,4,
                                                           ifelse(sum.stg17>=60&sum.stg17<90,5,
                                                                  ifelse(sum.stg17>=90&sum.stg17<180,6,
                                                                         ifelse(sum.stg17>=180,7,NA))))))))

rslt.stg16.sum=reshape2::dcast(subset(rslt.stg16,is.na(cat)==F),cat~Sch,value.var="count.event",sum,na.rm=T)
rslt.stg16.sum$Sch72=0
rslt.stg16.sum=rslt.stg16.sum[,c("cat",sch.order)]
rslt.stg17.sum=reshape2::dcast(subset(rslt.stg17,is.na(cat)==F),cat~Sch,value.var="count.event",sum,na.rm=T)
rslt.stg17.sum$Sch72=0
rslt.stg17.sum$Sch65=0
rslt.stg17.sum=rslt.stg17.sum[,c("cat",sch.order)]

tmp=rslt.stg17.sum[5,]
tmp$cat=7
tmp[,2:ncol(tmp)]=0
rslt.stg17.sum=rbind(rslt.stg17.sum,tmp)


sch.lab=c("Sch 7","Sch 65", "Sch 72", "Sch 78", "Run 25", "WSE","LORS08" )
cols=wesanderson::wes_palette("Zissou1",length(sch.order),"continuous")
# png(filename=paste0(plot.path,"z_RegSch/LOK_highstg_events_sch.png"),width=6.5,height=7,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,2,0.25),lwd=0.5);
layout(matrix(c(1:14),7,2,byrow=F))

ylim.val=c(0,6);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlabs=c("< 5","5 - 15", "15 - 30","30 - 60","60 - 90","90 - 180", "> 180")
for(i in 1:length(sch.order)){
  x=barplot(rslt.stg16.sum[,sch.order[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.stg16.sum)),space=c(0,0),yaxs="i",xaxs="i")
  text(x,rslt.stg16.sum[,sch.order[i]],rslt.stg16.sum[,sch.order[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(sch.order)){axis_fun(1,x,x,xlabs,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  mtext(side=3,line=-1.25,adj=0,paste0(" ",sch.lab[i]),font=2,cex=0.75)
  if(i==1){mtext(side=3,adj=0,"Daily Stage > 16 Ft NGVD")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")
mtext(side=2,line=0.5,outer=T,"Number of Events")

for(i in 1:length(sch.order)){
  x=barplot(rslt.stg17.sum[,sch.order[i]],beside=T,ylim=ylim.val,
            col=adjustcolor(cols[i],0.5),ann=F,axes=F,
            names.arg = rep(NA,nrow(rslt.stg17.sum)),space=c(0),yaxs="i",xaxs="i")
  text(x,rslt.stg17.sum[,sch.order[i]],rslt.stg17.sum[,sch.order[i]],pos=3)
  axis_fun(2,ymaj,ymin,ymaj)
  if(i==length(sch.order)){axis_fun(1,x,x,xlabs,line=-0.5,cex=0.8)}else{axis_fun(1,x,x,NA)}
  box(lwd=1)
  #mtext(side=3,line=-1.25,adj=0,paste0(" Alt: ",alts.sort[i]),font=2)
  if(i==1){mtext(side=3,adj=0,"Daily Stage \u2265 17 Ft NGVD")}
}
mtext(side=1,line=2.5,"Event Duration (Days)")

dev.off()