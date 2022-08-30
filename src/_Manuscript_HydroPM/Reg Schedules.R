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


## LOSOM
library(dssrip)

zones=c(paste("LOK",paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),sep="-"),
        paste("LOK",paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_"),sep="_"))
zone.alias=data.frame(zone=zones,
                      zone2=c(paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_")))

zones=c(paste("LOK",paste("ZONE",c("A","BC","D1","D2"),sep="_"),sep="-"),
        paste("LOK",paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_"),sep="_"))

zone.alias=rbind(zone.alias,
                 data.frame(
                   zone=c("LOK-ZONE_BC","LOWSM_D3_15_LEVEL"),
                   zone2=c("ZONE_BC","LOWSM_D3_15_LEVEL")))
alts=c("ECB19","NA25f","PA25")
reg.sch=data.frame()

  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts[3],"/RSMBN_output.dss"))  
  
for(i in 1:length(zones)){
    paths=paste0("/RSMBN/",zones[i],"/STAGE/01JAN1965 - 01JAN2016/1DAY/INPUT/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$zone=zones[i]
    tmp$Alt=alts[3]
    reg.sch=rbind(tmp,reg.sch)
    print(i)
}

reg.sch=subset(reg.sch,format(Date,"%Y")=="1966")
reg.sch$STAGE.m=ft.to.m(reg.sch$STAGE)
reg.sch$DOY=as.numeric(format(reg.sch$Date,"%j"))
# reg.sch$month=as.numeric(format(reg.sch$Date,"%m"))
# reg.sch$day=as.numeric(format(reg.sch$Date,"%d"))
reg.sch=merge(reg.sch,zone.alias,"zone")
# reg.sch2=reshape2::dcast(reg.sch,Alt+DOY~zone2,value.var = "STAGE.m",mean)
reg.sch2=reshape2::dcast(reg.sch,Alt+Date~zone2,value.var = "STAGE.m",mean)
reg.sch2$DOY=as.numeric(format(reg.sch2$Date,"%j"))
  
reg.sch2$day=as.numeric(format(reg.sch2$Date,'%d'))# as.numeric(format(as.Date(reg.sch2$DOY,origin="1900-01-01"),"%d"))
reg.sch2$month=as.numeric(format(reg.sch2$Date,'%m'))# as.numeric(format(as.Date(reg.sch2$DOY,origin="1900-01-01"),"%m"))
reg.sch2$Date=with(reg.sch2,date.fun(paste("1900",month,day,sep="-")))
tail(reg.sch2)

###

sch1972$ZoneA=ft.to.m(sch1972$ZoneA)
sch1972$ZoneB=ft.to.m(sch1972$ZoneB)

sch1978$ZoneA=ft.to.m(sch1978$ZoneA)
sch1978$ZoneB=ft.to.m(sch1978$ZoneB)

sch.run25[,2:7]=ft.to.m(sch.run25[,2:7])
sch.wse[,2:5]=ft.to.m(sch.wse[,2:5])
sch.LORS[,2:11]=ft.to.m(sch.LORS[,2:11])

ylim.val=c(3,5.75);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val2=c(10,19);by.y2=1;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2)
xlim.val=date.fun(c("1900-01-01","1900-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
txt.cex=0.75
title.cex=0.75
# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/RegSch_1972_2008.png"),width=10,height=3.25,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,1.5,0.5,0.5),oma=c(2,3,2,3.5));
layout(matrix(c(1:5),1,5,byrow=F))

plot(ZoneA~Date,sch1972,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch1972,lines(ZoneA~Date,lwd=1.25))
with(sch1972,lines(ZoneB~Date,lwd=1.25))
text(date.fun("1900-07-01"),ft.to.m(16),"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-04-15"),ft.to.m(14.5),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-09-15"),ft.to.m(14.5),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),ft.to.m(13.5),"Zone C",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25,las=2)
# axis_fun(1,xmin,xmin,format(xmin,"%m"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2.5,"Month")
mtext(side=2,line=2.5,"Stage Elevation (m, NGVD29)")
mtext(side=3,adj=0,"Interim\nRegulation Schedule (1972)",cex=title.cex)

plot(ZoneA~Date,sch1978,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch1978,lines(ZoneA~Date,lwd=1.25))
with(sch1978,lines(ZoneB~Date,lwd=1.25))
text(date.fun("1900-07-01"),ft.to.m(18.0),"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),ft.to.m(16.25),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),ft.to.m(15.5),"Zone C",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25,las=2)
# axis_fun(1,xmin,xmin,format(xmin,"%m"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2.5,"Month")
mtext(side=3,adj=0,"Interim\nRegulation Schedule (1978)",cex=title.cex)

plot(ZoneA~Date,sch.run25,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch.run25,lines(ZoneA~Date,lwd=1.25))
with(sch.run25,lines(ZoneB~Date,lwd=1.25))
with(sch.run25,lines(ZoneC~Date,lwd=1.25))
with(sch.run25,lines(ZoneD.L3~Date,lty=2))
with(sch.run25,lines(ZoneD.L2~Date,lty=2))
with(sch.run25,lines(ZoneD~Date,lwd=1.25))
text(date.fun("1900-06-15"),ft.to.m(17.5),"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),ft.to.m(16.75),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),ft.to.m(16.1),"Zone C",font=4,cex=txt.cex)
text(date.fun("1900-07-15"),ft.to.m(15.8),"Zone D",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),ft.to.m(15.25),"Zone E",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25,las=2)
# axis_fun(1,xmin,xmin,format(xmin,"%m"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2.5,"Month")
mtext(side=3,adj=0,"Run 25 (1994)",cex=title.cex)

plot(ZoneA~Date,sch.wse,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch.wse,lines(ZoneA~Date,lwd=1.25))
with(sch.wse,lines(ZoneB~Date,lwd=1.25))
with(sch.wse,lines(ZoneC~Date,lwd=1.25))
with(sch.wse,lines(ZoneD~Date,lwd=1.25))
text(date.fun("1900-03-15"),ft.to.m(18.75),"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(17.5),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(16.75),"Zone C",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(15),"Zone D",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(13),"Zone E",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25,las=2)
# axis_fun(1,xmin,xmin,format(xmin,"%m"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2.5,"Month")
mtext(side=3,adj=0,"WSE (1999)",cex=title.cex)

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
text(date.fun("1900-07-15"),ft.to.m(17.5),"High Lake Management Band",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(17),"High",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(16.25),"Intermediate",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(14.5),"Low",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(13),"Base Flow",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(12),"Beneficial Use",font=4,cex=txt.cex)
text(date.fun("1900-08-01"),ft.to.m(10.25),"Water Shortage Management Band",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25,las=2)
# axis_fun(1,xmin,xmin,format(xmin,"%m"),line=-0.5)
box(lwd=1)
mtext(side=1,line=2.5,"Month")
mtext(side=3,adj=0,"LORS08 (2008)",cex=title.cex)

axis_fun(4,ft.to.m(ymaj2),ft.to.m(ymin2),ymaj2)
mtext(side=4,line=2.5,"Stage Elevation (Ft, NGVD29)")
dev.off()



schs.all=data.frame(
  name=c(
  "Alternate\nRegulation\nSchedule",
  "Interim\nRegulation\nSchedule",
  "Interim\nRegulation\nSchedule",
  "Run25",
  "WSE",
  "LORS08",
  "LOSOM"),
  yr=c(1965,1972,1978,1994,1999,2008,2022),
  plot.val=1)

# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/RegSch_1972_2022.png"),width=5,height=7.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3.5,1,0.5,0.5),oma=c(2,3.5,0.5,3.5));
layout(matrix(c(1,1,2:7),4,2,byrow=T),heights=c(0.65,1,1,1))

xlim.val=c(1960,2030);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(0:1,0:1,type="n",ylim=c(-0.5,2),xlim=xlim.val,axes=F,ann=F)
lines(c(1959,1965),c(1,1),lty=2)
lines(c(2022,2050),c(1,1),lty=2)
lines(c(1965,2022),c(1,1),lty=1)
points(plot.val~yr,schs.all,pch=21,bg="grey",cex=2,lty=0.01)
#text(plot.val~yr,schs.all[c(1,3),],name,pos=3,cex=0.5)
text(plot.val~yr,schs.all[c(1,3,5,7),],name,pos=3,cex=0.75)
text(plot.val~yr,schs.all[c(seq(2,7,2)),],name,pos=1,cex=0.75)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
mtext(side=1,line=1.5,"Year")

par(mar=c(2,1,0.25,0.5))
ylim.val=c(3,5.75);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val2=c(10,19);by.y2=1;ymaj2=seq(ylim.val2[1],ylim.val2[2],by.y2);ymin2=seq(ylim.val2[1],ylim.val2[2],by.y2)
xlim.val=date.fun(c("1900-01-01","1900-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
txt.cex=0.75
title.cex=0.75

plot(ZoneA~Date,sch1972,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch1972,lines(ZoneA~Date,lwd=1.25))
with(sch1972,lines(ZoneB~Date,lwd=1.25))
text(date.fun("1900-07-01"),ft.to.m(16),"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-04-15"),ft.to.m(14.5),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-09-15"),ft.to.m(14.5),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),ft.to.m(13.5),"Zone C",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,xmaj,xmin,NA,line=-0.25)
# axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25)
# axis_fun(1,xmin,xmin,format(xmin,"%m"),line=-0.5)
box(lwd=1)
mtext(side=3,adj=0,"Interim Regulation Schedule (1972)",cex=title.cex)

plot(ZoneA~Date,sch1978,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch1978,lines(ZoneA~Date,lwd=1.25))
with(sch1978,lines(ZoneB~Date,lwd=1.25))
text(date.fun("1900-07-01"),ft.to.m(18.0),"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),ft.to.m(16.25),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-07-01"),ft.to.m(15.5),"Zone C",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,NA,line=-0.25)
# axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25)
box(lwd=1)
mtext(side=3,adj=0,"Interim Regulation Schedule (1978)",cex=title.cex)

axis_fun(4,ft.to.m(ymaj2),ft.to.m(ymin2),ymaj2)

plot(ZoneA~Date,sch.run25,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch.run25,lines(ZoneA~Date,lwd=1.25))
with(sch.run25,lines(ZoneB~Date,lwd=1.25))
with(sch.run25,lines(ZoneC~Date,lwd=1.25))
with(sch.run25,lines(ZoneD.L3~Date,lty=2))
with(sch.run25,lines(ZoneD.L2~Date,lty=2))
with(sch.run25,lines(ZoneD~Date,lwd=1.25))
text(date.fun("1900-06-15"),ft.to.m(17.5),"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),ft.to.m(16.75),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),ft.to.m(16.1),"Zone C",font=4,cex=txt.cex)
text(date.fun("1900-07-15"),ft.to.m(15.8),"Zone D",font=4,cex=txt.cex)
text(date.fun("1900-06-15"),ft.to.m(15.25),"Zone E",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,xmaj,xmin,NA,line=-0.25)
# axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25)
box(lwd=1)
mtext(side=3,adj=0,"Run 25 (1994)",cex=title.cex)
mtext(side=2,line=2.5,"Stage Elevation (m, NGVD29)")

plot(ZoneA~Date,sch.wse,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch.wse,lines(ZoneA~Date,lwd=1.25))
with(sch.wse,lines(ZoneB~Date,lwd=1.25))
with(sch.wse,lines(ZoneC~Date,lwd=1.25))
with(sch.wse,lines(ZoneD~Date,lwd=1.25))
text(date.fun("1900-03-15"),ft.to.m(18.75),"Zone A",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(17.5),"Zone B",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(16.75),"Zone C",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(15),"Zone D",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(13),"Zone E",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,NA)
axis_fun(1,xmaj,xmin,NA,line=-0.25)
# axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25)
box(lwd=1)
mtext(side=3,adj=0,"WSE (1999)",cex=title.cex)

axis_fun(4,ft.to.m(ymaj2),ft.to.m(ymin2),ymaj2)
mtext(side=4,line=2.5,"Stage Elevation (Ft, NGVD29)")

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
text(date.fun("1900-07-15"),ft.to.m(17.5),"High Lake Management Band",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(17),"High",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(16.25),"Intermediate",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(14.5),"Low",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(13),"Base Flow",font=4,cex=txt.cex)
text(date.fun("1900-03-15"),ft.to.m(12),"Beneficial Use",font=4,cex=txt.cex)
text(date.fun("1900-08-01"),ft.to.m(10.25),"Water Shortage Management Band",font=4,cex=txt.cex)
axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.25)
box(lwd=1)
mtext(side=1,line=2.5,"Month")
mtext(side=3,adj=0,"LORS08 (2008)",cex=title.cex)

plot(ZONE_A~Date,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="PA25"),lines(Date,ZONE_A,col="black",lwd=1.25))
with(subset(reg.sch2,Alt=="PA25"),lines(Date,ZONE_BC,col="black",lwd=1.25))
# with(subset(reg.sch2,Alt=="PA25"),lines(Date,ZONE_D1,col="grey",lwd=2,lty=2))
# with(subset(reg.sch2,Alt=="PA25"),lines(Date,ZONE_D2,col="grey",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(Date,LOWSM_15_LEVEL,col="black",lwd=1.25))
with(subset(reg.sch2,Alt=="PA25"&Date==date.fun(as.Date(30,origin="1900-01-01"))),
     text(date.fun(as.Date(30,origin="1900-01-01")),
          ZONE_A,"Zone A",pos=3,col="black",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&Date==date.fun(as.Date(90,origin="1900-01-01"))),
     text(date.fun(as.Date(90,origin="1900-01-01")),
          ZONE_BC,"Zone BC",pos=3,col="black",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&Date==date.fun(as.Date(30,origin="1900-01-01"))),
     text(date.fun(as.Date(85,origin="1900-01-01")),
          LOWSM_15_LEVEL+(ZONE_BC-LOWSM_15_LEVEL)/2,"Zone D",col="black",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&Date==date.fun(as.Date(250,origin="1900-01-01"))),
     text(date.fun(as.Date(250,origin="1900-01-01")),
          3+(LOWSM_15_LEVEL-3)/2,"Water Shortage\nManagement Band",col="black",cex=0.75,font=2))
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
box(lwd=1)
mtext(side=1,line=2.5,"Month")
mtext(side=3,adj=0,"LOSOM (2022)",cex=title.cex)

axis_fun(4,ft.to.m(ymaj2),ft.to.m(ymin2),ymaj2)
dev.off()





sch.LORS$DOY=as.numeric(format(sch.LORS$Date,"%j"))
DOY.fun=function(x)as.numeric(format(as.Date(x),"%j"))

## convert m to ft
sch.LORS[,3:11]=sapply(sch.LORS[,3:11],m.to.ft)
reg.sch2[,3:10]=sapply(reg.sch2[,3:10],m.to.ft)


# png(filename=paste0(plot.path,"Iteration3_Final/LORS_LOSOM.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,1,0.5,0.5),oma=c(1.5,2,0.5,0.5));
layout(matrix(1:2,1,2,byrow=T))

xlim.val.date=date.fun(c("1965-01-01","1965-12-31"));xmaj.dat=seq(xlim.val.date[1],xlim.val.date[2],"2 month");xmin.dat=seq(xlim.val.date[1],xlim.val.date[2],"1 month")
xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.dat,"%j"));xmin=as.numeric(format(xmin.dat,"%j"));
ylim.val=c(10,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/0.5)
txt.cex=0.75
title.cex=1

plot(High~DOY,sch.LORS,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(sch.LORS,lines(High~DOY,lwd=1.25,col="red"))
with(sch.LORS,lines(Intermediate~DOY,lwd=1.25,col="deepskyblue2"))
with(sch.LORS,lines(Low~DOY,lwd=1.25,col="orange"))
with(sch.LORS,lines(LowMid~DOY,lty=2,col="grey40"))
with(sch.LORS,lines(Inter1ft~DOY,lty=2))
with(sch.LORS,lines(LowLow~DOY,lty=2,col="grey40"))
with(sch.LORS,lines(BaseFlow~DOY,lwd=1.25))
with(sch.LORS,lines(BeneficialUse~DOY,lwd=1.25))
with(sch.LORS,lines(WSM~DOY,lwd=1.25,col="purple"))
text(DOY.fun("1900-07-15"),(17.5),"High Lake Management Band",font=4,cex=txt.cex,col="red")
text(DOY.fun("1900-03-15"),(17),"High",font=4,cex=txt.cex,col="deepskyblue2")
text(DOY.fun("1900-03-15"),(16.25),"Intermediate",font=4,cex=txt.cex,col="orange")
text(DOY.fun("1900-03-15"),(14.5),"Low",font=4,cex=txt.cex)
text(DOY.fun("1900-03-15"),(13),"Base Flow",font=4,cex=txt.cex)
text(DOY.fun("1900-04-15"),(12),"Beneficial Use",font=4,cex=txt.cex)
text(DOY.fun("1900-08-01"),(10.25),"Water Shortage Management Band",font=4,cex=txt.cex,col="blue")

mid.x=244;mx.x=15;mx.x2=9
min.y=subset(sch.LORS,DOY==mid.x)$BeneficialUse# subset(reg.sch2,Alt=="NA25f"&DOY==mid.x)$ZONE_D0
max.y=17.25;#subset(reg.sch2,Alt=="NA25f"&DOY==mid.x)$ZONE_A
h.val=0.75
xx=c(mid.x,mid.x-mx.x,mid.x-mx.x2,mid.x-mx.x2,mid.x-mx.x,
     mid.x,mid.x+mx.x,mid.x+mx.x2,mid.x+mx.x2,mid.x+mx.x,mid.x)
yy=c(min.y,min.y+h.val,min.y+h.val,
     max.y-h.val,max.y-h.val,max.y,max.y-h.val,max.y-h.val,
     min.y+h.val,min.y+h.val,min.y)
polygon(xx,yy,col=adjustcolor("deeppink",0.5),border="grey")
text(mid.x,min.y+(max.y-min.y)/2,"Flows South",srt=90,cex=0.8,offset=0)


axis_fun(2,ymaj,ymin,format(ymaj))
axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
box(lwd=1)
mtext(side=1,line=1.5,"Month")
mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"LORS08 (2008)",cex=title.cex)

plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i",yaxs="i",type="n")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_A,col="red",lwd=1.25))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,ZONE_BC,col="deepskyblue2",lwd=1.25))
# with(subset(reg.sch2,Alt=="PA25"),lines(Date,ZONE_D1,col="grey",lwd=2,lty=2))
# with(subset(reg.sch2,Alt=="PA25"),lines(Date,ZONE_D2,col="grey",lwd=2))
with(subset(reg.sch2,Alt=="PA25"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=1.25))
with(subset(reg.sch2,Alt=="PA25"&DOY==30),
     text(DOY,
          ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==90),
     text(DOY,
          ZONE_BC,"Zone BC",pos=3,col="deepskyblue2",cex=0.75,font=2))
with(subset(reg.sch2,Alt=="PA25"&DOY==85),
     text(DOY,
          LOWSM_15_LEVEL+(ZONE_BC-LOWSM_15_LEVEL)/2,"Zone D",col="black",cex=0.75,font=2))
text(DOY.fun("1900-08-01"),(10.25),"Water Shortage Management Band",font=4,cex=txt.cex,col="blue")
# with(subset(reg.sch2,Alt=="PA25"&DOY==255),
#      text(DOY,
#           10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
mid.x=244;mx.x=15;mx.x2=9
min.y=subset(reg.sch2,Alt=="PA25"&DOY==mid.x)$LOWSM_15_LEVEL
max.y=17.25;#subset(reg.sch2,Alt=="NA25f"&DOY==mid.x)$ZONE_A
h.val=0.75
xx=c(mid.x,mid.x-mx.x,mid.x-mx.x2,mid.x-mx.x2,mid.x-mx.x,
     mid.x,mid.x+mx.x,mid.x+mx.x2,mid.x+mx.x2,mid.x+mx.x,mid.x)
yy=c(min.y,min.y+h.val,min.y+h.val,
     max.y-h.val,max.y-h.val,max.y,max.y-h.val,max.y-h.val,
     min.y+h.val,min.y+h.val,min.y)
polygon(xx,yy,col=adjustcolor("deeppink",0.5),border="grey")
text(mid.x,min.y+(max.y-min.y)/2,"Flows South",srt=90,cex=0.8,offset=0)

axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
box(lwd=1)
mtext(side=1,line=1.5,"Month")
mtext(side=3,adj=0,"LOSOM (2022)",cex=title.cex)
dev.off()