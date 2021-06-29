
library(AnalystHelper)
library(reshape2)

## https://psl.noaa.gov/enso/data.html

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

layout(matrix(c(1:2),2,1,byrow=T))
ylim.val=c(-0.4,0.4);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1870-01-01","2016-12-01"));xmaj=seq(xlim.val[1],xlim.val[2],"20 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

plot(value~Date.mon,AMO.dat.melt,xlim=xlim.val,ylim=ylim.val,type="n")
#with(AMO.dat.melt,lines(Date.mon,ma,col="red"))
with(subset(AMO.dat.melt,is.na(value)==F),shaded.range(Date.mon,rep(0,length(Date.mon)),ifelse(value>0,value,0),"indianred1",lty=1))
with(subset(AMO.dat.melt,is.na(value)==F),shaded.range(Date.mon,ifelse(value<0,value,0),rep(0,length(Date.mon)),"dodgerblue1",lty=1))
abline(h=0)


## PDO
pdo.dat=read.csv("https://www.ncdc.noaa.gov/teleconnections/pdo/data.csv",skip=1,col.names = c("Date","Value"))
pdo.dat$year=substring(pdo.dat$Date,1,4)
pdo.dat$month=substring(pdo.dat$Date,5,6)
pdo.dat$Date.mon=with(pdo.dat,date.fun(paste(year,month,"01",sep="-")))
head(pdo.dat)
plot(Value~Date.mon,pdo.dat)

# Southern Oscillation Index
vars=c('year',month.abb)
row.count=length(seq(1866,2020,1))
noaa.soi.path="https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/soi.long.data"

SOI.dat=read.table(noaa.soi.path,header=F,skip=1,col.names=vars,nrows=row.count,na.string="-99.990")
SOI.dat.melt=melt(SOI.dat,id.vars="year")
SOI.dat.melt=merge(SOI.dat.melt,data.frame(variable=month.abb,month=1:12))
SOI.dat.melt$Date.mon=with(SOI.dat.melt,date.fun(paste(year,month,"01",sep="-")))
SOI.dat.melt=AMO.dat.melt[order(SOI.dat.melt$Date.mon),c("Date.mon","value")]

plot(value~Date.mon,SOI.dat.melt)


# North Atlantic Oscillation (NAO)
vars=c('year',month.abb)
row.count=length(seq(1821,2020,1))
noaa.nao.path="https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nao.long.data"

NAO.dat=read.table(noaa.nao.path,header=F,skip=1,col.names=vars,nrows=row.count,na.string="-99.990")
NAO.dat.melt=melt(NAO.dat,id.vars="year")
NAO.dat.melt=merge(NAO.dat.melt,data.frame(variable=month.abb,month=1:12))
NAO.dat.melt$Date.mon=with(NAO.dat.melt,date.fun(paste(year,month,"01",sep="-")))
NAO.dat.melt=AMO.dat.melt[order(NAO.dat.melt$Date.mon),c("Date.mon","value")]

plot(value~Date.mon,NAO.dat.melt)

# NCAR
# https://climatedataguide.ucar.edu/climate-data/atlantic-multi-decadal-oscillation-amo
vars=c('year',month.abb)
row.count=length(seq(1870,2021,1))
amo.path="https://climatedataguide.ucar.edu/sites/default/files/amo_monthly.10yrLP.txt"
# amo.path="https://climatedataguide.ucar.edu/sites/default/files/amo_monthly.txt"
AMO.dat=read.table(amo.path,header=F,skip=1,col.names=vars,nrows=row.count,na.string="-999.")
AMO.dat.melt=reshape2::melt(AMO.dat,id.vars="year")
AMO.dat.melt=merge(AMO.dat.melt,data.frame(variable=month.abb,month=1:12))
AMO.dat.melt$Date.mon=with(AMO.dat.melt,date.fun(paste(year,month,"01",sep="-")))
AMO.dat.melt=AMO.dat.melt[order(AMO.dat.melt$Date.mon),c("Date.mon","value")]

ylim.val=c(-0.4,0.4);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("1870-01-01","2016-12-01"));xmaj=seq(xlim.val[1],xlim.val[2],"20 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")

plot(value~Date.mon,AMO.dat.melt,xlim=xlim.val,ylim=ylim.val,type="n")
with(subset(AMO.dat.melt,is.na(value)==F),
    shaded.range(Date.mon,rep(0,length(Date.mon)),ifelse(value>0,value,0),"indianred1",lty=1))
with(subset(AMO.dat.melt,is.na(value)==F),
    shaded.range(Date.mon,ifelse(value<0,value,0),rep(0,length(Date.mon)),"dodgerblue1",lty=1))

