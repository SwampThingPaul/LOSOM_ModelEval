## 
## LOSOM
##
## Manuscript
## See Havens (2002)
## Ecological Envelope?
## Return interval calc
## event and duration
## cumulative hazard https://www.r-bloggers.com/2016/12/survival-analysis-basics/
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
# 
# ## DSS rip
# if(R.Version()$arch=="x86_64"){
#   # use 64-bit .jar and .dll
#   options(dss_override_location="C:\\Program Files (x86)\\HEC\\HEC-DSSVue\\")
#   Sys.setenv(JAVA_HOME=paste0(options("dss_override_location"), "java"))
# } else {
#   # use 32-bit .jar and .dll (old dssrip, no longer needed)
# }
# 


## Paths
wd="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval"

paths=paste0(wd,c("/Plots/_Manuscripts/LOK_PMEval/","/Export/","/Data/","/src/"))
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

# Lake Stage --------------------------------------------------------------
alts.iter2=c("ECBr","NA25","CC")
n.alts=length(alts.iter2)
lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts.iter2[i],"/RSMBN_output.dss"))
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts.iter2[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
unique(lakeO.stage$Alt)
range(lakeO.stage$Date)

tsp.mod=260467
n.alts=1
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3_Batch/20211027/RSMBN/model_",tsp.mod[i],"/RSMBN_output.dss"))  
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=tsp.mod[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
unique(lakeO.stage$Alt)
lakeO.stage=merge(lakeO.stage,data.frame(Alt=c(alts.iter2,tsp.mod),Alt2=c(alts.iter2,"TSP")),"Alt")

lakeO.stage$month=as.numeric(format(lakeO.stage$Date,'%m'))
lakeO.stage$day=as.numeric(format(lakeO.stage$Date,'%d'))
lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,'%Y'))
lakeO.stage$DoY=as.numeric(format(lakeO.stage$Date,'%j'))
lakeO.stage$WY=WY(lakeO.stage$Date)

lakeO.stage$Stage.m=ft.to.m(lakeO.stage$STAGE)
lakeO.stage$recess_1day=with(lakeO.stage,ave(Stage.m,Alt2,FUN=function(x) c(rep(NA,1),diff(x,lag=1))))
lakeO.stage$recess_7day=with(lakeO.stage,ave(Stage.m,Alt2,FUN=function(x) c(rep(NA,6),diff(x,lag=6))))
lakeO.stage$recess_30day=with(lakeO.stage,ave(Stage.m,Alt2,FUN=function(x) c(rep(NA,29),diff(x,lag=29))))

lakeO.stage$ExHigh=with(lakeO.stage,ifelse(Stage.m>5.2,1,0))
lakeO.stage$ModHigh=with(lakeO.stage,ifelse(Stage.m>4.6,1,0))
lakeO.stage$ExLow=with(lakeO.stage,ifelse(Stage.m<3.4,1,0))
lakeO.stage$ModLow=with(lakeO.stage,ifelse(Stage.m<3.7,1,0))

head(lakeO.stage,9L)
head(diff(lakeO.stage$Stage.m,lag=6),9L)

lakeO.stage[7,"Stage.m"]-lakeO.stage[1,"Stage.m"]

# Snail Kite PM -----------------------------------------------------------
plot(recess_7day~Date,subset(lakeO.stage,Alt=="NA25"))
plot(recess_7day~Date,subset(lakeO.stage,Alt2=="TSP"))

date.fill=data.frame(expand.grid(CY=seq(min(lakeO.stage$CY),max(lakeO.stage$CY),1),
                                 DoY=1:366))

recess.dat=lakeO.stage[,c("Alt2","CY","DoY","STAGE","recess_7day")]
bks=c(min(recess.dat$recess_7day,na.rm=T),ft.to.m(c(-0.16,-0.05,0.05,0.25)),max(recess.dat$recess_7day,na.rm=T))
recess.dat$cat2=as.factor(findInterval(recess.dat$recess_7day,bks,rightmost.closed = T,left.open = T))
# recess.dat$cat2=with(recess.dat,ifelse(stgchange_7day>=-0.05&stgchange_7day<=0.05,1,
#                                        ifelse(stgchange_7day<(-0.05)&stgchange_7day>(-0.16),2,
#                                               ifelse(stgchange_7day<=(-0.16),3,
#                                                      ifelse(stgchange_7day>0.05&stgchange_7day<=0.25,4,
#                                                             ifelse(stgchange_7day>0.25,5,NA))))))

ddply(recess.dat,c("Alt2","cat2"),summarise,min.val=min(recess_7day,na.rm=T),max.val=max(recess_7day,na.rm=T))


recess.dat=merge(recess.dat,date.fill,by=c("CY",'DoY'),all.y=T)
recess.dat=recess.dat[order(recess.dat$Alt,recess.dat$CY,recess.dat$DoY),]

library(ggplot2)
cols=c("1"="red","2"="yellow","3"="green","4"="goldenrod2","5"="darkred")
tmp=subset(recess.dat,Alt2=="NA25")
rec.plot.FWO=ggplot(tmp, aes(x = DoY, y = CY, fill = cat2)) +
  geom_tile(aes(group = cat2), colour = "black",size=0.05)+
  #geom_text(aes(label=format(round(mean.flow,0),nsmall=0),fontface = "bold"),size=2,colour=q.dat.wk.mean$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = tmp$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1,366,30),labels= seq(1,366,30))+
  scale_fill_manual(values = cols,
                    name="Weekly Recession\nRate (ft/wk)",
                    breaks=c(1:5),
                    # labels=c("\u2265 0.05 & \u2264 -0.05","< -0.05 & > -0.16","\u2264 -0.16", "> 0.05 & \u2264 0.25", "> 0.25"),
                    labels=c("\u2264 -0.16","> -0.16 & < -0.05","> -0.05 & \u2264 0.05","> 0.05 & \u2264 0.25","> 0.25"),
                    na.value="white") +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Lake Okeechobee Recession/Accession Rate",
       subtitle = "Modelled Weekly Recession/Accession Rates (NA25)",
       caption = "LOSOM: NA25 (FWO)",
       x="Day of Year",
       y="Year")
rec.plot.FWO
# ggsave(paste0(plot.path,"Lake_FWO_recess.png"),rec.plot.FWO,device="png",height =7,width=6,units="in")


tmp=subset(recess.dat,Alt2=="TSP")
rec.plot.TSP=ggplot(tmp, aes(x = DoY, y = CY, fill = cat2)) +
  geom_tile(aes(group = cat2), colour = "black",size=0.05)+
  #geom_text(aes(label=format(round(mean.flow,0),nsmall=0),fontface = "bold"),size=2,colour=q.dat.wk.mean$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = tmp$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1,366,30),labels= seq(1,366,30))+
  scale_fill_manual(values = cols,
                    name="Weekly Recession\nRate (ft/wk)",
                    breaks=c(1:5),
                    # labels=c("\u2265 0.05 & \u2264 -0.05","< -0.05 & > -0.16","\u2264 -0.16", "> 0.05 & \u2264 0.25", "> 0.25"),
                    labels=c("\u2264 -0.16","> -0.16 & < -0.05","> -0.05 & \u2264 0.05","> 0.05 & \u2264 0.25","> 0.25"),
                    na.value="white") +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Lake Okeechobee Recession/Accession Rate",
       subtitle = "Modelled Weekly Recession/Accession Rates (TSP; Model Run 260467)",
       caption = "LOSOM: TSP (Iteration 3 Model Run 260467) ",
       x="Day of Year",
       y="Year")
rec.plot.TSP

# ggsave(paste0(plot.path,"Lake_recess_TSP.png"),rec.plot.TSP,device="png",height =7,width=6,units="in")
# Monthly Mean ------------------------------------------------------------
mon.stg=ddply(lakeO.stage,c("Alt2","month","CY"),summarise,mean.stg=mean(Stage.m,na.rm=T))

FWO.test=subset(mon.stg,Alt2=="NA25")
FWO.test$ExLow=with(FWO.test,ifelse(mean.stg<3.4,1,0))



# Consecutive Events Stage ------------------------------------------------
alts.sort=c(alts.iter2,tsp.mod)
stg_consec=data.frame()
for(j in 1:length(alts.sort)){
  tmp=subset(lakeO.stage,Alt==alts.sort[j])
  tmp$ExHigh.stg=0
  tmp$ModHigh.stg=0
  tmp$ExLow.stg=0
  tmp$ModLow.stg=0
  for(i in 2:nrow(tmp)){
    tmp$ExHigh.stg[i]=with(tmp,ifelse(ExHigh[i-1]==0&ExHigh[i]>0,1,
                                 ifelse(ExHigh[i-1]>0&ExHigh[i]>0,1,0)))
    tmp$ModHigh.stg[i]=with(tmp,ifelse(ModHigh[i-1]==0&ModHigh[i]>0,1,
                                      ifelse(ModHigh[i-1]>0&ModHigh[i]>0,1,0)))
    tmp$ExLow.stg[i]=with(tmp,ifelse(ExLow[i-1]==0&ExLow[i]>0,1,
                                      ifelse(ExLow[i-1]>0&ExLow[i]>0,1,0)))
    tmp$ModLow.stg[i]=with(tmp,ifelse(ModLow[i-1]==0&ModLow[i]>0,1,
                                      ifelse(ModLow[i-1]>0&ModLow[i]>0,1,0)))
  }
  
  exhighstg=consec.startend(tmp$ExHigh.stg>0)
  tmp$sum.ExHigh.stg=0
  for(i in 1:length(exhighstg$ends)){
    tmp[exhighstg$ends[i],]$sum.ExHigh.stg=with(tmp[c(exhighstg$starts[i]:exhighstg$ends[i]),],sum(ExHigh.stg,na.rm=T))
  }
  modhighstg=consec.startend(tmp$ModHigh.stg>0)
  tmp$sum.ModHigh.stg=0
  for(i in 1:length(modhighstg$ends)){
    tmp[modhighstg$ends[i],]$sum.ModHigh.stg=with(tmp[c(modhighstg$starts[i]:modhighstg$ends[i]),],sum(ModHigh.stg,na.rm=T))
  }
  exlowstg=consec.startend(tmp$ExLow.stg>0)
  tmp$sum.ExLow.stg=0
  for(i in 1:length(exlowstg$ends)){
    tmp[exlowstg$ends[i],]$sum.ExLow.stg=with(tmp[c(exlowstg$starts[i]:exlowstg$ends[i]),],sum(ExLow.stg,na.rm=T))
  }
  modlowstg=consec.startend(tmp$ModLow.stg>0)
  tmp$sum.ModLow.stg=0
  for(i in 1:length(modlowstg$ends)){
    tmp[modlowstg$ends[i],]$sum.ModLow.stg=with(tmp[c(modlowstg$starts[i]:modlowstg$ends[i]),],sum(ModLow.stg,na.rm=T))
  }
  
  stg_consec=rbind(tmp,stg_consec)
  print(j)
}

bks=c(1,15,30,60,90,180)
# proof of concept
# test=ddply(stg_consec,c("Alt2","sum.ExLow.stg"),summarise,count.event=N.obs(sum.ExLow.stg))
# test$cat1=findInterval(test$sum.ExLow.stg,bks,left.open = FALSE,rightmost.closed = TRUE)
# test$cat2=with(test,ifelse(sum.ExLow.stg>0&sum.ExLow.stg<15,1,
#                                   ifelse(sum.ExLow.stg>=15&sum.ExLow.stg<30,2,
#                                          ifelse(sum.ExLow.stg>=30&sum.ExLow.stg<60,3,
#                                                 ifelse(sum.ExLow.stg>=60&sum.ExLow.stg<90,4,
#                                                        ifelse(sum.ExLow.stg>=90&sum.ExLow.stg<180,5,
#                                                               ifelse(sum.ExLow.stg>=180,6,NA)))))))
# test
# plot(cat1~cat2,test);abline(0,1)

# PMs=c("ExHigh","ModHigh","ModLow","ExLow")
# fill.vals=expand.grid(PM=PMs,
#             cat=c(1:6),
#             Alt2=alts.iter2)
## Doesn't exactly work the way I wanted it to...doesnt give correct sums.
# stg_consec.sum=data.frame()
# for(i in 1:length(PMs)){
#   tmp=ddply(stg_consec,c("Alt2",paste("sum",PMs[i],"stg",sep=".")),summarise,count.event=N.obs(paste("sum",PMs[i],"stg",sep=".")))
#   colnames(tmp)=c("Alt2","Event.Dur","count.event")
#   tmp$PM=PMs[i]
#   tmp$cat=findInterval(tmp$Event.Dur,bks,left.open = FALSE,rightmost.closed = TRUE)
#   stg_consec.sum=rbind(tmp,stg_consec.sum)
#   print(i)
# }

stg_consec.sum=data.frame()
tmp=ddply(stg_consec,c("Alt2","sum.ExHigh.stg"),summarise,count.event=N.obs(sum.ExHigh.stg),.progress="text")
colnames(tmp)=c("Alt2","Event.Dur","count.event")
tmp$PM="ExHigh"
tmp$cat=findInterval(tmp$Event.Dur,bks,left.open = FALSE,rightmost.closed = TRUE)
stg_consec.sum=rbind(tmp,stg_consec.sum)

tmp=ddply(stg_consec,c("Alt2","sum.ModHigh.stg"),summarise,count.event=N.obs(sum.ModHigh.stg))
colnames(tmp)=c("Alt2","Event.Dur","count.event")
tmp$PM="ModHigh"
tmp$cat=findInterval(tmp$Event.Dur,bks,left.open = FALSE,rightmost.closed = TRUE)
stg_consec.sum=rbind(tmp,stg_consec.sum)

tmp=ddply(stg_consec,c("Alt2","sum.ModLow.stg"),summarise,count.event=N.obs(sum.ModLow.stg))
colnames(tmp)=c("Alt2","Event.Dur","count.event")
tmp$PM="ModLow"
tmp$cat=findInterval(tmp$Event.Dur,bks,left.open = FALSE,rightmost.closed = TRUE)
stg_consec.sum=rbind(tmp,stg_consec.sum)

tmp=ddply(stg_consec,c("Alt2","sum.ExLow.stg"),summarise,count.event=N.obs(sum.ExLow.stg))
colnames(tmp)=c("Alt2","Event.Dur","count.event")
tmp$PM="ExLow"
tmp$cat=findInterval(tmp$Event.Dur,bks,left.open = FALSE,rightmost.closed = TRUE)
stg_consec.sum=rbind(tmp,stg_consec.sum)


#stg_consec.sum=subset(stg_consec.sum, Event.Dur>0)
sort(subset(stg_consec,Alt2=="NA25"&sum.ModLow.stg>0)$sum.ModLow.stg)
subset(stg_consec.sum,PM=="ModLow"&Alt2=="NA25")
subset(stg_consec.sum,PM=="ModLow"&Alt2=="NA25")$Event.Dur

subset(stg_consec.sum,PM=="ModLow"&Alt2=="NA25"&Event.Dur>12)
sum(subset(stg_consec.sum,PM=="ModLow"&Alt2=="NA25"&Event.Dur>12)$count.event)

nrow(subset(stg_consec.sum,PM=="ModLow"&Alt2=="NA25"&Event.Dur>12))
nrow(subset(stg_consec.sum,PM=="ModLow"&Alt2=="ECBr"&Event.Dur>12))

# Average event per decade
# dec.mean=reshape2::dcast(stg_consec.sum,Alt2~PM,value.var = "Event.Dur",
#                          fun.aggregate = function(x)round(length(x)/5.2,1))
dec.mean=ddply(stg_consec,c("Alt2"),summarise,
               ExHigh=sum(sum.ExHigh.stg>0,na.rm=T)/5.2,
               ModHigh=sum(sum.ModHigh.stg>12,na.rm=T)/5.2,
               ModLow=sum(sum.ModLow.stg>12,na.rm=T)/5.2,
               ExLow=sum(sum.ExLow.stg>0,na.rm=T)/5.2)

rec.rate=ddply(subset(stg_consec,month%in%seq(1,6,1)),c("Alt2","CY"),summarise,
                ann.mean=mean(recess_30day,na.rm=T),
                min.stg=min(Stage.m,na.rm=T),
                max.stg=max(Stage.m,na.rm=T))
rec.rate$recess.event=with(rec.rate,ifelse(abs(ann.mean)>0.1,1,0))
rec.rate.dec=ddply(rec.rate,"Alt2",summarise,recess.event=sum(recess.event,na.rm=T)/5.2)


ann.events=ddply(stg_consec,c("Alt2","CY"),summarise,
      ExHigh=sum(sum.ExHigh.stg>0,na.rm=T),
      ModHigh=sum(sum.ModHigh.stg>0,na.rm=T),
      ModLow=sum(sum.ModLow.stg>0,na.rm=T),
      ExLow=sum(sum.ExLow.stg>0,na.rm=T))
ann.events$ExHigh.dec=with(ann.events,ave(ExHigh,Alt2,
                                          FUN=function(x) rollapply(x,10,sum,by=10,fill=NA,align="right")))
ann.events$ModHigh.dec=with(ann.events,ave(ModHigh,Alt2,
                                          FUN=function(x) rollapply(x,10,sum,by=10,fill=NA,align="right")))
ann.events$ModLow.dec=with(ann.events,ave(ModLow,Alt2,
                                          FUN=function(x) rollapply(x,10,sum,by=10,fill=NA,align="right")))
ann.events$ExLow.dec=with(ann.events,ave(ExLow,Alt2,
                                          FUN=function(x) rollapply(x,10,sum,by=10,fill=NA,align="right")))

ddply(ann.events,"Alt2",summarise,
      ExHigh.mean=mean(ExHigh.dec,na.rm=T),
      ModHigh.mean=mean(ModHigh.dec,na.rm=T),
      ModLow.mean=mean(ModLow.dec,na.rm=T),
      ExLow.mean=mean(ExLow.dec,na.rm=T))









#### 
rslt.ExLow=ddply(stg_consec,c("Alt","sum.ExLow.stg"),summarise,count.event=N.obs(sum.ExLow.stg))
max(rslt.ExLow$sum.ExLow.stg)
rslt.ExLow$cat=with(rslt.ExLow,ifelse(sum.ExLow.stg>0&sum.ExLow.stg<15,1,
                                      ifelse(sum.ExLow.stg>=15&sum.ExLow.stg<30,2,
                                             ifelse(sum.ExLow.stg>=30&sum.ExLow.stg<60,3,
                                                    ifelse(sum.ExLow.stg>=60&sum.ExLow.stg<90,4,
                                                           ifelse(sum.ExLow.stg>=90&sum.ExLow.stg<180,5,
                                                                  ifelse(sum.ExLow.stg>=180,6,NA)))))))

nrow(subset(rslt.ExLow,Alt=='NA25'&sum.ExLow.stg>0))/(30/10)


xlim.val=date.fun(c("1965-01-01","2016-12-30"))
plot(Stage.m~Date,subset(stg_consec,Alt=="NA25"),xlim=xlim.val,ylim=c(2,6),xaxs="i",type="n")
with(subset(stg_consec,Alt=="NA25"),lines(Date,Stage.m))
cols=adjustcolor(c("maroon","indianred1"),0.5)
abline(h=c(5.2,4.6,3.7,3.4),col=c(cols,rev(cols)),lty=c(1,2,2,1),lwd=2)
ylim.vals=c(0,600)
par(new=T);plot(sum.ExLow.stg~Date,subset(stg_consec,Alt=="NA25"),xaxs="i",type="n",axes=F,ann=F,xlim=xlim.val,ylim=ylim.vals)
with(subset(stg_consec,Alt=="NA25"),segments(Date,rep(0,length(Date)),Date,sum.ExLow.stg,col="darkgreen",lwd=2))


dec.periods=data.frame(CY=seq(1965,2016,1),
           dec.period=sort(c(rep(1:5,10),6,6)))


test=subset(stg_consec,Alt2=="NA25")
test=merge(test,dec.periods,"CY",all.x=T)

test2=ddply(test,c("CY"),summarise,
      ExHigh=sum(sum.ExHigh.stg>0,na.rm=T),
      ModHigh=sum(sum.ModHigh.stg>0,na.rm=T),
      # ModHigh.365=sum(sum.ModHigh.stg>365,na.rm=T),
      ModLow=sum(sum.ModLow.stg>0,na.rm=T),
      ExLow=sum(sum.ExLow.stg>0,na.rm=T))
test.recess=ddply(subset(test,month%in%seq(1,6,1)),c("CY"),summarise,
                  min.recess=min(recess_7day,na.rm=T),
                  max.recess=max(recess_7day,na.rm=T),
                  count.recess=sum((recess_7day)>0.1,na.rm=T)
                  )
# boxplot(recess_7day~CY,subset(test,month%in%seq(1,6,1)))
# test2=merge(test2,dec.periods,"CY",all.x=T)
test2=merge(test2,test.recess,"CY")

score.vals=data.frame(val=0:52,score=c(1.0,0.9,0.7,0.4,rep(0,49)))
score.vals2=data.frame(val=10:1,score=c(1.0,0.9,0.7,0.4,rep(0,6)))
test2$ExHigh.dec=rollapply(test2$ExHigh,10,sum,by=10,fill=NA,align="right")
test2$ModHigh.dec=rollapply(test2$ModHigh,10,sum,by=10,fill=NA,align="right")
test2$ExLow.dec=rollapply(test2$ExLow,10,sum,by=10,fill=NA,align="right")
test2$ModLo.dec=rollapply(test2$ModLo,10,sum,by=10,fill=NA,align="right")
test2$count.recess.dec=rollapply(test2$count.recess>0,10,sum,by=10,fill=NA,align="right")

test2=merge(test2,score.vals,by.x="ExHigh.dec",by.y="val",all.x=T,sort=F)
test2=rename(test2,c("score"="ExHigh.dec.scr"))
test2=merge(test2,score.vals,by.x="ModHigh.dec",by.y="val",all.x=T,sort=F)
test2=rename(test2,c("score"="ModHigh.dec.scr"))
test2=merge(test2,score.vals,by.x="ExLow.dec",by.y="val",all.x=T,sort=F)
test2=rename(test2,c("score"="ExLow.dec.scr"))
test2=merge(test2,score.vals,by.x="ModLo.dec",by.y="val",all.x=T,sort=F)
test2=rename(test2,c("score"="ModLo.dec.scr"))
test2=merge(test2,score.vals2,by.x="count.recess.dec",by.y="val",all.x=T,sort=F)
test2=rename(test2,c("score"="count.recess.dec.scr"))
test2=test2[order(test2$CY),]


((sum(test2$ExHigh.dec.scr,na.rm=T)*5)+(sum(test2$ModHigh.dec.scr,na.rm=T)*5)+
  (sum(test2$ModLo.dec.scr,na.rm=T)*4)+(sum(test2$ExLow.dec.scr,na.rm=T)*4)+
  (sum(test2$count.recess.dec.scr,na.rm=T)*3))/21

((2*5)+(10*5)+(3*4)+(5*4)+(2*3))/21
((0.7*5)+(0*5)+(0.4*4)+(0*4)+(0*3))/21
# -------------------------------------------------------------------------
mon.range=ddply(lakeO.stage,c("Alt2","CY","month"),summarise,Max.stg=max(Stage.m),Min.stg=min(Stage.m))


test=subset(mon.range,Alt2=="NA25")
test$ExHigh=with(test,ifelse(Max.stg>5.2,1,0))
test$ModHigh=with(test,ifelse(Max.stg>4.6,1,0))
test$ExLow=with(test,ifelse(Min.stg<3.4,1,0))
test$ModLow=with(test,ifelse(Min.stg<3.7,1,0))

plot(test$Max.stg)
abline(h=c(5.2,4.6))

test$ExHigh.dec=rollapply(test$ExHigh,120,sum,by=120,fill=NA,align="right")
test$ModHigh.dec=rollapply(test$ModHigh,120,sum,by=120,fill=NA,align="right")
test$ExLow.dec=rollapply(test$ExLow,120,sum,by=120,fill=NA,align="right")
test$ModLo.dec=rollapply(test$ModLo,120,sum,by=120,fill=NA,align="right")


subset(test,is.na(ExHigh.dec)==F)
subset(test,is.na(ExLow.dec)==F)
















# -------------------------------------------------------------------------

### 
test=reshape2::dcast(lakeO.stage,Alt2+CY~month,value.var = "Stage.m",mean)
test=test[,c("Alt2","CY","1","6")]
colnames(test)=c("Alt2","CY","Jan","Jun")
test$sprecc=with(test,Jun-Jan)
plot(sprecc~CY,subset(test,Alt2=="NA25"))


sp.rec=ddply(subset(lakeO.stage,month%in%c(1:6)),c("Alt2","CY"),summarise,max.recess=max(recess_1day,na.rm=T))

plot(max.recess~CY,subset(sp.rec,Alt2=="NA25"))

ex.high=reshape2::dcast(lakeO.stage,CY~Alt2,value.var = "Stage.m",fun.aggregate = function(x) sum(x>5.2))
ex.low=reshape2::dcast(lakeO.stage,CY~Alt2,value.var = "Stage.m",fun.aggregate = function(x) sum(x<3.4))
# rollapply(ex.high$CC,10,sum,by=10)
# rollapply(ex.low$NA25,10,sum,by=10,align="right")

score.vals=data.frame(val=0:52,score=c(1.0,0.9,0.7,0.4,rep(0,49)))
ann.range=ddply(lakeO.stage,c("Alt2","CY"),summarise,Max.stg=max(Stage.m),Min.stg=min(Stage.m))
test=subset(ann.range,Alt2=="NA25")
test$ExHigh=with(test,ifelse(Max.stg>5.2,1,0))
test$ModHigh=with(test,ifelse(Max.stg>4.6,1,0))
test$ExLow=with(test,ifelse(Min.stg<3.4,1,0))
test$ModLow=with(test,ifelse(Min.stg<3.7,1,0))
test$ExHigh.dec=rollapply(test$ExHigh,10,sum,by=10,fill=NA,align="right")
test$ModHigh.dec=rollapply(test$ModHigh,10,sum,by=10,fill=NA,align="right")
test$ExLow.dec=rollapply(test$ExLow,10,sum,by=10,fill=NA,align="right")
test$ModLo.dec=rollapply(test$ModLo,10,sum,by=10,fill=NA,align="right")

test=merge(test,score.vals,by.x="ExHigh.dec",by.y="val",all.x=T,sort=F)
test=rename(test,c("score"="ExHigh.dec.scr"))
test=merge(test,score.vals,by.x="ModHigh.dec",by.y="val",all.x=T,sort=F)
test=rename(test,c("score"="ModHigh.dec.scr"))
test=merge(test,score.vals,by.x="ExLow.dec",by.y="val",all.x=T,sort=F)
test=rename(test,c("score"="ExLow.dec.scr"))
test=merge(test,score.vals,by.x="ModLo.dec",by.y="val",all.x=T,sort=F)
test=rename(test,c("score"="ModLo.dec.scr"))
test=test[order(test$CY),]

subset(test,Alt2=="NA25")


test2=data.frame(
ExHigh.dec=rollapply(test$ExHigh,10,sum,by=10,align="right")
)
test2=merge(test2,score.vals,by.x="ExHigh.dec",by.y="val")
