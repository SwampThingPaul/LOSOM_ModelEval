## 
## LOSOM
##
## Iteration 3 final array
## NOTES: C10A renamed to S271
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
poly.step=function(x,y.L, y.U, bg, col = bg, lty = 3, col.adj = 0.25, 
                   lwd = 1,...){
  n=length(x)
  xs <- rep(1:n, each = 2)[-2*n]
  ys <- c(1, rep(2:n, each = 2))
  xx <- x[xs]
  y.low <- y.L[ys]
  y.hi <- y.U[ys]
  
  xx=c(xx,rev(xx))
  yy=c(y.low,rev(y.hi))
  polygon(xx, yy, col = adjustcolor(bg, col.adj), border = col, 
          lty = lty, lwd = lwd)
}


# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_3/Model_Output/"))
alts=alts[!alts%in%c("Northern_Estuaries","Lake_Okeechobee")]
alts=c("ECB19","NA25f","PA25")
n.alts=length(alts)
alts.sort=c("ECB19","NA25f","PA25")

# cols.alts=c(grey.colors(2),wesanderson::wes_palette("Zissou1",1,"continuous"))
cols.alts=c("grey60","black","indianred1")
cols.alts2=cols.alts# c("#4D4D4D", "#E6E6E6", "#F21A00")
alts.sort2=c("ECB19","NA25f","PA25")
n.alts=length(alts.sort2)

# Lake Stage --------------------------------------------------------------

n.alts=length(alts.sort2)
lakeO.stage=data.frame()
for(i in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3/Model_Output/",alts.sort2[i],"/RSMBN_output.dss"))
  paths=paste0("/RSMBN/LOK/STAGE/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")
  tmp=data.frame(getFullTSC(dss_out,paths))
  tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
  rownames(tmp)<-NULL
  tmp$Alt=alts.sort2[i]
  lakeO.stage=rbind(tmp,lakeO.stage)
  print(i)
}
unique(lakeO.stage$Alt)
range(lakeO.stage$Date)

lakeO.stage$month=as.numeric(format(lakeO.stage$Date,'%m'))
lakeO.stage$day=as.numeric(format(lakeO.stage$Date,'%d'))
lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,'%Y'))
lakeO.stage$DoY=as.numeric(format(lakeO.stage$Date,'%j'))
lakeO.stage$WY=WY(lakeO.stage$Date)

lakeO.stage$Stage.m=ft.to.m(lakeO.stage$STAGE)
# lakeO.stage$recess_1day=with(lakeO.stage,ave(Stage.m,Alt2,FUN=function(x) c(rep(NA,1),diff(x,lag=1))))
lakeO.stage$recess_7day=with(lakeO.stage,ave(STAGE,Alt,FUN=function(x) c(rep(NA,6),diff(x,lag=6))))
# lakeO.stage$recess_30day=with(lakeO.stage,ave(Stage.m,Alt2,FUN=function(x) c(rep(NA,29),diff(x,lag=29))))

lakeO.stage$ExHigh=with(lakeO.stage,ifelse(Stage.m>ft.to.m(17),1,0))
lakeO.stage$ModHigh=with(lakeO.stage,ifelse(Stage.m>ft.to.m(16),1,0))
lakeO.stage$ExLow=with(lakeO.stage,ifelse(Stage.m<ft.to.m(10),1,0))
lakeO.stage$ModLow=with(lakeO.stage,ifelse(Stage.m<ft.to.m(11),1,0))

# Consecutive Events Stage ------------------------------------------------
highstg_consec=data.frame()
for(j in 1:length(alts.sort)){
  tmp=subset(lakeO.stage,Alt==alts.sort[j])
  tmp$stg17=0
  tmp$stg16=0
  tmp$stg11=0
  tmp$stg10=0
  for(i in 2:nrow(tmp)){
    # tmp$consec[i]=with(tmp,ifelse(Q_GT2100[i-1]>0&Q_GT2100[i]>0,1,0))
    tmp$stg16[i]=with(tmp,ifelse(ModHigh[i-1]==0&ModHigh[i]>0,1,
                                 ifelse(ModHigh[i-1]>0&ModHigh[i]>0,1,0)))
    tmp$stg17[i]=with(tmp,ifelse(ExHigh[i-1]==0&ExHigh[i]>0,1,
                                 ifelse(ExHigh[i-1]>0&ExHigh[i]>0,1,0)))
    tmp$stg11[i]=with(tmp,ifelse(ModLow[i-1]==0&ModLow[i]>0,1,
                                 ifelse(ModLow[i-1]>0&ModLow[i]>0,1,0)))
    tmp$stg10[i]=with(tmp,ifelse(ExLow[i-1]==0&ExLow[i]>0,1,
                                 ifelse(ExLow[i-1]>0&ExLow[i]>0,1,0)))
  }
  
  modhighstg=consec.startend(tmp$stg16>0)
  tmp$sum.stg16=0
  for(i in 1:length(modhighstg$ends)){
    tmp[modhighstg$ends[i],]$sum.stg16=with(tmp[c(modhighstg$starts[i]:modhighstg$ends[i]),],sum(stg16,na.rm=T))
  }
  exhighstg=consec.startend(tmp$stg17>0)
  tmp$sum.stg17=0
  for(i in 1:length(exhighstg$ends)){
    tmp[exhighstg$ends[i],]$sum.stg17=with(tmp[c(exhighstg$starts[i]:exhighstg$ends[i]),],sum(stg17,na.rm=T))
  }
  modlowstg=consec.startend(tmp$stg11>0)
  tmp$sum.stg11=0
  for(i in 1:length(modlowstg$ends)){
    tmp[modlowstg$ends[i],]$sum.stg11=with(tmp[c(modlowstg$starts[i]:modlowstg$ends[i]),],sum(stg11,na.rm=T))
  }
  exlowstg=consec.startend(tmp$stg10>0)
  tmp$sum.stg10=0
  for(i in 1:length(exlowstg$ends)){
    tmp[exlowstg$ends[i],]$sum.stg10=with(tmp[c(exlowstg$starts[i]:exlowstg$ends[i]),],sum(stg10,na.rm=T))
  }
  
  highstg_consec=rbind(tmp,highstg_consec)
  print(j)
}

bks=c(1,15,30,60,90,180,365)
rslt.stg16=ddply(highstg_consec,c("Alt","sum.stg16"),summarise,count.event=N.obs(sum.stg16))
rslt.stg16$cat=findInterval(rslt.stg16$sum.stg16,bks,left.open = FALSE,rightmost.closed = TRUE)
rslt.stg17=ddply(highstg_consec,c("Alt","sum.stg17"),summarise,count.event=N.obs(sum.stg17))
rslt.stg17$cat=findInterval(rslt.stg17$sum.stg17,bks,left.open = FALSE,rightmost.closed = TRUE)
rslt.stg11=ddply(highstg_consec,c("Alt","sum.stg11"),summarise,count.event=N.obs(sum.stg11))
rslt.stg11$cat=findInterval(rslt.stg11$sum.stg11,bks,left.open = FALSE,rightmost.closed = TRUE)
rslt.stg10=ddply(highstg_consec,c("Alt","sum.stg10"),summarise,count.event=N.obs(sum.stg10))
rslt.stg10$cat=findInterval(rslt.stg10$sum.stg10,bks,left.open = FALSE,rightmost.closed = TRUE)

rslt.stg16.sum=reshape2::dcast(subset(rslt.stg16,cat>0),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.stg17.sum=reshape2::dcast(subset(rslt.stg17,cat>0),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.stg11.sum=reshape2::dcast(subset(rslt.stg11,cat>0),cat~Alt,value.var="count.event",sum,na.rm=T)
rslt.stg10.sum=reshape2::dcast(subset(rslt.stg10,cat>0),cat~Alt,value.var="count.event",sum,na.rm=T)

# Survival/hazard Analysis ------------------------------------------------
# http://www.sthda.com/english/wiki/survival-analysis-basics
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
# https://rpubs.com/anshulkumar/WHAsurvival1
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model
# https://bioconnector.github.io/workshops/r-survival.html
library(survival)
library(survminer)

## survival analysis of moderate high events >90 days
# rslt.stg16=ddply(highstg_consec,c("CY","Alt","sum.stg16"),summarise,count.event=N.obs(sum.stg16))
# rslt.stg16=merge(rslt.stg16,data.frame(CY=1965:2016,CY.time=1:52),"CY")
# rslt.stg16$Modhigh=with(rslt.stg16,ifelse(sum.stg16>90,1,0))
rslt.stg16=ddply(highstg_consec,c("CY","Alt"),summarise,max.val=max(sum.stg16),min.val=min(sum.stg16))
rslt.stg16=merge(rslt.stg16,data.frame(CY=1965:2016,CY.time=1:52),"CY")
rslt.stg16$Modhigh=with(rslt.stg16,ifelse(max.val>90,1,0))

subset(highstg_consec,Alt=="PA25"&CY==1966&sum.stg16!=0)

# fit.ModHigh=survfit(formula=Surv(CY.time,Modhigh)~1,data=subset(rslt.stg16,Alt=="NA25f"))
fit.ModHigh=survfit(formula=Surv(CY.time,Modhigh)~Alt,data=rslt.stg16)

plot(fit.ModHigh)
sfit=summary(fit.ModHigh)

fit.ModHigh$strata
gr <- c(fit.ModHigh$strata)
ng <- max(length(gr), 1L)

# spl=strsplit(as.character(sfit$strata),split="=")
# sfit.ext=data.frame(Alt=sapply(spl,"[",2),
#            time=sfit$time,
#            surv=sfit$surv,
#            LCI=sfit$lower,
#            UCI=sfit$upper,
#            cumhaz=sfit$cumhaz)
mod.sfit.ext=with(fit.ModHigh,data.frame(
  order = rep(seq.int(ng), gr),
  Alt=rep(alts.sort2, gr),
  time=time,
  surv=surv,
  LCI=lower,
  UCI=upper,
  cumhaz=cumhaz))
mod.sfit.ext=merge(mod.sfit.ext,data.frame(CY=1965:2016,time=1:52),"time",all.x=T)

summary(fit.ModHigh,time=52)
round(summary(fit.ModHigh,time=52)$surv*100,1)
surv_diff.ModHigh=survdiff(Surv(CY.time,Modhigh)~Alt,data=rslt.stg16)
surv_diff.ModHigh
1 - pchisq(surv_diff.ModHigh$chisq, length(surv_diff.ModHigh$n) - 1)

rslt.stg16$Alt.num=as.numeric(factor(rslt.stg16$Alt,levels=c("ECB19","NA25f","PA25")))

cox.modhigh=coxph(Surv(CY.time,Modhigh)~Alt,data=rslt.stg16)
summary(cox.modhigh)

cox.modhigh=coxph(Surv(CY.time,Modhigh)~Alt,data=subset(rslt.stg16,Alt%in%c("NA25f","PA25")))
summary(cox.modhigh)
cox.modhigh=coxph(Surv(CY.time,Modhigh)~Alt.num,data=subset(rslt.stg16,Alt%in%c("NA25f","PA25")))
summary(cox.modhigh)
cox.zph(cox.modhigh)


## survival analysis of ex high events 
# rslt.stg17=ddply(highstg_consec,c("CY","Alt","sum.stg17"),summarise,count.event=N.obs(sum.stg17))
# rslt.stg17=merge(rslt.stg17,data.frame(CY=1965:2016,CY.time=1:52),"CY")
# rslt.stg17$Exhigh=with(rslt.stg17,ifelse(sum.stg17>1,1,0))
rslt.stg17=ddply(highstg_consec,c("CY","Alt"),summarise,max.val=max(sum.stg17),min.val=min(sum.stg17))
rslt.stg17=merge(rslt.stg17,data.frame(CY=1965:2016,CY.time=1:52),"CY")
rslt.stg17$Exhigh=with(rslt.stg17,ifelse(max.val>1,1,0))

# fit.ModHigh=survfit(formula=Surv(CY.time,Modhigh)~1,data=subset(rslt.stg16,Alt=="NA25f"))
fit.ExHigh=survfit(formula=Surv(CY.time,Exhigh)~Alt,data=rslt.stg17)

plot(fit.ExHigh)
sfit=summary(fit.ExHigh)

fit.ExHigh$strata
gr <- c(fit.ExHigh$strata)
ng <- max(length(gr), 1L)

ext.sfit.ext=with(fit.ExHigh,data.frame(
  order = rep(seq.int(ng), gr),
  Alt=rep(alts.sort2, gr),
  time=time,
  surv=surv,
  LCI=lower,
  UCI=upper,
  cumhaz=cumhaz))
ext.sfit.ext=merge(ext.sfit.ext,data.frame(CY=1965:2016,time=1:52),"time",all.x=T)


ggsurvplot(fit.ExHigh,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF","red"))

summary(fit.ExHigh,time=52)
round(summary(fit.ExHigh,time=52)$surv*100,1)
surv_diff.ExHigh=survdiff(Surv(CY.time,Exhigh)~Alt,data=rslt.stg17)
surv_diff.ExHigh
1 - pchisq(surv_diff.ExHigh$chisq, length(surv_diff.ExHigh$n) - 1)

rslt.stg17$Alt.num=as.numeric(factor(rslt.stg17$Alt,levels=c("ECB19","NA25f","PA25")))
cox.ExHigh=coxph(Surv(CY.time,Exhigh)~Alt,data=rslt.stg17)
summary(cox.ExHigh)

cox.ExHigh=coxph(Surv(CY.time,Exhigh)~Alt,data=subset(rslt.stg17,Alt%in%c("NA25f","PA25")))
summary(cox.ExHigh)
cox.ExHigh=coxph(Surv(CY.time,Exhigh)~Alt.num,data=subset(rslt.stg17,Alt%in%c("NA25f","PA25")))
summary(cox.ExHigh)
cox.zph(cox.ExHigh)

## survival analysis of mod low events 
rslt.stg11=ddply(highstg_consec,c("CY","Alt"),summarise,max.val=max(sum.stg11),min.val=min(sum.stg11))
rslt.stg11=merge(rslt.stg11,data.frame(CY=1965:2016,CY.time=1:52),"CY")
rslt.stg11$Modlow=with(rslt.stg11,ifelse(max.val>90,1,0))

# fit.ModHigh=survfit(formula=Surv(CY.time,Modhigh)~1,data=subset(rslt.stg16,Alt=="NA25f"))
fit.ModLow=survfit(formula=Surv(CY.time,Modlow)~Alt,data=rslt.stg11)

plot(fit.ModLow)
sfit=summary(fit.ModLow)

fit.ModLow$strata
gr <- c(fit.ModLow$strata)
ng <- max(length(gr), 1L)

modlow.sfit.ext=with(fit.ModLow,data.frame(
  order = rep(seq.int(ng), gr),
  Alt=rep(alts.sort2, gr),
  time=time,
  surv=surv,
  LCI=lower,
  UCI=upper,
  cumhaz=cumhaz))
modlow.sfit.ext=merge(modlow.sfit.ext,data.frame(CY=1965:2016,time=1:52),"time",all.x=T)

summary(fit.ModLow,time=52)
round(summary(fit.ModLow,time=52)$surv*100,1)
surv_diff.Modlow=survdiff(Surv(CY.time,Modlow)~Alt,data=rslt.stg11)
surv_diff.Modlow
1 - pchisq(surv_diff.Modlow$chisq, length(surv_diff.Modlow$n) - 1)

rslt.stg17$Alt.num=as.numeric(factor(rslt.stg17$Alt,levels=c("ECB19","NA25f","PA25")))
cox.ExHigh=coxph(Surv(CY.time,Modlow)~Alt,data=rslt.stg11)
summary(cox.ExHigh)

cox.ExHigh=coxph(Surv(CY.time,Modlow)~Alt,data=subset(rslt.stg11,Alt%in%c("NA25f","PA25")))
summary(cox.ExHigh)
cox.ExHigh=coxph(Surv(CY.time,Modlow)~Alt.num,data=subset(rslt.stg11,Alt%in%c("NA25f","PA25")))
summary(cox.ExHigh)
cox.zph(cox.ExHigh)



# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/FigX_survcurve_modHigh.png"),width=4.5,height=5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
par(family="serif",mar=c(1,2,0.5,1),oma=c(2.5,2,0.5,0.5),lwd=0.5);
layout(matrix(1:2,2,1))

plot(surv~CY,ext.sfit.ext,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
for(i in 1:3){
  tmp=subset(ext.sfit.ext,Alt==alts.sort2[i])
  # with(tmp,shaded.range(CY,LCI,UCI,bg=cols.alts2[i],lty=0))
  lines(surv~CY,tmp,lty=1,lwd=2,col=cols.alts[i],type="s")
  # with(tmp,poly.step(CY,UCI,LCI,bg=cols.alts[i],lty=0))
}
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Extreme-High Stage Events")
mtext(side=2,line=2.5,"Survival Probability")
legend("bottomleft",legend=alts.sort2,
       lty=1,lwd=2,col=cols.alts,
       # pch=22,pt.bg=adjustcolor(cols.alts,0.5),pt.cex=1.5,
       ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title=" Alternative")

plot(surv~CY,mod.sfit.ext,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
for(i in 1:3){
  tmp=subset(mod.sfit.ext,Alt==alts.sort2[i])
  # with(tmp,shaded.range(CY,LCI,UCI,bg=cols.alts2[i],lty=0))
  lines(surv~CY,tmp,lty=1,lwd=2,col=cols.alts[i],type="s")
  # with(tmp,poly.step(CY,UCI,LCI,bg=cols.alts[i],lty=0))
}
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Moderate-High Stage Events > 90 days")
mtext(side=2,line=2.5,"Survival Probability")
mtext(side=1,line=1.5,"Time (Year)")
dev.off()

# 7-day Recession Rate ----------------------------------------------------
date.fill=data.frame(expand.grid(CY=seq(min(lakeO.stage$CY),max(lakeO.stage$CY),1),
                                 DoY=1:366,
                                 Alt=alts.sort2))
snki.nest=seq(date.fun("1970-03-01"),date.fun("1970-06-15"),"1 days")

lakeO.stage$SNKI.period=with(lakeO.stage,ifelse(format(Date,"%m-%d")%in%format(snki.nest,"%m-%d"),1,0))
recess.dat=lakeO.stage[,c("Alt","CY","DoY","STAGE","SNKI.period","recess_7day")]
range(recess.dat$recess_7day,na.rm=T)

bks=c(min(recess.dat$recess_7day,na.rm=T),c(-0.16,-0.05,0.05,0.25),max(recess.dat$recess_7day,na.rm=T))
recess.dat$cat=as.factor(findInterval(recess.dat$recess_7day,bks,rightmost.closed = T,left.open = T))

ddply(recess.dat,c("Alt","cat"),summarise,min.val=min(recess_7day,na.rm=T),max.val=max(recess_7day,na.rm=T))

recess.dat=merge(recess.dat,date.fill,by=c("CY",'DoY','Alt'),all.y=T)
recess.dat=recess.dat[order(recess.dat$Alt,recess.dat$CY,recess.dat$DoY),]

recess.dat.snki=subset(recess.dat,SNKI.period==1)

rec.rate=ddply(subset(recess.dat.snki,SNKI.period==1),c("Alt","CY"),summarise,
               N.val=N.obs(Alt),
               recess.event=sum(cat==3,na.rm=T),
               min.val=min(ifelse(cat==3,recess_7day,NA),na.rm=T),
               max.val=max(ifelse(cat==3,recess_7day,NA),na.rm=T))
rec.rate$event.per=(rec.rate$recess.event/rec.rate$N.val)*100
rec.rate$event.per.score=with(rec.rate,ifelse(event.per<25,1,0))

ddply(rec.rate,"Alt",summarise,sum.recevent=sum(event.per.score,na.rm=T))

rec.rate.dec=ddply(rec.rate,"Alt",summarise,sum.recevent=sum(event.per.score,na.rm=T)/5.2)
rec.rate.dec$sum.recevent=round(rec.rate.dec$sum.recevent,2)
rec.rate.dec
rec.rate.dec$rec.score=round(1.7*log(rec.rate.dec$sum.recevent)-2.9,2)
rec.rate.dec$rec.score=ifelse(rec.rate.dec$rec.score<0,0,rec.rate.dec$rec.score)
# rec.score=data.frame(val=10:1,score=c(1.0,0.9,0.7,0.4,rep(0,6)))
# based on approximate log-linear model
rec.score=data.frame(val=seq(1,10,0.01),score=round(1.7*log(seq(1,10,0.01))-2.9,2))
rec.score$score[rec.score$score<0]=0
plot(score~val,rec.score)
points(rec.score~sum.recevent,rec.rate.dec,pch=21,bg="red",cex=2)
text(rec.rate.dec$sum.recevent,rec.rate.dec$rec.score+0.1,rec.rate.dec$Alt,cex=1,srt=90)
# rec.score=data.frame(val=seq(1,10,0.01),score=round(0.06*exp(seq(1,10,0.01)*0.3),2))
# plot(score~val,rec.score)
# rec.score$score[rec.score$val<5]=0

tmp=subset(rec.score,val%in%c(10:5))
tmp$score=round(tmp$score,1)
tmp

ddply(highstg_consec,c("Alt"),summarise,
      ExHigh=sum(sum.stg17>0,na.rm=T),
      ModHigh=sum(sum.stg16>90,na.rm=T),
      ModLow=sum(sum.stg11>90,na.rm=T),
      ExLow=sum(sum.stg10>0,na.rm=T))


dec.mean=ddply(highstg_consec,c("Alt"),summarise,
               ExHigh=sum(sum.stg17>0,na.rm=T)/5.2,
               ModHigh=sum(sum.stg16>90,na.rm=T)/5.2,
               ModLow=sum(sum.stg11>90,na.rm=T)/5.2,
               ExLow=sum(sum.stg10>0,na.rm=T)/5.2)
dec.mean
dec.mean[,2:ncol(dec.mean)]=round(dec.mean[,2:ncol(dec.mean)],2)
dec.mean$ExHigh.score=with(dec.mean,round(-0.16*ExHigh+1,2))
dec.mean$ModHigh.score=with(dec.mean,round(-0.16*ModHigh+1,2))
dec.mean$ModLow.score=with(dec.mean,round(-0.16*ModLow+1,2))
dec.mean$ExLow.score=with(dec.mean,round(-0.16*ExLow+1,2))

tbl.rslt=merge(dec.mean[,1:5],rec.rate.dec[,1:2],"Alt")
library(flextable)
tbl.rslt%>%
  flextable()%>%
  colformat_double(j=2:6,digits=1)%>%
  set_header_labels("Alt"="Alternative",
                    "ExHigh"="Extreme\nHigh Stage",
                    "ModHigh"="Moderate\nHigh Stage",
                    "ModLow"="Moderate\nLow Stage",
                    "ExLow"="Extreme\nLow Stage",
                    "sum.recevent"="Spring Recession Rate")%>%
  padding(padding=1,part="all")%>%
  flextable::font(fontname="Times New Roman",part="all")%>%
  fontsize(size=12,part="body")%>%
  fontsize(size=12,part="header")%>%
  bold(part="header")%>%
  align(j=2:6,align="center",part="all")%>%
  width(width=c(1,1,1,1,1,1)) %>%print(preview="docx")

# approximate fit from original scores
# stg.score=data.frame(val=seq(0,52,0.01),score=round(-0.1714*seq(0,52,0.01)+1.0143,2))
stg.score=data.frame(val=seq(0,52,0.01),score=round(-0.16*seq(0,52,0.01)+1,2))
stg.score$score[stg.score$score<0]=0
plot(score~val,stg.score)

tmp=subset(stg.score,val%in%c(1:6))
tmp$score=round(tmp$score,1)
tmp

# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/FigX_CompPMScores.png"),width=4,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,0.5,0.5),lwd=0.5);
layout(matrix(1:5,5,1))
ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

xlim.val=c(0,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(score~val,rec.score,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
x.val=seq(0,10,0.5)
score=round(1.7*log(x.val)-2.9,2)
score=ifelse(score<0,0,score)
lines(score~x.val,col="grey50",lwd=2)
points(rec.score~sum.recevent,rec.rate.dec,pch=21,bg=cols.alts2,cex=1.75)
text(rec.rate.dec$sum.recevent,rec.rate.dec$rec.score+0.3,rec.rate.dec$Alt,cex=0.9,srt=90)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Spring Recession",cex=0.8)
legend("topleft",legend=alts.sort2,
       pch=21,lty=0,lwd=0.1,pt.bg=cols.alts2,pt.cex=1.5,
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)

# xlim.val=c(1,10);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(score~val,stg.score,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
score=round(-0.16*x.val+1,2)
score=ifelse(score<0,0,score)
lines(score~x.val,col="grey50",lwd=2)
points(ExLow.score~ExLow,dec.mean,pch=21,bg=cols.alts2,cex=1.5)
# text(dec.mean$ExLow,dec.mean$ExHigh.score-0.25,dec.mean$Alt,cex=1,srt=90)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Extreme Low",cex=0.8)

plot(score~val,stg.score,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
lines(score~x.val,col="grey50",lwd=2)
points(ModLow.score~ModLow,dec.mean,pch=21,bg=cols.alts2,cex=1.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Moderate Low > 90 Days",cex=0.8)

plot(score~val,stg.score,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
lines(score~x.val,col="grey50",lwd=2)
points(ModHigh.score~ModHigh,dec.mean,pch=21,bg=cols.alts2,cex=1.5)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Moderate High > 90 Days",cex=0.8)

plot(score~val,stg.score,type="n",ylim=ylim.val,xlim=xlim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
lines(score~x.val,col="grey50",lwd=2)
points(ExHigh.score~ExHigh,dec.mean,pch=21,bg=cols.alts2,cex=1.5)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=1,"Extreme High",cex=0.8)
mtext(side=1,line=2,"Event Per Decade")
mtext(side=2,line=0.5,outer=T,"Score")
dev.off()

###
hydro.score.sum=merge(dec.mean,rec.rate.dec,"Alt")

vars=c("ExHigh.score","ModHigh.score", "ModLow.score", "ExLow.score","rec.score")
hydro.score.sum=hydro.score.sum[,c("Alt",vars)]

hydro.score.sum$S_ExHigh=hydro.score.sum$ExHigh.score*5
hydro.score.sum$S_ModHigh=hydro.score.sum$ModHigh.score*5
hydro.score.sum$S_ModLow=hydro.score.sum$ModLow.score*4
hydro.score.sum$S_ExLow=hydro.score.sum$ExLow.score*4
hydro.score.sum$S_rec=hydro.score.sum$rec.score*3

vars=c("S_ExHigh", "S_ModHigh", "S_ModLow", "S_ExLow", "S_rec")
hydro.score.sum$S_i=rowSums(hydro.score.sum[,vars])/21
hydro.score.sum
round(hydro.score.sum$S_i,1)

cols.stack=adjustcolor(c(colorRampPalette(c("red","grey","blue"))(4),"yellow"),0.5)

# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/FigX_PMScore.png"),width=5,height=4,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(1,2,0.5,1),oma=c(2.5,2,0.5,0.5),lwd=0.5);
layout(matrix(1:2,1,2),widths=c(1,0.5))

x=barplot(t(hydro.score.sum[,vars])/21,
          col=cols.stack,
          border=adjustcolor("grey30",0.5),
          space=0.05,
          ylim=c(0,1),axes=F,ann=F)
text(x,hydro.score.sum$S_i,round(hydro.score.sum$S_i,2),pos=3,offset=0.2)
axis_fun(1,x,x,alts.sort2,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
axis_fun(4,c(0,1),c(0,1),c("Severe stress/damaging\nConditions","Healthy/Optimal\nCondtions"),cex=0.75,font=3)
mtext(side=2,line=2.5,"Weighted Score")
mtext(side=1,line=2,"Alternative")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=rev(c("Extreme High", "Moderate High","Moderate Low","Extreme Low","Recession Rate")),
       pch=22,lty=0,lwd=0.1,pt.bg=rev(cols.stack),pt.cex=2,col=adjustcolor("grey30",0.5),
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1,
       title.adj = 0,title=" Performance Measure")
dev.off()


# Return Interval ---------------------------------------------------------
## https://rpubs.com/cassiorampinelli/528388
library(lmom)
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

recur.fun.cdf=function(x){
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
  gumbel=cdfgum(sorted.values,para)
  
  #Estimating the parameters for Log Pearson type 3 distribution
  para3<-pelpe3(fit)
  LP3=cdfpe3(sorted.values,para3)
  
  return(list(dat.val=sorted.values,
              emp.cdf=1-p,
              gumbel.cdf=gumbel,
              LP3.cdf=LP3))
}

ann.peak=ddply(lakeO.stage,c("CY","Alt"),summarise,max.stg=max(STAGE,na.rm=T))

recur.ECB=recur.fun(ft.to.m(subset(ann.peak,Alt==alts.sort2[1])$max.stg))
recur.ECB.cdf=recur.fun.cdf(ft.to.m(subset(ann.peak,Alt==alts.sort2[1])$max.stg))
recur.NA25=recur.fun(ft.to.m(subset(ann.peak,Alt==alts.sort2[2])$max.stg))
recur.NA25.cdf=recur.fun.cdf(ft.to.m(subset(ann.peak,Alt==alts.sort2[2])$max.stg))
recur.PA25=recur.fun(ft.to.m(subset(ann.peak,Alt==alts.sort2[3])$max.stg))
recur.PA25.cdf=recur.fun.cdf(ft.to.m(subset(ann.peak,Alt==alts.sort2[3])$max.stg))

cor(recur.ECB$emp.rec.tim,recur.ECB$gumbel)^2
cor(recur.ECB$emp.rec.tim,recur.ECB$LP3)^2

cor(recur.NA25$emp.rec.tim,recur.NA25$LP3)^2
cor(recur.PA25$emp.rec.tim,recur.PA25$LP3)^2

# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/FigSX_returnInterval_plots.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(2.5,2,0.5,0.5));
layout(matrix(1:6,3,2))

ylim.val=c(3,6);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,1);by.x=0.2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(dat.val~gumbel.cdf,recur.ECB.cdf,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
points(dat.val~emp.cdf,recur.ECB.cdf,pch=21,bg="grey",lwd=0.1)
# lines(dat.val~gumbel.cdf,recur.ECB.cdf,lwd=2,col='red')
lines(dat.val~LP3.cdf,recur.ECB.cdf,lwd=2,col=adjustcolor('dodgerblue1',0.75))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
legend("bottomright",legend=c("Empirical Probability","Fitted Distribution - LP3 Probability"),
       pch=c(21,NA),lty=c(NA,1),lwd=c(0.1,2),col=c("black",adjustcolor('dodgerblue1',0.75)),
       pt.bg="grey",pt.cex=1.5,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)
mtext(side=3,line=-1.25,adj=0," ECB19")

plot(dat.val~gumbel.cdf,recur.ECB.cdf,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
points(dat.val~emp.cdf,recur.NA25.cdf,pch=21,bg="grey",lwd=0.1)
# lines(dat.val~gumbel.cdf,recur.NA25.cdf,lwd=2,col='red')
lines(dat.val~LP3.cdf,recur.NA25.cdf,lwd=2,col=adjustcolor('dodgerblue1',0.75))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,line=-1.25,adj=0," NA25f")

plot(dat.val~gumbel.cdf,recur.ECB.cdf,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
points(dat.val~emp.cdf,recur.PA25.cdf,pch=21,bg="grey",lwd=0.1)
# lines(dat.val~gumbel.cdf,recur.PA25.cdf,lwd=2,col='red')
lines(dat.val~LP3.cdf,recur.PA25.cdf,lwd=2,col=adjustcolor('dodgerblue1',0.75))
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,line=-1.25,adj=0," PA25")
mtext(side=1,line=2,"Cumulative Probability ",cex=1)
mtext(side=2,line=0.5,"Stage (m, NGVD29)",cex=1,outer=T)

ylim.val=c(3,6);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0.9,50);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor");#by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(dat.val~emp.rec.tim,recur.ECB,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,log="x",xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
points(dat.val~emp.rec.tim,recur.ECB,pch=21,bg="grey",lwd=0.1)
# lines(dat.val~gumbel,recur.ECB,lwd=2,col='red')
lines(dat.val~LP3,recur.ECB,lwd=2,col=adjustcolor('dodgerblue1',0.75))
xx=c(0.8,max(subset(recur.ECB,round(dat.val,2)<=round(ft.to.m(17),2))$LP3))
yy=c(rep(max(subset(recur.ECB,round(dat.val,2)<=round(ft.to.m(17),2))$dat.val),2))
lines(xx,yy,lty=2)
xx=rep(max(subset(recur.ECB,round(dat.val,2)<=round(ft.to.m(17),2))$LP3),2)
yy=c(max(subset(recur.ECB,round(dat.val,2)<=round(ft.to.m(17),2))$dat.val),3)
lines(xx,yy,lty=2)
xx=c(0.8,max(subset(recur.ECB,round(dat.val,2)<=round(ft.to.m(16),2))$LP3))
yy=c(rep(max(subset(recur.ECB,round(dat.val,2)<=round(ft.to.m(16),2))$dat.val),2))
lines(xx,yy,lty=2)
xx=rep(max(subset(recur.ECB,round(dat.val,2)<=round(ft.to.m(16),2))$LP3),2)
yy=c(max(subset(recur.ECB,round(dat.val,2)<=round(ft.to.m(16),2))$dat.val),3)
lines(xx,yy,lty=2)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

plot(dat.val~emp.rec.tim,recur.NA25,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,log="x",xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
points(dat.val~emp.rec.tim,recur.NA25,pch=21,bg="grey",lwd=0.1)
# lines(dat.val~gumbel,recur.ECB,lwd=2,col='red')
lines(dat.val~LP3,recur.NA25,lwd=2,col=adjustcolor('dodgerblue1',0.75))
xx=c(0.8,max(subset(recur.NA25,round(dat.val,2)<=round(ft.to.m(17),2))$LP3))
yy=c(rep(max(subset(recur.NA25,round(dat.val,2)<=round(ft.to.m(17),2))$dat.val),2))
lines(xx,yy,lty=2)
xx=rep(max(subset(recur.NA25,round(dat.val,2)<=round(ft.to.m(17),2))$LP3),2)
yy=c(max(subset(recur.NA25,round(dat.val,2)<=round(ft.to.m(17),2))$dat.val),3)
lines(xx,yy,lty=2)
xx=c(0.8,max(subset(recur.NA25,round(dat.val,2)<=round(ft.to.m(16),2))$LP3))
yy=c(rep(max(subset(recur.NA25,round(dat.val,2)<=round(ft.to.m(16),2))$dat.val),2))
lines(xx,yy,lty=2)
xx=rep(max(subset(recur.NA25,round(dat.val,2)<=round(ft.to.m(16),2))$LP3),2)
yy=c(max(subset(recur.NA25,round(dat.val,2)<=round(ft.to.m(16),2))$dat.val),3)
lines(xx,yy,lty=2)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

plot(dat.val~emp.rec.tim,recur.PA25,type="n",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val,log="x",xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
points(dat.val~emp.rec.tim,recur.PA25,pch=21,bg="grey",lwd=0.1)
# lines(dat.val~gumbel,recur.ECB,lwd=2,col='red')
lines(dat.val~LP3,recur.PA25,lwd=2,col=adjustcolor('dodgerblue1',0.75))
xx=c(0.8,max(subset(recur.PA25,round(dat.val,2)<=round(ft.to.m(17),2))$LP3))
yy=c(rep(max(subset(recur.PA25,round(dat.val,2)<=round(ft.to.m(17),2))$dat.val),2))
lines(xx,yy,lty=2)
xx=rep(max(subset(recur.PA25,round(dat.val,2)<=round(ft.to.m(17),2))$LP3),2)
yy=c(max(subset(recur.PA25,round(dat.val,2)<=round(ft.to.m(17),2))$dat.val),3)
lines(xx,yy,lty=2)
xx=c(0.8,max(subset(recur.PA25,round(dat.val,2)<=round(ft.to.m(16),2))$LP3))
yy=c(rep(max(subset(recur.PA25,round(dat.val,2)<=round(ft.to.m(16),2))$dat.val),2))
lines(xx,yy,lty=2)
xx=rep(max(subset(recur.PA25,round(dat.val,2)<=round(ft.to.m(16),2))$LP3),2)
yy=c(max(subset(recur.PA25,round(dat.val,2)<=round(ft.to.m(16),2))$dat.val),3)
lines(xx,yy,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Return Interval (years)")
dev.off()

boot.n=500
boot.recur=data.frame()
pb=txtProgressBar(min=1,max=boot.n,style=3)
set.seed(78)
for(j in 1:n.alts){
for(i in 1:boot.n){

  tmp=subset(ann.peak,Alt==alts.sort2[j])
  tmp.samp=sample(tmp$max.stg,round(52*0.90,0))
  recur.tmp=recur.fun(tmp.samp)

rslt=data.frame(boot=i,Alt=alts.sort2[j],
           min16.emp=min(subset(recur.tmp,dat.val>=16)$emp.rec.tim,na.rm=T),
           min16.gumbel=min(subset(recur.tmp,dat.val>=16)$gumbel,na.rm=T),
           min16.LP3=min(subset(recur.tmp,dat.val>=16)$LP3,na.rm=T),
           min17.emp=min(subset(recur.tmp,dat.val>=17)$emp.rec.tim,na.rm=T),
           min17.gumbel=min(subset(recur.tmp,dat.val>=17)$gumbel,na.rm=T),
           min17.LP3=min(subset(recur.tmp,dat.val>=17)$LP3,na.rm=T))
boot.recur=rbind(rslt,boot.recur)
setTxtProgressBar(pb, i)
}
}
# https://blog.methodsconsultants.com/posts/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/
summary(boot.recur)
unique(boot.recur$Alt)

library(dunn.test)

boxplot(min16.emp~Alt,boot.recur)
boxplot(min17.emp~Alt,boot.recur)
with(boot.recur,dunn.test(min16.emp,Alt))
with(boot.recur,dunn.test(min17.emp,Alt))

boxplot(min16.LP3 ~Alt,boot.recur)
boxplot(min17.LP3 ~Alt,boot.recur,outline=F)

with(boot.recur,dunn.test::dunn.test(min16.LP3,Alt))
with(boot.recur,dunn.test::dunn.test(min17.LP3,Alt))

ddply(boot.recur,"Alt",summarise,mean.val=round(mean(min17.LP3),1),median.val=round(median(min17.LP3),1))
ddply(boot.recur,"Alt",summarise,mean.val=round(mean(min16.LP3),1),median.val=round(median(min16.LP3),1))

mean.RT=ddply(boot.recur,"Alt",summarise,mean.val=mean(min16.gumbel))
mean.RT$mean.val

# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/FigX_returnInterval.png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,2,0.5,1),oma=c(2,3,0.5,0.5));
layout(matrix(1:2,2,1))

ylim.val=c(5,45);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(min17.LP3 ~Alt,boot.recur,outline=F,ylim=ylim.val,ann=F,axes=F)
DT.min17=with(boot.recur,dunn.test(min17.LP3,Alt))
DT.lts=toupper(rcompanion::cldList(P.adjusted ~ comparison,data=DT.min17,threshold = 0.05)$Letter)
dunn.letters(3,1:3,x$stats[5,],DT.lts,"red",1)
axis_fun(1,1:3,1:3,NA)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"Annual Max Stage \u2265 5.18 m NGVD",cex=0.8)

ylim.val=c(2,6);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=boxplot(min16.LP3 ~Alt,boot.recur,outline=F,ylim=ylim.val,ann=F,axes=F)
DT.min16=with(boot.recur,dunn.test(min16.LP3,Alt))
DT.lts=toupper(rcompanion::cldList(P.adjusted ~ comparison,data=DT.min16,threshold = 0.05)$Letter)
dunn.letters(3,1:3,x$stats[5,],DT.lts,"red",1)
axis_fun(1,1:3,1:3,alts.sort2,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=0.5,"Return Period (years)\nLP3 Distribution",outer=T)
mtext(side=1,line=1.75,"Alternatives")
mtext(side=3,adj=0,"Annual Max Stage \u2265 4.88 m NGVD",cex=0.8)
dev.off()


# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/FigX_DailyStage.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,2,0.5,1),oma=c(2,1,0.5,0.5));
layout(matrix(1:4,2,2),widths=c(1,0.5))

ylim.val=c(2,6);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

xlim.val=date.fun(c("1965-01-01","1990-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"4 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(Stage.m~Date,lakeO.stage,type="n",xaxs="i",yaxs="i",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
abline(h=ft.to.m(c(17,16,11,10)),lty=2,col=adjustcolor("black",0.5))
for(i in 1:n.alts){
  lines(Stage.m~Date,subset(lakeO.stage,Alt==alts.sort2[i]),col=cols.alts2[i],lty=c(1,2,1)[i])
}
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,line=-1.25,adj=1,"A ")

xlim.val=date.fun(c("1991-01-01","2016-12-31"));xmaj=seq(xlim.val[1],xlim.val[2],"4 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
plot(Stage.m~Date,lakeO.stage,type="n",xaxs="i",yaxs="i",ann=F,axes=F,ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
abline(h=ft.to.m(c(17,16,11,10)),lty=2,col=adjustcolor("black",0.5))
for(i in 1:n.alts){
  lines(Stage.m~Date,subset(lakeO.stage,Alt==alts.sort2[i]),
        col=cols.alts2[i],lty=c(1,2,1)[i],
        lwd=c(1,1,1.5)[i])
}
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Year")
mtext(side=2,outer=T,line=-0.5,"Stage (m, NGVD29)")
mtext(side=3,line=-1.25,adj=1,"B ")

xlim.val=c(0,1);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(0:1,0:1,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort2[1])$Stage.m),lines(1-proportion,value,col=cols.alts2[1],lty=1,lwd=1))
with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort2[2])$Stage.m),lines(1-proportion,value,col=cols.alts2[2],lty=2,lwd=1))
with(ecdf_fun(subset(lakeO.stage,Alt==alts.sort2[3])$Stage.m),lines(1-proportion,value,col=cols.alts2[3],lwd=2))
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=1.5,"Stage (m, NGVD29)",cex=0.75)
mtext(side=1,line=2,"Proportion of Time\n\u2265 Stage Elevation",cex=0.75)
mtext(side=3,line=-1.25,adj=1,"C ")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c(alts.sort2,"Performance Measures"),
       lty=c(1,2,1,2),lwd=c(1,1,1.5,1),col=c(cols.alts2,adjustcolor("black",0.5)),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1,
       title.adj=0,title="Alternatives")
dev.off()



# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/LO_StageDuration_segments.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2.5,3.75,1,0.25));
layout(matrix(1:3,1,3,byrow=T))

xlim.val=c(0,1.05);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],0.1)
ylim.val=c(2,6);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(j in 1:n.alts){
  test=ecdf_fun(subset(lakeO.stage,Alt==alts.sort[j])$Stage.m)
  test$proportion=1-test$proportion
  plot(value~proportion,test,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(test,lines(proportion,value,col=adjustcolor(cols.alts2[j],0.5),lwd=2))
  
  prop.val=seq(0.1,0.9,0.1)
  for(i in 1:length(prop.val)){
    tmp=min(subset(test,proportion<prop.val[i])$value)
    segments(prop.val[i],0,prop.val[i],tmp,col=adjustcolor("red",0.25))
    points(prop.val[i],tmp,pch=21,bg=adjustcolor("red",0.25),col=adjustcolor("black",0.25))
    text(prop.val[i],tmp,pos=3,format(round(tmp,2),nsmall=2),cex=0.75)
  }
  axis_fun(1,xmaj,xmin,format(xmaj))
  if(j%in%c(1,5)){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
  box(lwd=1)
  mtext(side=3, adj=1,line=-1.25,paste0(alts.sort[j]," "))
}
mtext(side=2,line=1.5,outer=T,"Stage Elevation (m, NGVD29)")
mtext(side=1,line=1,outer=T,"Proportion of Time \u2265 Stage Elevation")
dev.off()