## 
## LOSOM - WQ evaluation
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

##GGPLOT theme defaults
theme_set(theme_minimal(base_size = 16))

# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
# alts=alts[!(alts%in%c("SPEF","SPAS","SPLC"))];# removed the Everglades Foundation, Audubon and Lake Commnuities alteratives
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))


# Discharge ---------------------------------------------------------------
RSM.sites=c("S77","S78","S79","S80","S308","S351","S352","S354","S77_QFC","S308_QFC","S308BF")
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

tmp=dcast(q.dat,Alt+Date+CY~SITE,value.var="FLOW",function(x)sum(cfs.to.acftd(x),na.rm=T))
tmp$S77_S308BF_per=with(tmp,S308BF/S77)
head(subset(tmp,S77_S308BF_per>1))

tflow.ann=dcast(q.dat,Alt+CY~SITE,value.var="FLOW",function(x)sum(cfs.to.acftd(x),na.rm=T))
tflow.ann$S77_S308BF_per=with(tflow.ann,S308BF/S77)
head(tflow.ann)

ylim.val=c(0,2600e3);by.y=1000e3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,180e3);by.x=50e3;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
# png(filename=paste0(plot.path,"Iteration_2/S308BF_S77_CY_corr.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:8),2,4,byrow=T))
par(family="serif",mar=c(1,1,0.25,0.5),oma=c(3,3,1,0.25));

for(i in 1:8){
plot(S77~S308BF,tflow.ann,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
  abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
  points(S77~S308BF,subset(tflow.ann,Alt==alts.sort[i]),pch=21,bg=adjustcolor(cols[i],0.5),col=adjustcolor("black",0.5),lwd=0.1,cex=1.25)
mod=loess(S77~S308BF,subset(tflow.ann,Alt==alts.sort[i]))
x.val=with(subset(tflow.ann,Alt==alts.sort[i]),seq(min(S308BF),max(S308BF),length.out=200))
mod.pred=predict(mod,data.frame(S308BF=x.val),se=T)
shaded.range(x.val,
             mod.pred$fit+qt(0.75,mod.pred$df)*mod.pred$se,
             mod.pred$fit-qt(0.75,mod.pred$df)*mod.pred$se,
             bg="forestgreen",lty=0)
lines(x.val,mod.pred$fit,lwd=2,col=adjustcolor("forestgreen",0.5))
if(i%in%c(5:8)){axis_fun(1,xmaj,xmin,xmaj/1000,line=-0.5)}else{axis_fun(1,xmaj,xmin,NA)}
if(i%in%c(1,5)){axis_fun(2,ymaj,ymin,ymaj/1000)}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
mtext(side=3,adj=1,line=-1.25,paste0(alts.sort[i]," "))
}
mtext(side=1,line=1,outer=T,"S308 Backflow (x1000 Ac-Ft CY\u207B\u00B9)")
mtext(side=2,line=1.5,outer=T,"S77 (x1000 Ac-Ft CY\u207B\u00B9)")
dev.off()

ylim.val=c(0,2.5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1965,2016);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
# png(filename=paste0(plot.path,"Iteration_2/S308BF_S77_CY_TS.png"),width=5,height=6,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:6),6,1,byrow=T))
par(family="serif",mar=c(1,2,0.25,1),oma=c(2,3,1,0.25));

for(i in 3:8){
plot(S77_S308BF_per~CY,tflow.ann,ylim=ylim.val,xlim=xlim.val,axes=F,ann=F,type="n")
  abline(h=ymaj,v=xmaj,lty=2,col=adjustcolor("grey",0.5))
lines(S77_S308BF_per~CY,subset(tflow.ann,Alt==alts.sort[1]),col=cols[1])
lines(S77_S308BF_per~CY,subset(tflow.ann,Alt==alts.sort[2]),col=cols[2],lty=2)
lines(S77_S308BF_per~CY,subset(tflow.ann,Alt==alts.sort[i]),col=adjustcolor(cols[i],0.5),lwd=2)
if(i==8){axis_fun(1,xmaj,xmin,xmaj,line=-0.5)}else{axis_fun(1,xmaj,xmin,NA)}
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
legend("topright",legend=c(alts.sort[c(1,2,i)]),
       lty=c(1,2,1),lwd=c(1,1,2),col=cols[c(1,2,i)],
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
}
mtext(side=1,line=2,"Calendar Year")
mtext(side=2,line=1,outer=T,"S308 Backflow / S77")
dev.off()



tflow.ann=ddply(q.dat,c("Alt","CY","SITE"),summarise,TFlow.kacft=sum(cfs.to.acftd(FLOW),na.rm=T)/1000)

POS.mean.tflow.ann=dcast(tflow.ann,Alt~SITE,value.var="TFlow.kacft",mean)
POS.mean.tflow.ann=POS.mean.tflow.ann[match(alts.sort,POS.mean.tflow.ann$Alt),]

POS.mean.tflow.ann%>%
  flextable()%>%
  padding(padding=0.1)%>%
  colformat_double(j=2:12,digits=1,big.mark="",na_str="---")%>%print(preview="docx")
