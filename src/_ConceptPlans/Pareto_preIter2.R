## 
## LOSOM
## Iteration 2 % Improvement eval (limited data)
## Using data from the 27k conceptual plans
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
library(zoo)
library(classInt)
library(Hmisc)
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
# library(dssrip)
# 
# RSM.sites=c("S79","S80")
# q.dat=data.frame()
# 
# alts=c("SPLC","ESLE")
# 
# for(j in 1:2){
#   dss_out=opendss(paste0(data.path,"Iteration_1/Model_Output/",alts[j],"/RSMBN/RSMBN_output.dss"))  
#   
#   for(i in 1:length(RSM.sites)){
#     paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
#     tmp=data.frame(getFullTSC(dss_out,paths))
#     tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
#     rownames(tmp)<-NULL
#     tmp$SITE=RSM.sites[i]
#     tmp$Alt=alts[j]
#     q.dat=rbind(tmp,q.dat)
#     print(i)
#   }
# }
# 
# head(q.dat,20)
# q.dat=q.dat[order(q.dat$Alt,q.dat$SITE,q.dat$Date),]
# q.dat$CY=as.numeric(format(q.dat$Date,'%Y'))
# 
# q.dat$Alt_SITE=paste(q.dat$Alt,q.dat$SITE,sep="_")
# q.dat$Q.14=with(q.dat,ave(FLOW,Alt_SITE,FUN=function(x) c(rep(NA,13),rollapply(x,width=14,FUN=function(x)mean(x,na.rm=T)))))
# q.dat$low.N=with(q.dat,ifelse(SITE=="S79"&Q.14<750,1,ifelse(SITE=="S80"&Q.14<150,1,0)))
# q.dat$opt.N=with(q.dat,ifelse(SITE=="S79"&Q.14>=750&Q.14<2100,1,ifelse(SITE=="S80"&Q.14>=150&Q.14<1400,1,0)))
# 
# test=ddply(q.dat,c("CY",'Alt',"SITE"),summarise,N.low=sum(low.N,na.rm=T),N.opt=sum(opt.N,na.rm=T))
# ddply(test,c("Alt","SITE"),summarise,N.opt=max(N.opt,na.rm=T))
# 
# PM50=with(subset(q.dat,SITE=="S79"),sum(ifelse(ifelse(Q.14<750,1,0),na.rm=T))
# PM51=with(subset(q.dat,SITE=="S79"&Alt=="SPLC"),sum(ifelse(Q.14>=750&Q.14<2100,1,0),na.rm=T))
# 
# plot(Q.14~Date,subset(q.dat,SITE=="S79"))
# abline(h=750)

# -------------------------------------------------------------------------
rslt.basline=read.xlsx(paste0(data.path,"Conceptual_Plan/LOSOM_CPA_PARETO_27k_2020Nov10.xlsx"),sheet=2,startRow = 13)[1:2,1:64]
rslt.basline[,3:64]=apply(rslt.basline[,3:64],2,as.numeric)
colnames(rslt.basline)<-c("Pindex","Model_Index",paste0("PM",1:62))

rslt=read.xlsx(paste0(data.path,"Conceptual_Plan/LOSOM_CPA_PARETO_27k_2020Nov10.xlsx"),sheet=2,startRow = 16)[,1:64]
summary(rslt[,3:21])

## selected plans
plans.model.index=c("4C-1_3307","4C-1_1655","4C-3_3840")
plans.model.index2=data.frame(Model_Index=plans.model.index,Plan.Name=c("AA","CC","DD"))

rslt.screen=subset(rslt,Model_Index%in%plans.model.index)
rslt.screen=rslt.screen[match(plans.model.index,rslt.screen$Model_Index),]

## Percent difference
rslt.basline$Model_Index=rslt.basline$Pindex
rslt.basline$Pindex=NA
all.plans=rbind(rslt.basline,rslt.screen)

all.plans[,c(paste0("PM",c(6:9)))]=all.plans[,c(paste0("PM",c(6:9)))]*100

per.diff.fwo=all.plans[,1:2]
for(i in 3:64){
  rslt=data.frame(tmp=round(((all.plans[,i]- all.plans[2,i])/ all.plans[2,i])*100,2))
  colnames(rslt)<-names(all.plans)[i]
  per.diff.fwo=cbind(per.diff.fwo,rslt)
  
}
per.diff.fwo[1,3:64]<-NA

per.diff.ecb=all.plans[,1:2]
for(i in 3:64){
  rslt=data.frame(tmp=round(((all.plans[,i]- all.plans[1,i])/ all.plans[1,i])*100,2))
  colnames(rslt)<-names(all.plans)[i]
  per.diff.ecb=cbind(per.diff.ecb,rslt)
  
}
per.diff.ecb[1,3:64]<-NA


##
## 
all.plans
per.diff.fwo
est.metric=data.frame(Estuary=c(rep("CRE",3),rep("SLE",3)),variable=paste0("PM",c(50,51,15,57,58,17)),Cat=rep(c("Low","Opt","Dam"),2))

per.diff.fwo=merge(per.diff.fwo,plans.model.index2,"Model_Index")

idvars=c("Model_Index","Plan.Name")
est.met.comp=melt(per.diff.fwo[,c(idvars,est.metric$variable)],id.var=idvars)
est.met.comp=merge(est.met.comp,est.metric,"variable")
est.met.comp$Cat=factor(est.met.comp$Cat,levels = c('Low','Opt','Dam'))
est.met.comp$Plan.Name=factor(est.met.comp$Plan.Name,levels=c("AA","CC","DD"))

est.met.comp2=reshape2::dcast(est.met.comp,Plan.Name+Cat~Estuary,value.var="value",mean)

ylim.val=c(-75,75);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Pre-Iteration_2/Iter2_Est_PlanCompare.png"),width=6.5,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:5,1,5,byrow=T),widths=c(1,1,1,1,0.5))

pln="AA"
x=barplot(t(subset(est.met.comp2,Plan.Name==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(subset(est.met.comp2,Plan.Name==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("dodgerblue1","indianred1"),add=T,xaxt="n")
x.val=x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,c("Low","Optimal","High"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
with(subset(plans.model.index2,Plan.Name==pln),mtext(side=3,adj=0,paste0("Plan ",Plan.Name," (",Model_Index,")"),cex=0.5))
mtext(side=2,line=2,"Percent Difference from FWO",cex=0.8)

x=barplot(t(subset(est.met.comp2,Plan.Name=="AA")[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x.val=x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,c("Low","Optimal","High"),line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=3,adj=0,"Plan BB",cex=0.5)

pln="CC"
x=barplot(t(subset(est.met.comp2,Plan.Name==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(subset(est.met.comp2,Plan.Name==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("dodgerblue1","indianred1"),add=T,xaxt="n")
x.val=x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,c("Low","Optimal","High"),line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
with(subset(plans.model.index2,Plan.Name==pln),mtext(side=3,adj=0,paste0("Plan ",Plan.Name," (",Model_Index,")"),cex=0.5))

pln="DD"
x=barplot(t(subset(est.met.comp2,Plan.Name==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(subset(est.met.comp2,Plan.Name==pln)[,3:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("dodgerblue1","indianred1"),add=T,xaxt="n")
x.val=x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,c("Low","Optimal","High"),line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=1)
with(subset(plans.model.index2,Plan.Name==pln),mtext(side=3,adj=0,paste0("Plan ",Plan.Name," (",Model_Index,")"),cex=0.5))
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

# ggplot(est.met.comp,aes(fill=Estuary,y=value,x=Cat))+
#   geom_bar(position="dodge",stat="identity")+
#   geom_hline(yintercept=0)+
#   facet_wrap(~Plan.Name)+
#   theme_bw()+
#   # scale_fill_manual(values=rev(cols),labels=c("Fe + Al","Ca + Mg"))+
#   scale_y_continuous(breaks=seq(-60,60,30))+
#   scale_fill_manual(values=c("dodgerblue1","indianred1"))+
#   labs(fill="Estuary",
#        x="Discharge Category",
#        y="Percent Difference from FWO",
#        subtitle="Plan AA (Conceptual Plan 4C-1_1655)")+
#   theme(text=element_text(family="serif"))

all.plans[,c("Model_Index","Pindex",paste0("PM",c(38,39,40)))]

tmp=per.diff.fwo[,c("Plan.Name",paste0("PM",c(38,39,40)))]
tmp.mt=tmp[1,]
tmp.mt[1,]<-NA
tmp.mt$Plan.Name="BB"
tmp.mt[,2:4]=0
tmp=rbind(tmp,tmp.mt)
tmp=tmp[order(tmp$Plan.Name),]

tmp.melt=melt(tmp,id.vars="Plan.Name")

ggplot(tmp.melt,aes(fill=variable,y=value,x=Plan.Name))+
  geom_bar(position="dodge",stat="identity")+
  geom_hline(yintercept=0)+
  theme_bw()+
  # scale_fill_manual(values=rev(cols),labels=c("Fe + Al","Ca + Mg"))+
  scale_y_continuous(breaks=seq(-60,60,30))+
  scale_fill_manual(values=c("dodgerblue1","indianred1"))+
  labs(fill="Estuary",
       x="Discharge Category",
       y="Percent Difference from FWO",
       subtitle="Plan AA (Conceptual Plan 4C-1_1655)")+
  theme(text=element_text(family="serif"))


ylim.val=c(-30,30);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Pre-Iteration_2/Iter2_Lake_PlanCompare.png"),width=6,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

x=barplot(t(tmp[,2:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(tmp[,2:4]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("dodgerblue1","forestgreen","indianred1"),add=T,xaxt="n")
x.val=x[2,] #x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,c("AA","BB","CC","DD"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Plan Name")
mtext(side=2,line=2,"Percent Difference from FWO",cex=0.8)
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

###

tmp=per.diff.fwo[,c("Plan.Name",paste0("PM",c(46,47)))]
tmp.mt=tmp[1,]
tmp.mt[1,]<-NA
tmp.mt$Plan.Name="BB"
tmp.mt[,2:3]=0
tmp=rbind(tmp,tmp.mt)
tmp=tmp[order(tmp$Plan.Name),]

tmp.melt=melt(tmp,id.vars="Plan.Name")


# png(filename=paste0(plot.path,"Pre-Iteration_2/Iter2_FlowSouth_PlanCompare.png"),width=6,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
layout(matrix(1:2,1,2,byrow=T))

ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(tmp$PM46,beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(tmp$PM46,beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("grey"),add=T,xaxt="n")
axis_fun(1,x,x,c("AA","BB","CC","DD"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Percent Difference from FWO",cex=0.8)
mtext(side=3,adj=0,"Flow South (S351 & S354)")

ylim.val=c(0,20);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(tmp$PM47,beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(tmp$PM47,beside=T,
          ylim=ylim.val,axes=F,ann=F,col=c("grey"),add=T,xaxt="n")
axis_fun(1,x,x,c("AA","BB","CC","DD"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"STA Outflow (STA-2 & STA-3/4)")
mtext(side=1,outer=T,line=0.75,"Plan Name")

dev.off()
