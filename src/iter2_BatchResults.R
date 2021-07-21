## 
## LOSOM
## Iteration 2 Batch Results
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

##GGPLOT theme defaults
theme_set(theme_minimal(base_size = 16))

# -------------------------------------------------------------------------
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

# -------------------------------------------------------------------------
rslt.basline=read.xlsx(paste0(data.path,"Iteration_2/Model_Output/_Batch_Results/LOSOM_Iteration2_AlternativeCC_19May2021.xlsx"),sheet=2,startRow = 13)[c(2,4),1:73]
rslt.basline[,3:73]=apply(rslt.basline[,3:73],2,as.numeric)
colnames(rslt.basline)<-c("Index","Model",paste0("PM",1:65),"zone_b_S77","zone_b_S308","sd_c_S77","mult_pulse","base_S77","qs_reg")

rslt=read.xlsx(paste0(data.path,"Iteration_2/Model_Output/_Batch_Results/LOSOM_Iteration2_AlternativeCC_19May2021.xlsx"),sheet=2,startRow = 22)[,1:73]
rslt[,3:73]=apply(rslt[,3:73],2,as.numeric)
colnames(rslt)<-c("Index","Model",paste0("PM",1:65),"zone_b_S77","zone_b_S308","sd_c_S77","mult_pulse","base_S77","qs_reg")
summary(rslt[,3:73])
rslt=subset(rslt,is.na(Model)==F)

plot(rslt$PM64)
plot(rslt$PM13)

## CRE Flow categories
vars=paste0("PM",c(53,54,13,14,51,52,55,56,57))
tmp=rslt[,c("Index",vars)]
tmp.melt=melt(tmp,id.vars = "Index")

CRE.Q.CC=data.frame(variable=vars,value=c(377,714,89+289,174+156,69,329,271,86,57))
CRE.Q.NA25=melt(rslt.basline[2,c("Index",vars)],id.vars = "Index")

par(family="serif",mar=c(6.5,3,0.25,1),oma=c(2,2,1,0.25));
ylim.val=c(0,1000);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val=c(49,1000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")#by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(value~variable,tmp.melt,outline=F,ann=F,axes=F,ylim=ylim.val,log="y")
points(1:9,c(377,714,89+289,174+156,69,329,271,86,57),pch=21,bg=adjustcolor(cols[alts.sort=="CC"],0.5),cex=1.25)
points(1:9,rslt.basline[2,vars],pch=21,bg=cols[alts.sort=="NA25"])
xlabs=c("Low (<750)","Optimal (750 - 2100)","Stress (2100 - 2600)","Damaging (>2600)","< 457cfs","457 - 750 cfs","2600 - 4500 cfs","4500 - 6500 cfs","> 6500 cfs")
axis_fun(1,1:9,1:9,xlabs,las=2)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=2,line=2.5,"Count of 14-d MA")
mtext(side=1,line=7,"CRE Flow Category")

ggplot(tmp.melt,aes(variable,value))+
  geom_jitter(alpha=0.5,shape=19,colour="grey")+
  geom_point(data=CRE.Q.CC,shape=21,fill=cols[alts.sort=="CC"],size=2)+
  geom_point(data=CRE.Q.NA25[,2:3],shape=21,fill=cols[alts.sort=="NA25"],size=2)

## 

## LOK Stage Envelope
vars=paste0("PM",c(37,38,39))
tmp=rslt[,c("Index",vars)]
tmp.melt=melt(tmp,id.vars = "Index")

LOK.env.CC=data.frame(variable=vars,value=c(0.36,0.23,0.40))
LOK.env.NA25=melt(rslt.basline[2,c("Index",vars)],id.vars = "Index")

par(family="serif",mar=c(3,3,0.25,1),oma=c(2,2,1,0.25));
ylim.val=c(0.1,0.7);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
boxplot(value~variable,tmp.melt,outline=F,ann=F,axes=F,ylim=ylim.val)
points(1:3,LOK.env.CC$value,pch=21,bg=adjustcolor(cols[alts.sort=="CC"],0.5),cex=1.25)
points(1:3,rslt.basline[2,vars],pch=21,bg=cols[alts.sort=="NA25"])
xlabs=c("% Below","% Within","% Above")
axis_fun(1,1:3,1:3,xlabs)
axis_fun(2,ymaj,ymin,format(ymaj*100))
box(lwd=1)
mtext(side=2,line=2.5,"Percent of exceedances")
mtext(side=1,line=7,"Relative to Stage Envelope")



rslt[order(-rslt$PM38),]
subset(rslt,PM38=max(rslt$PM38,na.rm=T))[1,]
