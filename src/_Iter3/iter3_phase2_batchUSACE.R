## 
## LOSOM
##
## Iteration 3 - Phase 2
## Batch Results
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

## Paths
wd="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]


# -------------------------------------------------------------------------
dat1=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch1_13Oct2021.csv"),skip=22)[,1:185]
dat2=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch2_13Oct2021.csv"),skip=22)[,1:185]
dat3=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch3_13Oct2021.csv"),skip=22)[,1:185]

baselines=read.csv(paste0(data.path,"Iteration_3_Batch/LOSOM_Iter3_Batch1_13Oct2021.csv"),skip=17,header=F)[1:3,1:185]
colnames(baselines)<-names(dat1)

baselines[,3:ncol(baselines)]=sapply(baselines[,3:ncol(baselines)],FUN=function(x) as.numeric(sub("%","",x)))

dat1$batch=1
dat2$batch=2
dat3$batch=3

# names(dat1)
# names(dat2)
# names(dat3)

dat=rbind(
  subset(dat1,is.na(Index)==F),
  subset(dat2,is.na(Index)==F),
  subset(dat3,is.na(Index)==F)
  )
unique(dat$Model)
## Dealing with character values as percent
# test=dat[1,]
# test[,c(7:11,14:16,121:126,130:144)]=as.numeric(sub("%","",test[,c(7:11,14:16,121:126,130:144)]))
# test2=dat[2,]
# sapply(test2[,3:ncol(dat)],FUN=function(x) as.numeric(sub("%","",x)))

dat[,3:ncol(dat)]=sapply(dat[,3:ncol(dat)],FUN=function(x) as.numeric(sub("%","",x)))

# USACE presentation - 2021-10-26 -----------------------------------------

dat$StressDam_lake.cre=rowSums(dat[,c("PM37","PM38")])
dat$StressDam_lake.sle=rowSums(dat[,c("PM86","PM87")])

dat$Stress_Basin.cre=with(dat,PM32-PM37)
dat$Dam_Basin.cre=with(dat,PM33-PM38)
dat$Stress_Basin.sle=with(dat,PM82-PM86)
dat$Dam_Basin.sle=with(dat,PM83-PM87)

dat$total.pen=rowSums(dat[,c("PM10","PM11")])

baselines$StressDam_lake.cre=rowSums(baselines[,c("PM37","PM38")])
baselines$StressDam_lake.sle=rowSums(baselines[,c("PM86","PM87")])
baselines$Stress_Basin.cre=with(baselines,PM32-PM37)
baselines$Dam_Basin.cre=with(baselines,PM33-PM38)
baselines$Stress_Basin.sle=with(baselines,PM82-PM86)
baselines$Dam_Basin.sle=with(baselines,PM83-PM87)
baselines$total.pen=rowSums(baselines[,c("PM10","PM11")])

usace.mods=c(173186,261768,262200,125463,138722,279349,168491,260467)
usace.dat=subset(dat,Index%in%usace.mods)

usace.dat$plot.ID=1:nrow(usace.dat)

## relative to FWO
range(((usace.dat$PM88-subset(baselines,Model=="NA25")$PM88)/subset(baselines,Model=="NA25")$PM88)*100)
range(((usace.dat$PM40-subset(baselines,Model=="NA25")$PM40)/subset(baselines,Model=="NA25")$PM40)*100)
((usace.dat$PM88-subset(baselines,Model=="CCTSP")$PM88)/subset(baselines,Model=="CCTSP")$PM88)*100

##
# vars=c(paste0("PM",c(30,31,37,38,36,80,81,86,87,85,88,40,39,20)),"total.pen")
vars=c(paste0("PM",c(30,31,37,38,36,80,81,86,87,85,88,40,39,20,5,6)),"total.pen")
baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
# write.csv(baseline2,paste0(export.path,"Iteration3/Iter3P2_baselines.csv"),row.names = F)
tmp.dat=rbind(subset(baseline2,Model%in%c("CCTSP"))[,c("Index",vars)],usace.dat[,c("Index",vars)])
# write.csv(tmp.dat,paste0(export.path,"Iteration3/Iter3P2_USACEselect.csv"),row.names = F)

FWO.compare=data.frame(Index=tmp.dat$Index)
for(i in 1:length(vars)){
FWO.val=subset(baseline2,Model=="NA25")[,vars[i]]
val=((tmp.dat[,vars[i]]-FWO.val)/FWO.val)*100
tmp=data.frame(val=val)
colnames(tmp)=paste0(vars[i],".FWO")
FWO.compare=cbind(FWO.compare,tmp)
}
FWO.compare

PM.xwalk=data.frame(PM=vars,
                    Descript=c(paste("CRE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               paste("SLE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               "S308 Regulatory Flow","S77 Regulatory Flow","CRE MFL","LOK MFL","LOK >17 Ft","LOK >16 Ft","LOK - Total Stage Envelope Penalty"))
PM.xwalk$variable=paste0(PM.xwalk$PM,".FWO")
FWO.compare2=reshape2::melt(FWO.compare,id.vars="Index")
FWO.compare2=merge(FWO.compare2,PM.xwalk[,c("variable","Descript")])
FWO.compare2.xtab=reshape2::dcast(FWO.compare2,Descript~Index,value.var = "value",fun.aggregate=function(x) round(mean(x),1))
FWO.compare2.xtab=FWO.compare2.xtab[,c("Descript",tmp.dat$Index)]
FWO.compare2.xtab=FWO.compare2.xtab[match(PM.xwalk$Descript,FWO.compare2.xtab$Descript),]

cols=adjustcolor(c("lightgreen",rep("white",6),"indianred1"),0.5)
rank(FWO.compare2.xtab[1,3:10])

#cols=adjustcolor(colorRampPalette(c("lightgreen","seashell","indianred"))(8),0.5)


FWO.compare2.xtab%>%
  flextable()%>%
  padding(padding=1,part="all")%>%
  fontsize(size=8,part="body")%>%
  fontsize(size=9,part="header")%>%
  font(fontname="Times New Roman",part="all")%>%
  autofit()%>%
  add_header("CC"="Model Index",
             "125463"="Model Index",
             "138722"="Model Index",
             "168491"="Model Index",
             "173186"="Model Index",
             "260467"="Model Index", 
             "261768"="Model Index",
             "262200"="Model Index",
             "279349"="Model Index")%>%
  merge_h(part="header")%>%
  align(align="center",part="header")%>%
  align(align="center",part="all")%>%
  align(j=1,align="left",part="all")%>%
  bg(j=3:10,i=1,bg=cols[rank(FWO.compare2.xtab[1,3:10])])%>%
  bg(j=3:10,i=2,bg=cols[rank(-FWO.compare2.xtab[2,3:10])])%>%
  bg(j=3:10,i=3,bg=cols[rank(FWO.compare2.xtab[3,3:10])])%>%
  bg(j=3:10,i=4,bg=cols[rank(FWO.compare2.xtab[4,3:10])])%>%
  bg(j=3:10,i=5,bg=cols[rank(FWO.compare2.xtab[5,3:10])])%>%
  bg(j=3:10,i=6,bg=cols[rank(FWO.compare2.xtab[6,3:10])])%>%
  bg(j=3:10,i=7,bg=cols[rank(-FWO.compare2.xtab[7,3:10])])%>%
  bg(j=3:10,i=8,bg=cols[rank(FWO.compare2.xtab[8,3:10])])%>%
  bg(j=3:10,i=9,bg=cols[rank(FWO.compare2.xtab[9,3:10])])%>%
  bg(j=3:10,i=10,bg=cols[rank(FWO.compare2.xtab[10,3:10])])%>%
  bg(j=3:10,i=11,bg=cols[rank(FWO.compare2.xtab[11,3:10])])%>%
  bg(j=3:10,i=12,bg=cols[rank(FWO.compare2.xtab[12,3:10])])%>%
  bg(j=3:10,i=13,bg=cols[rank(FWO.compare2.xtab[13,3:10])])%>%
  bg(j=3:10,i=14,bg=cols[rank(FWO.compare2.xtab[14,3:10])])%>%
  bg(j=3:10,i=15,bg=cols[rank(FWO.compare2.xtab[15,3:10])])%>%
  footnote(j=1,part="header",ref_symbols = "",value=as_paragraph("Percent Difference relative to FWO (NA25)"))%>%
  footnote(j=1,part="header",ref_symbols = "",value=as_paragraph("Green value indicates best and Red indicates worst for each metric across selected models"))%>%
  font(fontname="Times New Roman",part="all")%>%print("docx")


text.cex=0.7
# png(filename=paste0(plot.path,"Iteration3_Batch/SalEnv_USACEsubset.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:10,5,2,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(600,1000,500,200,100)
ylim.min=c(200,400,0,0,0)
by.y.val=c(200,250,250,100,25)
vars=paste0("PM",c(30,31,37,38,36))
labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress from LOK (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Extreme (>6500 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Count of 14-Day Periods",outer=T)

ylim.max=c(200,1000,150,150,200)
ylim.min=c(100,800,0,0,100)
by.y.val=c(50,100,50,50,50)
vars=paste0("PM",c(80,81,86,87,85))
labs=c("Low (<150 cfs)","Optimum (150 - 1400 cfs)","Stress from LOK (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Extreme (>4000 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
}

mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

text.cex=0.7
# png(filename=paste0(plot.path,"Iteration3_Batch/SalEnv2_USACEsubset.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:8,4,2,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(300,200,200,210)
ylim.min=c(0,0,0,0)
by.y.val=c(150,100,100,100)
vars=c("PM37","Stress_Basin.cre","PM38","Dam_Basin.cre")# paste0("PM",c(30,31,37,38,36))
labs=c("Stress from LOK (2100 - 2600 cfs)","Stress from Basin (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Damaging from Basin (>2600 cfs)")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}


ylim.max=c(150,310,150,500)
ylim.min=c(0,0,0,400)
by.y.val=c(50,150,50,50)
vars=c("PM86","Stress_Basin.sle","PM87","Dam_Basin.sle")# paste0("PM",c(30,31,37,38,36))
labs=c("Stress from LOK (1400 - 1700 cfs)","Stress from Basin (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Damaging from Basin (>1700 cfs)")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
}
mtext(side=2,line=0.25,"Count of 14-Day Periods",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()



cols.val2=c(rgb(255/255,255/255,0),rgb(143/255,188/255,143/255),rgb(100/255,149/255,237/255))
vars=paste0("PM",c(12,13,14))
ylim.val=c(0,60);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_EnvScore_BWA_USACE.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.5,1),oma=c(1.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),widths=c(1,0.25))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],usace.dat[,c("Index",vars)])
tmp2=tmp[,vars]

x=barplot(t(tmp[,vars]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=NA,border=NA,xaxt="n")
abline(h=ymaj,v=x[2,],lty=1,col=adjustcolor("grey",0.5),lwd=1)
abline(h=0,lwd=1)
x=barplot(t(tmp[,vars]),beside=T,
          ylim=ylim.val,axes=F,ann=F,col=cols.val2,add=T,xaxt="n")
with(tmp,text(x[1,],PM12/2,paste0(round(PM12,0),"%"),cex=0.7,srt=90,font=2))
with(tmp,text(x[2,],PM13/2,paste0(round(PM13,0),"%"),cex=0.7,srt=90,font=2))
with(tmp,text(x[3,],PM14/2,paste0(round(PM14,0),"%"),cex=0.7,srt=90,font=2))
x.val=x[2,] #x[1,]+(x[2,]-x[1,])/2
axis_fun(1,x.val,x.val,tmp$Index,line=-0.25,las=2,cex=0.8)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=3,"Model Index")
mtext(side=2,line=2,"Percent",cex=1)

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
legend(0.5,0.5,legend=c("% time Below\n(PM12)","% time Within\n(PM13)","% time Above\n(PM14)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols.val2,
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Stage Envelope")
dev.off()

cols.IMC=c(rgb(238,232,170,maxColorValue = 255),rgb(143,188,143,maxColorValue = 255))
vars=paste0("PM",c(10,11))
ylim.val=c(0,50000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_EnvScore_AllYrs_USACE.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.5,0.5),oma=c(1.5,2,0.5,0.5),lwd=0.5);
layout(matrix(c(1:2),1,2,byrow=T),widths=c(1,0.25))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],usace.dat[,c("Index",vars)])
tmp$total.pen=rowSums(tmp[,vars])
tmp2=tmp[,vars]

x=barplot(t(tmp2[,vars]),beside=F,ylim=ylim.val,col=NA,border=NA,axes=F,ann=F,names.arg=rep(NA,nrow(tmp2)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(t(tmp2[,vars]),beside=F,ylim=ylim.val,col=cols.IMC,axes=F,ann=F,names.arg=rep(NA,nrow(tmp2)),add=T)
with(tmp,text(x,PM10/2,round(PM10,0),cex=0.7,srt=90,font=2))
with(tmp,text(x,PM10+abs((PM10-total.pen)/2),round(PM11,0),cex=0.7,srt=90,font=2))
with(tmp,text(x,total.pen,round(total.pen,0),pos=3,cex=0.7))
axis_fun(1,x,x,tmp$Index,line=-0.25,las=2,cex=0.8)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3,"Stage Envelope Penalty Score")
mtext(side=1,line=3,"Model Index")

plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA)
mtext(side=3,line=-2,"Lake Okeechobee\nEnvelope Penalty Score\nAll Years",cex=0.7)
legend(0.5,0.5,legend=c("Lower Penalty\n(Below Envelope;\nPM10)","Upper Penalty\n(Above Envelope;\nPM11)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c(cols.IMC),
       pt.cex=2,ncol=1,cex=0.7,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title="Total Score")
mtext(side=1,line=-1,adj=1,"Iteration 3 - Batch Analysis\nSimulation Period of Record\n(1965-2016). Includes Normal\nand Recovery envelope.",cex=0.5)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_USACEsubset.png"),width=4,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:4,4,1,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(2,20,10,30)
ylim.min=c(0,0,0,10)
by.y.val=c(1,10,5,10)
vars=paste0("PM",c(5,6,7,13))
labs=c("% LOK Stage >17 Ft NGVD29","% LOK Stage >16 Ft NGVD29","% LOK Stage <10 Ft NGVD29","% within Stage Envelope")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"LOK")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/WS_USACEsubset.png"),width=4.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:6,6,1,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.min=c(0,0,0,0,0,0)
ylim.max=c(35,100,50,12,5,5)
by.y.val=ylim.max/2
vars=paste0("PM",c(124,126,127,130,131,132))
labs=c("LOSA Cutbacks","LOSA Duration","LOSA Severity","LOSA Sup. Demand not met EAA & Non-EAA","STOF - BCYP Demand Not Met", "SOTF - BR Demand Not Met")
xlabs=c("Weighted Avg %","Count","Score","Percent","Percent","Percent")
for(i in 1:6){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(usace.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(usace.dat$plot.ID,usace.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==6){axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  mtext(side=2,line=2.5,xlabs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Water Supply")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

cols=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))
vars=paste0("PM",c(21,40,88,118))
ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/AvgFloodControl_USACEsubset.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],usace.dat[,c("Index",vars)])

tmp2=tmp[,vars]

x=barplot(t(tmp2),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,nrow(tmp)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp2),beside=F,col=cols,ylim=ylim.val,axes=F,ann=F,names.arg =rep(NA,nrow(tmp)),add=T)
with(tmp,text(x,PM21/2,round(PM21,0),cex=0.7,col="white"))
with(tmp,text(x,PM21+(((PM40+PM21)-PM21)/2),round(PM40,0),cex=0.7))
with(tmp,text(x,(PM21+PM40)+(((PM40+PM21+PM88)-(PM21+PM40))/2),round(PM88,0),cex=0.7))
with(tmp,text(x,PM40+PM21+PM88+PM118,round(PM118,0),pos=3,offset=0.1,cex=0.7))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,tmp$Index,line=-0.25,las=2,cex=0.8);box(lwd=1)
mtext(side=2,line=2,"Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=2.75,"Model Index")

par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0,0,legend=c("Water Conservation Areas (PM21)","Caloosahatchee River (PM40)","St. Lucie River (PM88)","Lake Worth Lagoon (PM118)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols,
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=0.5)
text(1,0,"Iteration 3 screened batch results. Mean annual flood control\nreleases from Lake Okeechobee for the 52 year (1965 - 2016)\nsimulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Batch/Est_StressDam_USACEsubset.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
# layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

xlim.val=c(0,600);by.x=100;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,600);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(StressDam_lake.cre~StressDam_lake.sle,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(StressDam_lake.cre~StressDam_lake.sle,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(subset(usace.dat,!(Index%in%c("262200","279349"))),points(StressDam_lake.cre~StressDam_lake.sle,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),points(StressDam_lake.cre~StressDam_lake.sle,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),text(StressDam_lake.sle,StressDam_lake.cre,Index,pos=3,font=3,cex=0.75))
with(subset(usace.dat,Index=="279349"),points(StressDam_lake.cre~StressDam_lake.sle,pch=23,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="279349"),text(StressDam_lake.sle,StressDam_lake.cre,Index,pos=1,font=3,cex=0.75))
with(subset(baselines,Model=="NA25"),points(StressDam_lake.cre~StressDam_lake.sle,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(StressDam_lake.cre~StressDam_lake.sle,pch=16,col="black",cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2.5,"SLE Stress and Damaging From LOK\n(PM86+PM87)")
mtext(side=2,line=2.25,"CRE Stress and Damaging From LOK\n(PM37+PM38)")

# plot(0:1,0:1,axes=F,ann=F,type="n")
legend("topright",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","Model Index 262200","Model Index 279349","1:1 Line"),
       pch=c(19,16,15,21,22,23,NA),lwd=c(0.1,0.1,0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),adjustcolor("yellow",0.75),adjustcolor("yellow",0.75),NA),
       col=c("dodgerblue1","black","black","indianred1","black","black","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/Est_StressDam_USACEsubset_TSP.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
# layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

xlim.val=c(0,600);by.x=100;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,600);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(StressDam_lake.cre~StressDam_lake.sle,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(StressDam_lake.cre~StressDam_lake.sle,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(subset(usace.dat,!(Index%in%c("260467"))),points(StressDam_lake.cre~StressDam_lake.sle,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="260467"),points(StressDam_lake.cre~StressDam_lake.sle,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="260467"),text(StressDam_lake.sle,StressDam_lake.cre,Index,pos=1,font=3,cex=0.75))
with(subset(baselines,Model=="NA25"),points(StressDam_lake.cre~StressDam_lake.sle,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(StressDam_lake.cre~StressDam_lake.sle,pch=16,col="black",cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2.5,"SLE Stress and Damaging From LOK\n(PM86+PM87)")
mtext(side=2,line=2.25,"CRE Stress and Damaging From LOK\n(PM37+PM38)")

# plot(0:1,0:1,axes=F,ann=F,type="n")
legend("topright",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","Model Index 260467","1:1 Line"),
       pch=c(19,16,15,21,22,NA),lwd=c(0.1,0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),adjustcolor("yellow",0.75),NA),
       col=c("dodgerblue1","black","black","indianred1","black","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()


xlim.val=c(0,20);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,11);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_StageExt_USACEsubset.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
# layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

xlim.val=c(0,20);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,11);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM7~PM5,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM7~PM5,pch=19,col=adjustcolor("dodgerblue1",0.5)))
# with(usace.dat,points(PM7~PM5,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,!(Index%in%c("262200","279349"))),points(PM7~PM5,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),points(PM7~PM5,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),text(PM5,PM7,Index,pos=4,font=3,cex=0.75))
with(subset(usace.dat,Index=="279349"),points(PM7~PM5,pch=23,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="279349"),text(PM5,PM7,Index,pos=1,font=3,cex=0.75))

with(subset(baselines,Model=="NA25"),points(PM7~PM5,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM7~PM5,pch=16,col="black",cex=1.5))
# with(subset(usace.dat,Index%in%c(262200,279349)),text(PM5,PM7,Index,pos=2))
# with(subset(usace.dat,Index%in%c(262200,279349)),points(PM7~PM5,pch=21,col="purple",bg=adjustcolor("purple",0.75),cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"% of Time >17 Ft NGVD29 (PM5)")
mtext(side=2,line=2,"% of Time <10 Ft NGVD29 (PM7)")

# legend("topright",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","1:1 Line"),
#        pch=c(19,16,15,21,NA),lwd=c(0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,2),
#        pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),NA),col=c("dodgerblue1","black","black","indianred1","black"),
#        pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
legend("topright",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","Model Index 262200","Model Index 279349","1:1 Line"),
       pch=c(19,16,15,21,22,23,NA),lwd=c(0.1,0.1,0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),adjustcolor("yellow",0.75),adjustcolor("yellow",0.75),NA),
       col=c("dodgerblue1","black","black","indianred1","black","black","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

# plot(0:1,0:1,axes=F,ann=F,type="n")
# legend("topright",legend=c("Iteration","CC","NA25","USACE Selected Runs","1:1 Line"),
#        pch=c(19,16,15,21,NA),lwd=c(0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,2),
#        pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),NA),col=c("dodgerblue1","black","black","indianred1","black"),
#        pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_StageExt2_USACEsubset.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
# layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

xlim.val=c(0,40);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,11);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM7~PM6,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM7~PM6,pch=19,col=adjustcolor("dodgerblue1",0.5)))
# with(usace.dat,points(PM7~PM6,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,!(Index%in%c("262200","279349"))),points(PM7~PM6,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),points(PM7~PM6,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),text(PM6,PM7,Index,pos=3,font=3,cex=0.75))
with(subset(usace.dat,Index=="279349"),points(PM7~PM6,pch=23,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="279349"),text(PM6,PM7,Index,pos=1,font=3,cex=0.75))

with(subset(baselines,Model=="NA25"),points(PM7~PM6,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM7~PM6,pch=16,col="black",cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"% of Time >16 Ft NGVD29 (PM5)")
mtext(side=2,line=2,"% of Time <10 Ft NGVD29 (PM7)")

legend("topright",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","Model Index 262200","Model Index 279349","1:1 Line"),
       pch=c(19,16,15,21,22,23,NA),lwd=c(0.1,0.1,0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),adjustcolor("yellow",0.75),adjustcolor("yellow",0.75),NA),
       col=c("dodgerblue1","black","black","indianred1","black","black","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Batch/Est_mo_extreme_cnt.png"),width=4.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,0.5,1),lwd=0.5);
layout(matrix(1:2,2,1,byrow=F))
xlim.val=c(1,nrow(usace.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.val=c(0,35);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM79~plot.ID,usace.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
# abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(usace.dat,points(PM79~plot.ID,pch=21,bg=adjustcolor("dodgerblue1",0.5)))
abline(h=subset(baselines,Model=="CCTSP")$PM79,col="indianred1",lwd=1.5)
abline(h=subset(baselines,Model=="NA25")$PM79,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,NA,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"CRE - Extreme (>6500 cfs; PM79)")
legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
       pch=NA,lwd=2,lty=c(1,1),
       pt.bg=NA,col=c("grey","indianred1"),
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

ylim.val=c(40,50);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM117~plot.ID,usace.dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
# abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(usace.dat,points(PM117~plot.ID,pch=21,bg=adjustcolor("dodgerblue1",0.5)))
abline(h=subset(baselines,Model=="CCTSP")$PM117,col="indianred1",lwd=1.5)
abline(h=subset(baselines,Model=="NA25")$PM117,col="grey",lwd=1.5)
axis_fun(1,xmaj,xmaj,usace.dat$Index,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=3,adj=0,"SLE - Extreme (>4000 cfs; PM117)")

mtext(side=2,line=0,"Count of Months\nAvg Discharge > Extreme Threshold",outer=T)
mtext(side=1,line=3,"Model Index",outer=F)
dev.off()


labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress from LOK (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Extreme (>6500 cfs)")
# png(filename=paste0(plot.path,"Iteration3_Batch/CRE_RegDischarge_USACEsubset.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,0.5,1),oma=c(2.5,2,1,0.25),lwd=0.5);
layout(matrix(c(1:6),2,3,byrow=T))

xlim.val=c(200,800);by.x=300;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.val=c(200,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM30~PM40,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM30~PM40,pch=19,col=adjustcolor("dodgerblue1",0.5)))
# with(usace.dat,points(PM30~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,!(Index%in%c("262200","279349"))),points(PM30~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),points(PM30~PM40,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="279349"),points(PM30~PM40,pch=23,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(PM30~PM40,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM30~PM40,pch=16,col="black",cex=1.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,labs[1],cex=0.7)

ylim.val=c(350,1050);by.y=350;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM31~PM40,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM31~PM40,pch=19,col=adjustcolor("dodgerblue1",0.5)))
# with(usace.dat,points(PM31~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,!(Index%in%c("262200","279349"))),points(PM31~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),points(PM31~PM40,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="279349"),points(PM31~PM40,pch=23,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(PM31~PM40,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM31~PM40,pch=16,col="black",cex=1.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,labs[2],cex=0.7)

ylim.val=c(0,500);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM37~PM40,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM37~PM40,pch=19,col=adjustcolor("dodgerblue1",0.5)))
# with(usace.dat,points(PM37~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,!(Index%in%c("262200","279349"))),points(PM37~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),points(PM37~PM40,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="279349"),points(PM37~PM40,pch=23,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(PM37~PM40,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM37~PM40,pch=16,col="black",cex=1.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,labs[3],cex=0.7)

ylim.val=c(0,210);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM38~PM40,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM38~PM40,pch=19,col=adjustcolor("dodgerblue1",0.5)))
# with(usace.dat,points(PM38~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,!(Index%in%c("262200","279349"))),points(PM38~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),points(PM38~PM40,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="279349"),points(PM38~PM40,pch=23,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(PM38~PM40,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM38~PM40,pch=16,col="black",cex=1.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,labs[4],cex=0.7)

ylim.val=c(0,150);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM36~PM40,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM36~PM40,pch=19,col=adjustcolor("dodgerblue1",0.5)))
# with(usace.dat,points(PM36~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,!(Index%in%c("262200","279349"))),points(PM36~PM40,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(subset(usace.dat,Index=="262200"),points(PM36~PM40,pch=22,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(usace.dat,Index=="279349"),points(PM36~PM40,pch=23,col="black",bg=adjustcolor("yellow",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(PM36~PM40,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM36~PM40,pch=16,col="black",cex=1.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,labs[5],cex=0.7)

plot(0:1,0:1,ann=F,axes=F,type="n")

legend("center",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","Model Index 262200","Model Index 279349"),
       pch=c(19,16,15,21,22,23),lwd=c(0.1,0.1,0.1,0.1,0.1,0.1),lty=c(NA,NA,NA,NA,NA,NA),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),rep(adjustcolor("yellow",0.75),2)),
       col=c("dodgerblue1","black","black","indianred1",rep("black",2)),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj = 0,title="Iteration 3 - Phase 2 Batch Runs")

mtext(side=2,line=0.25,"Count of 14-Day Periods",outer=T)
mtext(side=1,line=0.25,"S-77 Reg. Discharge Volume (x1000 Ac-Ft Y\u207B\u00B9)",outer=T)
dev.off()



summary(dat[,paste0("PM",c(30,31,37,38,36))])
summary(baselines[,paste0("PM",c(30,31,37,38,36))])

# East Coast (Ben Hogarth) ------------------------------------------------
eastcoast=c(273238, 121211, 142495, 128556)
eastcoast=c(eastcoast,158755)
eastcoast.dat=subset(dat,Index%in%eastcoast)
eastcoast.dat$plot.ID=1:nrow(eastcoast.dat)

##
vars=c(paste0("PM",c(30,31,37,38,36,80,81,86,87,85,88,40,39,20,5,6)),"total.pen")
baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
# write.csv(baseline2,paste0(export.path,"Iteration3/Iter3P2_baselines.csv"),row.names = F)
tmp.dat.east=rbind(subset(baseline2,Model%in%c("CCTSP"))[,c("Index",vars)],eastcoast.dat[,c("Index",vars)])
# write.csv(tmp.dat.east,paste0(export.path,"Iteration3/Iter3P2_eastcoastselect.csv"),row.names = F)

FWO.compare=data.frame(Index=tmp.dat.east$Index)
for(i in 1:length(vars)){
  FWO.val=subset(baseline2,Model=="NA25")[,vars[i]]
  val=((tmp.dat.east[,vars[i]]-FWO.val)/FWO.val)*100
  tmp.val=data.frame(val=val)
  colnames(tmp.val)=paste0(vars[i],".FWO")
  FWO.compare=cbind(FWO.compare,tmp.val)
}
FWO.compare

PM.xwalk=data.frame(PM=vars,
                    Descript=c(paste("CRE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               paste("SLE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               "S308 Regulatory Flow","S77 Regulatory Flow","CRE MFL","LOK MFL","LOK >17 Ft","LOK >16 Ft","LOK - Total Stage Envelope Penalty"))
PM.xwalk$variable=paste0(PM.xwalk$PM,".FWO")
FWO.compare2=reshape2::melt(FWO.compare,id.vars="Index")
FWO.compare2=merge(FWO.compare2,PM.xwalk[,c("variable","Descript")])
FWO.compare2.xtab=reshape2::dcast(FWO.compare2,Descript~Index,value.var = "value",fun.aggregate=function(x) round(mean(x),1))
FWO.compare2.xtab=FWO.compare2.xtab[,c("Descript",tmp.dat.east$Index)]
FWO.compare2.xtab=FWO.compare2.xtab[match(PM.xwalk$Descript,FWO.compare2.xtab$Descript),]


# png(filename=paste0(plot.path,"Iteration3_Batch/Est_StressDam_EastCoastUSACEsubset.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
# layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

xlim.val=c(0,600);by.x=100;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,600);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(StressDam_lake.cre~StressDam_lake.sle,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(StressDam_lake.cre~StressDam_lake.sle,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(usace.dat,points(StressDam_lake.cre~StressDam_lake.sle,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(eastcoast.dat,points(StressDam_lake.cre~StressDam_lake.sle,pch=22,col="olivedrab3",bg=adjustcolor("olivedrab3",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(StressDam_lake.cre~StressDam_lake.sle,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(StressDam_lake.cre~StressDam_lake.sle,pch=16,col="black",cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2.5,"SLE Stress and Damaging From LOK\n(PM86+PM87)")
mtext(side=2,line=2.25,"CRE Stress and Damaging From LOK\n(PM37+PM38)")

legend("topright",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","East Coast Selected Runs","1:1 Line"),
       pch=c(19,16,15,21,22,NA),lwd=c(0.1,0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),adjustcolor("olivedrab3",0.75),NA),col=c("dodgerblue1","black","black","indianred1","olivedrab3","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_StageExt_EastCoastUSACEsubset.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);

xlim.val=c(0,20);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,11);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM7~PM5,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM7~PM5,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(usace.dat,points(PM7~PM5,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(eastcoast.dat,points(PM7~PM5,pch=22,col="olivedrab3",bg=adjustcolor("olivedrab3",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(PM7~PM5,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM7~PM5,pch=16,col="black",cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"% of Time >17 Ft NGVD29 (PM5)")
mtext(side=2,line=2,"% of Time <10 Ft NGVD29 (PM7)")

legend("topright",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","East Coast Selected Runs","1:1 Line"),
       pch=c(19,16,15,21,22,NA),lwd=c(0.1,0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),adjustcolor("olivedrab3",0.75),NA),col=c("dodgerblue1","black","black","indianred1","olivedrab3","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_StageExt2_EastCoastUSACEsubset.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2.5,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);

xlim.val=c(0,40);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,11);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PM7~PM6,dat,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(dat,points(PM7~PM6,pch=19,col=adjustcolor("dodgerblue1",0.5)))
with(usace.dat,points(PM7~PM6,pch=21,col="indianred1",bg=adjustcolor("indianred1",0.75),cex=1.5))
with(eastcoast.dat,points(PM7~PM6,pch=22,col="olivedrab3",bg=adjustcolor("olivedrab3",0.75),cex=1.5))
with(subset(baselines,Model=="NA25"),points(PM7~PM6,pch=15,col="black",cex=1.5))
with(subset(baselines,Model=="CCTSP"),points(PM7~PM6,pch=16,col="black",cex=1.5))
abline(0,1,lty=2)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"% of Time >16 Ft NGVD29 (PM5)")
mtext(side=2,line=2,"% of Time <10 Ft NGVD29 (PM7)")

legend("topright",legend=c("Iteration 3 Batch Runs","CC","NA25","USACE Selected Runs","East Coast Selected Runs","1:1 Line"),
       pch=c(19,16,15,21,22,NA),lwd=c(0.1,0.1,0.1,0.1,0.1,2),lty=c(NA,NA,NA,NA,NA,2),
       pt.bg=c(adjustcolor("dodgerblue1",0.5),"black","black",adjustcolor("indianred1",0.75),adjustcolor("olivedrab3",0.75),NA),col=c("dodgerblue1","black","black","indianred1","olivedrab3","black"),
       pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()


text.cex=0.7
# png(filename=paste0(plot.path,"Iteration3_Batch/SalEnv_EastCoastsubset.png"),width=5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:10,5,2,byrow=F))
xlim.val=c(1,nrow(eastcoast.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(600,1000,500,200,100)
ylim.min=c(200,400,0,0,0)
by.y.val=c(200,250,250,100,25)
vars=paste0("PM",c(30,31,37,38,36))
labs=c("Low (<750 cfs)","Optimum (750 - 2100 cfs)","Stress from LOK (2100 - 2600 cfs)","Damaging from LOK (>2600 cfs)","Extreme (>6500 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(eastcoast.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(eastcoast.dat$plot.ID,eastcoast.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,eastcoast.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Caloosahatchee")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Count of 14-Day Periods",outer=T)

ylim.max=c(200,1000,150,150,200)
ylim.min=c(100,800,0,0,100)
by.y.val=c(50,100,50,50,50)
vars=paste0("PM",c(80,81,86,87,85))
labs=c("Low (<150 cfs)","Optimum (150 - 1400 cfs)","Stress from LOK (1400 - 1700 cfs)","Damaging from LOK (>1700 cfs)","Extreme (>4000 cfs)")
for(i in 1:5){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(eastcoast.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(eastcoast.dat$plot.ID,eastcoast.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==5){axis_fun(1,xmaj,xmaj,eastcoast.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"St Lucie")}
}

mtext(side=1,line=2,"Model Index",outer=T)
dev.off()


# png(filename=paste0(plot.path,"Iteration3_Batch/WS_EastCoastsubset.png"),width=4,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:6,6,1,byrow=F))
xlim.val=c(1,nrow(eastcoast.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.min=c(0,0,0,0,0,0)
ylim.max=c(35,100,50,12,5,5)
by.y.val=ylim.max/2
vars=paste0("PM",c(124,126,127,130,131,132))
labs=c("LOSA Cutbacks","LOSA Duration","LOSA Severity","LOSA Sup. Demand not met EAA & Non-EAA","STOF - BCYP Demand Not Met", "SOTF - BR Demand Not Met")
xlabs=c("Weighted Avg %","Count","Score","Percent","Percent","Percent")
for(i in 1:6){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(eastcoast.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(eastcoast.dat$plot.ID,eastcoast.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==6){axis_fun(1,xmaj,xmaj,eastcoast.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  mtext(side=2,line=2.5,xlabs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"Water Supply")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

# png(filename=paste0(plot.path,"Iteration3_Batch/LOK_EastCoastsubset.png"),width=4,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,1,1),oma=c(3,2,2,1),lwd=0.5);
layout(matrix(1:4,4,1,byrow=F))
xlim.val=c(1,nrow(eastcoast.dat));by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

ylim.max=c(2,20,10,30)
ylim.min=c(0,0,0,10)
by.y.val=c(1,10,5,10)
vars=paste0("PM",c(5,6,7,13))
labs=c("% LOK Stage >17 Ft NGVD29","% LOK Stage >16 Ft NGVD29","% LOK Stage <10 Ft NGVD29","% within Stage Envelope")
for(i in 1:4){
  ylim.val=c(ylim.min[i],ylim.max[i]);by.y=by.y.val[i];ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(eastcoast.dat[,vars[i]],ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
  CC.val=subset(baselines,Model=="CCTSP")[,vars[i]]
  FWO.val=subset(baselines,Model=="NA25")[,vars[i]]
  points(eastcoast.dat$plot.ID,eastcoast.dat[,vars[i]],pch=21,bg="dodgerblue1")
  abline(h=CC.val,col="indianred1",lwd=1.5)
  abline(h=FWO.val,col="grey",lwd=1.5)
  if(i==4){axis_fun(1,xmaj,xmaj,eastcoast.dat$Index,las=2,cex=0.75)}else{axis_fun(1,xmaj,xmaj,NA,line=-0.5)}
  axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
  mtext(side=3,adj=0,labs[i],cex=text.cex)
  abline(v=c(8.5,16.5))
  if(i==1){mtext(side=3,adj=0,line=1.25,"LOK")}
  if(i==1){
    legend("bottomright",legend=c("NA25 (FWO)", "CC (TSP)"),
           pch=NA,lwd=2,lty=c(1,1),
           pt.bg=NA,col=c("grey","indianred1"),
           pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  }
}
mtext(side=2,line=0.5,"Percent of Time",outer=T)
mtext(side=1,line=2,"Model Index",outer=T)
dev.off()

cols=rev(wesanderson::wes_palette("Zissou1",4,"continuous"))
vars=paste0("PM",c(21,40,88,118))
ylim.val=c(0,1000);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/AvgFloodControl_EastCoastsubset.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

baseline2=baselines
baseline2$Index=baseline2$Model
baseline2$Index=with(baseline2,ifelse(Index=="CCTSP","CC",Index))
tmp=rbind(subset(baseline2,Model%in%c("NA25","CCTSP"))[,c("Index",vars)],eastcoast.dat[,c("Index",vars)])

tmp2=tmp[,vars]

x=barplot(t(tmp2),beside=F,col=NA,border=NA,ylim=ylim.val,axes=F,ann=F,names.arg = rep(NA,nrow(tmp)))
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
x=barplot(t(tmp2),beside=F,col=cols,ylim=ylim.val,axes=F,ann=F,names.arg =rep(NA,nrow(tmp)),add=T)
with(tmp,text(x,PM21/2,round(PM21,0),cex=0.7,col="white"))
with(tmp,text(x,PM21+(((PM40+PM21)-PM21)/2),round(PM40,0),cex=0.7))
with(tmp,text(x,(PM21+PM40)+(((PM40+PM21+PM88)-(PM21+PM40))/2),round(PM88,0),cex=0.7))
with(tmp,text(x,PM40+PM21+PM88+PM118,round(PM118,0),pos=3,offset=0.1,cex=0.7))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x,x,tmp$Index,line=-0.25,las=2,cex=0.8);box(lwd=1)
mtext(side=2,line=2,"Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=2.75,"Model Index")

par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0,0,legend=c("Water Conservation Areas (PM21)","Caloosahatchee River (PM40)","St. Lucie River (PM88)","Lake Worth Lagoon (PM118)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=cols,
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=0.5)
text(1,0,"Iteration 3 screened batch results. Mean annual flood control\nreleases from Lake Okeechobee for the 52 year (1965 - 2016)\nsimulation period of record.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()



# District suggestion -----------------------------------------------------
wmd.dat=subset(dat,Index==158755)

vars=c(paste0("PM",c(30,31,37,38,36,80,81,86,87,85,88,40,39,20)),"total.pen")
tmp.dat=rbind(subset(baseline2,Model%in%c("CCTSP"))[,c("Index",vars)],wmd.dat[,c("Index",vars)])
# write.csv(tmp.dat,paste0(export.path,"Iteration3/Iter3P2_USACEselect.csv"),row.names = F)

FWO.compare=data.frame(Index=tmp.dat$Index)
for(i in 1:length(vars)){
  FWO.val=subset(baseline2,Model=="NA25")[,vars[i]]
  val=((tmp.dat[,vars[i]]-FWO.val)/FWO.val)*100
  tmp=data.frame(val=val)
  colnames(tmp)=paste0(vars[i],".FWO")
  FWO.compare=cbind(FWO.compare,tmp)
}
FWO.compare

PM.xwalk=data.frame(PM=vars,
                    Descript=c(paste("CRE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               paste("SLE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               "S308 Regulatory Flow","S77 Regulatory Flow","CRE MFL","LOK MFL","LOK - Total Stage Envelope Penalty"))
PM.xwalk$variable=paste0(PM.xwalk$PM,".FWO")
PM.xwalk$Descript2=with(PM.xwalk,paste0(Descript," (",ifelse(PM=="total.pen","PM10+PM11",PM),")"))

FWO.compare2=reshape2::melt(FWO.compare,id.vars="Index")
FWO.compare2=merge(FWO.compare2,PM.xwalk[,c("variable","Descript")])
FWO.compare2.xtab=reshape2::dcast(FWO.compare2,Descript~Index,value.var = "value",fun.aggregate=function(x) round(mean(x),1))
FWO.compare2.xtab=FWO.compare2.xtab[,c("Descript",tmp.dat$Index)]
FWO.compare2.xtab=FWO.compare2.xtab[match(PM.xwalk$Descript,FWO.compare2.xtab$Descript),]

FWO.compare2.xtab%>%
  flextable()%>%
  padding(padding=1,part="all")%>%
  fontsize(size=8,part="body")%>%
  fontsize(size=9,part="header")%>%
  font(fontname="Times New Roman",part="all")%>%
  autofit()%>%
  add_header("CC"="Model Index",
             "158755"="Model Index")%>%
  merge_h(part="header")%>%
  align(align="center",part="header")%>%
  align(align="center",part="all")%>%
  align(j=1,align="left",part="all")%>%
  footnote(j=1,part="header",ref_symbols = "",value=as_paragraph("Percent Difference relative to FWO (NA25)"))%>%
  font(fontname="Times New Roman",part="all")%>%print("docx")


# District suggestion2 -----------------------------------------------------
wmd.mods=c(117916,173030,164792,122753,143525,140463,157494,227108,167195,126416,224937,107260,159280,162615,219225)
wmd.dat=subset(dat,Index%in%wmd.mods)


vars=c(paste0("PM",c(30,31,37,38,36,80,81,86,87,85,88,40,39,20,5,6)),"total.pen")
tmp.dat=rbind(subset(baseline2,Model%in%c("CCTSP"))[,c("Index",vars)],wmd.dat[,c("Index",vars)])
# write.csv(tmp.dat,paste0(export.path,"Iteration3/Iter3P2_USACEselect.csv"),row.names = F)

FWO.compare=data.frame(Index=tmp.dat$Index)
for(i in 1:length(vars)){
  FWO.val=subset(baseline2,Model=="NA25")[,vars[i]]
  val=((tmp.dat[,vars[i]]-FWO.val)/FWO.val)*100
  tmp=data.frame(val=val)
  colnames(tmp)=paste0(vars[i],".FWO")
  FWO.compare=cbind(FWO.compare,tmp)
}
FWO.compare

PM.xwalk=data.frame(PM=vars,
                    Descript=c(paste("CRE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               paste("SLE",c("Low","Optimum","Stress from LOK","Damaging from LOK","Extreme"),sep=" - "),
                               "S308 Regulatory Flow","S77 Regulatory Flow","CRE MFL","LOK MFL","LOK >17 Ft","LOK >16 Ft","LOK - Total Stage Envelope Penalty"))
PM.xwalk$variable=paste0(PM.xwalk$PM,".FWO")
FWO.compare2=reshape2::melt(FWO.compare,id.vars="Index")
FWO.compare2=merge(FWO.compare2,PM.xwalk[,c("variable","Descript")])
FWO.compare2.xtab=reshape2::dcast(FWO.compare2,Descript~Index,value.var = "value",fun.aggregate=function(x) round(mean(x),1))
FWO.compare2.xtab=FWO.compare2.xtab[,c("Descript",tmp.dat$Index)]
FWO.compare2.xtab=FWO.compare2.xtab[match(PM.xwalk$Descript,FWO.compare2.xtab$Descript),]

FWO.compare2.xtab%>%
  flextable()%>%
  padding(padding=1,part="all")%>%
  fontsize(size=8,part="body")%>%
  fontsize(size=9,part="header")%>%
  font(fontname="Times New Roman",part="all")%>%
  autofit()%>%
  # add_header("CC"="Model Index",
  #           "158755"="Model Index")%>%
  merge_h(part="header")%>%
  align(align="center",part="header")%>%
  align(align="center",part="all")%>%
  align(j=1,align="left",part="all")%>%
  footnote(j=1,part="header",ref_symbols = "",value=as_paragraph("Percent Difference relative to FWO (NA25)"))%>%
  font(fontname="Times New Roman",part="all")%>%print("docx")


tmp.dat=rbind(subset(baseline2,Index%in%c("NA25","CC"))[,c("Index",vars)],wmd.dat[,c("Index",vars)])

raw.tmp.dat=reshape2::melt(tmp.dat,id.vars="Index")
raw.tmp.dat=merge(raw.tmp.dat,PM.xwalk[,c("PM","Descript2")],by.x="variable",by.y="PM")
raw.tmp.dat.xtab=reshape2::dcast(raw.tmp.dat,Descript2~Index,value.var = "value",fun.aggregate=function(x) round(mean(x),1))
raw.tmp.dat.xtab=raw.tmp.dat.xtab[,c("Descript2",tmp.dat$Index)]
raw.tmp.dat.xtab=raw.tmp.dat.xtab[match(PM.xwalk$Descript2,raw.tmp.dat.xtab$Descript2),]

raw.tmp.dat.xtab%>%
  flextable()%>%
  colformat_double(j=2:16,big.mark="")%>%
  padding(padding=1,part="all")%>%
  fontsize(size=7,part="body")%>%
  fontsize(size=8,part="header")%>%
  font(fontname="Times New Roman",part="all")%>%
  # width(width=c(4,rep(1,14)))%>%
  autofit()%>%
  set_header_labels("Descript2"="Performance Measure")%>%
  merge_h(part="header")%>%
  align(align="center",part="header")%>%
  align(align="center",part="all")%>%
  align(j=1,align="left",part="all")%>%
  footnote(j=1,part="header",ref_symbols = "",value=as_paragraph("Percent Difference relative to FWO (NA25)"))%>%
  footnote(j=1,part="header",ref_symbols = "",value=as_paragraph("Units: Salinity metric, 14-day peroid count; Regulatory Discharge, x1000 Ac-Ft Yr\u207B\u00B9; MFL, number of exceedances; Stage Envelope Penalty, Score."))%>%
  font(fontname="Times New Roman",part="all")#%>%print("docx")

# RSMBN -------------------------------------------------------------------
# dput(list.files("C:/Julian_LaCie/_GitHub/LOSOM_ModelEval/Data/Iteration_3_Batch/20211027/RSMBN"))

RSM.sites=c("S308","S308_QFC","S308BF")
alts.iter2=c("NA25","CC")

S308.q.dat=data.frame()
n.alts=length(alts.iter2)
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts.iter2[j],"/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=alts.iter2[j]
    S308.q.dat=rbind(tmp,S308.q.dat)
    print(i)
  }
}

n.alts=length(usace.mods)
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_3_Batch/20211027/RSMBN/model_",usace.mods[j],"/output/RSMBN_output.dss"))  
  
  for(i in 1:length(RSM.sites)){
    paths=paste0("/RSMBN/",RSM.sites[i],"/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE=RSM.sites[i]
    tmp$Alt=usace.mods[j]
    S308.q.dat=rbind(tmp,S308.q.dat)
    print(i)
  }
}

S308.q.dat$CY=as.numeric(format(S308.q.dat$Date,'%Y'))

S308.q.dat.CY=ddply(S308.q.dat,c("CY","Alt","SITE"),summarise,TFlow=sum(cfs.to.acftd(FLOW),na.rm=T))
S308.q.dat.CY.xtab=reshape2::dcast(S308.q.dat.CY,Alt~SITE,value.var = "TFlow",fun.aggregate=function(x) mean(x,na.rm=T)/1000)
S308.q.dat.CY.xtab

alts.all=c(alts.iter2,usace.mods[order(usace.mods)])

S308.q.dat.CY.xtab=S308.q.dat.CY.xtab[match(alts.all,S308.q.dat.CY.xtab$Alt),]
S308.q.dat.CY.xtab$S308BF.pertotal=with(S308.q.dat.CY.xtab,(S308BF/S308)*100)

ylim.val=c(0,220);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"Iteration3_Batch/AvgS308_Discharge.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,0.25,1),oma=c(2.5,2,0.5,0.25),lwd=0.5);
layout(matrix(c(1:2),2,1,byrow=T),heights=c(1,0.2))

tmp=t(S308.q.dat.CY.xtab[,c("S308","S308BF")])
x=barplot(tmp,beside=T,names.arg = rep(NA,length(alts.all)),ann=F,axes=F,col=NA,border=NA,ylim=ylim.val)
abline(h=ymaj,lty=1,col=adjustcolor("grey",0.5))
barplot(tmp,beside=T,names.arg = rep(NA,length(alts.all)),ann=F,axes=F,ylim=ylim.val,add=T,col=c("grey20","grey80"))
text(x[1,],tmp[1,],round(tmp[1,],0),pos=1,offset = 0.15,col="white",cex=0.5)
text(x[2,],tmp[2,],round(tmp[2,],0),pos=1,offset = 0.15,cex=0.5)
segments(x[1,]+0.75,tmp[2,],x[1,]+0.75,tmp[1,],col="red",lwd=2,lend=1)
segments(x[1,]+0.75,tmp[2,]+(tmp[1,]-tmp[2,])/2,x[1,]+0.9,tmp[2,]+(tmp[1,]-tmp[2,])/2,col="red",lwd=2,lend=1)
text(x[1,]+0.9,tmp[2,]+(tmp[1,]-tmp[2,])/2,paste(round(S308.q.dat.CY.xtab$S308BF.pertotal,0),"%"),pos=4,cex=0.65,offset=0.1,col="red")
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,x[1,]+0.5,x[1,]+0.5,alts.all,las=2,cex=0.8)
box(lwd=1)
mtext(side=2,line=2,"Average Annual Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)")
mtext(side=1,line=2.75,"Model Index")

par(family="serif",mar=c(0,2,0.25,1))
plot(0:1,0:1,type="n",axes=F,ylab=NA,xlab=NA,xaxs="i",yaxs="i")
legend(0,0,legend=c("S308 (Total Flow)","S308 (Back Flow)"),
       pch=22,
       lty=0,lwd=0.01,
       col="black",
       pt.bg=c("grey20","grey80"),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=0.5)
text(1,0,"Iteration 3 screened batch results. Mean annual discharge\nand backflow volume at S308 for the 52 year (1965 - 2016)\nsimulation period of record with % backflow volume identified.",adj=1,xpd=NA,cex=0.65,font=3)
dev.off()