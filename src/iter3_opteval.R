
## 
## LOSOM
##
## Iteration 3 sensitivity technical eval
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

library(lmom)
## Paths
wd="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
dirs.val=list.dirs(paste0(data.path,"Iteration_3_Tech_Discussion"))

### Northern Estuaries
vals=grep("/Northern_Estuaries",dirs.val)

NE.data.path=dirs.val[vals]

cre.env=data.frame()
for(i in 1:length(NE.data.path)){
tmp=read.table(paste0(NE.data.path[i],"/cre_flow_envelope.txt"),skip=16)
PM.val=substring(strsplit(NE.data.path[i],"\\/")[[1]][7],1,1)
tmp$PM.grp=paste0("PM_",PM.val)
cre.env=rbind(tmp,cre.env)
}
vals=c("Alt","low","opt","stress.basin","stress.LOK","dam.basin","dam.LOK","low1","low2","high1","high2","high3","PM.grp")
colnames(cre.env)<-vals
cre.env
subset(cre.env,Alt=="CCsimp4")
cre.env$Alt=with(cre.env,ifelse(Alt=="CCsimp4"&PM.grp%in%c("PM_3","PM_4","PM_5"),"CCsimp4(2)",Alt))
subset(cre.env,Alt=="CCsimp4")



PMgrps=list(c('NA25','CC',paste0("CC_S",c(1,2,3,"3-4",4,5,6,7))),
     c('NA25','CC','CCTSP',"CCsimp4","CCsimp5","CCsimp5a"),
     c('NA25','CC','simp4','simp4s1'),
     c('NA25','CC',"CCsimp4(2)","smp4ZFS","smp4ZFW","smp4ZFSW"),
     c('NA25','CC',"CCsimp4(2)","271DS","271DSZC"),
     c('NA25','CC',"CCTSP","CCsimp4(2)","ZB_S77"),
     c('NA25','CC',"CCR1","R1ABZD","R1ABZC","R1ABZB"),
     c('NA25','CC',"CCR1","CCR1ED","CCR1MD1","CCR1MD2"),
     c('NA25','CC',"CCR1","R1WSEN","R1WSMF"))


cre.env=cre.env[duplicated(cre.env[,1:12])==F,]
# write.csv(cre.env[,1:12],paste0(export.path,"20210920_Iter3Opt_CREEnv.csv"),row.names = F)
# PMgrps=cre.env[,c("Alt","PM.grp")]
# nrow(PMgrps)

PMgrps.all=c(c('NA25','CC',paste0("CC_S",c(1,2,3,"3-4",4,5,6,7))),
             c('CCTSP',"CCsimp4","CCsimp5","CCsimp5a"),
             c('simp4','simp4s1'),
             c("CCsimp4(2)","smp4ZFS","smp4ZFW","smp4ZFSW"),
             c("271DS","271DSZC"),
             c("ZB_S77"),
             c("CCR1","R1ABZD","R1ABZC","R1ABZB"),
             c("CCR1ED","CCR1MD1","CCR1MD2"),
             c("R1WSEN","R1WSMF"))
cre.env2=subset(cre.env,Alt%in%PMgrps.all)
cre.env2=cre.env2[match(PMgrps.all,cre.env2$Alt),]

vars=c("low","opt","stress.LOK","dam.LOK","high3")
tmp.FWO=data.frame()
var.names=paste0("PerFWO.",c("low","opt","stress.LOK","dam.LOK","high3"))
for(k in 1:length(vars)){
  tmp.FWO=data.frame(val=((cre.env2[,vars[k]]-cre.env2[1,vars[k]])/cre.env2[1,vars[k]])*100)
  colnames(tmp.FWO)=var.names[k]
  cre.env2=cbind(cre.env2,tmp.FWO)
}

tmp.CC=data.frame()
var.names=paste0("PerCC.",c("low","opt","stress.LOK","dam.LOK","high3"))
for(k in 1:length(vars)){
  tmp.CC=data.frame(val=((cre.env2[,vars[k]]-cre.env2[2,vars[k]])/cre.env2[2,vars[k]])*100)
  tmp.CC[1,]=NA
  colnames(tmp.CC)=var.names[k]
  cre.env2=cbind(cre.env2,tmp.CC)
}

cre.env2[order(cre.env2$PerFWO.high3),]


# png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_RECOVER_SalEnv.png"),width=10,height=9,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(3,2,1,1),oma=c(2,2.5,2,1),lwd=0.5);
layout(matrix(1:45,9,5,byrow=T))

i=1
PMval=paste0("PM",0:8)
for(i in 1:9){
alts.val=PMgrps[[i]]
cols=c("grey20",wesanderson::wes_palette("Zissou1",length(alts.val)-1,"continuous"))
tmp=subset(cre.env,Alt%in%alts.val)
vars=c("low","opt","stress.LOK","dam.LOK","high3")
tmp=tmp[match(alts.val,tmp$Alt),c("Alt",vars)]


CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
ymax=c(800,1000,500,200,90)
yval=ymax/2
for(j in 2:6){
  ylim.val=c(0,ymax[j-1]);ymaj=seq(ylim.val[1],ylim.val[2],yval[j-1]);ymin=seq(ylim.val[1],ylim.val[2],yval[j-1]/2)
  x=barplot(tmp[,j],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,x,x,alts.val,las=2,cex=0.85)
  box(lwd=1)
  if(i==1){mtext(side=3,adj=0,CRE.labs[j-1],cex=0.7)}
  text(x,tmp[,j],round(tmp[,j],0),font=2,col="black",pos=1,cex=0.75,offset=0.25)
  if(i==1&j==2){mtext(side=3,adj=0,line=-1.25," CRE")}
  # if(i==2){mtext(side=2,line=2.5,"Count of 14-Day Periods")}
  if(j==6){mtext(side=4,line=0.25,PMval[i])}
}
}
mtext(side=2,outer=T,"Count of 14-Day Periods",line=1)
mtext(side=1,outer=T,"Alternative",line=1)

dev.off()


i=1
PMval=paste0("PM",0:8)
vars=c("low","opt","stress.LOK","dam.LOK","high3")
var.FWO=paste0("PerFWO.",vars)
var.CC=paste0("PerCC.",vars)
CRE.labs=c("Low Flow\n(<750 cfs)","Optimum\n(750 - 2100 cfs)","Stress From LOK\n(2100 - 2600 cfs)","Damaging From LOK\n(>2600 cfs)","Extreme\n(>6500 cfs)")
for(i in 1:9){
  alts.val=PMgrps[[i]]
  cols=c("grey20",wesanderson::wes_palette("Zissou1",length(alts.val)-1,"continuous"))
  tmp=subset(cre.env2,Alt%in%alts.val)
  
  tmp=tmp[match(alts.val,tmp$Alt),]
  
  # tmp.FWO=data.frame()
  # var.names=paste0("PerFWO.",c("low","opt","stress.LOK","dam.LOK","high3"))
  # for(k in 1:length(vars)){
  #   tmp.FWO=data.frame(val=((tmp[,vars[k]]-tmp[1,vars[k]])/tmp[1,vars[k]])*100)
  #   colnames(tmp.FWO)=var.names[k]
  #   tmp=cbind(tmp,tmp.FWO)
  # }
  # 
  # tmp.CC=data.frame()
  # var.names=paste0("PerCC.",c("low","opt","stress.LOK","dam.LOK","high3"))
  # for(k in 1:length(vars)){
  #   tmp.CC=data.frame(val=((tmp[,vars[k]]-tmp[2,vars[k]])/tmp[2,vars[k]])*100)
  #   tmp.CC[1,]=NA
  #   colnames(tmp.CC)=var.names[k]
  #   tmp=cbind(tmp,tmp.CC)
  # }


png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_RECOVER_SalEnv_",PMval[i],".png"),width=8,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(4,3.5,2,1),lwd=0.5);
layout(matrix(1:15,3,5,byrow=T))

  ymax=c(800,1000,500,200,90)
  yval=ymax/2
  for(j in 1:length(vars)){
    ylim.val=c(0,ymax[j]);ymaj=seq(ylim.val[1],ylim.val[2],yval[j]);ymin=seq(ylim.val[1],ylim.val[2],yval[j]/2)
    x=barplot(tmp[,vars[j]],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
    axis_fun(2,ymaj,ymin,ymaj)
    axis_fun(1,x,x,NA,las=2,cex=0.85)
    box(lwd=1)
    mtext(side=3,adj=0,CRE.labs[j],cex=0.7)
    text(x,tmp[,vars[j]],round(tmp[,vars[j]],0),font=2,col="black",pos=1,cex=0.7,offset=0.25)
    if(i==1&j==1){mtext(side=3,adj=0,line=-1.25," CRE")}
    if(j==1){mtext(side=2,line=2.5,"Count of\n14-Day Periods",cex=0.75)}
    if(j==5){mtext(side=3,adj=1,PMval[i])}
  }

  ylim.val=c(-85,85);by.y=40;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  for(j in 1:length(vars)){
    x=barplot(tmp[,var.FWO[j]],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
    abline(h=0)
    axis_fun(2,ymaj,ymin,ymaj)
    axis_fun(1,x,x,NA,las=2,cex=0.85)
    box(lwd=1)
    text(x,tmp[,var.FWO[j]],round(tmp[,var.FWO[j]],0),font=2,col="black",pos=ifelse(tmp[,var.FWO[j]]<0,1,3),cex=0.75,offset=0.1)
    if(j==1){mtext(side=2,line=2.5,"Average Percent\nDifference to FWO",cex=0.75)}
    #if(j==6){mtext(side=4,line=0.25,PMval[i])}
  }  
  
  for(j in 1:length(vars)){
    x=barplot(tmp[,var.CC[j]],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
    abline(h=0)
    axis_fun(2,ymaj,ymin,ymaj)
    axis_fun(1,x,x,alts.val,las=2,cex=0.85)
    box(lwd=1)
    text(x,tmp[,var.CC[j]],round(tmp[,var.CC[j]],0),font=2,col="black",pos=ifelse(tmp[,var.CC[j]]<0|is.na(tmp[,var.CC[j]])==T,1,3),cex=0.75,offset=0.1)
    if(j==12){mtext(side=2,line=2.5,"Average Percent\nDifference to CC",cex=0.75)}
    #if(j==6){mtext(side=4,line=0.25,PMval[i])}
  }  
mtext(side=1,outer=T,"Alternative",line=3)


dev.off()
}

### Lake Okeechobee
# vals=grep("/Lake_Okeechobee",dirs.val)
# 
# LOK.pdf.path=dirs.val[vals]
# 
# library(Rcpp)
# library(pdftools)
# 
# tmp=pdf_text(paste0(LOK.pdf.path[1],"/lok_regq.pdf"))
# 
# tmp <- strsplit(tmp, "\\s+");
# 
# alts=tmp[[1]][49:53]
# LWL=tmp[[1]]
# 
# 
# library(tm)
# read <- readPDF(control = list(text = "-layout"))
# 
# document <- Corpus(URISource(paste0(LOK.pdf.path[1],"/lok_regq.pdf")), readerControl = list(reader = read))
# doc <- content(document[[1]])
# tmp <- strsplit(doc, "\\s+")
# 
# doc[page_breaks[1]]

lok.reg=read.csv(paste0(data.path,"Iteration_3_Tech_Discussion/lok_regq_data.csv"))
range(lok.reg$CRE)

lok.reg=lok.reg[match(PMgrps.all,lok.reg$Alt),]


vars=c("LWL","SLE","CRE","WCAs")
tmp.FWO=data.frame()
var.names=paste0("PerFWO.",vars)
for(k in 1:length(vars)){
  tmp.FWO=data.frame(val=((lok.reg[,vars[k]]-lok.reg[1,vars[k]])/lok.reg[1,vars[k]])*100)
  colnames(tmp.FWO)=var.names[k]
  lok.reg=cbind(lok.reg,tmp.FWO)
}

tmp.CC=data.frame()
var.names=paste0("PerCC.",vars)
for(k in 1:length(vars)){
  tmp.CC=data.frame(val=((lok.reg[,vars[k]]-lok.reg[2,vars[k]])/lok.reg[2,vars[k]])*100)
  tmp.CC[1,]=NA
  colnames(tmp.CC)=var.names[k]
  lok.reg=cbind(lok.reg,tmp.CC)
}

lok.reg[order(lok.reg$PerFWO.CRE),]


i=1
PMval=paste0("PM",0:8)
vars=c("LWL","SLE","CRE","WCAs")
var.FWO=paste0("PerFWO.",vars)
var.CC=paste0("PerCC.",vars)
for(i in 1:9){
  alts.val=PMgrps[[i]]
  cols=c("grey20",wesanderson::wes_palette("Zissou1",length(alts.val)-1,"continuous"))
  tmp=subset(lok.reg,Alt%in%alts.val)
  
  tmp=tmp[match(alts.val,tmp$Alt),]
  
  # tmp.FWO=data.frame()
  # var.names=paste0("PerFWO.",vars)
  # for(k in 1:length(vars)){
  #   tmp.FWO=data.frame(val=((tmp[,vars[k]]-tmp[1,vars[k]])/tmp[1,vars[k]])*100)
  #   colnames(tmp.FWO)=var.names[k]
  #   tmp=cbind(tmp,tmp.FWO)
  # }
  # 
  # tmp.CC=data.frame()
  # var.names=paste0("PerCC.",vars)
  # for(k in 1:length(vars)){
  #   tmp.CC=data.frame(val=((tmp[,vars[k]]-tmp[2,vars[k]])/tmp[2,vars[k]])*100)
  #   tmp.CC[1,]=NA
  #   colnames(tmp.CC)=var.names[k]
  #   tmp=cbind(tmp,tmp.CC)
  # }
  # 
  
  png(filename=paste0(plot.path,"Iteration3_TechEval/CRE_RegQ_",PMval[i],".png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
  par(family="serif",mar=c(1,2,0.5,1),oma=c(4,3.5,2,1),lwd=0.5);
  layout(matrix(1:3,3,1,byrow=T))
  
  ylim.val=c(0,700);by.y=200;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  x=barplot(tmp[,"CRE"],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
    axis_fun(2,ymaj,ymin,ymaj)
    axis_fun(1,x,x,NA,las=2,cex=0.85)
    box(lwd=1)
    text(x,tmp[,"CRE"],round(tmp[,"CRE"],0),font=2,col="black",pos=1,cex=0.7,offset=0.25)
    mtext(side=3,adj=0,"Mean Annual Flood Control Releases",cex=0.8,col="grey40")
    mtext(side=3,adj=0,line=-1.25," CRE")
    mtext(side=2,line=2.5,"Discharge Volume\n(x1000 Ac-Ft Y\u207B\u00B9)",cex=0.75)
    mtext(side=3,adj=1,PMval[i],cex=0.75)

    ylim.val=c(-50,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    x=barplot(tmp[,"PerFWO.CRE"],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
    abline(h=0)
    axis_fun(2,ymaj,ymin,ymaj)
    axis_fun(1,x,x,NA,las=2,cex=0.85)
    box(lwd=1)
    text(x,tmp[,"PerFWO.CRE"],round(tmp[,"PerFWO.CRE"],0),font=2,col="black",pos=ifelse(tmp[,"PerFWO.CRE"]<0,1,3),cex=0.75,offset=0.1)
    mtext(side=2,line=2.5,"Average Percent\nDifference to FWO",cex=0.75)

    #ylim.val=c(-50,50);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    x=barplot(tmp[,"PerCC.CRE"],col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,axes=F,ann=F)
    abline(h=0)
    axis_fun(2,ymaj,ymin,ymaj)
    axis_fun(1,x,x,alts.val,las=2,cex=0.85)
    box(lwd=1)
    text(x,tmp[,"PerCC.CRE"],round(tmp[,"PerCC.CRE"],0),font=2,col="black",pos=ifelse(tmp[,"PerCC.CRE"]<0|is.na(tmp[,"PerCC.CRE"])==T,1,3),cex=0.75,offset=0.1)
    mtext(side=2,line=2.5,"Average Percent\nDifference to CC",cex=0.75)
    
    mtext(side=1,outer=T,"Alternative",line=3)
    
    
    dev.off()
}


# Regulation Schedule -----------------------------------------------------


zones=c(paste("LOK",paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),sep="-"),
        paste("LOK",paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_"),sep="_"))
zone.alias=data.frame(zone=zones,
                      zone2=c(paste("ZONE",c("A","B","C","D0","D1","D2","D3"),sep="_"),paste("LOWSM",c(15,30,45,60),"LEVEL",sep="_")))
reg.sch=data.frame()
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/CC/RSMBN_output.dss"))  
  
  for(i in 1:length(zones)){
    paths=paste0("/RSMBN/",zones[i],"/STAGE/01JAN1965 - 01JAN2016/1DAY/INPUT/")  
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$zone=zones[i]
    tmp$Alt="CC"
    reg.sch=rbind(tmp,reg.sch)
    print(i)
  }
  
  reg.sch$DOY=as.numeric(format(reg.sch$Date,"%j"))
  reg.sch=merge(reg.sch,zone.alias,"zone")
  reg.sch2=reshape2::dcast(reg.sch,Alt+DOY~zone2,value.var = "STAGE",mean)
  
  # png(filename=paste0(plot.path,"Iteration3_TechEval/CC_simp_REGSCH.png"),width=7,height=4,units="in",res=200,type="windows",bg="white")
  ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  par(family="serif",mar=c(1,2,0.5,0.25),oma=c(2,2,0.75,1),lwd=0.5);
  layout(matrix(1:2,1,2,byrow=T),widths=c(1,1))
  
  xlim.val.date=date.fun(c("1965-01-01","1965-12-31"));xmaj.dat=seq(xlim.val.date[1],xlim.val.date[2],"2 month");xmin.dat=seq(xlim.val.date[1],xlim.val.date[2],"1 month")
  xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.dat,"%j"));xmin=as.numeric(format(xmin.dat,"%j"));
  ylim.val=c(10,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/0.5)
  
  plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
  abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_A,col="red",lwd=2))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_B,col="deepskyblue2",lwd=2))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_C,col="orange",lwd=2))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_D1,col="grey",lwd=2))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_D2,col="grey",lty=2,lwd=2))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_D3,col="grey",lty=2,lwd=2))
  abline(h=13,col="green3",lwd=2)
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==90),text(85,ZONE_B,"Zone B",pos=3,col="deepskyblue2",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==105),text(105,ZONE_C,"Zone C",pos=3,col="orange",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,ZONE_D1+(ZONE_C-ZONE_D1)/2,"Zone D",col="grey",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(325,ZONE_D3+(ZONE_C-ZONE_D3)/2,"Zone D3",col="grey",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(325,ZONE_D2+(ZONE_D3-ZONE_D2)/2,"Zone D2",col="grey",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(325,ZONE_D1+(ZONE_D2-ZONE_D1)/2,"Zone D1",col="grey",cex=0.75,font=2))
    with(subset(reg.sch2,Alt=="CC"&DOY==300),text(300,13,"Zone E",pos=3,col="green3",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,LOWSM_15_LEVEL+(13-LOWSM_15_LEVEL)/2,"Zone F",col="purple",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
  axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=1,line=2,"Date (Month)")
  mtext(side=2,line=2,"Stage Elevation (Ft, NGVD29)")
  mtext(side=3,adj=0,"Alternative CC")
  
  plot(ZONE_A~DOY,reg.sch2,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n",xaxs="i")
  abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_A,col="red",lwd=2))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_C,col="deepskyblue2",lwd=2))
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,ZONE_D2,col="orange",lwd=2))
  abline(h=13,col="green3",lwd=2)
  with(subset(reg.sch2,Alt=="CC"),lines(DOY,LOWSM_15_LEVEL,col="purple",lwd=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,ZONE_A,"Zone A",pos=3,col="red",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==90),text(85,ZONE_C,"Zone B",pos=3,col="deepskyblue2",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,ZONE_D2+(ZONE_C-ZONE_D2)/2,"Zone C",pos=3,col="orange",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(325,ZONE_D2+(ZONE_D3-ZONE_D2)/2,"Zone D2",col="grey",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,13+(ZONE_D2-13)/2,"Zone D",pos=3,col="grey",cex=0.75,font=2))
    with(subset(reg.sch2,Alt=="CC"&DOY==30),text(30,LOWSM_15_LEVEL+(13-LOWSM_15_LEVEL)/2,"Zone F",col="purple",cex=0.75,font=2))
  with(subset(reg.sch2,Alt=="CC"&DOY==250),text(250,10+(LOWSM_15_LEVEL-10)/2,"Water Shortage\nManagement Band",col="blue",cex=0.75,font=2))
  axis_fun(1,xmaj,xmin,format(xmaj.dat,"%b"),line=-0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side=1,line=2,"Date (Month)")
  mtext(side=3,adj=0,"Alternative CCsimp4")
  dev.off()