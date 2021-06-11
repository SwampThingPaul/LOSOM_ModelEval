## 
## LOSOM
##
## Iteration 2 alternative evaluation
## CRE Salinity Modeling
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
library(dssrip)
library(zoo)
library(classInt)
#
library(magrittr)
library(flextable)

library(CalSalMod)
## Paths
wd="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
# alts=alts[!(alts%in%c("SPEF","SPAS","SPLC"))];# removed the Everglades Foundation, Audubon and Lake Commnuities alteratives
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))


# Rainfall & ET -----------------------------------------------------------
RF.dat=read.table(paste0(data.path,"CRE_RF_ET/basin_Ave_daily_rainfall.txt"),header=F,col.names = c("CY","Month","Day","RF_inch"))
RF.dat$Date=with(RF.dat,date.fun(paste(CY,Month,Day,sep="-")))
# RF.dat$WY=WY(RF.dat$date)

ET.dat=read.table(paste0(data.path,"CRE_RF_ET/RET_1965_2016.txt"),header=T,col.names = c("CY","Month","Day","ET_inch"))
ET.dat$Date=with(ET.dat,date.fun(paste(CY,Month,Day,sep="-")))

RF.ET=merge(RF.dat[,c("Date","RF_inch")],ET.dat[,c("Date","ET_inch")],'Date')
RF.ET$WY=WY(RF.ET$Date)
RF.ET$RF_2day=with(RF.ET,c(NA,rollapply(RF_inch,width=2,function(x)mean(x,na.rm=T))))
RF.ET$RF_2day[1]<-RF.ET$RF_inch[1]; # replaces first value with 0
RF.ET$RF_2day=round(RF.ET$RF_2day,2)
RF.ET$Q.tidalbasin=with(RF.ET,cost_Q(RF_2day,ET_inch))

plot(Q.tidalbasin~Date,RF.ET)

# Discharge ---------------------------------------------------------------
q.dat=data.frame()
for(j in 1:n.alts){
  dss_out=opendss(paste0(data.path,"Iteration_2/Model_Output/",alts[j],"/RSMBN_output.dss"))  
    paths="/RSMBN/S79/FLOW/01JAN1965 - 01JAN2016/1DAY/SIMULATED/"
    tmp=data.frame(getFullTSC(dss_out,paths))
    tmp$Date=date.fun(date.fun(rownames(tmp))-lubridate::ddays(1))
    rownames(tmp)<-NULL
    tmp$SITE="S79"
    tmp$Alt=alts[j]
    q.dat=rbind(tmp,q.dat)
  
}

test=reshape2::dcast(q.dat,Date~Alt,value.var = "FLOW",mean)
test=merge(test,RF.ET[,c("Date","Q.tidalbasin")],"Date")

sal.sites=SiteSalConstant$Site
sal.sites=sal.sites[sal.sites!="Bird-IS"]
n.sal=length(sal.sites)

sal.dat=data.frame()
for(i in 1:n.alts){
  
  for(j in 1:n.sal){
  tmp=subset(q.dat,Alt==alts[i])
  tmp=merge(tmp,RF.ET[,c("Date","Q.tidalbasin")],"Date")
  tmp$Q.total=rowSums(tmp[,c("Q.tidalbasin","FLOW")],na.rm=T)
  tmp$sal=SalMod(tmp$Q.total,sal.sites[j])
  tmp$SITE=sal.sites[j]
  sal.dat=rbind(sal.dat,tmp[,c("Date","SITE","Alt","sal")])
  }
  print(i)
}
# write.csv(sal.dat,paste0(export.path,"Iter2_CRE_sal_QWModel.csv"),row.names = F)

plot(sal~Date,subset(sal.dat,SITE=="S79"&Alt=="NA25"),type="l")
with(subset(sal.dat,SITE=="S79"&Alt=="AA"),lines(Date,sal,col="red"))
with(subset(sal.dat,SITE=="S79"&Alt=="CC"),lines(Date,sal,col="blue"))

head(sal.dat)
sal.dat$Alt_site=with(sal.dat,paste(Alt,SITE,sep="_"))
sal.dat$sal_7d=with(sal.dat,ave(sal,Alt_site,FUN=function(x) c(rep(NA,6),rollapply(x,width=7,function(x)mean(x,na.rm=T)))))

sal.dat.val=subset(sal.dat,SITE=="Val-I75")
sal.dat.val$opt.cat=with(sal.dat.val,ifelse(sal_7d<10,1,0))
sal.dat.val$stress.cat=with(sal.dat.val,ifelse(sal_7d>10&sal_7d<15,1,0))
sal.dat.val$dam.cat=with(sal.dat.val,ifelse(sal_7d>15,1,0))

sal.dat.val$Alt=factor(sal.dat.val$Alt,levels=alts.sort)
sal.dat.val$WY=WY(sal.dat.val$Date)
sal.dat.val=subset(sal.dat.val,WY%in%WYs)

sal.dat.val.cats=ddply(sal.dat.val,c("WY","SITE","Alt"),summarise,
                         opt.freq=sum(opt.cat,na.rm=T),
                         dam.freq=sum(dam.cat,na.rm=T),
                         stress.freq=sum(stress.cat,na.rm=T),N.val=N.obs(sal_7d))


library(ggtern)
ggtern(sal.dat.val.cats,aes(x=dam.freq,y=opt.freq,z=stress.freq,label=Alt))+
  geom_point(aes(colour=factor(Alt)),size=2)+
  scale_color_manual(values=cols)+
  geom_confidence_tern(colour="dodgerblue1",alpha=0.5,breaks=c(0.5,0.95),linetype=2)+
  #geom_text(hjust=0, vjust=0,alpha=0.5)+
  labs(title="Frequency of RECOVER Flow catergories (CRE)",
       subtitle=" FLWY 1966 - 2016",
       x="",xarrow="Damaging",
       y="",yarrow="Low",
       z="",zarrow="Optimum",color="Alternatives")+
  theme_bw(base_size=10)+theme_arrowcustomlength(0.1,0.85)+
  theme(panel.spacing=unit(0.5, "lines"),
        text=element_text(family="serif"),
        plot.margin=margin(0.25,1,0.25,1),
        tern.axis.arrow = element_line(color = "red",size=1.75))+
  facet_wrap(~ Alt, ncol = 4)
 
