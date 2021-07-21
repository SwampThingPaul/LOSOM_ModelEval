## 
## LOSOM
##
## Iteration 2 LOK Water Budget
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
library(reshape)
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

# -------------------------------------------------------------------------
WYs=seq(1966,2016,1)
alts=list.files(paste0(data.path,"Iteration_2/Model_Output/"))
alts=alts[!alts%in%c("_Batch_Results","Northern_Estuaries","Iteration2_STL_Flows_13Jul2021.xlsx")]
n.alts=length(alts)
alts.sort=c("NA25","ECBr","AA","BB","CC","DD","EE1","EE2")
cols=c("grey50","grey50",rev(wesanderson::wes_palette("Zissou1",length(alts.sort)-2,"continuous")))

# -------------------------------------------------------------------------
## Daily Water Budget Summary files provided on FTP site.
head.val=c("year","month","day","Rainfall", "ET", "HpmDelta", "C10ABK", 
           "LKTFPL", "MDS", "NELKSH_WS", "NLKSH_WS", "ISTOK_WS", "S77", 
           "S4_WS", "C10A", "C12A", "C12", "C10", "S352", "S351", "C4A", 
           "C3", "S354", "brighton_WS", "S308", "S65E", "fec_wm", "TOTAL_ISTOK", 
           "S77bf", "S4bp", "S3", "S2", "C12Abp", "C12bp", "C10bp", "C4Abp", 
           "S236", "p5wps", "S308BF", "tcnsq", "S154", "S135", "Residual", 
           "WBDelta", "WBError")
lok.wb=data.frame()
for(i in 1:length(alts.sort)){
  tmp=read.csv(paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/lok_WB.csv"),skip=1,col.names = head.val)
  tmp$Alt=alts[i]
  lok.wb=rbind(lok.wb,tmp)
  print(alts[i])
}

lok.wb$date=with(lok.wb,date.fun(paste(year,month,day,sep="-")))
head(lok.wb)

test=ddply(lok.wb,c("year","Alt"),summarise,S65E.tflow=sum(S65E,na.rm=T),S308BF.tflow=sum(S308BF,na.rm=T),S77.tflow=sum(abs(S77)))
plot(S65E.tflow~year,subset(test,Alt=="NA25"))
with(subset(test,Alt=="NA25"),lines(S308BF.tflow~year,col="red"))
with(subset(test,Alt=="NA25"),lines(S77.tflow~year,col="red"))

test2=reshape2::dcast(test,year~Alt,value.var = "S77.tflow",sum)
test2$FWO_CC.diff=with(test2,NA25-CC)
mean(subset(test2,FWO_CC.diff>0)$FWO_CC.diff)
sum(test2$FWO_CC.diff<0)

###

lok.wb.melt=reshape2::melt(lok.wb,id.vars = c("year","month","day","date","Alt"))
lok.wb.CY=reshape2::dcast(lok.wb.melt,year+Alt~variable,value.var = "value",sum)

plot(abs(S77)~year,subset(lok.wb.CY,Alt=="NA25"))

## 

lok.wb.CY=reshape2::dcast(lok.wb.melt,year+Alt~variable,value.var = "value",sum)
inflow.vars=c("S65E","fec_wm","TOTAL_ISTOK","S4bp","S3","S2","C12Abp","C12bp",
              "C10bp","C4Abp","S236","p5wps","S308BF","tcnsq","S154","S135")
lok.wb.CY$TFlow_in=rowSums(lok.wb.CY[,inflow.vars],na.rm=T)
lok.wb.CY$EAABF=rowSums(lok.wb.CY[,c("S4bp",'S3',"S2")])
lok.wb.CY$S308BF_per=with(lok.wb.CY,S308BF/TFlow_in)*100
lok.wb.CY$EAABF_per=with(lok.wb.CY,EAABF/TFlow_in)*100

ddply(lok.wb.CY,"Alt",summarise,
      mean.S308BF_per=mean(S308BF_per),
      min.S308BF_per=min(S308BF_per),
      max.S308BF_per=max(S308BF_per))

ddply(lok.wb.CY,"Alt",summarise,
      mean.EAABF_per=mean(EAABF_per),
      min.EAABF_per=min(EAABF_per),
      max.EAABF_per=max(EAABF_per))
