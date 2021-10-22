## 
## LOSOM - DMSTA Summary
##
## Iteration 1 alternative evaluation
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

##GGPLOT theme defaults
theme_set(theme_minimal(base_size = 16))


# -------------------------------------------------------------------------
alts.sort=c("LSM25B", "LSMECB","ABNE", "ECRE", "ELOK","ESFL", "ESLE", "WAS","WRDC","WRDS","NAV","REC","SPAS","SPEF","SPLC")
STA.val=c("STA2B","STA34","FEB_34")

DMSTA.sum=data.frame()
for(i in 1:length(alts.sort)){
  for(j in 1:length(STA.val)){

path.val=paste0(data.path,"Iteration_1/Model_Output/",alts.sort[i],"/DMSTA/PROJECT_",alts.sort[i],".XLSM_NET_Central_",STA.val[j],".xlsx")
tmp=read.xlsx(path.val,sheet=3,startRow=7)
colnames(tmp)<-c("term","cell","area.km2","flow.hm3yr","flow.cfs","flow.acftyr","load.kgyr","FWM.conc.ugL","GM.conc.ugL","storage.mgm2","depth.cm","freqLT5cm","dwnstrm.cell","param.set")
tmp=tmp[1:18,]
tmp$Alt=alts.sort[i]
tmp$STA=STA.val[j]
DMSTA.sum=rbind(DMSTA.sum,tmp)
print(STA.val[j])
  }
  print(alts.sort[i])
}

DMSTA.sum[,c(3:12)]=sapply(DMSTA.sum[,c(3:12)],as.numeric)
head(DMSTA.sum)
subset(DMSTA.sum,term=="Bypass")

##
bypass.vals=cast(subset(DMSTA.sum,term=="Bypass"),STA~Alt,value="flow.acftyr",mean)
bypass.vals=bypass.vals[,c("STA",alts.sort)]

bypass.vals2=cast(subset(DMSTA.sum,term=="Bypass"),Alt~STA,value="flow.acftyr",mean)
bypass.vals2=bypass.vals2[match(alts.sort,bypass.vals2$Alt),]
##
FEB_34_release=cast(subset(DMSTA.sum,term%in%c("Release 1","Release 2","Total Surface Outflow")&STA=="FEB_34"),Alt~term,value="flow.acftyr",mean)
colnames(FEB_34_release)<-c("Alt","Rel1","Rel2","TFlow")

FEB_34_release$perSTA34=with(FEB_34_release,Rel1/TFlow)*100
FEB_34_release$perSTA2B=with(FEB_34_release,Rel2/TFlow)*100
FEB_34_release$Alt=factor(FEB_34_release$Alt,levels=alts.sort)
FEB_34_release=FEB_34_release[match(alts.sort,FEB_34_release$Alt),]

cap.val="Summary of bypass flows (x1000 Ac-Ft Yr\u207B\u00B9) for each feature and alternative."
bypass.vals2%>%
  flextable()%>%
  colformat_double(j=2:4,digits=1,na_str="---")%>%
  fontsize(size=9,part="body")%>%
  fontsize(size=10,part="header")%>%
  font(fontname="Times New Roman",part="all")%>%
  align(j=2:4,part="all",align="center")%>%
  bold(part="header")%>%
  padding(padding=0.5,part="all")%>%
  set_header_labels("FEB_34"="A1 FEB",
                    "STA2B"="STA-2",
                    "STA34"="STA-3/4")%>%
  add_header_lines(values=cap.val)%>%print(preview="docx")

cap.val="Percent A1 FEB discharge to STA 3/4 and STA 2."
FEB_34_release[,c("Alt","perSTA34","perSTA2B")]%>%
  flextable()%>%
  colformat_double(j=2:3,digits=1,na_str="---")%>%
  fontsize(size=9,part="body")%>%
  fontsize(size=10,part="header")%>%
  font(fontname="Times New Roman",part="all")%>%
  align(j=2:3,part="all",align="center")%>%
  bold(part="header")%>%
  padding(padding=0.5,part="all")%>%
  add_header_lines(values=cap.val)%>%
  set_header_labels("perSTA34"="STA-3/4",
                    "perSTA2B"="STA-2")%>%
  footnote(part="header",j=1,
           value=as_paragraph("Under Restoration Strategies it was assumed that FEB discharges would be split 81% to STA3/4 and 19% to STA2"),ref_symbols =c(" 1 "))%>%
  footnote(part="header",j=2:3,
           value=as_paragraph("In DMSTA (FEB_34 file), it was assumed that Release 1 was to STA 3/4 and Release 2 was to STA 2."),ref_symbols =c(" 2 "))%>%print(preview="docx")
