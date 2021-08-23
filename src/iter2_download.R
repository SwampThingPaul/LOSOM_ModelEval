## 
## LOSOM
##
## Data download
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
library(RCurl)

## Paths on your PC
wd="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

# -------------------------------------------------------------------------
### Setting up folders
alts=c("ECBr",'NA25',"AA","BB","CC","DD","EE1","EE2")
alts.paths=paste0(data.path,"Iteration_2/Model_Output/",alts,"/")
#Folder.Maker(alts.paths)

## Downloading files
# https://felixfan.github.io/download-files/
# test=getURL(paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[1],"/RSMBN/"), ftp.use.epsv = FALSE, dirlistonly = TRUE)
# strsplit(test,"\r\n")

options(timeout = max(300, getOption("timeout")))
## RSMBN 
alts=c("ECBr","NA25","AA","BB","CC")
for(i in 1:length(alts)){
url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[i],"/RSMBN/")
# filenames=getURL(url,ftp.use.epsv = FALSE, dirlistonly = TRUE)
# filenames <- strsplit(filenames, "\r\n")
# filenames = unlist(filenames)
dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/RSMBN_output.dss")
download.file(paste0(url,"RSMBN_output.dss"),dest,mode="wb",cacheOK = F)
}

alts=c("DD","EE1","EE2")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/Model_Output/",alts[i],"/RSMBN/")
  # filenames=getURL(url,ftp.use.epsv = FALSE, dirlistonly = TRUE)
  # filenames <- strsplit(filenames, "\r\n")
  # filenames = unlist(filenames)
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/RSMBN_output.dss")
  download.file(paste0(url,"RSMBN_output.dss"),dest,mode="wb",cacheOK = F)
}


## RSMGL 
alts=c("ECBr","NA25","AA","BB","CC")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[i],"/RSMGL/RSMGL_output.dss")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/RSMGL_output.dss")
  download.file(url,dest,mode="wb")
}

alts=c("DD","EE1","EE2")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/Model_Output/",alts[i],"/RSMGL/RSMGL_output.dss")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/RSMGL_output.dss")
  download.file(url,dest,mode="wb")
}
## Downloaded 2021-06-09

## Northern Estuaries
alts=c("ECBr","NA25","AA","BB","CC")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[i],"/RSMGL/RSMGL_output.dss")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/RSMGL_output.dss")
  download.file(url,dest,mode="wb")
}

## LOK Daily waterbalance
alts=c("ECBr","NA25","AA","BB","CC")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/WaterBudgets/RSMBN/",alts[i],"/lok.csv")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/lok_WB.csv")
  download.file(url,dest,mode="wb")
}

alts=c("DD","EE1","EE2")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/WaterBudgets/RSMBN/",alts[i],"/lok.csv")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/lok_WB.csv")
  download.file(url,dest,mode="wb")
}

## Cal Estuary Daily waterbalance
alts=c("ECBr","NA25","AA","BB","CC")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/WaterBudgets/RSMBN/",alts[i],"/calestuary.csv")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/calestuary.csv")
  download.file(url,dest,mode="wb")
}

alts=c("DD","EE1","EE2")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/WaterBudgets/RSMBN/",alts[i],"/calestuary.csv")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/calestuary.csv")
  download.file(url,dest,mode="wb")
}

## Cal Estuary Daily waterbalance
# Did not download
# alts=c("ECBr","NA25","AA","BB","CC")
# for(i in 1:length(alts)){
#   url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/WaterBudgets/RSMBN/",alts[i],"/SLE.csv")
#   dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/SLE.csv")
#   download.file(url,dest,mode="wb")
# }
# 
# alts=c("DD","EE1","EE2")
# for(i in 1:length(alts)){
#   url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/WaterBudgets/RSMBN/",alts[i],"/SLE.csv")
#   dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/SLE.csv")
#   download.file(url,dest,mode="wb")
# }

# Northern Estuaries Summary
data.path.new=paste0(data.path,"Iteration_2/Model_Output/Northern_Estuaries/")
# Folder.Maker(data.path.new)
files=c("cre_flow_envelope","sle_flow_envelope","lake_worth_lagoon_report")
for(i in 1:length(files)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Northern_Estuaries/",files[i],".txt")
  dest=paste0(data.path.new,files[i],"AA_BB_CC.txt")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/Northern_Estuaries/",files[i],".txt")
  dest=paste0(data.path.new,files[i],"DD_EE1_EE2.txt")
  download.file(url,dest,mode="wb")
  print(i)
}

## DMSTA
alts=c("ECBr","NA25","AA","BB","CC")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[i],"/DMSTA/project_",alts[i],".xlsm")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/project_",alts[i],".xlsm")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".xlsm_NET_Central_FEB_34.xlsx")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/PROJECT_",alts[i],".xlsm_NET_Central_FEB_34.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".xlsm_NET_Central_FEB34_OUT.xlsx")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/PROJECT_",alts[i],".xlsm_NET_Central_FEB34_OUT.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".xlsm_NET_Central_STA2B.xlsx")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/PROJECT_",alts[i],".xlsm_NET_Central_STA2B.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".xlsm_NET_Central_STA34.xlsx")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/PROJECT_",alts[i],".xlsm_NET_Central_STA34.xlsx")
  download.file(url,dest,mode="wb")
}


alts=c("DD","EE1","EE2")
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/Model_Output/",alts[i],"/DMSTA/project_",alts[i],".xlsm")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/project_",alts[i],".xlsm")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".xlsm_NET_Central_FEB_34.xlsx")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/PROJECT_",alts[i],".xlsm_NET_Central_FEB_34.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".xlsm_NET_Central_FEB34_OUT.xlsx")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/PROJECT_",alts[i],".xlsm_NET_Central_FEB34_OUT.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".xlsm_NET_Central_STA2B.xlsx")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/PROJECT_",alts[i],".xlsm_NET_Central_STA2B.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_DD_EE1_EE2/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".xlsm_NET_Central_STA34.xlsx")
  dest=paste0(data.path,"Iteration_2/Model_Output/",alts[i],"/PROJECT_",alts[i],".xlsm_NET_Central_STA34.xlsx")
  download.file(url,dest,mode="wb")
}

## Batch Results
dest=paste0(data.path,"Iteration_2/Model_Output/_Batch_Results/")
# Folder.Maker(dest)

alts.val=c("AA","BB","CC","DD")
for(i in 1:length(alts.val)){
  url=paste0("ftp://ftppub.sfwmd.gov/pub/jabarne/Batch_Results/LOSOM_Iteration2_Alternative",alts.val[i],"_19May2021.xlsx")
  dest2=paste0(dest,"LOSOM_Iteration2_Alternative",alts.val[i],"_19May2021.xlsx")
  download.file(url,dest2,mode="wb")
}