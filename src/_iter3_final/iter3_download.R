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
### Iteration 3
### Setting up folders
alts=c("ECB19","NA22f","NA25f","PA22","PA25")
alts.paths=paste0(data.path,"Iteration_3/Model_Output/",alts,"/")
# Folder.Maker(alts.paths)

# ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/
options(timeout = max(300, getOption("timeout")))
## RSMBN 
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Model_Output/",alts[i],"/RSMBN/")
  # filenames=getURL(url,ftp.use.epsv = FALSE, dirlistonly = TRUE)
  # filenames <- strsplit(filenames, "\r\n")
  # filenames = unlist(filenames)
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/RSMBN_output.dss")
  download.file(paste0(url,"RSMBN_output.dss"),dest,mode="wb",cacheOK = F)
}

## RSMGL 
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Model_Output/",alts[i],"/RSMGL/RSMGL_output.dss")
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/RSMGL_output.dss")
  download.file(url,dest,mode="wb")
}

## DMSTA
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Model_Output/",alts[i],"/DMSTA/project_",alts[i],".xlsm")
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/project_",alts[i],".xlsm")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".XLSM_NET_Central_FEB_34.xlsx")
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/PROJECT_",alts[i],".XLSM_NET_Central_FEB_34.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".XLSM_NET_Central_FEB34_OUT.xlsx")
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/PROJECT_",alts[i],".XLSM_NET_Central_FEB34_OUT.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".XLSM_NET_Central_STA2B.xlsx")
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/PROJECT_",alts[i],".XLSM_NET_Central_STA2B.xlsx")
  download.file(url,dest,mode="wb")
  
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Model_Output/",alts[i],"/DMSTA/PROJECT_",alts[i],".XLSM_NET_Central_STA34.xlsx")
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/PROJECT_",alts[i],".XLSM_NET_Central_STA34.xlsx")
  download.file(url,dest,mode="wb")
}



## LOK Daily waterbalance
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Water_Budgets/RSMBN/",alts[i],"/lok.csv")
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/lok_WB.csv")
  download.file(url,dest,mode="wb")
}

## Cal Estuary Daily waterbalance
for(i in 1:length(alts)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Water_Budgets/RSMBN/",alts[i],"/calestuary.csv")
  dest=paste0(data.path,"Iteration_3/Model_Output/",alts[i],"/calestuary.csv")
  download.file(url,dest,mode="wb")
}

# Northern Estuaries Summary
data.path.new=paste0(data.path,"Iteration_3/Model_Output/Northern_Estuaries/")
# Folder.Maker(data.path.new)
files=c("cre_flow_envelope","sle_flow_envelope","lake_worth_lagoon_report")
for(i in 1:length(files)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Northern_Estuaries/",files[i],".txt")
  dest=paste0(data.path.new,files[i],".txt")
  download.file(url,dest,mode="wb")
}

# Northern Estuaries Summary
data.path.new=paste0(data.path,"Iteration_3/Model_Output/Lake_Okeechobee/")
# Folder.Maker(data.path.new)
files=c("lo_mfl.txt","lok_stage_stats_report.txt","lo_mfl.pdf","lok_stage_envelope.pdf","lo_extreme_hi_lo.pdf")
for(i in 1:length(files)){
  url=paste0("ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_3/PM_ECB19_NA22f_NA25f_PA22_PA25/Lake_Okeechobee/",files[i])
  dest=paste0(data.path.new,files[i])
  download.file(url,dest,mode="wb")
}
