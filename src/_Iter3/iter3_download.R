

## Download Iteration 3 Phase 1 data
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


# -------------------------------------------------------------------------
link="ftp://ftppub.sfwmd.gov/pub/jabarne/DOI/"

result=getURL(link)
result2=strsplit(result, "\r*\n")[[1]]
result2=strsplit(result2,"\\s+")

alt=sapply(result2,"[",9)
alt=alt[3:length(alt)]
alt=alt[!(alt%in%c("NA25","CCTSP"))]

data.path1="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval/Data/Iteration_3_Tech_Discussion/DSSfiles"
# Folder.Maker(paste(data.path1,alt,sep="/"))
meta.data=data.frame()
for(i in 1:length(alt)){
tmp=getURL(paste0(link,alt[i],"/"))
tmp=strsplit(tmp, "\r*\n")[[1]]
if(length(tmp)>3){
  for(j in 3:length(tmp)){
    tmp2=strsplit(tmp,"\\s+")[[j]][9]
    meta.data=rbind(meta.data,
                    data.frame(Alt=alt[i],output.info=tmp2))    
  }
}else{
tmp=strsplit(tmp,"\\s+")[[3]][9]
meta.data=rbind(meta.data,
                data.frame(Alt=alt[i],output.info=tmp))
}
print(i)

}
meta.data
# write.csv(meta.data,paste0(data.path1,"Iteration_3_Tech_Discussion/DSSfiles/output_metadata_info.csv"),row.names=F)

for(i in 1:length(alt)){
  
tmp=getURL(paste0(link,alt[i],"/"))
tmp=strsplit(tmp, "\r*\n")[[1]]
tmp=strsplit(tmp,"\\s+")[[3]][9]
tmp

  url=paste0(link,alt[i],"/",tmp,"/")
  dest=paste0(data.path1,"/",alt[i],"/RSMBN_output.dss")
  download.file(paste0(url,"RSMBN_output.dss"),dest,mode="wb",cacheOK = F)
  print(i)
}

## Manually downloaded CCsimp4 #1
# -------------------------------------------------------------------------
PMgrps.all=c(c('NA25','CC',paste0("CC_S",c(1,2,3,"3-4",4,5,6,7))),
             c('CCTSP',"CCsimp4","CCsimp5","CCsimp5a"),
             c('simp4','simp4s1'),
             c("CCsimp4(2)","smp4ZFS","smp4ZFW","smp4ZFSW"),
             c("271DS","271DSZC"),
             c("ZB_S77"),
             c("CCR1","R1ABZD","R1ABZC","R1ABZB"),
             c("CCR1ED","CCR1MD1","CCR1MD2"),
             c("R1WSEN","R1WSMF"))
alt
dput(alt)
PMgrps.all
PMgrps.all[duplicated(c(PMgrps.all,alt))==F]

PMgrps.all[order(match(PMgrps.all,alt))][16:length(PMgrps.all)]



##### 
alt=paste0("CC_S",c(2,3,"3-4",4,5,7))
data.path1="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval/Data/Iteration_3_Tech_Discussion/DSSfiles"
# Folder.Maker(paste(data.path1,alt,sep="/"))

for(i in 1:length(alt)){
  
  tmp=getURL(paste0(link,alt[i],"/"))
  tmp=strsplit(tmp, "\r*\n")[[1]]
  tmp=strsplit(tmp,"\\s+")[[3]][9]
  tmp
  
  url=paste0(link,alt[i],"/",tmp,"/")
  dest=paste0(data.path1,"/",alt[i],"/RSMBN_output.dss")
  download.file(paste0(url,"RSMBN_output.dss"),dest,mode="wb",cacheOK = F)
  print(i)
}
