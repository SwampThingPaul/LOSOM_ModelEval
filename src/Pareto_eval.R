## 
## LOSOM
## Pareto screening
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
rslt.basline=read.xlsx(paste0(data.path,"Conceptual_Plan/LOSOM_CPA_PARETO_27k_2020Nov10.xlsx"),sheet=2,startRow = 13)[1:2,1:64]
rslt.basline[,3:64]=apply(rslt.basline[,3:64],2,as.numeric)
colnames(rslt.basline)<-c("Pindex","Model_Index",paste0("PM",1:62))

rslt=read.xlsx(paste0(data.path,"Conceptual_Plan/LOSOM_CPA_PARETO_27k_2020Nov10.xlsx"),sheet=2,startRow = 16)[,1:64]
summary(rslt[,3:21])

subset(rslt,Pindex==26803)
subset(rslt,Model_Index=="3_3823")

## MAPL Values
PM1.MAPL=537 # Dam safety>17.25
PM2.MAPL=830 # Dam safety > LORS Zone A
PM3.MAPL=300 # Sam safety >18
PM4.MAPL=20 # Sam safety >18 consecutive
PM5.MAPL=0 # WQBEL index
PM6.MAPL=rslt.basline$PM6[1] # SOTF SCYP demand
PM7.MAPL=rslt.basline$PM7[1] # SOTF BR deman
PM8.MAPL=rslt.basline$PM8[1] # LOSA cutback
PM9.MAPL=rslt.basline$PM9[1] # LOSA irrigation
PM10.MAPL=rslt.basline$PM10[1] # LOSA water shortage
PM11.MAPL=rslt.basline$PM11[2] # Central Flow path to GE
PM12.MAPL=rslt.basline$PM12[1] # Lake Stage Env - lower penalty 
PM13.MAPL=rslt.basline$PM13[2] # Lake Stage Env - upper penalty
PM14.MAPL=rslt.basline$PM14[2] # CAE High flow count
PM15.MAPL=rslt.basline$PM15[2] # CAE Damaging flow count
PM16.MAPL=rslt.basline$PM16[2] # SLE High flow count
PM17.MAPL=rslt.basline$PM17[2] # SLE Damaging flow count
PM18.MAPL=rslt.basline$PM18[1] # Lake < 12.56 Ft (NAV)
PM19.MAPL=rslt.basline$PM19[1] # Lake < 11.0  Ft (REC)

##beta version
PM39.MAPL=99999 # rslt.basline$PM39[2] # Percentage of the time wihtin the Stage envelope
PM46.MAPL=99999 # Flow k-Acft south S351,354
PM48.MAPL=99999 # count of 14-days Flows <457cfs
PM51.MAPL=99999 # CRE Optimum Flows; FWO 600
PM54.MAPL=99999 # count of 14-days flow >6500
PM58.MAPL=99999 # SLE Optimum Flows; FWO 831

##
scn.val=with(rslt,ifelse(PM1<=PM1.MAPL&
                         PM2<=PM2.MAPL&
                         PM3<=PM3.MAPL&
                         PM4<=PM4.MAPL&
                         PM5<=PM5.MAPL&
                         PM6<=PM6.MAPL&
                         PM7<=PM7.MAPL&
                         PM8<=PM8.MAPL&
                         PM9<=PM9.MAPL&
                         PM10<=PM10.MAPL&
                         PM11<=PM11.MAPL&
                         PM12<=PM12.MAPL&
                         PM13<=PM13.MAPL&
                         PM14<=PM14.MAPL&
                         PM15<=PM15.MAPL&
                         PM16<=PM16.MAPL&
                         PM17<=PM17.MAPL&
                         PM18<=PM18.MAPL&
                         PM19<=PM19.MAPL&
                           PM48<=PM48.MAPL&
                           PM54<=PM54.MAPL,Pindex,NA))
                           #PM39<=PM39.MAPL&
                           #PM46<=PM46.MAPL&
                           #PM51<=PM51.MAPL&
                           #PM58<=PM58.MAPL))
N.obs(scn.val)

tmp105=subset(rslt,Pindex%in%scn.val)
View(head(tmp105[order(tmp105$PM6,tmp105$PM7,tmp105$PM8,tmp105$PM9,tmp105$PM10,
            -tmp105$PM11,-tmp105$PM12,-tmp105$PM13,-tmp105$PM14,-tmp105$PM15,
            -tmp105$PM16,-tmp105$PM17,tmp105$PM18,tmp105$PM19),]))

subset(tmp105,Pindex%in%c(3742,5930,7023,6653,8307))
subset(tmp105,Model_Index%in%c("LORS08simp_2253","1C-1_6622"))
subset(tmp105,Pindex%in%c(8159,8932,8217,3329))

tmp105[order(tmp105$PM48,tmp105$PM54),c("Model_Index","PM48","PM54")]

tmp105[order(-tmp105$PM51,tmp105$PM12,-tmp105$PM46,-tmp105$PM58),c("Model_Index","PM51","PM58","PM46","PM12")]

## Re-define MAPLs for FWO only
PM6.MAPL=rslt.basline$PM6[2] # SOTF SCYP demand
PM7.MAPL=rslt.basline$PM7[2] # SOTF BR deman
PM8.MAPL=rslt.basline$PM8[2] # LOSA cutback
PM9.MAPL=rslt.basline$PM9[2] # LOSA irrigation
PM10.MAPL=rslt.basline$PM10[2] # LOSA water shortage
PM11.MAPL=rslt.basline$PM11[2] # Central Flow path to GE
PM12.MAPL=rslt.basline$PM12[2] # Lake Stage Env - lower penalty 
PM13.MAPL=rslt.basline$PM13[2] # Lake Stage Env - upper penalty
PM14.MAPL=rslt.basline$PM14[2] # CAE High flow count
PM15.MAPL=rslt.basline$PM15[2] # CAE Damaging flow count
PM16.MAPL=rslt.basline$PM16[2] # SLE High flow count
PM17.MAPL=rslt.basline$PM17[2] # SLE Damaging flow count
PM18.MAPL=rslt.basline$PM18[2] # Lake < 12.56 Ft (NAV)
PM19.MAPL=rslt.basline$PM19[2] # Lake < 11.0  Ft (REC)
PM61.MAPL=9999 # S308 Regulatory flow

scn.val=with(rslt,ifelse(PM1<=PM1.MAPL&
                           PM2<=PM2.MAPL&
                           PM3<=PM3.MAPL&
                           PM4<=PM4.MAPL&
                           PM5<=PM5.MAPL&
                           PM6<=PM6.MAPL&
                           PM7<=PM7.MAPL&
                           PM8<=PM8.MAPL&
                           PM9<=PM9.MAPL&
                           PM10<=PM10.MAPL&
                           PM11<=PM11.MAPL&
                           PM12<=PM12.MAPL&
                           PM13<=PM13.MAPL&
                           PM14<=PM14.MAPL&
                           PM15<=PM15.MAPL&
                           PM16<=PM16.MAPL&
                           PM17<=PM17.MAPL&
                           PM18<=PM18.MAPL&
                           PM19<=PM19.MAPL&
                           PM48<=PM48.MAPL&
                           PM54<=PM54.MAPL&
                           PM61<=PM61.MAPL,Pindex,NA))

noharm.FWO=subset(rslt,Pindex%in%scn.val)

screen=noharm.FWO[order(-noharm.FWO$PM51,noharm.FWO$PM61,noharm.FWO$PM12,-noharm.FWO$PM46),c("Pindex","Model_Index","PM51","PM12","PM46","PM61")]
screen[1:5,]

screen[1:5,]%>%
  flextable()%>%
  colformat_double(j=c(1,4),digits=0,na_str="---",big.mark = "")%>%
  colformat_double(j=5,digits=1,na_str="---",big.mark = "")%>%
  fontsize(size=11,part="body")%>%
  fontsize(size=12,part="header")%>%
  padding(padding=1,part="all")%>%
  set_header_labels(PM51="CAE Optimal Flow\n(PM51)",
                    PM58="SLE Optimal Flow\n(PM58) ",
                    PM46="Flow South\n(PM46) ",
                    PM12="Lake Okeechobee Stage\nRec.  Envelope\n(PM12)")%>%
  align(j=1:2,align="left",part="all")%>%
  align(j=3:6,align="center",part="all")%>%
  font(fontname="Times New Roman",part="all")%>%
  footnote(part="header",j=3,
           value=as_paragraph("Number of times 14-day moving average >= 750 and < 2100 cfs."),
           ref_symbols =c(" 1 "))%>%
  footnote(part="header",j=4,
           value=as_paragraph("Count of weighted exceedances (penalty) below LOK stage envelope."),
           ref_symbols =c(" 2 "))%>%
  footnote(part="header",j=5,
           value=as_paragraph("Average annual flow for POR (S351, S354)."),
           ref_symbols =c(" 3 "))%>%
  footnote(part="header",j=6,
           value=as_paragraph("Number of times 14-day moving average >= 150 and < 1400 cfs."),
           ref_symbols =c(" 4 "))%>%print(preview="docx")
  

#### 
## Lee County Team screening
subset(rslt,Pindex%in%c(6329,25731,26803,26597,26516,26803))
lee.screen=subset(rslt,Model_Index%in%c("1C-1_8086","4C-3_1310","4C-3_4418","4C-3_3822","4C-3_3593","3_3823","3_5602"))

lee.screen=lee.screen[match(c("1C-1_8086","4C-3_1310","4C-3_4418","4C-3_3822","4C-3_3593","3_3823","3_5602"),lee.screen$Model_Index),]

vars=c("Pindex","Model_Index",paste0("PM",c(48,49,51,14,52,53,54,55,56,61,12,13,46,6:10)))
lee.screen[,vars]%>%write.csv(paste0(export.path,"lee.screen.csv"),row.names = F)
rslt.basline[,vars]%>%write.csv(paste0(export.path,"lee.screen.base.csv"),row.names = F)
## Peters sorting
# PM61 Average annual flow for S308 regulatory flow discharge.
# PM48 <457
# PM54 >6500
# PM53 4500 - 6000
# PM52 2600 - 4500
lee.screen1=subset(lee.screen1,PM61>220&
                     PM48<rslt.basline$PM48[2]&
                     PM54<rslt.basline$PM54[2]&
                     PM53<rslt.basline$PM53[2])
lee.screen1[order(lee.screen1$PM48,lee.screen1$PM54,lee.screen1$PM53),]

plot(PM52~PM61,lee.screen1)
          
range(lee.screen1$PM52)
rslt.basline$PM52[2]
# -------------------------------------------------------------------------
####
summary(tmp105[,c("PM48","PM54")])
PM48.MAPL.rng=seq(min(tmp105$PM48),max(tmp105$PM48),length.out=50)
PM54.MAPL.rng=seq(min(tmp105$PM54),max(tmp105$PM54),length.out=50)

set.seed(123)
plans=data.frame(tmp105$Pindex)
for(i in 1:50){
  PM48.MAPL=sample(PM48.MAPL.rng,1)
  PM54.MAPL=sample(PM54.MAPL.rng,1)
  
  tmp.scn=data.frame(scn=with(tmp105,ifelse(PM1<=PM1.MAPL&
                             PM2<=PM2.MAPL&
                             PM3<=PM3.MAPL&
                             PM4<=PM4.MAPL&
                             PM5<=PM5.MAPL&
                             PM6<=PM6.MAPL&
                             PM7<=PM7.MAPL&
                             PM8<=PM8.MAPL&
                             PM9<=PM9.MAPL&
                             PM10<=PM10.MAPL&
                             PM11<=PM11.MAPL&
                             PM12<=PM12.MAPL&
                             PM13<=PM13.MAPL&
                             PM14<=PM14.MAPL&
                             PM15<=PM15.MAPL&
                             PM16<=PM16.MAPL&
                             PM17<=PM17.MAPL&
                             PM18<=PM18.MAPL&
                             PM19<=PM19.MAPL&
                             PM48<=PM48.MAPL&
                             PM54<=PM54.MAPL,Model_Index,NA)))
  colnames(tmp.scn)<-paste("iter",i,sep="_")
  plans=cbind(plans,tmp.scn)
}
plans.melt=reshape::melt(plans,id.vars = "tmp105.Pindex")
plans.melt=subset(plans.melt,is.na(value)==F)

test=ddply(plans.melt,"value",summarise,N.val=N.obs(value))

head(test)
test=test[order(-test$N.val),]
head(test)
subset(test,N.val>25)

### 

PM13.MAPL.rng=seq(3000,12160,10)
PM15.MAPL.rng=seq(200,333,2)
PM17.MAPL.rng=seq(440,582,2)
PM39.MAPL.rng=seq(0.27,0.5,0.1)
PM46.MAPL.rng=seq(56,200,5)


summary(rslt[,paste0("PM",11:19)])
PM12.MAPL.rng=seq(quantile(rslt$PM12,probs=0.25),rslt.basline$PM12[1],100) # Lake Stage Env - lower penalty 
PM13.MAPL.rng=seq(quantile(rslt$PM13,probs=0.25),rslt.basline$PM13[2],100) # Lake Stage Env - upper penalty
PM14.MAPL.rng=seq(quantile(rslt$PM14,probs=0.25),rslt.basline$PM14[2],10) # CAE High flow count
PM15.MAPL.rng=seq(quantile(rslt$PM15,probs=0.25),rslt.basline$PM15[2],10) # CAE Damaging flow count
PM16.MAPL.rng=seq(quantile(rslt$PM16,probs=0.25),rslt.basline$PM16[2],10) # SLE High flow count
PM17.MAPL.rng=seq(quantile(rslt$PM17,probs=0.25),rslt.basline$PM17[2],10) # SLE Damaging flow count

set.seed(123)
plans=data.frame(rslt$Pindex)
for(i in 1:50){

  PM12.MAPL=sample(PM12.MAPL.rng,1)
  PM13.MAPL=sample(PM13.MAPL.rng,1)
  PM14.MAPL=sample(PM14.MAPL.rng,1)
  PM15.MAPL=sample(PM15.MAPL.rng,1)
  PM16.MAPL=sample(PM16.MAPL.rng,1)
  PM17.MAPL=sample(PM17.MAPL.rng,1)
  
tmp=data.frame(scn=with(rslt,ifelse(PM1<=PM1.MAPL&
                   PM2<=PM2.MAPL&
                   PM3<=PM3.MAPL&
                   PM4<=PM4.MAPL&
                   PM5<=PM5.MAPL&
                   PM6<=PM6.MAPL&
                   PM7<=PM7.MAPL&
                   PM8<=PM8.MAPL&
                   PM9<=PM9.MAPL&
                   PM10<=PM10.MAPL&
                   PM11<=PM11.MAPL&
                   PM12<=PM12.MAPL&
                   PM13<=PM13.MAPL&
                   PM14<=PM14.MAPL&
                   PM15<=PM15.MAPL&
                   PM16<=PM16.MAPL&
                   PM17<=PM17.MAPL&
                   PM18<=PM18.MAPL&
                   PM19<=PM19.MAPL&
                   PM39<=PM39.MAPL&
                   PM46<=PM46.MAPL&
                   PM51<=PM51.MAPL&
                   PM58<=PM58.MAPL,Model_Index,NA)))
colnames(tmp)<-paste("iter",i,sep="_")
plans=cbind(plans,tmp)
}

plans.melt=reshape::melt(plans,id.vars = "rslt.Pindex")
plans.melt=subset(plans.melt,is.na(value)==F)

test=ddply(plans.melt,"value",summarise,N.val=N.obs(rslt.Pindex))

head(test)
head(test[order(-test$N.val),])

subset(test,N.val>=3)

subset(rslt,Model_Index%in%subset(test,N.val>3)$value)[,c("Model_Index",paste0("PM",c(13,15,17,39,46)))]%>%
  flextable()%>%
  colformat_double(j=2,digits=0,na_str="---",big.mark = "")%>%
  colformat_double(j=5:6,digits=2,na_str="---",big.mark = "")%>%
  fontsize(size=11,part="body")%>%
  fontsize(size=12,part="header")%>%
  padding(padding=0.5,part="all")%>%
  font(fontname="Times New Roman",part="all")%>%autofit()#%>%print(preview="docx")
