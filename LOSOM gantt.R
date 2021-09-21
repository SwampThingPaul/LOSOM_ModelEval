## 
## LOSOM - gant chart
##
## Iteration 2 alternative evaluation
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
library(plyr)
library(reshape2)


## Data

dat=data.frame(event=c("Public Scoping Meetings","Lake Management Webinars","Public Workshop","1st PDT",
                       "Develop & Evaluate Conceptual Plan","Iteration 1 Analysis","Iteration 2",
                       "Iteration 3","Iteration 3","Iteration 3","Iteration 3",
                       "Draft EIS/WCP Devlopment","DRAFT EIS/WCP Review","Final EIS"),
               date.start=c("2019-02-05","2019-05-20","2019-09-16","2019-08-20","2019-05-21",
                            "2021-01-26","2021-05-10","2021-09-09",
                            "2021-09-09","2021-09-09","2021-10-15","2022-02-1"),
               date.end=c("2019-02-28","2019-06-04","2019-09-17","2019-08-20","2021-01-26",
                          "2021-05-07","2021-09-06","2021-10-14","2021-10-14","2021-10-14","2021-10-14","2022-02-11"),
               iteration=c(rep("scoping",3),rep("Plan Formulation",2),
                           "Iteration 1",
                           "Balanced Array",
                           "Recommended Schedule",
                           "Preferred Alternative",
                           "Operational Guidance",NA,NA,NA))
