

library(zoo)
q.dat1.xtab=reshape2::dcast(q.dat1,Alt+Date+CY~SITE,value.var="FLOW",function(x)mean(x,na.rm=T))
q.dat1.xtab$S79.30d=with(q.dat1.xtab,ave(S79,Alt,FUN=function(x) c(rep(NA,29),rollapply(x,width=30,FUN=function(x)mean(x,na.rm=T)))))

test=subset(q.dat1.xtab,Alt=="NA25")
test=subset(q.dat1.xtab,Alt=="CC")
test$exceed=with(test,ifelse(is.na(S79.30d)==T,0,ifelse(S79.30d<457,1,0)))

## Adapted from mflst_cre_v2.py
for(i in 2:nrow(test)){
  if(test$exceed[i-1]==1&test$exceed[i]==0){
    test$exceed_end[i-1]=1 #found the last exceedance dates 
  }else{
    test$exceed_end[i-1]=0
  }
}

# subset(test,exceed_end==1)

test$countdown=0
test$exceed2=NA
counts=0
exc_n=0
for(i in 30:nrow(test)){
  # rest counts
  if(test$exceed[i-1]==0&test$exceed[i]==1){
    counts=1
  }
  
  if(test$exceed_end[i]==1){
    if(test$countdown[i-1]<1){
      test$countdown[i]=365
    }else{
      test$countdown[i]=test$countdown[i-1]-1
      if(test$countdown[i]==0 & test$exceed[i]==1){
        test$countdown[i]=365
      }
    }
  }else{
    test$countdown[i]=test$countdown[i-1]-1
    counts=counts+1
    
    if(counts>366 & test$exceed[i]==1){
      test$countdown[i]=365
      counts=0
    }
    if(test$countdown[i]==0 & test$exceed[i]==1){
      test$countdown[i]=365
    }
  }
  
  #identify yearly violations
  if(test$countdown[i]<0){
    if(test$exceed[i]==1){
      if(test$exceed[i-1]!=1){
        test$exceed2[i]=1
        exc_n=exc_n+1}else{
          test$exceed2[i]=0
        }
        }else{test$exceed2[i]=0}
  }else{
        if(test$countdown[i]==365 & test$exceed_end[i]==0){
          test$exceed2[i]==1
          exc_n=exc_n+1
        }else{
          test$exceed2[i]=0
        }
      }
    }
  

counts
exc_n

test$plot_exc=with(test,ifelse(countdown<0&exceed==1,S79.30d,NA))
test$plot_exc365=with(test,ifelse(countdown>0&exceed==1,S79.30d,NA))

plot(S79.30d~Date,test,type="n")
abline(h=457,col="brown",lty=2)
with(test,lines(Date,plot_exc,col="orange"))
with(test,lines(Date,plot_exc365,col="grey"))
