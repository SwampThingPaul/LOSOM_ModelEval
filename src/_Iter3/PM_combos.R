# test=expand.grid(var1=1:144,var2=1:144)
# test=test[test$var1!=test$var2,]
# nrow(test[test$var1!=test$var2,])
# 
# library(mgcv)
# 
# X=matrix(c(1:144,1:144),2,144,byrow=T)
# uniquecombs(X)
# 
# 
# combn(1:144,5)

test2=expand.grid(dam=1:4,LOK=5:20,EVER=21:27,CRE=28:79,SLE=80:117,LWL=118,WS=119:132,BGHAB=133:144)
nrow(test2)

test2=expand.grid(LOK=5:20,EVER=21:27,CRE=28:79,SLE=80:117,LWL=118,WS=119:132,BGHAB=133:144)
nrow(test2)/1000000
