## Bone Yard
# Daily Data --------------------------------------------------------------
# lakeO.stage2=lakeO.stage# subset(lakeO.stage,Alt=="PA25")
lakeO.stage2=merge(lakeO.stage,
                   data.frame(Date=seq(date.fun("1965-01-01"),date.fun("2016-12-31"),"1 days"),
                              time.val=1:18993),
)
lakeO.stage2=lakeO.stage2[order(lakeO.stage2$Alt,lakeO.stage2$Date),]
lakeO.stage2$DY=lubridate::decimal_date(lakeO.stage2$Date)
# lakeO.stage2$ModHigh.HiLo=with(lakeO.stage2,ifelse(ModHigh==1,1,2))
# lakeO.stage2$ModLow.HiLo=with(lakeO.stage2,ifelse(ModLow==1,1,2))

fit.ModHigh=survfit(formula=Surv(time.val,ModHigh)~Alt,data=lakeO.stage2)
## estimate probability of high stage in 1 year (time variable - month)
plot(fit.ModHigh)
summary(fit.ModHigh)
summary(fit.ModHigh)$table
# probability of "surviving" 52 year
test=summary(fit.ModHigh,times=365.25*52)
test$surv

## PA25 has a lower probability of an event not happening.

# naive probability
ddply(lakeO.stage2,"Alt",summarise,modhigh.N=sum(ModHigh),N.val=N.obs(ModHigh),prob.modhigh=modhigh.N/N.val)


test2=summary(fit.ModHigh)
unlist(test2)

ggsurvplot(fit.ModHigh,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF","red"))
ggsurvplot(fit.ModHigh,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF","red"),
           fun = "event")

ggsurvplot(fit.ModHigh,
           pval = F, conf.int = F,
           risk.table = F, # Add risk table
           # risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF","red"))

# cummulative hazard
ggsurvplot(fit.ModHigh,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF","red"),
           fun = "cumhaz")
# number of events that would be expected for each alternatives during POS if 
# the event were repeatable process

surv_diff=survdiff(Surv(time.val,ModHigh)~Alt,data=lakeO.stage2)
1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
surv_diff

res.cox=coxph(Surv(time.val,ModHigh)~Alt,data=lakeO.stage2)
summary(res.cox)
cox.zph(res.cox)
# 4x as many exceedances of 16 Ft in PA25 than NA25

# annual ------------------------------------------------------------------

ann.peak=ddply(lakeO.stage,c("CY","Alt"),summarise,max.stg=max(STAGE,na.rm=T),min.stg=min(STAGE,na.rm=T))
ann.peak=merge(ann.peak,data.frame(CY=1965:2016,CY.num=1:52),"CY")
ann.peak$GT17=with(ann.peak,ifelse(max.stg>=17,1,0))
ann.peak$GT16=with(ann.peak,ifelse(max.stg>=16,1,0))

ann.peak$LT11=with(ann.peak,ifelse(min.stg<=11,1,0))
ann.peak$LT10=with(ann.peak,ifelse(min.stg<=10,1,0))


# ann.peak=subset(ann.peak,Alt=='PA25')
ann.peak$HiLo=with(ann.peak,ifelse(GT17==1,2,1))
ann.peak$HiLo=with(ann.peak,ifelse(GT16==1,2,1))
ann.peak$HiLo=with(ann.peak,ifelse(LT11==1,2,1))
with(ann.peak,Surv(CY,HiLo))

# ann.peak=subset(ann.peak,Alt%in%c("NA25f","PA25"))


fit=survfit(formula=Surv(CY.num,LT11)~Alt,data=ann.peak)
## estimate probability of high stage in 1 year (time variable - month)
summary(fit)
ddply(ann.peak,'Alt',summarise,N=sum(GT16))

summary(fit)$table

# Log-Rank test comparing survival curves: survdiff()
surv_diff <- survdiff(Surv(CY.num,LT11)~Alt,data=ann.peak)
surv_diff

ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = cols.alts2[1:3])

# cummulative hazard
ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF","red"),
           fun = "cumhaz")

# surv_summary(fit)

# Log-Rank test comparing survival curves: survdiff()
surv_diff <- survdiff(Surv(CY.num,HiLo)~Alt,data=ann.peak)
surv_diff

surv_diff <- survdiff(Surv(CY.num,HiLo)~Alt,data=subset(ann.peak,Alt%in%alts.sort2[2:3]))
surv_diff

surv_diff <- survdiff(Surv(CY.num,HiLo)~Alt,data=subset(ann.peak,Alt%in%alts.sort2[c(1,3)]))
surv_diff

res.cox=coxph(Surv(CY.num,HiLo)~Alt,data=ann.peak)
summary(res.cox)


fit=survfit(formula=Surv(CY.num,GT17)~Alt,data=ann.peak)
## estimate probability of high stage in 1 year (time variable - month)
summary(fit)


ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = cols.alts2[1:3])


ann.peak$Alt=as.factor(ann.peak$Alt)
res.cox=coxph(Surv(CY.num,GT16)~Alt,data=ann.peak)
summary(res.cox)

test.ph=cox.zph(res.cox)
ggcoxzph(test.ph)
ggcoxdiagnostics(res.cox, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(res.cox, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())







## 
library(extremeStat)

data(annMax) 
dlf <-distLextreme(annMax, selection=c("wak","gum","gev","nor")) 
dlfB <-distLexBoot(dlf, nbest=4, conf.lev=0.5, n=10) # n low for quick example tests 
plotLexBoot(dlfB)

# Estimate discharge that could occur every 80 years (at least empirically):
Q80 <-distLextreme(dlf=dlf, RPs=80)$returnlev
round(sort(Q80[1:17,1]),1) 
# 99 to 143 m^3/s can make a relevant difference in engineering! 
# That's why the rows weighted by GOF are helpful. Weights are given as in 
plotLweights(dlf) # See also section weighted mean below # For confidence intervals see ?distLexBoot


dlf <-distLfit(annMax)
str(dlf, max.lev=2) 
printL(dlf) 
plotLfit(dlf)

data(annMax)
dlf <- distLextreme(annMax, selection=c("wak","gum","gev","nor"))
dlfB <- distLexBoot(dlf, nbest=4, conf.lev=0.5, n=10) # n low for quick example tests
plotLexBoot(dlfB)


ann.peak=ddply(lakeO.stage,c("CY","Alt"),summarise,max.stg=max(STAGE,na.rm=T))
ann.peak$GT17=with(ann.peak,ifelse(round(max.stg,1)>=16.9,1,0))

dat=subset(ann.peak,Alt=='PA25')$max.stg
# dat <- dat[dat>=17]
dat <- sort(dat, decreasing=TRUE)

dn <- lmomco::dist.list()
names(dn) <- dn

# L-Moments of sample  # package lmomco
mom <- lmomco::lmoms(dat, nmom=5)
gum=lmomco::lmom2par(mom, type="gum")

para=lmomco::vec2par(c(gum$para),type="gum")

tcdfs=lmomco::plmomco(dat,para)
ecdfs <- ecdf(dat)(dat) # Empirical CDF

berryFunctions::rmse(tcdfs,ecdfs)
berryFunctions::rsquare(tcdfs,ecdfs)

distLfit(dat=subset(ann.peak,Alt=='PA25')$max.stg)
