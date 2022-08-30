## 
## LOSOM
## SNKI Lake Recession Rate 
##
## Code was compiled by Paul Julian
## contact info: pjulian@sccf.org

## Libraries
library(ggplot2)


# -------------------------------------------------------------------------
lakeO.stage; # Lake Okeechobee stage elevation data from RSMBN

# Format Data - break out month, day, year & DOY
lakeO.stage$month=as.numeric(format(lakeO.stage$Date,'%m'))
lakeO.stage$day=as.numeric(format(lakeO.stage$Date,'%d'))
lakeO.stage$CY=as.numeric(format(lakeO.stage$Date,'%Y'))
lakeO.stage$DoY=as.numeric(format(lakeO.stage$Date,'%j'))

## uses ave(...) function if more 
## than one alternative and data is sorted
lakeO.stage$recess_7day=with(lakeO.stage,
                             ave(STAGE,Alt,
                                 FUN=function(x) c(rep(NA,6),diff(x,lag=6))))

# define breakpoints
bks=c(min(recess.dat$recess_7day,na.rm=T),
      c(-0.16,-0.05,0.05,0.25),
      max(recess.dat$recess_7day,na.rm=T))

lakeO.stage$cat=as.factor(
  findInterval(lakeO.stage$recess_7day,bks,
               rightmost.closed = T,left.open = T))



cols=c("1"="red","2"="yellow","3"="green","4"="goldenrod2","5"="darkred")

# Using LOSOM Iteration 2 RSMBN output
tmp=subset(lakeO.stage,Alt=="NA25")
rec.plot.FWO=ggplot(tmp, aes(x = DoY, y = CY, fill = cat2)) +
  geom_tile(aes(group = cat2), colour = "black",size=0.05)+
  scale_y_reverse(expand = c(0, 0), breaks = tmp$CY) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1,366,30),labels= seq(1,366,30))+
  scale_fill_manual(values = cols,
                    name="Weekly Recession\nRate (ft/wk)",
                    breaks=c(1:5),
                    labels=c("\u2264 -0.16","> -0.16 & < -0.05","> -0.05 & \u2264 0.05","> 0.05 & \u2264 0.25","> 0.25"),
                    na.value="white") +
  theme_bw() +
  theme(
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Lake Okeechobee Recession/Accession Rate",
       subtitle = "Modelled Weekly Recession/Accession Rates (NA25)",
       caption = "LOSOM: NA25 (FWO)",
       x="Day of Year",
       y="Year")
rec.plot.FWO
# ggsave("Lake_FWO_recess.png",rec.plot.FWO,device="png",height =7,width=6,units="in")