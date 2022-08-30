##
## Map for Manusrcipt 
##
## Code was compiled by Paul Julian
## contact info: pjulian.sccf.org

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#GIS Libraries
library(sp)
library(rgdal)
library(PROJ)
library(rgeos)
library(tmap)
library(raster)

## Paths
wd="C:/Julian_LaCie/_GitHub/LOSOM_ModelEval"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

gen.GIS="C:/Julian_LaCie/_GISData"
db.path=paste(gen.GIS,"/SFER_GIS_Geodatabase.gdb",sep=""); 

wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")

# GIS ---------------------------------------------------------------------
bath.path="C:/Julian_LaCie/_Github/LakeO_Sediment/GIS"
bath=raster::raster(paste0(bath.path,"/LakeOkeechobee_Usace/spatial/export_raster/Bathym_50ft.tif"))
proj4string(bath)<-CRS("+init=epsg:2236")# utm17
bath=projectRaster(bath,crs=utm17)

bath=bath-(-1.32)
bath.m=bath*0.3048

plot(bath)
plot(bath.m)

cellStats(bath,"min")
cellStats(bath.m,"min")



ogrListLayers(paste(gen.GIS,"/SFER_GIS_Geodatabase.gdb",sep=""))
ogrListLayers(paste(gen.GIS,"/AHED_release/AHED_20171102.gdb",sep=""))


HHD=spTransform(readOGR(paste0(gen.GIS,"/SFWMD/HHD"),"HHD_Boundary"),wkt(utm17))
A2=spTransform(readOGR(paste0(gen.GIS,"/CERP"),"A2"),wkt(utm17))
CERP.all=spTransform(readOGR(paste0(gen.GIS,"/CERP"),"bdwmdcrp"),wkt(utm17))
unique(CERP.all$PROJECT)
C43=subset(CERP.all,PROJECT=="CALOOSAHATCHEE RIVER (C-43) WEST BASIN STORAGE RESERVOIR")
C44=subset(CERP.all,COMPONENT%in%c("IRL - C-44 Reservoir","IRL - C-44 West STA","IRL - C-44 East STA"))

WMA=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"HoleyLand_Rotenberger"),utm17)
rsfeatures=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"RestorationStrategies_Features"),utm17)
EAA=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"EvergladesAgriculturalArea"),utm17)
STAs=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"EvergladesSTAs"),utm17)
EPA=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"EPA_Boundary"),utm17)
C139Ann=spTransform(readOGR(paste0(gen.GIS,"/SFER_GIS_Geodatabase.gdb"),"C139Annex"),utm17)

canal=spTransform(readOGR(paste(gen.GIS,"/SFER_GIS_Geodatabase.gdb",sep=""),"SFWMD_Canals"),utm17)
shore=spTransform(readOGR(paste0(gen.GIS,"/FWC"),"FWC_Shoreline"),utm17)
# shore=gSimplify(shore,500)

lakeO=spTransform(readOGR(paste0(gen.GIS,"/SFWMD"),"LakeOkeechobee_general"),wkt(utm17))

lakeO.lit=spTransform(readOGR(paste0(gen.GIS,"/LakeOkeechobee/Littoral"),"LAKEOKEELITTORALZONE_Dissolv"),wkt(utm17))
lakeO.vegall=spTransform(readOGR(paste0(gen.GIS,"/LakeOkeechobee/Littoral"),"LAKEO_LITTORALZONE_VEG2007"),wkt(utm17))
lakeO.pelagic=spTransform(readOGR(paste0(gen.GIS,"/LakeOkeechobee/Littoral"),"LAKEoMINUSLITTORAL"),wkt(utm17))

plot(lakeO.pelagic,col="dodgerblue1")
plot(lakeO.vegall,col="khaki",add=T)
plot(lakeO.lit,col="green",add=T)

basins=spTransform(readOGR(paste0(gen.GIS,"/AHED_release/AHED_20171102.gdb"),"WATERSHED"),utm17)
wmd.struct=spTransform(readOGR(paste0(gen.GIS,"/AHED_release/AHED_20171102.gdb"),"STRUCTURE"),utm17)

wmd=spTransform(readOGR(paste(gen.GIS,"/SFER_GIS_Geodatabase.gdb",sep=""),
                         "WMD_ALL"),utm17)
sfwmd=subset(wmd,NAME=='SFWMD')
plot(sfwmd)
# plot(gSimplify(sfwmd,2000))

## 390 = lakes/ponds; 436 = reservoir; 466 = wetland
## https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm
# lakes=spTransform(readOGR(paste0(gen.GIS,"/NHD"),"NHD100_Waterbody"),utm17)
# lakes2=subset(lakes,FTYPE%in%c("390"))
# wetland=subset(lakes,FTYPE%in%c("466"))
lakes=spTransform(readOGR(paste0(gen.GIS,"/AHED_release/Waterbodies"),"Waterbody"),utm17)
# lakes=subset(lakes,FTYPE==390&SHAPE_AREA>3000000)
# lakes.sfwmd=gIntersection(lakes,sfwmd,byid = F, drop_lower_td = TRUE)
# plot(lakes.sfwmd)

kiss.chain=c("LAKE HART","LAKE MARY JANE","LAKE MYRTLE","LAKE PRESTON",
             "LAKE JOEL","TROUT LAKE","LAKE LIZZIE","ALLIGATOR LAKE",
             "LAKE GENTRY","FELLS COVE","EAST LAKE TOHOPEKALIGA",
             "LAKE TOHOPEKALIGA","CYPRESS LAKE","LAKE HATCHINEHA",
             "LAKE KISSIMMEE","LAKE ROSALIE","TIGER LAKE",
             "LAKE WEOHYAKAPKA","LAKE ISTOKPOGA")
lakes.sub=subset(lakes,NAME%in%c(kiss.chain,"LAKE HICPOCHEE"))
plot(lakes.sub)

library(USAboundaries)
states.shp=us_boundaries(resolution ="low")
states.shp=as(states.shp,"Spatial")
states.shp=spTransform(states.shp,utm17)
attributes(states.shp)
SW.US=c("Florida","Georgia","Alabama","South Carolina")
FL.shp=us_boundaries(resolution ="low",states=SW.US)
FL.shp=as(FL.shp,"Spatial")
FL.shp=spTransform(FL.shp,utm17)
attributes(FL.shp)

crop(lakeO,gBuffer(lakeO.vegall,width=500))

# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/FigX_LOKMap.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.5,0.5,0.5,0.5),mar=c(0.1,0.1,0.1,0.1),xpd=F)
layout(matrix(c(1,1:3),2,2,byrow=F),widths=c(1,0.5))

bbox.lims=bbox(gBuffer(lakeO,width=1500))
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(canal,add=T,col="dodgerblue2",lwd=1.5)
plot(lakeO.pelagic,col="lightblue",border=NA,add=T)
plot(lakeO.vegall,col="khaki",border=NA,add=T)
plot(lakeO.lit,col="darkolivegreen3",border=NA,add=T)
plot(subset(wmd.struct,NAME%in%c("S77",'S308',"S65E","S2","S3","S4",'C10A','S72',
                                 "S71","C10",'S352',"S135","S191","S193","S127",
                                 "S129",'S131','C5',"C5A",'S310')),
     add=T,pch=21,bg="grey",cex=1.5,lwd=0.01)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)

AOI=raster::extent(gBuffer(lakeO,width=5000))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17

bbox.lims=bbox(shore)# bbox(gBuffer(region.mask,width=2000))
plot(states.shp,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],col="grey65",border="white",lwd=0.2,xpd=F)
plot(sfwmd,add=T,col=adjustcolor("grey85",0.5),border=F)
plot(lakes.sub,add=T,border=NA,col="lightblue")
plot(lakeO,add=T,border=NA,col="lightblue")
plot(canal,add=T,col="lightsteelblue3",lwd=0.1)
# plot(roads.all,add=T,col="grey50",lwd=0.5,lty=1)
plot(AOI.poly,add=T,border=adjustcolor("red",0.5),lwd=2,lty=1)
box(lwd=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c("Littoral Zone","Nearshore Zone","Limnetic/Pelagic Zone","Water Control Structures","Canals"),
       pch=c(22,22,22,21,NA),lty=c(rep(NA,4),1),lwd=c(rep(0.1,4),2),
       col=c(rep(NA,3),"black","dodgerblue2"),
       pt.bg=c("darkolivegreen3","khaki","lightblue","grey",NA),
       pt.cex=c(2.5,2.5,2.5,1.5,NA),ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)

dev.off()




lakeO.vegall2=crop(lakeO,gBuffer(lakeO.vegall,width=200))
lakeO.lit2=crop(lakeO,gBuffer(lakeO.lit,width=50))
## before final change shore

# png(filename=paste0(plot.path,"_Manuscripts/LOK_PMEval/FigX_LOKMap_v2.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.5,0.5,0.5,0.5),mar=c(0.1,0.1,0.1,0.1),xpd=F)
layout(matrix(c(1,2,2,3,2,2,4,4,5,4,4,5),4,3,byrow=T),widths=c(0.5,1,0.4),heights=c(0.5,0.75,1))

AOI=raster::extent(gBuffer(lakeO,width=5000))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17

bbox.lims=bbox(shore)# bbox(gBuffer(region.mask,width=2000))
plot(states.shp,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],col="grey65",border="white",lwd=0.2,xpd=F)
plot(sfwmd,add=T,col=adjustcolor("grey85",0.5),border=F)
plot(lakes.sub,add=T,border=NA,col="lightblue")
plot(lakeO,add=T,border=NA,col="lightblue")
plot(canal,add=T,col="lightsteelblue3",lwd=0.1)
# plot(roads.all,add=T,col="grey50",lwd=0.5,lty=1)
plot(AOI.poly,add=T,border=adjustcolor("red",0.5),lwd=2,lty=1)
box(lwd=1)

bbox.lims=bbox(bind(HHD,EAA,C43))
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(sfwmd,add=T,border="grey85")
plot(subset(lakes,WATERBODYT==3),col=adjustcolor("darkolivegreen4",0.25),border=NA,add=T)
plot(subset(lakes,WATERBODYT==2&NAME!="LAKE OKEECHOBEE"),col="lightblue",border=NA,add=T)
plot(lakeO,add=T,col="lightblue",border=NA)
plot(EAA,col="lightgoldenrod1",border="forestgreen",add=T)
plot(subset(C139Ann,NAME=="C-139 ANNEX"),add=T,col="darkolivegreen")
plot(lakeO.vegall,col="darkolivegreen4",border=NA,add=T)
plot(gBuffer(HHD,width=500),add=T,col="black")
plot(C43,add=T,col="indianred1",border="red",cex=0.5)
plot(subset(C44,COMPONENT=="IRL - C-44 Reservoir"),add=T,col="indianred1",border="red")
plot(A2,add=T,col="indianred1",border="red")
plot(subset(rsfeatures,Type=="Storage"),add=T,col="indianred1",border="red")
plot(subset(C44,COMPONENT!="IRL - C-44 Reservoir"),add=T,col="dodgerblue1",border="blue")
plot(subset(rsfeatures,Type=="Water_Quality"),add=T,col="dodgerblue1",border="blue")
plot(WMA,add=T,col="grey80")
plot(STAs,add=T,col="dodgerblue1",border="blue")
plot(EPA,add=T,col="grey80")
plot(canal,add=T,col="dodgerblue2",lwd=1.25)
plot(subset(wmd.struct,NAME%in%c("S77",'S308',"S65E","S2","S3","S4",'C10A','S72',
                                 "S71","C10",'S352',"S135","S191","S193","S127",
                                 "S129",'S131','C5',"C5A",'S310',
                                 "S5A","S6","S7","S8","S79","S80","S155","CWPB2S")),
     add=T,pch=21,bg="grey50",cex=1,lwd=0.01)
txt.cex=0.75
raster::text(C43,"C-43",pos=3,halo=T,offset=0.75,cex=txt.cex)
raster::text(subset(C44,COMPONENT=="IRL - C-44 Reservoir"),"C-44",pos=3,halo=T,offset=0.75,cex=txt.cex)
raster::text(A2,"A-2",pos=3,halo=T,offset=0.75,cex=txt.cex)
raster::text(lakeO,"Lake\nOkeechobee",halo=T,cex=txt.cex)
raster::text(subset(EAA,BASIN=="EAA"),"EAA",halo=T,cex=txt.cex)
raster::text(subset(rsfeatures,Type=="Storage"),c("C-139\nFEB","L-8\nFEB","A-1\nFEB"),pos=c(2,4,4),halo=T,offset=0.5,cex=txt.cex)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",
       legend=c("Storage","Water Quality Treatment",
                "Herbert Hoover Dike",
                "Everglades Agricultural Area",
                "Wildlife Management Area\nEverglades Protection Area",
                "Major Water Control Structures",
                "Major Canals"),
       pch=c(rep(22,5),21,NA),pt.bg=c("indianred1","dodgerblue1","black","lightgoldenrod1","grey","grey50",NA),
       lty=c(rep(0,5),0,1),lwd=c(rep(0.5,5),0.5,2),col=c("red","blue","black","forestgreen","grey","black","dodgerblue2"),
       pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=0.9,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)


bks1=seq(-3.7,3,0.3)
bks2=seq(max(bks1),13,1)
bks=c(bks1,bks2[-1])
blue.col <- colorRampPalette(c("darkblue", "lightblue"))
grns.col <- colorRampPalette(c("forestgreen", "palegreen2","khaki"))
# pal=c(blue.col(length(bks1)-1),terrain.colors(length(bks2[-1])))
pal=c(blue.col(length(bks1)-1),grns.col(length(bks2[-1])))

bbox.lims=bbox(gBuffer(lakeO,width=2000))
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
# bks=seq(-5,14,0.5)
# pal=viridis::magma(length(bks)-1)
image(bath.m,breaks=bks,col=pal,add=T)
plot(EAA,col="lightgoldenrod1",border="forestgreen",add=T)
plot(subset(C44,COMPONENT=="IRL - C-44 Reservoir"),add=T,col="indianred1",border="red")
plot(subset(rsfeatures,Type=="Storage"),add=T,col="indianred1",border="red")
plot(subset(C44,COMPONENT!="IRL - C-44 Reservoir"),add=T,col="dodgerblue1",border="blue")
plot(subset(rsfeatures,Type=="Water_Quality"),add=T,col="dodgerblue1",border="blue")
plot(STAs,add=T,col="dodgerblue1",border="blue")
plot(subset(lakes,WATERBODYT==3),col=adjustcolor("darkolivegreen4",0.25),border=NA,add=T)
plot(canal,add=T,col="dodgerblue2",lwd=1.5)
plot(lakeO.vegall2,border="khaki",lwd=1.25,lty=1,add=T)
plot(lakeO.lit2,border="darkolivegreen",lwd=1.25,lty=1,add=T)
# plot(lakeO.lit,col=adjustcolor("darkolivegreen",0.5),border="darkolivegreen",lwd=1,lty=2,add=T)
plot(gBuffer(HHD,width=100),add=T,col="black")

plot(subset(wmd.struct,NAME%in%c("S77",'S308',"S65E","S2","S3","S4",'C10A','S72',
                                 "S71","C10",'S352',"S135","S191","S193","S127",
                                 "S129",'S131','C5',"C5A",'S310')),
     add=T,pch=21,bg="grey50",cex=1.5,lwd=0.01)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
top.val=0.8
bot.val=0.2
mid.v.val=0.3# bot.val+(top.val-bot.val)/2
x.max=0.6
x.min=0.4
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
legend_image=as.raster(matrix(rev(blue.col(length(bks1)-1)),ncol=1))
rasterImage(legend_image,x.min,bot.val,x.max,top.val/2)
labs=c(min(bks1),max(bks1))
lab.loc=scales::rescale(bks1,to=c(bot.val,top.val/2))
text(x=x.max,
     y = lab.loc[bks1%in%labs], 
     labels = format(labs),cex=0.75,adj=0,pos=4,offset=0.5)
text(x=x.min,
     y = lab.loc[bks1%in%labs], 
     labels = format(round(labs*3.28084,1)),cex=0.75,adj=0,pos=2,offset=0.25)
legend_image=as.raster(matrix(rev(grns.col(length(bks2[-1]))),ncol=1))
rasterImage(legend_image,x.min,top.val/2,x.max,top.val)
labs=c(max(bks2))
lab.loc=scales::rescale(bks2,to=c(top.val/2,top.val))
text(x=x.max,
     y = lab.loc[bks2%in%labs], 
     labels = format(labs),cex=0.75,adj=0,pos=4,offset=0.5)
text(x=x.min,
     y = lab.loc[bks2%in%labs], 
     labels = format(round(labs*3.28084,1)),cex=0.75,adj=0,pos=2,offset=0.25)
text(x=x.min,y=top.val+0.03,"(Ft)",cex=0.75,adj=0,pos=2,offset=0.5)
text(x=x.max,y=top.val+0.03,"(m)",cex=0.75,adj=0,pos=4,offset=0.5)
text(x=mid.val,y=top.val+0.03,"Bathymetric Elevation\n(NGVD29)",adj=0,cex=0.8,pos=3,xpd=NA)

legend(0.5,bot.val,legend=c("Littoral Zone","Nearshore Zone"),
       lty=1,lwd=2,col=c("darkolivegreen","khaki"),
       pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()