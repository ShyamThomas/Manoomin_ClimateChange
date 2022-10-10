library(sf)
library(ggplot2) 
library(tidyverse) 
library(raster)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/Manoomin_ClimateChange")

lakes=st_read("GIS_Data/MN_dnr_watersheds/dnr_watersheds_auto_catchment_lakes.shp")

st_geometry(lakes)
st_crs(lakes)
PerchLake=lakes%>%filter(LAKE_ID==40005400)
PerchLake

ggplot() + 
  geom_sf(data = PerchLake, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("Minnesota Lakes") + 
  coord_sf()

wtrshds=st_read("GIS_Data/MN_dnr_watersheds/dnr_watersheds_dnr_level_04_huc_08_majors.shp")
st_geometry(wtrshds)
PerchLake_ctrd=st_centroid(PerchLake)
plot(st_geometry(wtrshds), axes=TRUE, graticule=TRUE)
plot(st_geometry(PerchLake_ctrd), pch = 16, add = TRUE, col="red")

Minn.LAI = stack("GIS_Data/GLASS01D01.V60.A2000057.h11v04.2022012.LAI.jpg")  

### Steps to find the correct MODIS tile for the state of Minnesota & Wisconsin
library(mapdata)
install.packages("MODIS", repos="http://R-Forge.R-project.org")
library(MODIS)

minn.map = map("state", region = "minnesota", fill = T)
getTile(minn.map)

wisc.map = map("state", region = "wisconsin", fill = T)
getTile(wisc.map)

### Use the Terra package to read the GLASS LAI files in .hdf 
library(terra)
#s=sds("GIS_Data/GLASS01D01.V60.A2000129.h00v08.2022012.hdf")
r=rast("GIS_Data/GLASS01D01.V60.A2000129.h00v08.2022012.hdf")
crs(r, describe=T, proj=T)

minn=vect("GIS_Data/Minn.map.shp")
minn
crs(minn, describe=T, proj=T)
plot(minn, "NAME")

wisc=vect("GIS_Data/Wisconsin_State_Boundary_24K.shp")
wisc
plot(wisc)
crs(wisc, describe=T, proj=T)

################################################## DOWNLOAD AND PROCESS MODIS SATELLITE IMAGERY##############################################

library(raster)
LAI_r=raster("GIS_Data/GLASS01B02.V40.A2000089.2019353.hdf")
LAI_r
plot(LAI_r)

LAI_WGS84=projectRaster(LAI_r, crs="+proj=longlat +datum=WGS84 +no_defs")
LAI_WGS84
plot(LAI_WGS84)

#### Get the Minnesota state shape file and clip the relevant MN state raster area
Minn.shp=st_read("GIS_Data/Minn.map.shp")
st_crs(Minn.shp)
plot(Minn.shp$geometry, add=T)

Minn_crop=crop(LAI_WGS84,Minn.shp)
Minn_mask=mask(Minn_crop, Minn.shp)
plot(Minn_mask)

Minn_mask_UTM=projectRaster(Minn_mask, crs="+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")
Minn_mask_UTM
plot(Minn_mask_UTM)

#### Get the Wisconsin state shape file and clip the relevant WI state raster area
Wisc.shp=st_read("GIS_Data/Wisconsin_State_Boundary_24K.shp")
Wisc_WGS84=st_transform(Wisc.shp, crs(LAI_newR))
st_crs(Wisc_WGS84)
plot(Wisc_WGS84$geometry, add=T)

Wisc_crop=crop(LAI_WGS84,Wisc_WGS84)
Wisc_mask=mask(Wisc_crop, Wisc_WGS84)
plot(Wisc_mask)

Wisc_mask_UTM=projectRaster(Wisc_mask, crs="+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs") 
Wisc_mask_UTM
plot(Wisc_mask_UTM)

WI.wtrshds=st_read("GIS_Data/WI_watersheds/watersheds.shp")
st_geometry(WI.wtrshds)
plot(st_geometry(WI.wtrshds), axes=TRUE, graticule=TRUE)
WI.wtrshds$WSHED_NAME

WI.wtrshds_bear=WI.wtrshds[WI.wtrshds$WSHED_NAME=="Bear River",]
WI.wtrshds_bear
plot(st_geometry(WI.wtrshds_bear), axes=TRUE)
st_crs(WI.wtrshds_bear)

mycol = rgb(1,0,0,0.09)
plot(Wisc_mask_UTM)
plot(WI.wtrshds_bear_UTM, col = mycol, add=TRUE)
LAI.vals=raster::extract(Wisc_mask_UTM,WI.wtrshds_bear_UTM)
hist(LAI.vals[[1]])

##############################################################################################################################################

LAI_r=raster("GIS_Data/GLASS01B02.V40.A2000169.2019353.hdf")
LAI_r
plot(LAI_r)

LAI_WGS84=projectRaster(LAI_r, crs="+proj=longlat +datum=WGS84 +no_defs")
LAI_WGS84
plot(LAI_WGS84)

#### Get the Minnesota state shape file and clip the relevant MN state raster area
Minn.shp=st_read("GIS_Data/Minn.map.shp")
st_crs(Minn.shp)
plot(Minn.shp$geometry, add=T)

Minn_crop=crop(LAI_WGS84,Minn.shp)
Minn_mask=mask(Minn_crop, Minn.shp)
plot(Minn_mask)

Minn_mask_UTM=projectRaster(Minn_mask, crs="+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")
Minn_mask_UTM
plot(Minn_mask_UTM)

##################################################################################################################################################
#### Get the Wisconsin state shape file and clip the relevant WI state raster area
Wisc.shp=st_read("GIS_Data/Wisconsin_State_Boundary_24K.shp")
LAI_newR=projectRaster(LAI_r, crs="+proj=longlat +datum=WGS84 +no_defs")

Wisc_WGS84=st_transform(Wisc.shp, crs(LAI_newR))
st_crs(Wisc_WGS84)
plot(Wisc_WGS84$geometry)

Wisc_crop=crop(LAI_WGS84,Wisc_WGS84)
Wisc_mask=mask(Wisc_crop, Wisc_WGS84)

plot(Wisc_mask)

Wisc_mask_UTM=projectRaster(Wisc_mask, crs="+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs") 
Wisc_mask_UTM
plot(Wisc_mask_UTM)

WI.wtrshds=st_read("GIS_Data/WI_watersheds/watersheds.shp")
st_geometry(WI.wtrshds)
plot(st_geometry(WI.wtrshds), axes=TRUE, graticule=TRUE)
WI.wtrshds$WSHED_NAME

WI.wtrshds_bear=WI.wtrshds[WI.wtrshds$WSHED_NAME=="Bear River",]
WI.wtrshds_bear
plot(st_geometry(WI.wtrshds_bear), axes=TRUE)
st_crs(WI.wtrshds_bear)

mycol = rgb(1,0,0,0.09)
plot(Wisc_mask_UTM)
WI.wtrshds_bear_UTM=st_transform(WI.wtrshds_bear, crs=st_crs(Wisc_mask_UTM))
WI.wtrshds_bear_UTM
plot(WI.wtrshds_bear_UTM, col = mycol, add=TRUE)

bbox.Bear.wtrshd=st_as_sfc(st_bbox(WI.wtrshds_bear_UTM$geometry))
plot(bbox.Bear.wtrshd)
plot(bbox.Bear.wtrshd, axes=TRUE)
plot(WI.wtrshds_bear_UTM$geometry, add=TRUE, col=mycol)

(285931.4-260830.9)/1000
(5113292-5086001)/1000
st_centroid(bbox.Bear.wtrshd)
st_transform(st_centroid(bbox.Bear.wtrshd),crs = "+proj=longlat +datum=WGS84")

files_sin = list.files("GIS_Data/GTiff_sin", pattern="_Lai_500m.tif", full.names=TRUE)
files_sin
rs_lai.sin=stack(files)



Bear.LAI=read_csv("RawData/Bear_Lai_500m.csv") ### clean data dowloaded from ORNL using bounding box polygon
LAI.80per=Bear.LAI%>%filter(per_cent_pixels_pass_qa>0.8)
LAI.80per


TimeSeries <- ggplot(LAI.80per, aes(x=as.Date(date, "%m/%d/%y"), y = mean)) +
geom_line(colour = "#2D708EFF", aes(y=mean)) + # draw the line for the mean value
ggtitle(" ") + # add a title
ylab("Mean LAI") + # add a y-axis label
xlab("Date") + # add a x-axis label
theme_bw() + # set the plot theme
theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=20), axis.title.y = element_text(face="bold", size=20), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16))
TimeSeries
ggsave(paste("Figures/R_MODIS_Plots_", product,"_Fig1_timeseries.png", sep=''), width=25, height=16, units='cm')


Boxplot_Monthly <- ggplot(LAI.80per, aes(factor(month), mean)) +
geom_boxplot(fill="#2D708EFF") +
geom_jitter(width = 0) +
ggtitle("Mean Values by Month") +
xlab("Month") +
ylab("Mean LAI") +
theme_bw() +
theme(plot.title = element_text(lineheight=.8, face="bold", size=20), legend.position="none", axis.title.x = element_text(face="bold", size=20),    axis.text.x = element_text(angle=90, vjust=0.5, size=16), axis.title.y = element_text(face="bold", size=20), axis.text.y = element_text(size=16)) +
guides(fill=FALSE)

Boxplot_Monthly
ggsave(paste("Figures/R_MODIS_Plots_", product,"_Fig2_monthyMean.png", sep=''), width=25, height=16, units='cm')


StackedByYear <- ggplot(LAI.80per, aes(x=as.numeric(substr(LAI.80per$`date[YYYYDDD]`,6,8)), y=mean, color=as.factor(year))) +
scale_color_viridis(discrete=T) +
geom_point(size=2) +
geom_smooth(aes(factor=year), se=FALSE, size=1.2) +
ggtitle("LAI Mean Value by Day of Year") +
theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
xlab("Day of Year") +
ylab("Mean LAI") +
theme_bw() +
theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=20), axis.text.x = element_text(angle=90, vjust=0.5, size=16), axis.title.y = element_text(face="bold", size=20), axis.text.y = element_text(size=16)) +theme(legend.title = element_blank())
StackedByYear
ggsave(paste("Figures/R_MODIS_Plots_", product,"_Fig5_stackedYears.png", sep=''), width=25, height=16, units='cm')



mar=ggplot(Max.LAI_mar, aes(x=year, y=maxLAI))+geom_point()+geom_smooth(method='lm')+ylab("max avg.LAI")+ggtitle("March")
apr=ggplot(Max.LAI_apr, aes(x=year, y=maxLAI))+geom_point()+geom_smooth(method='lm')+ylab("max avg.LAI")+ggtitle("April")
may=ggplot(Max.LAI_may, aes(x=year, y=maxLAI))+geom_point()+geom_smooth(method='lm')+ylab("max avg.LAI")+ggtitle("May")
jun=ggplot(Max.LAI_jun, aes(x=year, y=maxLAI))+geom_point()+geom_smooth(method='lm')+ylab("max avg.LAI")+ggtitle("June")

grid.arrange(mar, apr, may, jun)


####################################################################################################################################
### Plot a map showing all the Rice field sites
US_shp=st_read("GIS_Data/gadm41_USA_shp/gadm41_USA_1.shp")
US_shp
MN_WI_shp=US_shp%>%filter(NAME_1=="Minnesota" | NAME_1=="Wisconsin")
MN_WI_shp
plot(MN_WI_shp$geometry)

RiceFieldSites=read_csv("GIS_Data/RiceFieldSites.csv")
RiceFieldSites_sf=st_as_sf(RiceFieldSites, coords = c("Lon","Lat"))
RiceFieldSites_sf
st_set_crs(RiceFieldSites_sf, 4326)
plot(MN_WI_shp$geometry, axes=TRUE)
plot(RiceFieldSites_sf$geometry, add=TRUE, col="red")


