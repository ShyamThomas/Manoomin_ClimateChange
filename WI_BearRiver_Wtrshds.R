library(sf)
library(ggplot2) 
library(tidyverse) 
library(raster)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/Manoomin_ClimateChange")

### Firs the state shapefle
Wisc.shp=st_read("GIS_Data/Wisconsin_State_Boundary_24K.shp")
Wisc_WGS84=st_transform(Wisc.shp, crs="+proj=longlat +datum=WGS84 +no_defs")
st_crs(Wisc_WGS84)
plot(Wisc_WGS84$geometry, axes=TRUE)

Wisc_UTM=st_transform(Wisc.shp, crs="+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs")
st_crs(Wisc_UTM)
plot(Wisc_UTM$geometry, axes=TRUE)

###HUC 12 watersheds
WI.wtrshds=st_read("GIS_Data/WI_watersheds/watersheds.shp")
st_crs(WI.wtrshds)
st_geometry(WI.wtrshds)
plot(st_geometry(WI.wtrshds), axes=TRUE, graticule=TRUE)
WI.wtrshds$WSHED_NAME

WI.wtrshds_bear=WI.wtrshds[WI.wtrshds$WSHED_NAME=="Bear River",]
WI.wtrshds_bear
plot(st_geometry(WI.wtrshds_bear), axes=TRUE)
st_crs(WI.wtrshds_bear)

mycol = rgb(1,0,0,0.09)
WI.wtrshds_bear_UTM=st_transform(WI.wtrshds_bear, crs="+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs")
plot(WI.wtrshds_bear_UTM$geometry, col = mycol, add=TRUE)

### HUC 8 sub-basins
WI.subbasins=st_read("GIS_Data/WI_HUC8/Hydrologic_Units_-_8_digit_(Subbasins).shp")
WI.subasin_bear2=WI.subbasins[WI.subbasins$HUC8_CODE=="07050003" | WI.subbasins$HUC8_CODE=="07050002",]
st_crs(WI.subasin_bear2)
WI.subbasin_bear_UTM=st_transform(WI.subasin_bear2, crs="+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs")
WI.subasin_bear3=WI.subbasins[WI.subbasins$HUC8_CODE=="07070001",]
WI.subasin_bear3_UTM=st_transform(WI.subasin_bear3, crs="+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs")


plot(st_geometry(WI.subbasin_bear_UTM), add=T, col="light blue")
plot(st_geometry(WI.subasin_bear3_UTM), add=T, col="light blue")
plot(WI.wtrshds_bear_UTM$geometry, col = mycol, add=TRUE)

### Make a bounding box around the sub-watershed
### This bounding box was used to extract LAAI from MODIS sattelite data
bbox.Bear.wtrshd=st_as_sfc(st_bbox(WI.wtrshds_bear_UTM$geometry))
bbox.Bear.wtrshd
plot(bbox.Bear.wtrshd, axes=TRUE)
plot(bbox.Bear.wtrshd, add=TRUE, lty=2)

### Estimate the center of the bounding box
bearwtrshd_centr=st_centroid(bbox.Bear.wtrshd)
bearwtrshd_centr
buff_14km=st_buffer(bearwtrshd_centr, dist= 14000)
plot(buff_15km, add=TRUE, lty=1)
ggsave("WI_subbasins_wtrshed.Map.png", path="Figures/", device="png",width=4.5, height=6)


plot(st_geometry(WI.subbasin_bear_UTM), col="light blue", axes=TRUE)
plot(st_geometry(WI.subasin_bear3_UTM), add=T, col="light blue")
plot(WI.wtrshds_bear_UTM$geometry, col = mycol, add=TRUE)
#plot(bbox.Bear.wtrshd, add=TRUE, lty=2)
plot(buff_15km, add=TRUE, lty=1, lwd=1.5)
