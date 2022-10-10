library(sf)
library(ggplot2)
library(tidyverse)

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

library(raster)
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
