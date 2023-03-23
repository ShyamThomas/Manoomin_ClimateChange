library(ggpubr)
library(tidyverse)
library(ggplot2)
library(geobgu)
library(stars)

### Read and map PRISM climate normals data
#PRISM_data=read_stars("RawData/PRISM_data/PRISM_tmean_30yr_normal_4kmM4_annual_asc.asc")
PRISM_data.hi=read_stars("GIS_Data/PRISM_tmean_30yr_normal_800mM4_annual_asc/PRISM_tmean_30yr_normal_800mM4_annual_asc.asc")

PRISM_data.hi
### Start with MN
MN_sf2=st_transform(MN_shp, crs=st_crs(PRISM_data.hi))
MN_tmean=st_crop(PRISM_data.hi,MN_sf2)
ggplot() +
geom_stars(data = MN_tmean) +
scale_fill_viridis_c() +
geom_sf(data = MN_sf2, fill = "transparent")
MN_sf.UTM=st_transform(MN_shp, crs="+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")


############ Now select the watersheds in MN to start with...
HUC8.poly_MN_SEL.ECORGNS=st_read("ProcessedData/HUC8.poly_MN_Sel.EcoRgns.shp")
HUC_08_wtrshds_centr_buff18k=st_read("GIS_Data/MN_HUC08_wtrshds_buff18k.shp")
#HUC8_MN_centr=st_read("GIS_Data/HUC8_MN_centr.shp")
#HUC_08_wtrshds_centr_buff18k=Sel_HUC_08_wtrshds_centr_buff18k%>%distinct(HUC8, .keep_all = TRUE)
#HUC_08_wtrshds_centr_buff18k
HUC_08_wtrshds_centr_buff18k$tmean_30yr=raster_extract(MN_tmean,HUC_08_wtrshds_centr_buff18k, fun=mean, na.rm=TRUE)
HUC_08_wtrshds_centr_buff18k
ggplot(MN_sf.UTM)+geom_sf()+
geom_sf(data=HUC8.poly_MN_SEL.ECORGNS)+
geom_sf(data=HUC_08_wtrshds_centr_buff18k, aes(fill=tmean_30yr))

### Get the coordinates of the watersheds and join it to extracted raster data
MN_HUC8.WTRSHDS=read_csv("ProcessedData/All.MN_HUC8.wtrshds.csv")
MN_HUC8.WTRSHDS
HUC_08_wtrshds_centr_buff18k.data=st_drop_geometry(HUC_08_wtrshds_centr_buff18k)%>%select(9:12,15)
HUC_08_wtrshds_centr_buff18k.data$HUC8=as.numeric(HUC_08_wtrshds_centr_buff18k.data$HUC8)
MN_HUC08_wtrshds_buff18k.data.coords=inner_join(HUC_08_wtrshds_centr_buff18k.data,MN_HUC8.WTRSHDS, by="HUC8")
MN_HUC08_wtrshds_buff18k.data.coords

### Final visualization of latitude and annual mean temp.
MN_HUC08_wtrshds_buff18k.data.coords%>%ggplot(., aes(Lat,tmean_30yr))+geom_point()+geom_smooth(method=lm, lty=2)+
  stat_cor(label.x=47.5)+xlab("Latitude")+ylab("Annual average temperature")


MN_wtrshds.FIA_LAI_Temp.jn=inner_join(MN_HUC08_wtrshds_buff18k.data.coords,MN_wtrshds.FIA_LAI.jn,
                                      by=c("HUC8", "Lon", "Lat", "ID"))
tmean_30yr=as.data.frame(MN_wtrshds.FIA_LAI_Temp.jn$tmean_30yr)
tmean_30yr
MN_wtrshds.FIA_LAI_Temp.jn$Tmean=tmean_30yr$V1
MN_wtrshds.FIA_LAI_Temp.jn
MN_wtrshds.FIA_LAI_Temp.jn[,-5]%>%View()
write_csv(MN_wtrshds.FIA_LAI_Temp.jn[,-5], "ProcessedData/MN_wtrshds.FIA_LAI_Temp.jn.csv")


MN_wtrshds.FIA_LAI_Temp.jn%>%ggplot(.,aes(Lat,Tmean))+geom_point()+geom_smooth(method="lm")
MN_wtrshds.FIA_LAI_Temp.jn%>%ggplot(.,aes(Tmean, Prpn.SoftBA))+geom_point()+geom_smooth(method="lm")+stat_cor()
MN_wtrshds.FIA_LAI_Temp.jn%>%ggplot(.,aes(Prpn.SoftBA, covarLAI))+geom_point()+geom_smooth(method="lm")+stat_cor()
MN_wtrshds.FIA_LAI_Temp.jn%>%ggplot(.,aes(Prpn.SoftBA, meanLAI))+geom_point()+geom_smooth(method="lm")+stat_cor()
MN_wtrshds.FIA_LAI_Temp.jn%>%ggplot(.,aes(Tmean, covarLAI))+geom_point()+geom_smooth(method="lm")+stat_cor()
MN_wtrshds.FIA_LAI_Temp.jn%>%ggplot(.,aes(Lat, covarLAI))+geom_point()+geom_smooth(method="lm")+stat_cor()

########################################################################################################################
########################################################################################################################
#### Merging LAI and FIA data
HUC8_LAI_FIA.jn=left_join(Prpn.HardSoft.BA_All8HUC, All8.HUC_LatLon, by="HUC_8")
HUC8_LAI_FIA.jn=read_csv("ProcessedData/HUC8_LAI_FIA.jn.csv")
HUC8_LAI_FIA.jn

#### Merging LAI and FIA data with watershed temperature data
HUC8_Temp_LAI_FIA.jn=inner_join(HUC08_wtrshds_buff18k.data.coords,HUC8_LAI_FIA.jn, by=c("HUC_8", "Lat"))
HUC8_Temp_LAI_FIA.jn

HUC8_Temp_LAI_FIA.jn%>%ggplot(., aes(tmean_30yr,Prpn.SoftBA))+geom_point()+geom_smooth(method="lm")+stat_cor(label.x = 6)+
xlab("Annual mean temperature")+ylab("Proportion of sofwood basal area")
ggsave("WtrshdTemp_Prpn.SoftWoodBA.png", path="Figures/", device="png",width = 9, height = 6.5, dpi=900)

HUC8_Temp_LAI_FIA.jn%>%ggplot(., aes(Prpn.SoftBA, covarLAI))+geom_point()+geom_smooth(method="lm")+stat_cor(label.x=0.4)+
ylab("LAI coefficient of variation")+xlab("Proportion of sofwood basal area")
ggsave("WtrshdPrpn.SoftBA_LAIcovar.png", path="Figures/", device="png",width = 9, height = 6.5, dpi=900)

HUC8_Temp_LAI_FIA.jn%>%ggplot(., aes(tmean_30yr,maxBA))+geom_point()+geom_smooth(method="lm")+stat_cor(label.x = 6)+
xlab("Annual mean temperature")+ylab("Max. sofwood basal area")
ggsave("WtrshdTemp_Max.SoftWoodBA.png", path="Figures/", device="png",width = 9, height = 6.5, dpi=900)

#############################################################################################################################
### Next WI
WI_sf2=st_transform(WI.map, crs=st_crs(PRISM_data))
WI_tmean=st_crop(PRISM_data,WI_sf2)
ggplot() +
  geom_stars(data = WI_tmean) +
  scale_fill_viridis_c() +
  geom_sf(data = WI_sf2, fill = "transparent")

HUC_08_wtrshds_centr_buff18k=st_read("GIS_Data/HUC8_WI_centr.shp")
#HUC8_MN_centr=st_read("GIS_Data/HUC8_MN_centr.shp")
#HUC_08_wtrshds_centr_buff18k=Sel_HUC_08_wtrshds_centr_buff18k%>%distinct(HUC_8, .keep_all = TRUE)
HUC_08_wtrshds_centr_buff18k
HUC_08_wtrshds_centr_buff18k$tmean_30yr=raster_extract(WI_tmean,HUC_08_wtrshds_centr_buff18k, fun=mean, na.rm=TRUE)
HUC_08_wtrshds_centr_buff18k
ggplot(WI_sf.UTM)+geom_sf()+
  #geom_sf(data=Sel_HUC_08_wtrshds)+
  geom_sf(data=HUC_08_wtrshds_centr_buff18k, aes(fill=tmean_30yr))

### Get the coordinates of the watersheds and join it to extracted raster data
HUC_08_wtrshds_centr_buff18k.data=st_drop_geometry(HUC_08_wtrshds_centr_buff18k)%>%select(8:12,28)%>%rename(HUC_8=HUC8)
HUC_08_wtrshds_centr_buff18k.data
WI_RAND.6.coords_data
WI_HUC08_wtrshds_buff18k.data.coords=inner_join(HUC_08_wtrshds_centr_buff18k.data,WI_RAND.6.coords_data, by="HUC_8")
WI_HUC08_wtrshds_buff18k.data.coords

### Final visualization of latitude and annual mean temp.
WI_HUC08_wtrshds_buff18k.data.coords%>%ggplot(., aes(Lat,tmean_30yr))+geom_point()+geom_smooth(method=lm, lty=2)+
  stat_cor(label.x=44.5)+xlab("Latitude")+ylab("Annual average temperature")

#####################################################################################################################
### Next MI
MI_sf2=st_transform(MI.map, crs=st_crs(PRISM_data))
MI_tmean=st_crop(PRISM_data,MI_sf2)
ggplot() +
  geom_stars(data = MI_tmean) +
  scale_fill_viridis_c() +
  geom_sf(data = MI_sf2, fill = "transparent")


HUC_08_wtrshds_centr_buff18k=st_read("GIS_Data/HUC8_MI_centr.shp")
#HUC8_MN_centr=st_read("GIS_Data/HUC8_MN_centr.shp")
#HUC_08_wtrshds_centr_buff18k=Sel_HUC_08_wtrshds_centr_buff18k%>%distinct(HUC_8, .keep_all = TRUE)
HUC_08_wtrshds_centr_buff18k
HUC_08_wtrshds_centr_buff18k$tmean_30yr=raster_extract(MI_tmean,HUC_08_wtrshds_centr_buff18k, fun=mean, na.rm=TRUE)
HUC_08_wtrshds_centr_buff18k
ggplot(MI_sf2)+geom_sf()+
  #geom_sf(data=Sel_HUC_08_wtrshds)+
  geom_sf(data=HUC_08_wtrshds_centr_buff18k, aes(fill=tmean_30yr))

### Get the coordinates of the watersheds and join it to extracted raster data
HUC_08_wtrshds_centr_buff18k.data=st_drop_geometry(HUC_08_wtrshds_centr_buff18k)%>%select(8:12,28)%>%rename(HUC_8=HUC8)
HUC_08_wtrshds_centr_buff18k.data
MI_RAND.6.coords_data
MI_HUC08_wtrshds_buff18k.data.coords=inner_join(HUC_08_wtrshds_centr_buff18k.data,MI_RAND.6.coords_data, by="HUC_8")
MI_HUC08_wtrshds_buff18k.data.coords

### Final visualization of latitude and annual mean temp.
MI_HUC08_wtrshds_buff18k.data.coords%>%ggplot(., aes(Lat,tmean_30yr))+geom_point()+geom_smooth(method=lm, lty=2)+
  stat_cor(label.x=44.5)+xlab("Latitude")+ylab("Annual average temperature")


All3States_tmean=bind_rows(MN_HUC08_wtrshds_buff18k.data.coords[7:10],WI_HUC08_wtrshds_buff18k.data.coords[c(4,6:8)],
                           MI_HUC08_wtrshds_buff18k.data.coords[c(4,6:8)])
All3States_LAI_FIA.jn=read_csv("ProcessedData/All3States_LAI_FIA.JOIN.csv")
All3States_LAI_FIA.jn=All3States_LAI_FIA.jn%>%rename(HUC_8=HUC8)
All3States_tmean$temp=All3States_tmean$tmean_30yr[,1]
All3States_SEMdata=inner_join(All3States_tmean[,-2],All3States_LAI_FIA.jn, by=c("HUC_8","Lat"))
head(All3States_SEMdata)

write_csv(All3States_SEMdata,"ProcessedData/All3States_SEMdata.csv")

