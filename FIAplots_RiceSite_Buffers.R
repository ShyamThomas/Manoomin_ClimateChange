library(tidyverse)
library(sf)
library(lubridate)
library(ggpubr)
library(stars)

RiceFieldSites=read_csv("GIS_Data/RiceFieldSites.csv")
US_shp=st_read("GIS_Data/gadm41_USA_shp/gadm41_USA_1.shp")
MN_ecorgns=st_read("GIS_Data/mn_ecoregions/mn_eco_l4.shp")

MN_shp=US_shp%>%filter(NAME_1=="Minnesota")
plot(MN_shp$geometry, axes=TRUE)
MN_sf=st_as_sf(MN_shp, crs=4326)
MN_sf

MN_sf.UTM=st_transform(MN_sf, crs="+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")
MN_sf.UTM

RiceSites_MN=st_intersection(RiceFieldSites_sf,MN_sf)
MN.RiceSites_UTM=st_transform(RiceSites_MN, crs="+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")
MN.RiceSites_UTM

MN.RiceSites2_UTM=MN.RiceSites_UTM%>%filter(Name!="Twin lake")

MN.RiceSites_UTM_6k.buff=st_buffer(MN.RiceSites2_UTM, endCapStyle='SQUARE', 6000)
MN.RiceSites_UTM_12k.buff=st_buffer(MN.RiceSites2_UTM, endCapStyle='SQUARE', 12000)
MN.RiceSites_UTM_18k.buff=st_buffer(MN.RiceSites2_UTM, endCapStyle='SQUARE', 18000)

ggplot(MN_sf.UTM)+geom_sf()+
  geom_sf(data=MN.RiceSites_UTM_6k.buff, fill=NA)+
  geom_sf(data=MN.RiceSites_UTM_12k.buff, fill=NA)+
  geom_sf(data=MN.RiceSites_UTM_18k.buff, fill=NA)+
    geom_sf(data=MN.RiceSites2_UTM, size=0.2, col="red")

MN_cond.tree_join

### Extract MN FIA plots for 6k buffer
MN.RiceSites_6k.buff=st_transform(MN.RiceSites_UTM_6k.buff, crs=4326)

Plots_In_MN.RiceSites_6k.buff=st_join(MN_plot_sf, MN.RiceSites_6k.buff, join=st_within)%>%filter(Name != "NA")

ggplot(MN.RiceSites_6k.buff)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_6k.buff, aes(col=as.factor(INVYR)))
Plots_In_MN.RiceSites_6k.buff_2K.yr=Plots_In_MN.RiceSites_6k.buff%>%filter(INVYR>1999)
ggplot(MN.RiceSites_6k.buff)+geom_sf()+
geom_sf(data = Plots_In_MN.RiceSites_6k.buff_2K.yr)+facet_wrap(~INVYR)

### Extract MN FIA plots for 12k buffer
MN.RiceSites_12k.buff=st_transform(MN.RiceSites_UTM_12k.buff, crs=4326)

Plots_In_MN.RiceSites_12k.buff=st_join(MN_plot_sf, MN.RiceSites_12k.buff, join=st_within)%>%filter(Name != "NA")

ggplot(MN.RiceSites_12k.buff)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_12k.buff, aes(col=as.factor(INVYR)))
Plots_In_MN.RiceSites_12k.buff_2K.yr=Plots_In_MN.RiceSites_12k.buff%>%filter(INVYR>1999)
ggplot(MN.RiceSites_12k.buff)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_12k.buff_2K.yr)+facet_wrap(~INVYR)

### Extract MN FIA plots for 18k buffer
MN.RiceSites_18k.buff=st_transform(MN.RiceSites_UTM_18k.buff, crs=4326)

Plots_In_MN.RiceSites_18k.buff=st_join(MN_plot_sf, MN.RiceSites_18k.buff, join=st_within)%>%filter(Name != "NA")
Plots_In_MN.RiceSites_18k.buff
ggplot(MN.RiceSites_18k.buff)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_18k.buff, aes(col=as.factor(INVYR)))
Plots_In_MN.RiceSites_18k.buff_2K.yr=Plots_In_MN.RiceSites_18k.buff%>%filter(INVYR>1999)
ggplot(MN.RiceSites_18k.buff)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_18k.buff_2K.yr, alpha=0.3)+facet_wrap(~INVYR)

########################################################################################################################
###Clip and extract the FIA data using the wild rice buffers created above
### Need the tree database from FIA
MN_tree=read_csv("RawData/MN_TREE.csv")
MN_tree_Yr2000=MN_tree%>% filter(INVYR>1999)
MN_tree_Yr2000
MN_tree_Yr2000.red=MN_tree_Yr2000[,c(1:21)]
MN_tree_Yr2000.red

MN_cond
MN_cond.tree_join=left_join(MN_tree_Yr2000.red, MN_cond, by=c("PLT_CN", "CONDID", "PLOT", "INVYR"))
MN_cond.tree_join
Plots_In_MN.RiceSites_18k.buff_2K.yr ## from above

### Get the Lat Lon columns
Plots_In_MN.RiceSites_18k.buff_2K.yr.coords = Plots_In_MN.RiceSites_18k.buff_2K.yr %>%
mutate(Lon = unlist(map(Plots_In_MN.RiceSites_18k.buff_2K.yr$geometry,1)),
Lat = unlist(map(Plots_In_MN.RiceSites_18k.buff_2K.yr$geometry,2)))
Plots_In_MN.RiceSites_18k.buff_2K.yr_data=st_drop_geometry(Plots_In_MN.RiceSites_18k.buff_2K.yr.coords)
Plots_In_MN.RiceSites_18k.buff_2K.yr_data

cond.tree_18kBuff2kyr.join=left_join(Plots_In_MN.RiceSites_18k.buff_2K.yr_data,MN_cond.tree_join, by=c("PLOT", "INVYR"))
length(unique(cond.tree_18kBuff2kyr.join$Lat))
length(unique(cond.tree_18kBuff2kyr.join$PLOT)) ### again some discrepency between plots and lat/lon

cond.tree_18kBuff2kyr.join.sf=st_as_sf(cond.tree_18kBuff2kyr.join, coords =c("Lon", "Lat"), crs=4326)
cond.tree_18kBuff2kyr.join.sf
ggplot(MN.RiceSites_18k.buff)+geom_sf()+
geom_sf(data = cond.tree_18kBuff2kyr.join.sf, alpha=0.5)+facet_wrap(~INVYR)

write_csv(cond.tree_18kBuff2kyr.join ,"ProcessedData/RiceSites_w18KBuff_TreeData.Join.csv")

### Visualizing the discrpency in lat/lon and plot ids
Plots_wMultiLatLon=cond.tree_18kBuff2kyr.join%>%group_by(PLOT)%>%tally(n_distinct(Lat))%>%filter(n==2)%>%pull(PLOT)
Plots_wMultiLatLon
Plots_wMultiLatLon.df=cond.tree_18kBuff2kyr.join%>%filter(PLOT %in% Plots_wMultiLatLon)
Plots_wMultiLatLon.df
Plots_wMultiLatLon.sf=st_as_sf(Plots_wMultiLatLon.df, coords = c("Lon", "Lat"), crs=4326)
ggplot(MN.RiceSites_18k.buff)+geom_sf()+
geom_sf(data = Plots_wMultiLatLon.sf, alpha=0.5)+
geom_label(data = Plots_wMultiLatLon.df, aes(Lon, Lat, label = PLOT), fontface = "bold", size=2)

################################################################################################################
################################################################################################################
################################################################################################################

MN_wtrshds=st_read("GIS_Data/MN_dnr_watersheds/dnr_watersheds_dnr_level_04_huc_08_majors.shp")
MN_ecorgns=st_read("GIS_Data/mn_ecoregions/mn_eco_l4.shp")
US_shp=st_read("GIS_Data/gadm41_USA_shp/gadm41_USA_1.shp")

MN_shp=US_shp%>%filter(NAME_1=="Minnesota")
plot(MN_shp$geometry, axes=TRUE)
MN_sf=st_as_sf(MN_shp, crs=4326)
MN_sf.UTM=st_transform(MN_sf, crs="+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")
MN_sf.UTM

##Extract watersheds within selected ecoregions
MN_ecorgns_UTM=st_transform(MN_ecorgns, crs="+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")
MN_ecorgns_US_L3CODE.50=MN_ecorgns_UTM%>%filter(US_L3CODE==50)
MN_wtrshds_UTM=st_transform(MN_wtrshds, crs="+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs")
MN_wtrshds_ecoregion_US_L3CODE.50=st_join(MN_wtrshds_UTM,MN_ecorgns_US_L3CODE.50, join=st_overlaps)%>%filter(US_L4NAME!= "NA")
MN_wtrshds_ecoregion_US_L3CODE.50_center=st_centroid(MN_wtrshds_ecoregion_US_L3CODE.50)

ggplot(MN_sf.UTM)+geom_sf()+
geom_sf(data=MN_wtrshds_ecoregion_US_L3CODE.50_center)+
  geom_sf_text(data=MN_wtrshds_ecoregion_US_L3CODE.50_center, aes(label=HUC_8))

TrialList=c("09030003","04010101","07010102", "07010201",
            "04010201", "07030003","09030006", "07030005")

Sel_HUC_08_wtrshds=MN_wtrshds_ecoregion_US_L3CODE.50%>%filter(HUC_8 %in% TrialList)
Sel_HUC_08_wtrshds_centr=MN_wtrshds_ecoregion_US_L3CODE.50_center%>%filter(HUC_8 %in% TrialList)

### Final map of Minnesota showing all the 8 selected watersheds
ggplot(MN_sf.UTM)+geom_sf()+
geom_sf(data=Sel_HUC_08_wtrshds)+geom_sf_text(data=Sel_HUC_08_wtrshds, aes(label=HUC_8))+
geom_sf(data=Sel_HUC_08_wtrshds_centr, pch=21, col="red")+xlab(" ")+ylab(" ")

### Make square buffers around the centroids
Sel_HUC_08_wtrshds_centr_buff18k=st_buffer(Sel_HUC_08_wtrshds_centr, endCapStyle='SQUARE', 18000)
Sel_HUC_08_wtrshds_centr_buff18k

ggplot(MN_sf.UTM)+geom_sf()+
geom_sf(data=Sel_HUC_08_wtrshds)+
geom_sf(data=Sel_HUC_08_wtrshds_centr, pch=21, col="red")+
geom_sf(data=Sel_HUC_08_wtrshds_centr_buff18k, fill="NA", col="white")

### Extract MN FIA plots for 18k buffer
Sel_HUC_08_wtrshds_centr_buff18k=st_transform(Sel_HUC_08_wtrshds_centr_buff18k, crs=4326)

Plots_In_MN.RiceSites_18k.buff=st_join(MN_plot_sf,Sel_HUC_08_wtrshds_centr_buff18k , join=st_within)%>%filter(HUC_8 != "NA")
Plots_In_MN.RiceSites_18k.buff
ggplot(Sel_HUC_08_wtrshds_centr_buff18k)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_18k.buff, aes(col=as.factor(INVYR)))
Plots_In_MN.RiceSites_18k.buff_2K.yr=Plots_In_MN.RiceSites_18k.buff%>%filter(INVYR>1999)
ggplot(Sel_HUC_08_wtrshds_centr_buff18k)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_18k.buff_2K.yr, alpha=0.3)+facet_wrap(~INVYR)

Plots_In_MN.RiceSites_18k.buff_2K.yr.coords = Plots_In_MN.RiceSites_18k.buff_2K.yr %>%
  mutate(Lon = unlist(map(Plots_In_MN.RiceSites_18k.buff_2K.yr$geometry,1)),
         Lat = unlist(map(Plots_In_MN.RiceSites_18k.buff_2K.yr$geometry,2)))
Plots_In_MN.RiceSites_18k.buff_2K.yr.coords

Plots_In_MN.RiceSites_18k.buff_2K.yr_data=st_drop_geometry(Plots_In_MN.RiceSites_18k.buff_2K.yr.coords)
Plots_In_MN.RiceSites_18k.buff_2K.yr_data
cond.tree_18kBuff2kyr.join=left_join(Plots_In_MN.RiceSites_18k.buff_2K.yr_data,MN_cond.tree_join, by=c("PLOT", "INVYR"))
cond.tree_18kBuff2kyr.join
length(unique(cond.tree_18kBuff2kyr.join$Lat))
length(unique(cond.tree_18kBuff2kyr.join$PLOT)) ### again some discrepency between plots and lat/lon

write_csv(cond.tree_18kBuff2kyr.join ,"ProcessedData/EcoRgns_US_L3CODE.50_HUC08_Wtrshds_w18KBuff_TreeData.Join.csv")

cond.tree_18kBuff2kyr.join.sf=st_as_sf(cond.tree_18kBuff2kyr.join, coords =c("Lon", "Lat"), crs=4326)
cond.tree_18kBuff2kyr.join.sf

ggplot(Sel_HUC_08_wtrshds_centr_buff18k)+geom_sf()+
geom_sf(data = cond.tree_18kBuff2kyr.join.sf, alpha=0.5)+facet_wrap(~INVYR)

###################################################################################################################
###################################################################################################################
#### Now extract and put together the LAI data for each of the selected wtrshds
### First, need centroids of the wtrshds to defined the buffering polgon which will be used to extract LAI
### LAI was extracted from ORNL's global subset tool
MN_wtrshds_LAT.LON=st_transform(MN_wtrshds, crs = "+proj=longlat +datum=WGS84")
MN_wtrshds_LAT.LON
Sel8Wtrshds=MN_wtrshds_LAT.LON%>%filter(HUC_8 %in% TrialList)
Sel8Wtrshds
Sel8Wtrshds_centr=st_centroid(Sel8Wtrshds)
Sel8Wtrshds_centr

Sel8Wtrshds_centr.coords = Sel8Wtrshds_centr %>%
  mutate(Lon = unlist(map(Sel8Wtrshds_centr$geometry,1)),
         Lat = unlist(map(Sel8Wtrshds_centr$geometry,2)))%>%st_drop_geometry()

Sel8Wtrshds_centr.coords
LatLon.Sel8Wtrshds=Sel8Wtrshds_centr.coords[,c(7,10,11)]
LatLon.Sel8Wtrshds


Wshd1_LAI=read_csv("RawData/Wtrshd1_statistics_Lai_500m .csv")
Wshd2_LAI=read_csv("RawData/Wtrshd2_statistics_Lai_500m .csv")
Wshd3_LAI=read_csv("RawData/Wtrshd3_statistics_Lai_500m .csv")
Wshd4_LAI=read_csv("RawData/Wtrshd4_statistics_Lai_500m .csv")
Wshd5_LAI=read_csv("RawData/Wtrshd5_statistics_Lai_500m .csv")
Wshd6_LAI=read_csv("RawData/Wtrshd6_statistics_Lai_500m .csv")
Wshd7_LAI=read_csv("RawData/Wtrshd7_statistics_Lai_500m .csv")
Wshd8_LAI=read_csv("RawData/Wtrshd8_statistics_Lai_500m .csv")

Wshd1_LAI_v2=Wshd1_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))
Wshd2_LAI_v2=Wshd2_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))
Wshd3_LAI_v2=Wshd3_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))
Wshd4_LAI_v2=Wshd4_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))
Wshd5_LAI_v2=Wshd5_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))
Wshd6_LAI_v2=Wshd6_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))
Wshd7_LAI_v2=Wshd7_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))
Wshd8_LAI_v2=Wshd8_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))

### the final LAI data
Wshd1_LAI_noNA=Wshd1_LAI_v2%>%filter(!is.na(mean))%>%mutate(HUC_8=rep("07010102")) 
Wshd2_LAI_noNA=Wshd2_LAI_v2%>%filter(!is.na(mean))%>%mutate(HUC_8=rep("07010201"))
Wshd3_LAI_noNA=Wshd3_LAI_v2%>%filter(!is.na(mean))%>%mutate(HUC_8=rep("04010101")) 
Wshd4_LAI_noNA=Wshd4_LAI_v2%>%filter(!is.na(mean))%>%mutate(HUC_8=rep("09030003")) 
Wshd5_LAI_noNA=Wshd5_LAI_v2%>%filter(!is.na(mean))%>%mutate(HUC_8=rep("07030003"))
Wshd6_LAI_noNA=Wshd6_LAI_v2%>%filter(!is.na(mean))%>%mutate(HUC_8=rep("07030005")) 
Wshd7_LAI_noNA=Wshd7_LAI_v2%>%filter(!is.na(mean))%>%mutate(HUC_8=rep("09030006")) 
Wshd8_LAI_noNA=Wshd8_LAI_v2%>%filter(!is.na(mean))%>%mutate(HUC_8=rep("04010201")) 

### Trial visualization of LAI
Wshd1_mean.lai=ggplot(Wshd1_LAI_noNA, aes(x=jday, y=mean, col=as.factor(year)))+geom_point()+geom_smooth(method="gam",se=FALSE)+
  theme(legend.title=element_blank())
Wshd1_mean.lai

All8.NoNA.df=bind_rows(Wshd1_LAI_noNA,Wshd2_LAI_noNA,Wshd3_LAI_noNA,Wshd4_LAI_noNA,Wshd5_LAI_noNA,
                       Wshd6_LAI_noNA,Wshd7_LAI_noNA,Wshd8_LAI_noNA)
All8.NoNA.df
All8.NoNA.df_LatLon=left_join(All8.NoNA.df, LatLon.Sel8Wtrshds, by="HUC_8")
All8.NoNA.df_LatLon

ggplot(All8.NoNA.df_LatLon, aes(x=jday, y=mean, col=as.factor(round(Lat,2))))+geom_smooth(method="gam", se=FALSE)+
labs(color="Lat")+scale_color_viridis_d()+xlab("Julian day")+ylab("Average LAI")
ggsave("LAI_Jday_8WtrshdLats.png", path="Figures/", device="png",width = 9, height = 6.5, dpi=900)

ggplot(All8.NoNA.df, aes(x=jday, y=mean, col=as.factor(HUC_8)))+geom_smooth(method="gam")+
  theme(legend.title=element_blank())

All8.NoNA.df_LatLon%>%group_by(HUC_8)%>%summarise(
  maxMeanLAI=max(mean),
  Lat=max(Lat))%>%
    ggplot(.,aes(Lat, maxMeanLAI))+geom_point()+geom_smooth(method="glm")+
    xlab("Watershed latitude")+ylab("Peak annual LAI")+
      stat_cor(label.y = 5.6, size=4.5)
ggsave("WtrshdLat_AnnPeakLAI.png", path="Figures/", device="png",width = 9, height = 6.5, dpi=900)

All8.laijday.GAM=gam(mean~s(jday)+s(year), data=All8.NoNA.df)
summary(All8.laijday.GAM)
plot(All8.laijday.GAM)
All8.laijday.GAM.resid=residuals.gam(All8.laijday.GAM)
All8.NoNA.df_LatLon$All8.laijday.GAM.resid=All8.laijday.GAM.resid

All8.Wtrshds.resid=All8.NoNA.df_LatLon%>%group_by(HUC_8)%>%summarise(
meanAnnMean_LAI.GAM.resid=mean(All8.laijday.GAM.resid),
varAnnMean_LAI.GAM.resid=sd(All8.laijday.GAM.resid),
Lat=max(Lat)
)

All8.Wtrshds.resid.covar=All8.NoNA.df_LatLon%>%group_by(HUC_8)%>%summarise(
meanAnnMean_LAI.GAM.resid=mean(All8.laijday.GAM.resid),
varAnnMean_LAI.GAM.resid=sd(All8.laijday.GAM.resid),
covarAnnMean_LAI.GAM.resid=varAnnMean_LAI.GAM.resid/meanAnnMean_LAI.GAM.resid,
Lat=max(Lat)
)

All8.Wtrshds.Only_year.resid=All8.NoNA.df_LatLon%>%group_by(year, HUC_8)%>%summarise(
  meanAnnMean_LAI.GAM.resid=mean(All8.laijday.GAM.resid)
)

plot(SelWtrshds.coords.tb$Y,All8.Wtrshds.resid$meanAnnMean_LAI.GAM.resid)


#######
Prpn.HardSoft.Tree_All8HUC=cond.tree_18kBuff2kyr.join%>%filter(!is.na(SPCD))%>%group_by(HUC_8)%>%summarise(
                             total=n(),
                              prpn.totalHard=sum(SPGRPCD>24 & SPGRPCD < 51)/total,
                              prpn.totalSoft=sum(SPGRPCD<25)/total,
                              prpn.totOther=sum(SPGRPCD > 50)/total)

Prpn.HardSoft.Tree_All8HUC

left_join(Prpn.HardSoft.Tree_All8HUC, All8.Wtrshds.resid, by="HUC_8")%>%
  ggplot(.,aes(Lat,meanAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")+stat_cor(label.y=0.55)

left_join(Prpn.HardSoft.Tree_All8HUC, All8.Wtrshds.resid, by="HUC_8")%>%
  ggplot(.,aes(Lat,prpn.totalSoft))+geom_point()+geom_smooth(method="glm")+stat_cor(label.y=0.55)

left_join(Prpn.HardSoft.Tree_All8HUC, All8.Wtrshds.resid, by="HUC_8")%>%
  ggplot(.,aes(prpn.totalSoft,meanAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")+stat_cor(label.y=0.55)



BA.HardSoft_All8HUC=cond.tree_18kBuff2kyr.join %>%mutate(WoodType=case_when(SPGRPCD < 25 ~ 'Softwood', SPGRPCD > 24 ~ 'Hardwood'))%>%
dplyr::select(2:10,WoodType,DIA,HUC_8)%>%filter(!is.na(DIA))%>%group_by(HUC_8,WoodType)%>%summarise(
meanBA=mean(DIA),
maxBA=max(DIA),
varBA=sd(DIA),
totBA=sum(DIA))

totBA.HUC_8=cond.tree_18kBuff2kyr.join%>%filter(!is.na(DIA))%>%group_by(HUC_8)%>%summarise(
grnd.totBA=sum(DIA)
)

BA.Hard_All8HUC=BA.HardSoft_All8HUC%>%filter(WoodType=="Hardwood")
BA.Soft_All8HUC=BA.HardSoft_All8HUC%>%filter(WoodType=="Softwood")
BA.Hard_All8HUC
BA.Soft_All8HUC

Prpn.HardSoft.BA_All8HUC=left_join(BA.Soft_All8HUC, totBA.HUC_8, by="HUC_8")%>%mutate(Prpn.SoftBA=totBA/grnd.totBA,Prpn.HardBA=1-Prpn.SoftBA)
Prpn.HardSoft.BA_All8HUC
left_join(Prpn.HardSoft.BA_All8HUC, All8.Wtrshds.resid, by="HUC_8")%>%
ggplot(.,aes(Lat,meanAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")+stat_cor()
left_join(Prpn.HardSoft.BA_All8HUC, All8.Wtrshds.resid, by="HUC_8")%>%
ggplot(.,aes(Prpn.SoftBA,meanAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")+stat_cor(label.y = 0.47)



Tot.SpRichness=cond.tree_18kBuff2kyr.join%>%filter(!is.na(SPCD))%>%group_by(HUC_8)%>%summarise(
Sp.Richness=n_distinct(SPCD))
left_join(Tot.SpRichness,All8.Wtrshds.resid, by="HUC_8")%>%
ggplot(.,aes(Sp.Richness,meanAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")+stat_cor(label.y = 0.35)

Hard.SpRichness=cond.tree_18kBuff2kyr.join%>%filter(!is.na(SPCD))%>%group_by(HUC_8, SPGRPCD)%>%summarise(
Sp.Richness=n_distinct(SPCD))%>%filter(SPGRPCD>24)%>%group_by(HUC_8)%>%summarise(HardSp.Richness=sum(Sp.Richness))
left_join(Hard.SpRichness,All8.Wtrshds.resid, by="HUC_8")%>%
ggplot(.,aes(HardSp.Richness,meanAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")+stat_cor()


Soft.SpRichness=cond.tree_18kBuff2kyr.join%>%filter(!is.na(SPCD))%>%group_by(HUC_8, SPGRPCD)%>%summarise(
Sp.Richness=n_distinct(SPCD))%>%filter(SPGRPCD<25)%>%group_by(HUC_8)%>%summarise(SoftSp.Richness=sum(Sp.Richness))
left_join(Soft.SpRichness,All8.Wtrshds.resid, by="HUC_8")%>%
ggplot(.,aes(SoftSp.Richness,meanAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")+stat_cor()


##################################################################################################################
##################################################################################################################
##### Getting temperature and GDD data
PRISM_data=read_stars("RawData/PRISM_data/PRISM_tmin_30yr_normal_4kmM3_annual_asc.asc")
PRISM_data

MN_sf2=st_transform(MN_shp, crs=st_crs(PRISM_data))
MN_tmin=st_crop(PRISM_data,MN_sf2)
ggplot() +
geom_stars(data = MN_tmin) +
scale_fill_viridis_c() +
geom_sf(data = MN_sf2, fill = "transparent")
MN_tmin

PRISM_data_mean=read_stars("RawData/PRISM_data/PRISM_tmean_30yr_normal_4kmM4_annual_asc.asc")
MN_sf2=st_transform(MN_shp, crs=st_crs(PRISM_data_mean))

MN_tmean=st_crop(PRISM_data_mean,MN_sf2)
ggplot() +
geom_stars(data = MN_tmean) +
scale_fill_viridis_c() +
geom_sf(data = MN_sf2, fill = "transparent")
MN_tmean

### Get the buffer polygons for the selected 8 watersheds and remove the duplicates
Sel_HUC_08_wtrshds_centr_buff18k
HUC_08_wtrshds_centr_buff18k=Sel_HUC_08_wtrshds_centr_buff18k%>%distinct(HUC_8, .keep_all = TRUE)
HUC_08_wtrshds_centr_buff18k

library(geobgu)

HUC_08_wtrshds_centr_buff18k$tmean_30yr=raster_extract(MN_tmean,HUC_08_wtrshds_centr_buff18k, fun=mean, na.rm=TRUE)
HUC_08_wtrshds_centr_buff18k
ggplot(MN_sf.UTM)+geom_sf()+
geom_sf(data=Sel_HUC_08_wtrshds)+
geom_sf(data=HUC_08_wtrshds_centr_buff18k, aes(fill=tmean_30yr))


NLCD=read_stars("GIS_Data/nlcd_2001_2019_change_index_l48_20210604/nlcd_2001_2019_change_index_l48_20210604.img")
MN_sf_nlcd=st_transform(MN_shp, crs=st_crs(NLCD))
MN_nlcd=st_crop(NLCD,MN_sf_nlcd)
plot(MN_nlcd)
