library(ggpubr)
library(tidyverse)
library(ggplot2)
library(terra)
library(raster)
library(exactextractr)
library(sf)
library(tidyterra)

### Starting with reading NLCD 2019 data
NLCD_19_data=rast("GIS_Data/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")

MN_shp=US_shp%>%filter(NAME_1 %in% "Minnesota")
MN_new=st_transform(MN_shp ,crs=st_crs(NLCD_19_data))
MN_new
MN_terra=vect(MN_new)

#MN_nlcd_2019=st_crop(NLCD_data,MN_new)

#MN_HUC8_poly=st_read("ProcessedData/HUC8.poly_MN_Sel.EcoRgns.shp")
MN_HUC8_buffs =st_read("GIS_Data/MN_HUC08_wtrshds_buff18k.shp")
MN_HUC8_buff_new=st_transform(MN_HUC8_buffs ,crs=st_crs(NLCD_19_data))
MN_HUC8_buff_terra=vect(MN_HUC8_buff_new)
MN_HUC8_nlcd19=terra::crop(NLCD_19_data,MN_HUC8_buff_terra, mask=TRUE)

NLCD19_HUC08 = ggplot()+geom_spatraster(data = MN_HUC8_nlcd19)+geom_spatvector(data=MN_HUC8_buff_terra, fill="transparent")+
               geom_spatvector(data=MN_terra, fill="transparent")+scale_fill_viridis_d()+ggtitle("NLCD 2019")
NLCD19_HUC08

NLCD2019_HUC8_frac=exact_extract(NLCD_19_data, st_as_sf(MN_HUC8_buff_terra), c('count', 'frac'), append_cols = 'HUC8')
write_csv(NLCD2019_HUC8_frac,"ProcessedData/HUC8_2019LULC_Prpn.csv")

#### Same codes as above, but with 2001 nlcd
NLCD_01_data=rast("GIS_Data/nlcd_2001_land_cover_l48_20210604/nlcd_2001_land_cover_l48_20210604.img")

MN_HUC8_buff_new=st_transform(MN_HUC8_buffs ,crs=st_crs(NLCD_01_data))
MN_HUC8_buff_terra=vect(MN_HUC8_buff_new)
MN_HUC8_nlcd01=crop(NLCD_01_data,MN_HUC8_buff_terra, mask=TRUE)

NLCD01_HUC08 = ggplot()+geom_spatraster(data = MN_HUC8_nlcd01)+geom_spatvector(data=MN_HUC8_buff_terra, fill="transparent")+
               geom_spatvector(data=MN_terra, fill="transparent")+scale_fill_viridis_d()+ggtitle("NLCD 2001")
NLCD01_HUC08
NLCD2001_HUC8_frac=exact_extract(NLCD_01_data, st_as_sf(MN_HUC8_buff_terra), c('count', 'frac'), append_cols = 'HUC8')
write_csv(NLCD2001_HUC8_frac,"ProcessedData/HUC8_2001LULC_Prpn.csv")

library(gridExtra)
grid.arrange(NLCD01_HUC08,NLCD19_HUC08, ncol=2)

### Summarizing the NLCD land-use/cover fractions
NLCD2019_HUC8_frac.agg=NLCD2019_HUC8_frac%>%rowwise()%>% 
                        mutate(prpn.for = sum(c_across(frac_41:frac_43)), 
                        prpn.anthro=sum(c(frac_21, frac_22,frac_23,frac_24,frac_81,frac_82)),
                        prpn.dist=sum(c_across(frac_21: frac_24)), prpn.agr=sum(c(frac_81,frac_82)),
                        prpn.opw=frac_11, prpn.wet=sum(c(frac_90, frac_95)))

NLCD2001_HUC8_frac.agg=NLCD2001_HUC8_frac%>%rowwise()%>% 
  mutate(prpn.for = sum(c_across(frac_41:frac_43)), 
         prpn.anthro=sum(c(frac_21, frac_22,frac_23,frac_24,frac_81,frac_82)),
         prpn.dist=sum(c_across(frac_21: frac_24)), prpn.agr=sum(c(frac_81,frac_82)),
         prpn.opw=frac_11, prpn.wet=sum(c(frac_90, frac_95)))

NLCD2019_HUC8_frac.agg.only=NLCD2019_HUC8_frac.agg%>% select(c(1,2,18:23))
NLCD2001_HUC8_frac.agg.only=NLCD2001_HUC8_frac.agg%>% select(c(1,2,18:23))

NLCD_01_19_chng=left_join(NLCD2001_HUC8_frac.agg.only,NLCD2019_HUC8_frac.agg.only, by="HUC8")%>%mutate(
                  prpn.for.chng=(prpn.for.y - prpn.for.x)*100,
                  prpn.anthro.chng=(prpn.anthro.y-prpn.anthro.x)*100,
                  prpn.dist.chng=(prpn.dist.y-prpn.dist.x)*100,
                  prpn.agr.chng=(prpn.agr.y-prpn.agr.x)*100,
                      prpn.for.tot=(prpn.for.y + prpn.for.x)/2,
                      prpn.anthro.tot=(prpn.anthro.y+prpn.anthro.x)/2,
                      prpn.dist.tot=(prpn.dist.y+prpn.dist.x)/2,
                      prpn.agr.tot=(prpn.agr.y+prpn.agr.x)/2,
                      prpn.opw.tot=(prpn.opw.y+prpn.opw.x)/2,
                      prpn.wet.tot=(prpn.wet.y+prpn.wet.x)/2) %>%
                        select(c(1,2,17:25))

NLCD_01_19_chng$HUC8=as.numeric(NLCD_01_19_chng$HUC8) 
NLCD_01_19_chng### the final land use change data for each watershed

MN_wtrshds_FIA_LAI_Temp_jn = read_csv("ProcessedData/MN_wtrshds.FIA_LAI_Temp.jn.csv")
MN_wtrshds_FIA_LAI_Temp_jn
MN_wtrshds_FIA_LAI_Temp_lulc.jn=left_join(MN_wtrshds_FIA_LAI_Temp_jn,NLCD_01_19_chng, by="HUC8")
MN_wtrshds_FIA_LAI_Temp_lulc.jn%>%View()
write_csv(MN_wtrshds_FIA_LAI_Temp_lulc.jn,"ProcessedData/MN_wtrshds.FIA_LAI_Temp_lulc.jn.csv")
MN_wtrshds_FIA_LAI_Temp_lulc.jn=read_csv("ProcessedData/MN_wtrshds.FIA_LAI_Temp_lulc.jn.csv")

########################################################################################################################
library(piecewiseSEM)
### exploring the direct and indirect effects of temperature & lulc on leaf area index
### a) effect on inter-annual variabilty of LAI - coefficient of variance

MN_wtrshds_FIA_LAI_Temp_lulc.jn

psem.model.temp=psem(
lm(Tmean~Lat, MN_wtrshds_FIA_LAI_Temp_lulc.jn),
lm(Prpn.SoftBA~Tmean, MN_wtrshds_FIA_LAI_Temp_lulc.jn),
lm(covarLAI~Prpn.SoftBA + Tmean + Lat, MN_wtrshds_FIA_LAI_Temp_lulc.jn)
)
summary(psem.model.temp)
plot(psem.model.temp,digits=2,
     node_attrs = data.frame(fontsize = 6, shape= "rectangle"))

psem.model.lulc=psem(
  lm(prpn.anthro.tot~Lat,MN_wtrshds_FIA_LAI_Temp_lulc.jn ),
  lm(Prpn.SoftBA~prpn.anthro.tot+Lat ,MN_wtrshds_FIA_LAI_Temp_lulc.jn),
  lm(covarLAI~Prpn.SoftBA+prpn.anthro.tot+Lat, MN_wtrshds_FIA_LAI_Temp_lulc.jn)
)
summary(psem.model.lulc)
plot(psem.model.lulc,digits=2,
     node_attrs = data.frame(fontsize = 6, shape= "rectangle"))

psem.model.full=psem(
lm(Tmean~Lat, MN_wtrshds_FIA_LAI_Temp_lulc.jn),
lm(prpn.anthro.tot~Lat,MN_wtrshds_FIA_LAI_Temp_lulc.jn ),
lm(Prpn.SoftBA~Tmean +prpn.anthro.tot ,MN_wtrshds_FIA_LAI_Temp_lulc.jn),
lm(covarLAI~Prpn.SoftBA + Tmean +prpn.anthro.tot, MN_wtrshds_FIA_LAI_Temp_lulc.jn)
)
summary(psem.model.full)
plot(psem.model.full, digits=2,
     node_attrs = data.frame(fontsize = 6, shape= "rectangle"))
##################
### b) effect on average annual LAI - mean
psem.avg.model.temp=psem(
  lm(Tmean~Lat, MN_wtrshds_FIA_LAI_Temp_lulc.jn),
  lm(Prpn.SoftBA~Tmean, MN_wtrshds_FIA_LAI_Temp_lulc.jn),
  lm(meanLAI~Prpn.SoftBA + Tmean + Lat, MN_wtrshds_FIA_LAI_Temp_lulc.jn)
)
summary(psem.avg.model.temp)
plot(psem.avg.model.temp,digits=2,
     node_attrs = data.frame(fontsize = 6, shape= "rectangle"))

psem.avg.model.lulc=psem(
  lm(prpn.anthro.tot~Lat,MN_wtrshds_FIA_LAI_Temp_lulc.jn ),
  lm(Prpn.SoftBA~prpn.anthro.tot+Lat ,MN_wtrshds_FIA_LAI_Temp_lulc.jn),
  lm(meanLAI~Prpn.SoftBA+prpn.anthro.tot+Lat, MN_wtrshds_FIA_LAI_Temp_lulc.jn)
)
summary(psem.avg.model.lulc)
plot(psem.avg.model.lulc,digits=2,
     node_attrs = data.frame(fontsize = 6, shape= "rectangle"))

psem.avg.model.full=psem(
  lm(Tmean~Lat, MN_wtrshds_FIA_LAI_Temp_lulc.jn),
  lm(prpn.anthro.tot~Lat,MN_wtrshds_FIA_LAI_Temp_lulc.jn ),
  lm(Prpn.SoftBA~Tmean +prpn.anthro.tot ,MN_wtrshds_FIA_LAI_Temp_lulc.jn),
  lm(meanLAI~Prpn.SoftBA + Tmean +prpn.anthro.tot, MN_wtrshds_FIA_LAI_Temp_lulc.jn)
)
summary(psem.avg.model.full)
plot(psem.avg.model.full, digits=2,
     node_attrs = data.frame(fontsize = 6, shape= "rectangle"))

#US.selStates_new=st_transform(US.selStates ,crs=st_crs(NLCD_data))
#Sel3States_nlcd=st_crop(NLCD_data,US.selStates_new)
#plot(Sel3States_nlcd)
#ggplot() +
 # geom_stars(data = Sel3States_nlcd) +
 #  scale_fill_viridis_c() +
  # geom_sf(data = US.selStates_new , fill = "transparent")

#write_stars(Sel3States_nlcd, "GIS_Data/Sel3States_nlcd.tif")