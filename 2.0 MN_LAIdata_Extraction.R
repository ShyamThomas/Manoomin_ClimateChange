library(tidyverse)
library(sf)
library(lubridate)
library(fs)

#### Extract and put together the Leaf Area Index (LAI) data for each of the selected wtrshds buffers
### First, need centroids of the wtrshds to defined the buffering polygon which will be used to extract LAI
### LAI was extracted from ORNL's global subset tool [source: https://modis.ornl.gov/globalsubset/]

MN_wtrshds_centr=st_read("GIS_Data/HUC8_MN_centr.shp") ### a shapefile of MN watersheds
MN_HUC8.WTRSHDS=read_csv("ProcessedData/All.MN_HUC8.wtrshds.csv") ## the list of MN watersheds


dir_ls("RawData/Minn")
df = dir_ls("RawData/Minn") %>%
  map_df(~read_csv(.x)%>% mutate(file = basename(.x)))

all.wtrshds.LAI=df%>%mutate_at("file", str_trunc, width = 4, side='right', ellipsis = '')%>%
                   mutate(year= year(date), month=month(date), jday=yday(date))

all.wtrshds.LAI%>%ggplot(.,aes(jday, mean, col=file))+geom_smooth(method="gam", se=F)+
  scale_color_viridis_d()+
  xlab("Julian day")+ylab("Average LAI")

all.wtrshds.LAIstats=all.wtrshds.LAI%>%rename(ID=file)%>% filter(!is.na(mean))%>%group_by(ID)%>%summarise(
meanLAI=mean(max),
medianLAI=median(max),
maxLAI=max(max),
varLAI=var(max),
covarLAI=(sqrt(varLAI)/meanLAI))

all.wtrshds.LAIstats

left_join(HUC8_FIA.jn, all.wtrshds.LAIstats, by="ID")%>%
  ggplot(., aes(Lat, Prpn.SoftBA ))+geom_point()+geom_smooth(method="lm")+stat_cor()

left_join(HUC8_FIA.jn, all.wtrshds.LAIstats, by="ID")%>%
  ggplot(., aes(Prpn.SoftBA, covarLAI))+geom_point()+geom_smooth(method="lm")+stat_cor()

MN_wtrshds.FIA_LAI.jn=left_join(HUC8_FIA.jn, all.wtrshds.LAIstats, by="ID")
write_csv(MN_wtrshds.FIA_LAI.jn,"ProcessedData/MN_wtrshds.FIA_LAI.jn.csv") 

MN_wtrshds.FIA_LAI.jn=read_csv("ProcessedData/MN_wtrshds.FIA_LAI.jn.csv")

#######################################################################################################################
#######################################################################################################################


### Trial visualization of LAI of single wtrshd
Wshd1_mean.lai=ggplot(Wshd1_LAI_noNA, aes(x=jday, y=mean, col=as.factor(year)))+geom_point()+geom_smooth(method="gam",se=FALSE)+
  theme(legend.title=element_blank())
Wshd1_mean.lai

### Combine LAI data across all wtrshds
All8.NoNA.df=bind_rows(Wshd1_LAI_noNA,Wshd2_LAI_noNA,Wshd3_LAI_noNA,Wshd4_LAI_noNA,Wshd5_LAI_noNA,
                       Wshd6_LAI_noNA,Wshd7_LAI_noNA,Wshd8_LAI_noNA)
All8.NoNA.df
All8.NoNA.df_LatLon=left_join(All8.NoNA.df, LatLon.Sel8Wtrshds, by="HUC_8")
All8.NoNA.df_LatLon
write_csv(All8.NoNA.df_LatLon,"ProcessedData/All8.NoNA.df_LatLon.csv") 
All8.NoNA.df_LatLon=read_csv("ProcessedData/All8.NoNA.df_LatLon.csv")
All8.NoNA.df_LatLon
#######################################################################################################################
### Visualize LAI variation across all 8 wtrshds
ggplot(All8.NoNA.df_LatLon, aes(x=jday, y=mean, col=as.factor(round(Lat,2))))+geom_smooth(method="gam", se=FALSE)+
  labs(color="Lat")+scale_color_viridis_d()+xlab("Julian day")+ylab("Average LAI")
ggsave("LAI_Jday_8WtrshdLats.png", path="Figures/", device="png",width = 9, height = 6.5, dpi=900)

ggplot(All8.NoNA.df, aes(x=jday, y=mean, col=as.factor(HUC_8)))+geom_smooth(method="gam")+
  theme(legend.title=element_blank())

### Group by HUC_8
All8.NoNA.df_LatLon%>%group_by(HUC_8)%>%summarise(
  maxMeanLAI=mean(max),
  Lat=max(Lat))%>%
  ggplot(.,aes(Lat, maxMeanLAI))+geom_point()+geom_smooth(method="glm")+
  xlab("Watershed latitude")+ylab("Peak annual LAI")+
  stat_cor(label.y = 5.6, size=4.5)
ggsave("WtrshdLat_AnnPeakLAI.png", path="Figures/", device="png",width = 9, height = 6.5, dpi=900)

### Group by Lat and Year
All8.HUC_LatLon=All8.NoNA.df_LatLon%>%group_by(Lat,year)%>%summarise(
                meanLAI=mean(max),
                varLAI=var(max),
                covarLAI=(varLAI/meanLAI))

All8.HUC_LatLon=All8.NoNA.df_LatLon%>%group_by(HUC_8,Lat)%>%summarise(
                meanLAI=mean(max),
                varLAI=var(max),
                covarLAI=(varLAI/meanLAI))
All8.HUC_LatLon
All8.HUC_LatLon%>%
  ggplot(.,aes(as.factor(round(Lat,2)),covarLAI))+geom_boxplot(outlier.colour = "NA")+geom_point(aes(col=as.factor(year)))+
  geom_smooth(method = "lm", lty=2, aes(group=1))+xlab("Watershed latitude")+ylab("Coefficient of variation max-LAI")+scale_color_viridis_d()+
  theme(legend.title = element_blank())

### A final join of extracted LAI data with FIA data (from 1.0)
HUC8_LAI_FIA.jn=left_join(Prpn.HardSoft.BA_All8HUC, All8.HUC_LatLon, by="HUC_8")
HUC8_LAI_FIA.jn

write_csv(HUC8_LAI_FIA.jn, "ProcessedData/HUC8_LAI_FIA.jn.csv")
