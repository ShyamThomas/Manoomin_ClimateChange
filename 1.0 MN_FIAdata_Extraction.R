library(tidyverse)
library(sf)
library(ggpubr)

MN_tree=read_csv("RawData/MN_TREE.csv")
MN_cond=read_csv("RawData/MN_COND.csv")
MN_plot=read_csv("RawData/MN_PLOT.csv")

MN_tree
MN_tree_Yr2000=MN_tree%>% filter(INVYR>1999)
MN_tree_Yr2000
MN_tree_Yr2000.red=MN_tree_Yr2000[,c(1:21)]
MN_tree_Yr2000.red ## Final FIA tree data reduced and year 2000 onwards only 

MN_cond
MN_cond.tree_join=left_join(MN_tree_Yr2000.red, MN_cond, by=c("PLT_CN", "CONDID", "PLOT", "INVYR"))
MN_cond.tree_join

MN_plot
MN_plot_sf=st_as_sf(MN_plot, coords=c("LON", "LAT"))
MN_plot_sf=st_set_crs(MN_plot_sf, 4326)
MN_plot_sf

########## Extract MN FIA plots for buffers around  all HUC8 centroids
### First, read HUC8 watershed centroids shapefile and make buffers around them
HUC8_MN_centr.shp=st_read("GIS_Data/HUC8_MN_centr.shp")
Sel_HUC_08_wtrshds_centr_buff18k2=st_buffer(HUC8_MN_centr.shp,endCapStyle='SQUARE', 18000)
Sel_HUC_08_wtrshds_centr_buff18k=st_transform(Sel_HUC_08_wtrshds_centr_buff18k2, crs=4326)
Sel_HUC_08_wtrshds_centr_buff18k
st_write(Sel_HUC_08_wtrshds_centr_buff18k2,"GIS_data/MN_HUC08_wtrshds_buff18k.shp")

### join plots to the buffers
Plots_In_MN.RiceSites_18k.buff=st_join(MN_plot_sf,Sel_HUC_08_wtrshds_centr_buff18k , join=st_within)%>%
 filter(HUC8 != "NA")
Plots_In_MN.RiceSites_18k.buff
ggplot(Sel_HUC_08_wtrshds_centr_buff18k)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_18k.buff, aes(col=as.factor(INVYR)))
### remove plots prior to year 2000
Plots_In_MN.RiceSites_18k.buff_2K.yr=Plots_In_MN.RiceSites_18k.buff%>%filter(INVYR>1999)
ggplot(Sel_HUC_08_wtrshds_centr_buff18k)+geom_sf()+
  geom_sf(data = Plots_In_MN.RiceSites_18k.buff_2K.yr, alpha=0.3)+facet_wrap(~INVYR)

## create coordinate attributes
Plots_In_MN.RiceSites_18k.buff_2K.yr.coords = Plots_In_MN.RiceSites_18k.buff_2K.yr %>%
  mutate(Lon = unlist(map(Plots_In_MN.RiceSites_18k.buff_2K.yr$geometry,1)),
         Lat = unlist(map(Plots_In_MN.RiceSites_18k.buff_2K.yr$geometry,2)))
Plots_In_MN.RiceSites_18k.buff_2K.yr.coords

Plots_In_MN.RiceSites_18k.buff_2K.yr_data=st_drop_geometry(Plots_In_MN.RiceSites_18k.buff_2K.yr.coords)
Plots_In_MN.RiceSites_18k.buff_2K.yr_data
cond.tree_18kBuff2kyr.join=left_join(Plots_In_MN.RiceSites_18k.buff_2K.yr_data,MN_cond.tree_join, 
                                     by=c("PLOT", "INVYR"))
cond.tree_18kBuff2kyr.join ## the final merged data showing FIA tree data within each buffer for each year
length(unique(cond.tree_18kBuff2kyr.join$Lat))
length(unique(cond.tree_18kBuff2kyr.join$PLOT)) ###  some discrepancy between plots and lat/lon

write_csv(cond.tree_18kBuff2kyr.join ,"ProcessedData/EcoRgns_US_L3CODE.50_HUC08_Wtrshds_w18KBuff_TreeData.Join.csv")
cond.tree_18kBuff2kyr.join=read_csv("ProcessedData/EcoRgns_US_L3CODE.50_HUC08_Wtrshds_w18KBuff_TreeData.Join.csv")

cond.tree_18kBuff2kyr.join.sf=st_as_sf(cond.tree_18kBuff2kyr.join, coords =c("Lon", "Lat"), crs=4326)
cond.tree_18kBuff2kyr.join.sf

ggplot(Sel_HUC_08_wtrshds_centr_buff18k)+geom_sf()+
  geom_sf(data = cond.tree_18kBuff2kyr.join.sf, alpha=0.5)+facet_wrap(~INVYR)
#######################################################################################################################
### Visualize Hardwood Softwood composition in FIA plots extracted from selected wtrshd buffers

#### Proportion of hard and softwood based on total count
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

#################################### 
#### Proportion of hard and softwood based on DBH (diameter at breast height)
BA.HardSoft_All8HUC=cond.tree_18kBuff2kyr.join %>%mutate(WoodType=case_when(SPGRPCD < 25 ~ 'Softwood', SPGRPCD > 24 ~ 'Hardwood'))%>%
  dplyr::select(2:10,WoodType,DIA,HUC8)%>%filter(!is.na(DIA))%>%group_by(HUC8,WoodType)%>%summarise(
    meanBA=mean(DIA),
    maxBA=max(DIA),
    varBA=sd(DIA),
    totBA=sum(DIA))

totBA.HUC_8=cond.tree_18kBuff2kyr.join%>%filter(!is.na(DIA))%>%group_by(HUC8)%>%summarise(
  grnd.totBA=sum(DIA)
)

BA.Hard_All8HUC=BA.HardSoft_All8HUC%>%filter(WoodType=="Hardwood")
BA.Soft_All8HUC=BA.HardSoft_All8HUC%>%filter(WoodType=="Softwood")
BA.Hard_All8HUC
BA.Soft_All8HUC

Prpn.HardSoft.BA_All8HUC=left_join(BA.Soft_All8HUC, totBA.HUC_8, by="HUC8")%>%
        mutate(Prpn.SoftBA=totBA/grnd.totBA,Prpn.HardBA=1-Prpn.SoftBA)
Prpn.HardSoft.BA_All8HUC

#Prpn.HardSoft.BA_All8HUC$HUC8=as.numeric(Prpn.HardSoft.BA_All8HUC$HUC8)

### A final join of extracted FIA data with LAI data (from 2.0)
MN_HUC8.WTRSHDS=read_csv("ProcessedData/All.MN_HUC8.wtrshds.csv")
MN_HUC8.WTRSHDS
Prpn.HardSoft.BA_All8HUC$HUC8=as.numeric(Prpn.HardSoft.BA_All8HUC$HUC8)

HUC8_FIA.jn=left_join(Prpn.HardSoft.BA_All8HUC, MN_HUC8.WTRSHDS, by="HUC8")
HUC8_FIA.jn
write_csv(HUC8_FIA.jn,"ProcessedData/MN_wtrshds.FIA.jn.csv") 

HUC8_FIA.jn%>%ggplot(., aes(Lat, Prpn.SoftBA))+geom_point()+geom_smooth(method="lm")+stat_cor()+
xlab("Latitude")+ylab("Proportion of softwood basal area")

HUC8_FIA.jn%>%ggplot(., aes(Prpn.SoftBA, meanBA))+geom_point()+geom_smooth(method="lm")+stat_cor()+
  xlab("Proportion of softwood basal area")+ylab("Average basal area")



######################################################################################################################
######################################################################################################################




################
st=c("WI","MI")
WI_MI_HUC8_LAI_FIA.jn=rbind(WI_HUC8_LAI_FIA.jn[,-10], MI_HUC8_LAI_FIA.jn[,-10])%>%ungroup()%>%mutate(State=rep(st,each=6))
MN_HUC8_LAI.jn=MN_HUC8_LAI_FIA.jn[,c(1:10)]%>%rename(.,HUC8=HUC_8)%>%ungroup()%>%mutate(State=rep("MN",8))

WI_MI_MN_HUC8_LAI_FIA.jn=rbind(MN_HUC8_LAI.jn,WI_MI_HUC8_LAI_FIA.jn)
WI_MI_MN_HUC8_LAI_FIA.jn
WI_MI_MN_HUC8_LAI_FIA.jn%>%ggplot(., aes(Lat,Prpn.SoftBA, col=State))+geom_point()+geom_smooth(method="glm")+stat_cor()

