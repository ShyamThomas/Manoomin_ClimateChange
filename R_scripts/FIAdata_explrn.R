
library(sf)
library(tidyverse) 
library(raster)

### All the FIA databases needed:
MN_plot=read_csv("RawData/MN_PLOT.csv")
MN_tree=read_csv("RawData/MN_TREE.csv")
MN_cond=read_csv("RawData/MN_COND.csv")

### All the shapefiles needed:
US_shp=st_read("GIS_Data/gadm41_USA_shp/gadm41_USA_1.shp")
MN_wtrshds=st_read("GIS_Data/MN_dnr_watersheds/dnr_watersheds_dnr_level_04_huc_08_majors.shp")

### Finally, the wild rice sampling sites
RiceFieldSites=read_csv("GIS_Data/RiceFieldSites.csv")

##############################################################################################################
### Plot a map showing all the Wild Rice sampling sites and FIA plots for MN
### First the state shapefiles
US_shp
MN_shp=US_shp%>%filter(NAME_1=="Minnesota")
plot(MN_shp$geometry, axes=TRUE)
MN_sf=st_as_sf(MN_shp, crs=4326)
MN_sf
### Both MN & WI
MN_WI_shp=US_shp%>%filter(NAME_1=="Minnesota" | NAME_1=="Wisconsin")
MN_WI_shp
plot(MN_WI_shp$geometry, axes= TRUE)

### The MN FIA Plots data from FIA.db
MN_plot
MN_plot_sf=st_as_sf(MN_plot, coords=c("LON", "LAT"), crs=4326)
MN_unq.plot_sf=MN_plot_sf%>%distinct(CN, .keep_all = TRUE)
MN_unq.plot_sf
plot(MN_unq.plot_sf$geometry)

### The Rice sampling sites
RiceFieldSites 
RiceFieldSites_sf=st_as_sf(RiceFieldSites, coords = c("Lon","Lat"), crs=4326)
RiceFieldSites_sf
plot(RiceFieldSites_sf$geometry, col="red", axes=TRUE)

### Map of MN plots and rice sampling sites
ggplot(MN_WI_shp)+geom_sf()+
  geom_sf(data = MN_unq.plot_sf, alpha=0.1)+
  geom_sf(data=RiceFieldSites_sf, col="green")

### Minnesota HUC 8 watersheds
MN_wtrshds
MN_wtrshds_sf=st_transform(MN_wtrshds, 4326)
MN_wtrshds_sf

sf::sf_use_s2(FALSE)

RiceSites_Wtrshd_join=st_join(MN_wtrshds_sf,RiceFieldSites_sf, join=st_contains)
RiceSites_Wtrshd_join ## all wtrshd polygons with a subset containing rice sampling sites

Wtrsh_With_RiceSites=RiceSites_Wtrshd_join%>%filter(Name !="NA")
Wtrsh_With_RiceSites ### only wtrshd polygons with rice sites
BigRice_Wtrshd=Wtrsh_With_RiceSites%>%filter(Name== "Big rice lake")
BigRice_Wtrshd
plot(BigRice_Wtrshd$geometry)
Plots_OnlyIn_BigRiceWtrshd=st_join(MN_plot_sf, BigRice_Wtrshd, join=st_within)%>%filter(Name != "NA")
Plots_OnlyIn_BigRiceWtrshd

ggplot(BigRice_Wtrshd)+geom_sf()+
geom_sf(data = Plots_OnlyIn_BigRiceWtrshd, aes(col=as.factor(INVYR)))

Plots_OnlyIn_BigRiceWtrshd_2K.yr=Plots_OnlyIn_BigRiceWtrshd%>%filter(INVYR>1999)
Plots_OnlyIn_BigRiceWtrshd_2K.yr_coords <- Plots_OnlyIn_BigRiceWtrshd_2K.yr %>%
       mutate(Lon = unlist(map(Plots_OnlyIn_BigRiceWtrshd_2K.yr$geometry,1)),
              Lat = unlist(map(Plots_OnlyIn_BigRiceWtrshd_2K.yr$geometry,2)))
Plots_OnlyIn_BigRiceWtrshd_2K.yr_data=st_drop_geometry(Plots_OnlyIn_BigRiceWtrshd_2K.yr_coords)

### Maps of Big Rice wtrshd showing distrbution FIA plots
ggplot(BigRice_Wtrshd)+geom_sf()+
  geom_sf(data = Plots_OnlyIn_BigRiceWtrshd_2K.yr, aes(col=as.factor(INVYR)))

ggplot(BigRice_Wtrshd)+geom_sf()+
geom_sf(data = Plots_OnlyIn_BigRiceWtrshd_2K.yr, alpha=0.5)+facet_wrap(~INVYR)

##############################################################################################################
#### Now get the Tree data, first reduce the data to a minimal easy to work size
MN_tree
MN_tree_Yr2000=MN_tree%>% filter(INVYR>1999)
MN_tree_Yr2000
MN_tree_Yr2000.red=MN_tree_Yr2000[,c(1:21)]
MN_tree_Yr2000.red 

### Join Condition data to Tree datafile
MN_cond
MN_cond.tree_join=left_join(MN_tree_Yr2000.red, MN_cond, by=c("PLT_CN", "CONDID", "PLOT", "INVYR"))
MN_cond.tree_join

cond.tree_BigRiceplot.join=left_join(Plots_OnlyIn_BigRiceWtrshd_2K.yr_data,MN_cond.tree_join, by=c("PLOT", "INVYR"))
cond.tree_BigRiceplot.join
length(unique(cond.tree_BigRiceplot.join$PLOT))
length(unique(cond.tree_BigRiceplot.join$Lat)) ### there is a discrepency between plots and lat/lon
### plots that have 2 lat lon attributed to them
Plots_wMultiLatLon=cond.tree_BigRiceplot.join%>%group_by(PLOT)%>%tally(n_distinct(Lat))%>%filter(n==2)%>%pull(PLOT)
### trying to visualize where these plots are
Plots_wMultiLatLon.df=cond.tree_BigRiceplot.join%>%filter(PLOT %in% Plots_wMultiLatLon)
Plots_wMultiLatLon.df
Plots_wMultiLatLon.sf=st_as_sf(Plots_wMultiLatLon.df, coords = c("Lon", "Lat"), crs=4326)

ggplot(BigRice_Wtrshd)+geom_sf()+
geom_sf(data = Plots_wMultiLatLon.sf, alpha=0.5)+
geom_label(data = Plots_wMultiLatLon.df, aes(Lon, Lat, label = PLOT), fontface = "bold", size=4)

### the final distribution of plots in the wtrshd
cond.tree_BigRiceplot.sf=st_as_sf(cond.tree_BigRiceplot.join, coords = c("Lon", "Lat"), crs=4326)
cond.tree_BigRiceplot.sf
ggplot(BigRice_Wtrshd)+geom_sf()+
  geom_sf(data = cond.tree_BigRiceplot.sf, alpha=0.5)+facet_wrap(~INVYR)

### Saving the merged data
write_csv(cond.tree_BigRiceplot.join ,"ProcessedData/BigRice_TreeData.Join.csv")

### Find the 5 most common species based on plot-level occurrence
CommonSp=cond.tree_BigRiceplot.join%>%group_by(SPCD)%>%tally(n_distinct(PLOT))%>%top_n(5, wt=n)%>%pull(SPCD)
CommonSp

### Species composition plot level
BigRice_wtrshd_plot.spcomp=cond.tree_BigRiceplot.join%>%group_by(PLOT, Lon, Lat, INVYR, SPCD)%>%tally()
BigRice_wtrshd_plot.spcomp

BigRice_wtrshd_plot.Comm.SpComp=BigRice_wtrshd_plot.spcomp%>%filter(SPCD %in% CommonSp)
BigRice_wtrshd_plot.Comm.SpComp
BigRice_wtrshd_plot.Comm.SpComp_sf=st_as_sf(BigRice_wtrshd_plot.Comm.SpComp, coords = c("Lon","Lat"), crs=4326)
BigRice_wtrshd_plot.Comm.SpComp_sf

ggplot(BigRice_Wtrshd)+geom_sf()+
geom_sf(data = BigRice_wtrshd_plot.Comm.SpComp_sf, aes( col=n))+facet_wrap(~SPCD)+scale_colour_viridis_c()

### Species composition as basal area at plot level
BigRice_wtrshd_plot.sp.BA=cond.tree_BigRiceplot.join%>%group_by(PLOT, Lon, Lat, SPCD)%>%summarise(totBasArea=sum(DIA, na.rm = TRUE))
BigRice_wtrshd_plot.sp.BA

BigRice_wtrshd_plot.Comm.SpBA=BigRice_wtrshd_plot.sp.BA%>%filter(SPCD %in% CommonSp)
BigRice_wtrshd_plot.Comm.SpBA
BigRice_wtrshd_plot.Comm.SpBA_sf=st_as_sf(BigRice_wtrshd_plot.Comm.SpBA, coords = c("Lon","Lat"), crs=4326)
BigRice_wtrshd_plot.Comm.SpBA_sf

ggplot(BigRice_Wtrshd)+geom_sf()+
  geom_sf(data = BigRice_wtrshd_plot.Comm.SpBA_sf, aes( col=totBasArea))+facet_wrap(~SPCD)+scale_colour_viridis_c()

### Species composition plot and year level
Cycle1=c(2000, 2005,2010, 2015) ## selecting a repeat sampling cycle
Cycle1

BigRice_wtrshd_plot.year.spcomp=cond.tree_BigRiceplot.join%>%group_by(INVYR, PLOT, Lon, Lat, SPCD)%>%tally()
BigRice_wtrshd_plot.year.spcomp


BigRice_wtrshd_plot.year.Comm.SpComp=BigRice_wtrshd_plot.year.spcomp%>%filter(SPCD == 12 & INVYR %in% Cycle1)
BigRice_wtrshd_plot.year.Comm.SpComp
BigRice_wtrshd_plot.year.Comm.SpComp_sf=st_as_sf(BigRice_wtrshd_plot.year.Comm.SpComp, coords = c("Lon","Lat"), crs=4326)
BigRice_wtrshd_plot.year.Comm.SpComp_sf

ggplot(BigRice_Wtrshd)+geom_sf()+
  geom_sf(data = BigRice_wtrshd_plot.year.Comm.SpComp_sf, aes( col=n))+facet_wrap(~SPCD+INVYR)+scale_colour_viridis_c()

### A look art species richness
MN_plots_sprich=MN_tree_Yr2000%>%group_by(INVYR)%>%summarise(
SpRichness=n_distinct(SPCD),
TotPlots=n_distinct(CN)
)
MN_sp.comp=MN_tree_Yr2000%>%group_by(INVYR, CN, SPCD)%>%tally()
MN_sp.comp

##########################################################################################
### Extracts and maps wtrshds with rice sites
#Wtrshd_RiceSites_join=st_join(RiceFieldSites_sf,wtrshds_sf, join=st_within)
#Wtrshd_RiceSites_join
#rice.sites_clip=Wtrshd_RiceSites_join%>%filter(HUC_8!="NA")
#rice.sites_clip

#MN_shp=US_shp%>%filter(NAME_1=="Minnesota")

#plot(MN_shp$geometry, axes=TRUE)
#plot(Wtrsh_With_RiceSites$geometry, add=TRUE, col="gray")
#plot(rice.sites_clip$geometry, add=TRUE, col="red", pch=16)

###Merge first MN_tree to MN_cond by PLT_CN

##########################################################################################
### Estimating change in softwood / hardwood cover and basal area across the watershed
BigRice_plots.WoodType.BA=cond.tree_BigRiceplot.join%>%mutate(WoodType=case_when(SPGRPCD<25 ~ 'Softwood', SPGRPCD > 24 ~ 'Hardwood'))%>%
group_by(INVYR, WoodType)%>%summarise(totBasArea=sum(DIA, na.rm = TRUE))%>%filter(!is.na(WoodType))
BigRice_plots.WoodType.BA
basal.area_ByWoodYear=ggplot(BigRice_plots.WoodType.BA, aes(INVYR,totBasArea, col=WoodType))+geom_point()+geom_line(lty=2)+
  ylab("Total basal area")+xlab("")


Prpn.HardSoft.Tree=cond.tree_BigRiceplot.join%>%filter(!is.na(SPCD))%>%group_by(INVYR)%>%summarise(
    total=n(),
    prpn.totalHard=sum(SPGRPCD>24 & SPGRPCD < 51)/total,
    prpn.totalSoft=sum(SPGRPCD<25)/total,
    prpn.totOther=sum(SPGRPCD > 50)/total)

Prpn.HardSoft.Tree
Prpn.HardSoft.Tree_lon =Prpn.HardSoft.Tree%>%pivot_longer(!c(INVYR,total,prpn.totOther),names_to = "WoodType", values_to = "n_prpn")
Prpn.HardSoft.Tree_lon
prpn_ByWoodYear=ggplot(Prpn.HardSoft.Tree_lon, aes(INVYR,n_prpn, col=WoodType))+geom_point()+geom_line(lty=2)+ylab("Proprtion")+xlab("")

prpn_ByWoodYear+geom_smooth(method="glm")
ggsave("FIA_BigRice_ChangeInMajorCover.png", path="Figures/", device="png",width = 9, height = 6)
basal.area_ByWoodYear+geom_smooth(method="glm")
ggsave("FIA_BigRice_ChangeInMajorCover_basalarea.png", path="Figures/", device="png",width = 9, height = 6)


