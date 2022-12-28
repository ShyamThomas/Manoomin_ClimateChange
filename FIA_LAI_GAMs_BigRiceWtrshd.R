library(tidyverse)
library(mgcv)

Cond.tree_BigRiceplot.join=read_csv("ProcessedData/BigRice_TreeData.Join.csv")
BigRice_LAI=read_csv("RawData/BigRice_statistics_Lai_500m.csv")

### Going back to extracting proportions of softwood and hardwood in the watershed
Prpn.HardSoft.Tree=cond.tree_BigRiceplot.join%>%filter(!is.na(SPCD))%>%group_by(INVYR)%>%summarise(
  total=n(),
  prpn.totalHard=sum(SPGRPCD>24 & SPGRPCD < 51)/total,
  prpn.totalSoft=sum(SPGRPCD<25)/total,
  prpn.totOther=sum(SPGRPCD > 50)/total)
Prpn.HardSoft.Tree ## the final data showing the needed proportions over years
Prpn.HardSoft.Tree_lon =Prpn.HardSoft.Tree%>%pivot_longer(!c(INVYR,total,prpn.totOther),names_to = "WoodType", values_to = "n_prpn")
Prpn.HardSoft.Tree_lon
Prpn.Soft=Prpn.HardSoft.Tree_lon%>%filter(WoodType=="prpn.totalSoft")%>%pull(n_prpn)
Prpn.Soft

BigRice_LAI_v2=BigRice_LAI%>%mutate(year= year(date), month=month(date), jday=yday(date))
BigRice_LAI_noNA=BigRice_LAI_v2%>%filter(!is.na(mean)) ### the final LAI data

BigRice_mean.lai=ggplot(BigRice_LAI_noNA, aes(x=jday, y=mean, col=as.factor(year)))+geom_point()+geom_smooth(method="gam")+
  theme(legend.title=element_blank())

BigRice_max.lai=ggplot(BigRice_LAI_noNA, aes(x=jday, y=max, col=as.factor(year)))+geom_point()+geom_smooth(method="gam")+
        theme(legend.title=element_blank())

maxLAI.Jday_GAM=gam(max ~ s(jday) +s(year), data=BigRice_LAI_noNA)
summary(maxLAI.Jday_GAM)
plot(maxLAI.Jday_GAM)
resid_maxLAI.GAM= residuals.gam(maxLAI.Jday_GAM)
BigRice_LAI_noNA$maxLAI.GAM_resid=resid_maxLAI.GAM

### A quick residual analysis
BigRice_LAI_noNA%>%group_by(year)%>%summarise(
  medianAnnMax_LAI.GAM.resid=median(maxLAI.GAM_resid)
)%>%ggplot(., aes(year,medianAnnMax_LAI.GAM.resid))+geom_point()+geom_smooth(method = "glm")

### Repeat the same as above but with mean LAI as response
meanLAI.Jday_GAM=gam(mean ~ s(jday) +s(year), data=BigRice_LAI_noNA)
summary(meanLAI.Jday_GAM)
plot(meanLAI.Jday_GAM)
resid_meanLAI.GAM= residuals.gam(meanLAI.Jday_GAM)
BigRice_LAI_noNA$meanLAI.GAM_resid=resid_meanLAI.GAM
BigRice_LAI_noNA

### A quick residual analysis again
BigRice_LAI_noNA%>%group_by(year)%>%summarise(
  medianAnnMean_LAI.GAM.resid=median(meanLAI.GAM_resid)
)%>%ggplot(., aes(year,medianAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method = "glm")

### Testing residuals of GAMs with proportion of softwood to check for any potential realtion
### First we need proprtion of softwood
Prpn.Soft=Prpn.HardSoft.Tree_lon%>%filter(WoodType=="prpn.totalSoft")%>%pull(n_prpn)
Prpn.Soft
### Pull GAM residuals for max LAI 
medianAnnMax_LAI.GAM.resid=BigRice_LAI_noNA%>%group_by(year)%>%summarise(
                                medianAnnMax_LAI.GAM.resid=median(maxLAI.GAM_resid))%>%
                                pull(medianAnnMax_LAI.GAM.resid)
meanAnnMax_LAI.GAM.resid=BigRice_LAI_noNA%>%filter(year>2000)%>%group_by(year)%>%summarise(
                                meanAnnMax_LAI.GAM.resid=mean(maxLAI.GAM_resid))%>%
                                pull(meanAnnMax_LAI.GAM.resid)

### Pull GAM residuals for mean LAI 
medianAnnMean_LAI.GAM.resid=BigRice_LAI_noNA%>%filter(year>2000)%>%group_by(year)%>%summarise(
                                    medianAnnMean_LAI.GAM.resid=median(meanLAI.GAM_resid))%>%
                                    pull(medianAnnMean_LAI.GAM.resid)

meanAnnMean_LAI.GAM.resid=BigRice_LAI_noNA%>%filter(year>2000)%>%group_by(year)%>%summarise(
                              meanAnnMean_LAI.GAM.resid=mean(meanLAI.GAM_resid))%>%
                              pull(meanAnnMean_LAI.GAM.resid)

### Combine all the pulled data into a df
residual_prpnSoft.df=as.data.frame(cbind(Prpn.Soft,medianAnnMean_LAI.GAM.resid,meanAnnMean_LAI.GAM.resid,medianAnnMax_LAI.GAM.resid,meanAnnMax_LAI.GAM.resid))
residual_prpnSoft.df

residual_prpnSoft.df%>%ggplot(., aes(x=Prpn.Soft,medianAnnMax_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")
residual_prpnSoft.df%>%ggplot(., aes(x=Prpn.Soft,meanAnnMax_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")

residual_prpnSoft.df%>%ggplot(., aes(x=Prpn.Soft,meanAnnMean_LAI.GAM.resid))+geom_point()+geom_smooth(method="glm")

Cycle_1=c(2000, 2005,2010,2015)
Cycle_2=c(2001, 2006,2011,2016)

BigRice_LAI_Cycle2=BigRice_LAI_v2%>%filter(year %in% Cycle_2)
BigRice_LAI_Cycle2
ggplot(BigRice_LAI_Cycle2, aes(x=jday, y=max, col=as.factor(year)))+geom_point()+geom_smooth(method="gam")+
      theme(legend.title=element_blank())

LAI.Jday_GAM=gam(mean ~ s(jday), data=BigRice_LAI_Cycle1)
GAM.resid= residuals.gam(LAI.Jday_GAM)
GAM.resid
BigRice_LAI_Cycle1_GAM=BigRice_LAI_Cycle1%>%filter(!is.na(mean))%>%mutate(GAM.resid=GAM.resid)
BigRice_LAI_Cycle1_GAM%>%ggplot(., aes(x=as.factor(year), y=GAM.resid))+geom_boxplot(outlier.alpha = 0.33)


