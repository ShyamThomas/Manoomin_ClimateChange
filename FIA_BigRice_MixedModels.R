library(tidyverse)
library(lme4)

Cond.tree_BigRiceplot.join=read_csv("ProcessedData/BigRice_TreeData.Join.csv")
TreeDia_BigRice=cond.tree_BigRiceplot.join%>%mutate(WoodType=case_when(SPGRPCD < 25 ~ 'Softwood', SPGRPCD > 24 ~ 'Hardwood'))%>%
dplyr::select(2:10,WoodType,DIA)%>%mutate(
Cycle=case_when(
INVYR==2000 |INVYR==2005|INVYR==2010|INVYR==2015 ~'1',
INVYR==2001 |INVYR==2006 |INVYR==2011|INVYR==2016 ~'2',
INVYR==2002|INVYR==2007|INVYR==2012|INVYR==2017 ~'3',
INVYR==2003|INVYR==2008|INVYR==2013|INVYR==2018 ~'4',
INVYR==2004|INVYR==2009|INVYR==2014|INVYR==2019 ~'5'))%>%filter(!is.na(DIA))

TreeDia_BigRice
dia.yr_rand.int.mod=lmer(DIA~INVYR+(1|INVYR), data=TreeDia_BigRice)
TreeDia_BigRice$randInt.fit=predict(dia.yr_rand.int.mod)
p1=ggplot(TreeDia_BigRice, aes(INVYR,randInt.fit, col=WoodType))+geom_smooth(method="lm")

dia.yr_rand.slopCycles.mod=lmer(DIA~INVYR+(1+INVYR|Cycle), data=TreeDia_BigRice)
TreeDia_BigRice$randIntSlop.fit=predict(dia.yr_rand.slopCycles.mod)
p2=ggplot(TreeDia_BigRice, aes(INVYR,randIntSlop.fit, col=Cycle, lty=WoodType))+geom_smooth(method="lm")

dia.yr_rand.slopWoodCycles.mod=lmer(DIA~INVYR+(1+INVYR|WoodType:Cycle), data=TreeDia_BigRice)
TreeDia_BigRice$randIntSlop.fit2=predict(dia.yr_rand.slopWoodCycles.mod)
p3=ggplot(TreeDia_BigRice, aes(INVYR,randIntSlop.fit2, col=Cycle, lty=WoodType))+geom_smooth(method="lm")

anova(dia.yr_rand.int.mod,dia.yr_rand.slopCycles.mod,dia.yr_rand.slopWoodCycles.mod)
p3+xlab('Year')+ylab('Basal area')+ylim(c(5.0,7.0)) ### the best model, as per anova results

TreeDia_BigRice%>%group_by(WoodType, INVYR)%>%summarise(
mean.pred.wood=mean(randIntSlop.fit2))%>%ggplot(.,aes(INVYR,mean.pred.wood, col=WoodType))+geom_line(lty=2)+geom_point()+geom_smooth(method = "glm")+xlab("Year")+ylab("Mean basal area")+ylim(c(5,7))
ggsave("BasalArea_LMER.fullmodel.preds_avg.png", path="Figures/", device="png",width = 9, height = 6, dpi=900)

TreeDia_BigRice%>%group_by(WoodType, INVYR)%>%summarise(
sum.pred.wood=sum(randIntSlop.fit2))%>%ggplot(.,aes(INVYR,sum.pred.wood, col=WoodType))+geom_line(lty=2)+geom_smooth(method = "lm")+ylim(c(5000,15000))+ylab("Total basal area")+xlab("Year")
ggsave("BasalArea_LMER.fullmodel.preds_sum.png", path="Figures/", device="png",width = 9, height = 6, dpi=900)


