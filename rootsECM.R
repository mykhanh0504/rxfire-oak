library(tidyverse)
library(dplyr)
library(ggplot2)
library(gt)
library(car)
library(multcomp)
library(rstatix)
library(kableExtra)
library(ggpubr)

ecm <- read_csv("rootsECM.csv")

#Add Treatment & Pair columns
ecm <- ecm %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

ecm <- ecm %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))

ecm <- ecm %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","3/1")~"2",
  Unit %in% c("28/2","12/2")~"3",
  Unit=="Unnamed"~"4"))

ecm <- ecm %>% 
  mutate(Unit=as.factor(Unit),
         Disturbance=as.factor(Disturbance),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair))

ecm <- ecm %>% mutate(pct_colonized=colonized_tips/(colonized_tips+uncolonized_tips)*100)

ecm <- ecm %>% filter(Pair %in% c("1","2","3"))

ecmb <- ecm %>% filter(Disturbance=="B")
stats_ecmb <- get_summary_stats(ecmb)
stats_ecmb <- stats_ecmb %>% mutate(Disturbance="B")
#stats_ecmb <- stats_ecmb %>% 
  #select(Disturbance,variable,min,max,median,mean,sd,se) %>% 
  #filter(variable %in% c("Age","Height_cm","Extension_growth_cm","DRC_mm",
                         #"nleaves","nlive_branches","ndead_branches",
                         #"Herbivory_pct","Pathogen_damage_pct","pct_colonized"))

ecmc <- ecm %>% filter(Disturbance=="C")
stats_ecmc <- get_summary_stats(ecmc)
stats_ecmc <- stats_ecmc %>% mutate(Disturbance="C")
#stats_ecmc <- stats_ecmc %>% 
  #select(Disturbance,variable,min,max,median,mean,sd,se) %>% 
  #filter(variable %in% c("Age","Height_cm","Extension_growth_cm","DRC_mm",
                         #"nleaves","nlive_branches","ndead_branches",
                         #"Herbivory_pct","Pathogen_damage_pct","pct_colonized"))

stats_ecm <- stats_ecmb %>% full_join(stats_ecmc)
stats_ecm %>% kable(caption="Summarized statistics of 
                     Q. rubra seedling measurements in 2024.") %>% 
  kable_styling(html_font="Arial",
                bootstrap_options = c("striped", "hover","condensed", "responsive"))

ecm %>% 
  ggplot(aes(x=Pair,y=pct_colonized,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("% colonized")+
  labs(fill="Pairs")

aov_ecm <- aov(pct_colonized~Disturbance+Pair,data=ecm)
summary(aov_ecm)

ecm %>% 
  ggplot(aes(x=Height_cm,y=pct_colonized))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_point(aes(color=Disturbance))+
  geom_smooth(method='lm')+
  scale_fill_manual(values=c(p6,bc))+
  facet_wrap(vars(Pair),scales='free')+
  xlab("Height")+
  ylab("pct colonized")

ecm2 <- ecm %>% pivot_longer(cols=-c(Location,Unit,Disturbance,
                                     Tag,Treatment,Pair,pct_colonized,
                                     colonized_tips,uncolonized_tips,
                                     nleaves,nlive_branches,Herbivory_pct,
                                     DRC_mm,ndead_branches,Age),
                             names_to='Variable',values_to='Value')

ecm2 %>% ggplot(aes(x=Value,y=pct_colonized))+
  geom_point()+
  facet_grid(rows=vars(Disturbance),cols=vars(Variable),scales='free_x')+
  geom_smooth(method='lm')+                 
  stat_cor(method="pearson",label.x=1,label.y=1)

am <- read_csv("rootsAM.csv")
