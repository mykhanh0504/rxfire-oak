library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(multcomp)
library(ggpubr)
library(ggstatsplot)
library(ggnewscale)

char <- read_csv("standchar.csv")

#Add Treatment & Pair columns
char2 <- char %>% mutate(Treatment=case_when(Unit %in% c("3/16","5/15","28/2","12/2") 
                                           ~ "Shelterwood",
                                           Unit %in% c("20/2","3/1") 
                                           ~ "Seedtree",
                                           Unit %in% c("44B","45C","46B","46C") 
                                           ~ "Clearcut"))

char2 <- char2 %>% replace_na(list(Unit="Unnamed",Treatment="Unmanaged"))

char2 <- char2 %>% mutate(Pair=case_when(Unit %in% c("3/16","5/15")~"1",
                                       Unit %in% c("20/2","3/1")~"2",
                                       Unit %in% c("28/2","12/2")~"3",
                                       Unit=="Unnamed"~"4",
                                       Unit %in% c("44B","45C")~"5",
                                       Unit %in% c("46B","46C")~"6"))

char2 <- char2 %>% 
  mutate(Location=as.factor(Location),
         Unit=as.factor(Unit),
         Disturbance=as.factor(Disturbance),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair),
         Microtopography=as.factor(Microtopography))

#summarize data
plotarea <- 0.00785398163397448
char2 <- char2 %>% na.omit() %>% 
  mutate(Density=OAKcount/plotarea)
char2$Density <- round(char2$Density,0)

mt <- c("STS"="#D60407","MDS"="#FF7909","SLS"="#EAEF04","FLT"="#02D68B","CVX"="#017DFF","CCV"="#6B0DCC")
p6 <- c("1"="#FBE3D6","2"="#FFFFCC","3"="#E8E8E8",
        "4"="#C2F1C8","5"="#DCEAF7","6"="#DCEAF7")

fig_mictop <- char2 %>% filter(Pair==c("1","2","3","4")) %>% 
  ggplot(aes(x=Location,y=Density,fill=Microtopography))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  scale_fill_manual(values=p6,name="Pairs")+
  new_scale_fill()+
  geom_boxplot(aes(fill=Microtopography),width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=mt,name="Microtopography")+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("Stem count (per ha)")

fig_mictop + stat_compare_means(aes(label=paste0("p=",after_stat(p.format))))

char2 %>% filter(Pair==c("1","2","3")) %>% 
  ggplot(aes(x=BareSoil_pct,y=Density))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_point(aes(color=Disturbance))+
  geom_smooth(method='lm')+
  scale_fill_manual(values=c(p6,bc))+
  facet_wrap(vars(Pair),scales='free')+
  xlab("Bare soil %")+
  ylab("Seedling density ha-1")
