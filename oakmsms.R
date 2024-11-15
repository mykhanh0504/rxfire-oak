library(tidyverse)
library(dplyr)
library(ggplot2)
library(gt)
library(car)
library(multcomp)
library(rstatix)

ms23 <- read_csv("oakmsms23.csv")

ms23 <- ms23 %>% mutate(Location=as.factor(Location)) %>% 
  mutate(Location=fct_recode(Location,
                             "CF"="Crawford Notch State Park",
                             "HOG"="Hogsback (Blueberry Mountain)",
                             "SB"="Stevens Brook (Plummers Ledge)"))

#Add Treatment & Pair columns
ms23x <- ms23 %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

ms23x <- ms23x %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))

ms23x <- ms23x %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","3/1")~"2",
  Unit %in% c("28/2","12/2")~"3",
  Unit=="Unnamed"~"4"))

ms23x <- ms23x %>% 
  mutate(Unit=as.factor(Unit),
         Disturbance=as.factor(Disturbance),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair))

#Repeat for ms24
ms24 <- read_csv("oakmsms24.csv")

ms24 <- ms24 %>% mutate(Location=as.factor(Location)) %>% 
  mutate(Location=fct_recode(Location,
                             "CF"="Crawford Notch State Park",
                             "HOG"="Hogsback (Blueberry Mountain)",
                             "SB"="Stevens Brook (Plummers Ledge)"))

ms24x <- ms24 %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

ms24x <- ms24x %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))

ms24x <- ms24x %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","3/1")~"2",
  Unit %in% c("28/2","12/2")~"3",
  Unit=="Unnamed"~"4"))

ms24x <- ms24x %>% 
  mutate(Unit=as.factor(Unit),
         Disturbance=as.factor(Disturbance),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair),
         Survey=as.factor(Survey))

ms24x <- ms24x %>% drop_na()

ms24x <- ms24x %>% mutate(herbivory_pct=na_if(herbivory_pct,"na")) %>% 
  mutate(Pathogen_damage_pct=na_if(Pathogen_damage_pct,"na"))

ms24x$nleaves <- replace(ms24x$nleaves,ms24x$nleaves=="100+","100") %>%
  as.numeric()

#Stats 2023
ms23b <- ms23x %>% filter(Disturbance=="B")
stats_ms23b <- get_summary_stats(ms23b)
stats_ms23b <- stats_ms23b %>% select(variable,min,max,median,mean,sd,se) %>% 
  filter(variable %in% c("Height_cm","DRC_mm",
                         "nlive_branches","ndead_branches"))
stats_ms23b

ms23c <- ms23x %>% filter(Disturbance=="C")
stats_ms23c <- get_summary_stats(ms23c)
stats_ms23c <- stats_ms23c %>% select(variable,min,max,median,mean,sd,se) %>% 
  filter(variable %in% c("Height_cm","DRC_mm",
                         "nlive_branches","ndead_branches"))
stats_ms23c

#DRC 2023
aov_drc23 <- aov(DRC_mm~Disturbance+Pair,data=ms23x)
summary(aov_drc23)

bc <- c("B"="#CC6677","C"="#88CCEE")
p6 <- c("1"="#FBE3D6","2"="#FFFFCC","3"="#E8E8E8",
        "4"="#C2F1C8","5"="#DCEAF7","6"="#DCEAF7")

ms23x %>% 
  ggplot(aes(x=Pair,y=DRC_mm,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("2023 diameter at root collar (mm)")+
  labs(fill="Pairs")

#stats 2024
ms24b <- ms24x %>% filter(Disturbance=="B")
stats_ms24b <- get_summary_stats(ms24b)
stats_ms24b <- stats_ms24b %>% select(variable,min,max,median,mean,sd,se) %>% 
  filter(variable %in% c("Height_cm","Extension_growth_cm","DRC_mm",
                         "nleaves","nlive_branches","ndead_branches"))
stats_ms24b

ms24c <- ms24x %>% filter(Disturbance=="C")
stats_ms24c <- get_summary_stats(ms24c)
stats_ms24c <- stats_ms24c %>% select(variable,min,max,median,mean,sd,se) %>% 
  filter(variable %in% c("Height_cm","Extension_growth_cm","DRC_mm",
                         "nleaves","nlive_branches","ndead_branches"))
stats_ms24c

#height 2024
aov_height24 <- aov(Height_cm~Disturbance+Pair+Survey,data=ms24x)
summary(aov_height24)

ms24x %>% 
  ggplot(aes(x=Pair,y=Height_cm,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("2024 height (cm)")+
  labs(fill="Pairs")

#EXTENSION GROWTH 2024
aov_exgr24 <- aov(Extension_growth_cm~Disturbance+Pair+Survey,data=ms24x)
summary(aov_exgr24)

ms24x %>% 
  ggplot(aes(x=Pair,y=Extension_growth_cm,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("2024 extension growth (cm)")+
  labs(fill="Pairs")

#DRC 2024
aov_drc24 <- aov(DRC_mm~Disturbance+Pair+Survey,data=ms24x)
summary(aov_drc24)

ms24x %>% 
  ggplot(aes(x=Pair,y=DRC_mm,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("2024 diameter at root collar (mm)")+
  labs(fill="Pairs")

#LIVE BRANCHES 2024
aov_lb24 <- aov(nlive_branches~Disturbance+Pair+Survey,data=ms24x)
summary(aov_lb24)

ms24x %>% filter(Survey=="1") %>% 
  ggplot(aes(x=Pair,y=nlive_branches,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("2024 number of live branches, survey 1")+
  labs(fill="Pairs")

ms24x %>% filter(Survey=="2") %>% 
  ggplot(aes(x=Pair,y=nlive_branches,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("2024 number of live branches, survey 2")+
  labs(fill="Pairs")

#DEAD BRANCHES 2024
aov_db24 <- aov(ndead_branches~Disturbance+Pair+Survey,data=ms24x)
summary(aov_db24)

ms24x %>% filter(Survey=="1") %>% 
  ggplot(aes(x=Pair,y=ndead_branches,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("2024 number of dead branches, survey 1")+
  labs(fill="Pairs")

ms24x %>% filter(Survey=="2") %>% 
  ggplot(aes(x=Pair,y=ndead_branches,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  ylab("2024 number of dead branches, survey 2")+
  labs(fill="Pairs")

aov_damage24 <- aov(Pathogen_damage_pct~Disturbance+Pair+Survey,data=ms24x)
summary(aov_damage24)

#LEAVES 2024
aov_leaves24 <- aov(nleaves~Disturbance+Pair+Survey,data=ms24x)
summary(aov_leaves24)

tukey <- TukeyHSD(aov_leaves24)
tukey

tukey_df <- as.data.frame(tukey$Pair)
names(tukey_df)[names(tukey_df) == "p adj"] <- "p.adj"
sig_pairs <- subset(tukey_df,p.adj < 0.05)
tukey_df

comparisons <- strsplit(row.names(sig_pairs), "-")
comparisons