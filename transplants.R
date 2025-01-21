library(tidyverse)
library(dplyr)
library(ggplot2)
library(gt)
library(car)
library(multcomp)
library(rstatix)

transplants <- read_csv("transplants.csv")

transplants <- transplants %>% filter(Presence=="1",
                                      Block %in% c("1","3","5","7","9",
                                                   "11","13","15","17","19"))
transplants <- transplants %>% select(Survey,AcornID,Soil.type,Sterilized,Height_cm,
                                      Egrowth_cm,nleaves,nlive_branches,
                                      ndead_branches)

transplants <- transplants %>% 
  mutate(Survey=as.factor(Survey),
         AcornID=as.factor(AcornID),
         Soil.type=as.factor(Soil.type),
         Sterilized=as.factor(Sterilized))

ster <- c("1"="#CC6677","0"="#88CCEE")

#height survey 1
transplants %>% filter(Survey=="1") %>%  
  ggplot(aes(x=Soil.type,y=Height_cm,fill=Sterilized))+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  facet_grid(~Soil.type,scales='free_x')+
  scale_fill_manual(values=ster)

#height survey 2
transplants %>% filter(Survey=="2") %>%  
  ggplot(aes(x=Soil.type,y=Height_cm,fill=Sterilized))+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  facet_grid(~Soil.type,scales='free_x')+
  scale_fill_manual(values=ster)

aov_height <- aov(Height_cm~Soil.type+Sterilized+Survey,data=transplants)
summary(aov_growth)

#egrowth survey 1
transplants %>% filter(Survey=="1") %>%  
  ggplot(aes(x=Soil.type,y=Egrowth_cm,fill=Sterilized))+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  facet_grid(~Soil.type,scales='free_x')+
  scale_fill_manual(values=ster)

#egrowth survey 2
transplants %>% filter(Survey=="2") %>%  
  ggplot(aes(x=Soil.type,y=Egrowth_cm,fill=Sterilized))+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  facet_grid(~Soil.type,scales='free_x')+
  scale_fill_manual(values=ster)

aov_growth <- aov(Egrowth_cm~Soil.type+Sterilized+Survey,data=transplants)
summary(aov_growth)

#nleaves survey 1
transplants %>% filter(Survey=="1") %>%  
  ggplot(aes(x=Soil.type,y=nleaves,fill=Sterilized))+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  facet_grid(~Soil.type,scales='free_x')+
  scale_fill_manual(values=ster)

#nleaves survey 2
transplants %>% filter(Survey=="2") %>%  
  ggplot(aes(x=Soil.type,y=nleaves,fill=Sterilized))+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  facet_grid(~Soil.type,scales='free_x')+
  scale_fill_manual(values=ster)

aov_leaves <- aov(nleaves~Soil.type+Sterilized+Survey,data=transplants)
summary(aov_leaves)

tukey <- TukeyHSD(aov_leaves)
tukey

tukey_df <- as.data.frame(tukey$Soil.type)
names(tukey_df)[names(tukey_df) == "p adj"] <- "p.adj"
sig_pairs <- subset(tukey_df,p.adj < 0.05)
tukey_df

comparisons <- strsplit(row.names(sig_pairs), "-")
comparisons