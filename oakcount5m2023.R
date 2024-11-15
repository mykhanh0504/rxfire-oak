library(tidyverse)
library(dplyr)
library(ggplot2)
library(gt)
library(car)
library(multcomp)
library(rstatix)

count23 <- read_csv("oakcount5m2023.csv")

#Add Treatment & Pair columns
count23x <- count23 %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

count23x <- count23x %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))

count23x <- count23x %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","20/3","20/4","20/5","3/1")~"2",
  Unit %in% c("28/2","28/3","12/2")~"3",
  Unit=="Unnamed"~"4",
  Unit %in% c("44B","45C")~"5",
  Unit %in% c("46B","46C")~"6"))

count23x <- count23x %>% mutate(Location=as.factor(Location)) %>% 
  mutate(Location=fct_recode(Location,
                             "BEF"="Bartlett Experimental Forest",
                             "CF"="Crawford Notch State Park",
                             "HOG"="Hogsback (Blueberry Mountain)",
                             "SB"="Stevens Brook (Plummers Ledge)"))

count23x <- count23x %>% 
  mutate(Unit=as.factor(Unit),
         Disturbance=as.factor(Disturbance),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair))

#scale up to per ha
b1 <- 66
c1 <- 46
b2 <- 31
c2 <- 30
b3 <- 30
c3 <- 45
b4 <- 43
c4 <- 19
b5 <- 10 
c5 <- 13
b6 <- 28
c6 <- 32

plotarea <- 0.00785398163397448

#summarize data
count23x <- count23x %>% na.omit() %>% 
  mutate(Density=OAKcount/plotarea)

#count23x <- count23x %>% mutate(Density=if_else(
#  Pair=="1" & Disturbance=="B",
#  OAKcount/b1/plotarea,if_else(Pair=="1" & Disturbance=="C",
#  OAKcount/c1/plotarea,if_else(Pair=="2" & Disturbance=="B",
#  OAKcount/b2/plotarea,if_else(Pair=="2" & Disturbance=="C",
#  OAKcount/c2/plotarea,if_else(Pair=="3" & Disturbance=="B",
#  OAKcount/b3/plotarea,if_else(Pair=="3" & Disturbance=="C",
#  OAKcount/c3/plotarea,if_else(Pair=="4" & Disturbance=="B",
#  OAKcount/b4/plotarea,if_else(Pair=="4" & Disturbance=="C",
#  OAKcount/c4/plotarea,if_else(Pair=="5" & Disturbance=="B",
#  OAKcount/b5/plotarea,if_else(Pair=="5" & Disturbance=="C",
#  OAKcount/c5/plotarea,if_else(Pair=="6" & Disturbance=="B", 
#  OAKcount/b6/plotarea,OAKcount/c6/plotarea)))))))))))) 

count23x$Density <- round(count23x$Density,0)

#visualize stem density
bc <- c("B"="#CC6677","C"="#88CCEE")
p6 <- c("1"="#FBE3D6","2"="#FFFFCC","3"="#E8E8E8","4"="#C2F1C8","5"="#DCEAF7","6"="#DCEAF7")

count23x %>% 
  ggplot(aes(x=Pair,y=Density,fill=Disturbance))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  theme(text=element_text(size=30))+
  ylab("Stem count (per ha)")+
  labs(fill="Pairs")

#levenes test
leveneTest(summary$Density~summary$Disturbance)

#anova
count23x <- count23x %>% filter(Pair %in% c("1","2","3","4"))
one.way <- aov(OAKcount~Disturbance+Pair,data=count23x)
summary(one.way)

tukey <- TukeyHSD(one.way)
tukey

#Extracting significant pairs
tukey_df <- as.data.frame(tukey$Pair)
names(tukey_df)[names(tukey_df) == "p adj"] <- "p.adj"
sig_pairs <- subset(tukey_df,p.adj < 0.05)
tukey_df

# Create list of comparisons from significant pairs
comparisons <- strsplit(row.names(sig_pairs), "-")
comparisons

count23b <- count23x %>% filter(Disturbance=="B")
stats_count23b <- get_summary_stats(count23b)
stats_count23b <- stats_count23b %>% filter(variable=="Density") %>% 
  select(variable,min,max,median,mean,sd,se)
stats_count23b

count23c <- count23x %>% filter(Disturbance=="C")
stats_count23c <- get_summary_stats(count23c)
stats_count23c <- stats_count23c %>% filter(variable=="Density") %>% 
  select(variable,min,max,median,mean,sd,se)
stats_count23c