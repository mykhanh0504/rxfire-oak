library(tidyverse)
library(dplyr)
library(ggplot2)

comp <- read_csv("competition.csv")

#Add Treatment & Pair columns
comp2 <- comp %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

comp2 <- comp2 %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))

comp2 <- comp2 %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","20/3","20/4","20/5","3/1")~"2",
  Unit %in% c("28/2","28/3","12/2")~"3",
  Unit=="Unnamed"~"4",
  Unit %in% c("44B","45C")~"5",
  Unit %in% c("46B","46C")~"6"))

comp2$Spcode <- replace(comp2$Spcode,
                        comp2$Spcode %in% c("RUID","RUFR","RUOC"),
                        "RUS")

comp2 <- comp2 %>% filter(Spcode %in% 
                            c("ABBA","ACPE","ACRU","ACSA",
                              "BEAL","BEPA","BEPO","FAGR",
                              "FRAM","OSVI","PIST","POGR",
                              "POTR","PRPE","RUS","SACA",
                              "TSCA"))

comp2 <- comp2 %>% 
  mutate(Location=as.factor(Location),
         Unit=as.factor(Unit),
         Disturbance=as.factor(Disturbance),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair),
         Spcode=as.factor(Spcode))

#spcode count
count <- comp2 %>% group_by(Pair,Disturbance,Spcode) %>% 
  summarize(stemcount=sum(Count))

count <- count %>% group_by(Pair,Disturbance) %>% 
  slice_max(stemcount,n=3)

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

plotarea <- 0.0001

count <- count %>% mutate(Density=if_else(
  Pair=="1" & Disturbance=="B",
  stemcount/b1/plotarea,if_else(Pair=="1" & Disturbance=="C",
  stemcount/c1/plotarea,if_else(Pair=="2" & Disturbance=="B",
  stemcount/b2/plotarea,if_else(Pair=="2" & Disturbance=="C",
  stemcount/c2/plotarea,if_else(Pair=="3" & Disturbance=="B",
  stemcount/b3/plotarea,if_else(Pair=="3" & Disturbance=="C",
  stemcount/c3/plotarea,if_else(Pair=="4" & Disturbance=="B",
  stemcount/b4/plotarea,if_else(Pair=="4" & Disturbance=="C",
  stemcount/c4/plotarea,if_else(Pair=="5" & Disturbance=="B",
  stemcount/b5/plotarea,if_else(Pair=="5" & Disturbance=="C",
  stemcount/c5/plotarea,if_else(Pair=="6" & Disturbance=="B",
  stemcount/b6/plotarea,stemcount/c6/plotarea)))))))))))) 

count$Density <- round(count$Density,0)

#visualize stem density
bc <- c("B"="#CC6677","C"="#88CCEE")
p6 <- c("1"="#FBE3D6","2"="#FFFFCC","3"="#E8E8E8","4"="#C2F1C8","5"="#DCEAF7","6"="#DCEAF7")

count %>% 
ggplot(aes(x=Spcode,y=Density))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_col(aes(fill=Disturbance),width=0.6,
           position=position_dodge(preserve='single'))+
  scale_fill_manual(values=c(p6,bc))+
  facet_wrap(vars(Pair),scales='free')+
  theme(aspect.ratio=1)+
  xlab("Species")+
  ylab("Stem count (per ha)")
