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

count <- count %>% group_by(Pair, Disturbance) %>% 
  slice_max(stemcount,n=6)

