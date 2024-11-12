library(tidyverse)
library(dplyr)
library(ggplot2)
library(gt)
library(car)
library(multcomp)

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
         Pair=as.factor(Pair))