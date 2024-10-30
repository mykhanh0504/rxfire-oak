library(tidyverse)
library(dplyr)
library(ggplot2)

count23 <- read_csv("oakcount5m2023.csv")

#Add Treatment & Pair columns
count23x <- count23 %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

count23x <- count23 %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))

count23x <- count23 %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","20/3","20/4","20/5","3/1")~"2",
  Unit %in% c("28/2","28/3","12/2")~"3",
  Unit=="Unnamed"~"4",
  Unit %in% c("44B","45C")~"5",
  Unit %in% c("46B","46C")~"6"))