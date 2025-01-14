library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(viridis)

trees <- read_csv("overstory.csv")

#Add Treatment & Pair columns
trees2 <- trees %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

trees2 <- trees2 %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))

trees2 <- trees2 %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","20/3","20/4","3/1")~"2",
  Unit %in% c("28/2","12/2","12/3","12/4","12/5","12/6",
              "12/7","12/8","12/9","12/10","12/11")~"3",
  Unit=="Unnamed"~"4",
  Unit %in% c("44B","45C")~"5",
  Unit %in% c("46B","46C")~"6"))

trees2 <- trees2 %>% mutate(Location=as.factor(Location)) %>% 
  mutate(Location=fct_recode(Location,
                             "BEF"="Bartlett Experimental Forest",
                             "CF"="Crawford Notch State Park",
                             "HOG"="Hogsback (Blueberry Mountain)",
                             "SB"="Stevens Brook (Plummers Ledge)"))

trees2 <- trees2 %>% 
  mutate(Location=as.factor(Location),
         Unit=as.factor(Unit),
         Disturbance=as.factor(Disturbance),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair),
         Spcode=as.factor(Spcode))

#spcode count
tally <- trees2 %>% group_by(Pair,Disturbance,Spcode) %>% 
  summarize(tally=length(Spcode))

tally <- tally %>% group_by(Pair, Disturbance) %>% 
  slice_max(tally,n=5)

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

tally <- tally %>% mutate(Density=if_else(
  Pair=="1" & Disturbance=="B",
  tally/b1/plotarea,if_else(Pair=="1" & Disturbance=="C",
  tally/c1/plotarea,if_else(Pair=="2" & Disturbance=="B",
  tally/b2/plotarea,if_else(Pair=="2" & Disturbance=="C",
  tally/c2/plotarea,if_else(Pair=="3" & Disturbance=="B",
  tally/b3/plotarea,if_else(Pair=="3" & Disturbance=="C",
  tally/c3/plotarea,if_else(Pair=="4" & Disturbance=="B",
  tally/b4/plotarea,if_else(Pair=="4" & Disturbance=="C",
  tally/c4/plotarea,if_else(Pair=="5" & Disturbance=="B",
  tally/b5/plotarea,if_else(Pair=="5" & Disturbance=="C",
  tally/c5/plotarea,if_else(Pair=="6" & Disturbance=="B",
  tally/b6/plotarea,tally/c6/plotarea)))))))))))) 

tally$Density <- round(tally$Density,0)

#visualize stem density
bc <- c("B"="#CC6677","C"="#88CCEE")
p6 <- c("1"="#FBE3D6","2"="#FFFFCC","3"="#E8E8E8",
        "4"="#C2F1C8","5"="#DCEAF7","6"="#DCEAF7")

tally %>% 
  ggplot(aes(x=Spcode,y=Density))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_col(aes(fill=Disturbance),width=0.6,
           position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_wrap(vars(Pair),scales='free')+
  theme_light()+
  xlab("Species")+
  ylab("Tree count (per ha)")

#dbh classes
trees2 <- trees2 %>% mutate(dbhClass=if_else(
  `DBH (cm)`<=5,"2-5",if_else(`DBH (cm)`>5 & `DBH (cm)`<=10,
                "5-10",if_else(`DBH (cm)`>10 & `DBH (cm)`<=20,
                "10-20",if_else(`DBH (cm)`>20 & `DBH (cm)`<=30,
                "20-30",if_else(`DBH (cm)`>30 & `DBH (cm)`<=40,
                "30-40",if_else(`DBH (cm)`>40 & `DBH (cm)`<=50,
                "40-50",if_else(`DBH (cm)`>50 & `DBH (cm)`<=60,
                "50-60",">60"))))))))

trees2 <- trees2 %>% mutate(dbhClass=fct_relevel(dbhClass,"2-5","5-10","10-20",
                                                "20-30","30-40","40-50","50-60",">60"))

#basal areas
trees2 <- trees2 %>% mutate(ba=0.00007854*(`DBH (cm)`^2))

sumba <- trees2 %>% select(Pair,Disturbance,Spcode,dbhClass,ba)
sumba <- sumba %>% group_by(Pair,Disturbance,Spcode,dbhClass) %>% 
  summarize(sumba=sum(ba))

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

sumba <- sumba %>% mutate(m2ha=if_else(
  Pair=="1" & Disturbance=="B",
  sumba/b1/plotarea,if_else(Pair=="1" & Disturbance=="C",
  sumba/c1/plotarea,if_else(Pair=="2" & Disturbance=="B",
  sumba/b2/plotarea,if_else(Pair=="2" & Disturbance=="C",
  sumba/c2/plotarea,if_else(Pair=="3" & Disturbance=="B",
  sumba/b3/plotarea,if_else(Pair=="3" & Disturbance=="C",
  sumba/c3/plotarea,if_else(Pair=="4" & Disturbance=="B",
  sumba/b4/plotarea,if_else(Pair=="4" & Disturbance=="C",
  sumba/c4/plotarea,if_else(Pair=="5" & Disturbance=="B",
  sumba/b5/plotarea,if_else(Pair=="5" & Disturbance=="C",
  sumba/c5/plotarea,if_else(Pair=="6" & Disturbance=="B",
  sumba/b6/plotarea,sumba/c6/plotarea)))))))))))) 

sumba <- sumba %>% filter(Spcode %in% c("ABBA","ACPE","ACRU","ACSA",
                                        "BEAL","BEPA","BEPO","FAGR",
                                        "FRAM","OSVI","PIRU","PIST",
                                        "POGR","POTR","PRPE","PRSE",
                                        "QURU","TIAM","TSCA"))

#n <- 8
#qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#col_vector = unlist(mapply(brewer.pal, 
#                           qual_col_pals$maxcolors, 
#                           rownames(qual_col_pals)))
#pie(rep(1,n), col=sample(col_vector, n))

#viridis_pal(option = "A")(19)

sumba <- sumba %>% mutate(Genus=case_when(
  Spcode %in% c("ACPE","ACRU","ACSA") ~ "Maples",
  Spcode %in% c("BEAL","BEPA","BEPO") ~ "Birches",
  Spcode %in% "FAGR" ~ "A. beech",
  Spcode %in% "PIST" ~ "W. pine",
  Spcode %in% c("POGR","POTR") ~ "Aspens",
  Spcode %in% c("PRPE","PRSE") ~ "Cherries",
  Spcode %in% "QURU" ~ "R. oak",
  Spcode %in% c("ABBA","FRAM","OSVI","PIRU","TIAM","TSCA")~"Others"
))

#let's try to plot this lol
p6 <- c("1"="#FBE3D6","2"="#FFFFCC","3"="#E8E8E8","4"="#C2F1C8","5"="#DCEAF7","6"="#DCEAF7")
sp <- c("Maples"="#88CCEE","R. oak"="#CC6677","Birches"="#DDCC77",
        "A. beech"="#117733","W. pine"="#FFC20A", "Aspens"="#332288",
        "Cherries"="#AA4499","Others"="#40B0A6")

sumba %>% 
  ggplot(aes(x=dbhClass,y=m2ha))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_col(aes(fill=Genus))+
  scale_fill_manual(values=c(p6,sp))+
  facet_grid(rows=vars(Pair),cols=vars(Disturbance),scales='free')+
  theme_light()+
  xlab("DBH size classes (cm)")+
  ylab("Basal area (per ha)")

#we have not taken into account dead/alive trees