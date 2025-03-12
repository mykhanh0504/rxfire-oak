library(tidyverse)
library(dplyr)
library(ggplot2)
library(gt)
library(car)
library(multcomp)
library(rstatix)

soiln <- read_csv("soilN.csv")

soiln <- soiln %>% filter(Pair %in% c("2","3","4","5","6","8"))
soiln <- soiln[-c(2),]

soiln2 <- soiln %>% mutate(Cut=case_when(Stand %in% c("3/16","5/15","28/2","12/2") 
                                           ~ "Shelterwood",
                                           Stand %in% c("20/2","3/1") 
                                           ~ "Seedtree",
                                           Stand %in% c("44","46N","46") 
                                           ~ "Clearcut"))

soiln2 <- soiln2 %>% replace_na(list(Stand="Unnamed",Cut="Unmanaged"))

soiln2 <- soiln2 %>% mutate(Pair2=case_when(Stand %in% c("3/16","5/15")~"1",
                                       Stand %in% c("20/2","3/1")~"2",
                                       Stand %in% c("28/2","12/2")~"3",
                                       Stand=="Unnamed"~"4",
                                       Stand %in% c("44","46N")~"5",
                                       Stand=="46"~"6"))

#soiln2 <- soiln2 %>% select(Site,Stand,Treatment,
                            #NO3_mgkg,NH4_mgkg,Cut,Pair2,OM_pct)

soiln2 <- soiln2 %>% 
  mutate(Pair2=as.factor(Pair2),
         Site=as.factor(Site),
         Treatment=as.factor(Treatment),
         Cut=as.factor(Cut))

bc <- c("B"="#CC6677","C"="#88CCEE")
p6 <- c("1"="#FBE3D6","2"="#FFFFCC","3"="#E8E8E8",
        "4"="#C2F1C8","5"="#DCEAF7","6"="#DCEAF7")

#totN, N/OMpct
soiln2 <-  soiln2 %>% mutate(TotN=NO3_mgkg+NH4_mgkg,
                             NO3_OM=NO3_mgkg/(OM_pct/100),
                             NH4_OM=NH4_mgkg/(OM_pct/100),
                             TotN_OM=NO3_OM+NH4_OM)

#NO3 graph
soiln2 %>% 
  ggplot(aes(x=Pair2,y=NO3_OM,fill=Treatment))+
  geom_rect(aes(fill=Pair2),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_col(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair2,scales='free_x')+
  theme_minimal()+
  xlab("Pair")+
  ylab("NO3 (mg/kg soil)")+
  labs(fill="Pairs")

#NH4 graph
soiln2 %>% 
  ggplot(aes(x=Site,y=NH4_OM,fill=Treatment))+
  geom_rect(aes(fill=Pair2),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_col(width=0.6,position=position_dodge(width=0.7))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair2,scales='free_x')+
  theme_minimal()+
  xlab("Pair")+
  ylab("NH4 (mg/kg soil)")+
  labs(fill="Pairs")

