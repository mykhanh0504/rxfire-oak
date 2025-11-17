library(tidyverse)
library(dplyr)
library(ggplot2)

# Clean up count df -------------------------------------------------------

count23 <- read_csv("oakcount5m2023.csv")

count23x <- count23 %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

count23x <- count23x %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))


count23x <- count23x %>% mutate(Unit=ifelse(Unit=="Unnamed" & Disturbance =="B","Unnamed_B",
                                            ifelse(Unit=="Unnamed" & Disturbance == "C", "Unnamed_C",Unit)))

count23x <- count23x %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","20/3","20/4","20/5","3/1")~"2",
  Unit %in% c("28/2","28/3","12/2")~"3",
  Unit %in% c("Unnamed_B","Unnamed_C")~"4",
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
         Disturbance=factor(Disturbance,levels=c("C","B")),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair))

count23x$Plot_unique <- with(count23x,paste(Location, Unit, Transect, Plot, sep="_"))
count23x$Plot_unique <- as.factor(count23x$Plot_unique)

df <- count23x %>% filter(!Location %in% c("BEF","CF"))

# ANOVA count -------------------------------------------------------------

aov_count <- aov(OAKcount~Disturbance+Pair+Error(Unit),data=df)
summary(aov_count)

# Clean up 2023 df --------------------------------------------------------

ms23 <- read_csv("oakmsms23.csv")

ms23 <- ms23 %>% mutate(Location=as.factor(Location)) %>% 
  mutate(Location=fct_recode(Location,
                             "CF"="Crawford Notch State Park",
                             "HOG"="Hogsback (Blueberry Mountain)",
                             "SB"="Stevens Brook (Plummers Ledge)"))

ms23x <- ms23 %>% mutate(Treatment=case_when(
  Unit %in% c("3/16","5/15","28/2","12/2") ~ "Shelterwood",
  Unit %in% c("20/2","3/1") ~ "Seedtree",
  Unit %in% c("44B","45C","46B","46C") ~ "Clearcut"))

ms23x <- ms23x %>% replace_na(list(
  Unit="Unnamed",Treatment="Unmanaged"))

ms23x <- ms23x %>% mutate(Unit=ifelse(Unit=="Unnamed" & Disturbance =="B","Unnamed_B",
                                            ifelse(Unit=="Unnamed" & 
                                                     Disturbance == "C", "Unnamed_C",
                                                   Unit)))

ms23x <- ms23x %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","20/3","20/4","20/5","3/1")~"2",
  Unit %in% c("28/2","28/3","12/2")~"3",
  Unit %in% c("Unnamed_B","Unnamed_C")~"4"))

ms23x <- ms23x %>% 
  mutate(Unit=as.factor(Unit),
         Age=as.factor(Age),
         Disturbance=factor(Disturbance,levels=c("C","B")),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair))

ms23x$Age <- factor(ms23x$Age,levels=c('1','2','3','4','5','6','7',
                                       '8','9','10','11','UNK'))

ms23x$Plot_unique <- with(ms23x,paste(Location, Unit, Transect, Plot, sep="_"))
ms23x$Plot_unique <- as.factor(ms23x$Plot_unique)

# ANOVA DRC ---------------------------------------------------------------

aov_drc23 <- aov(DRC_mm~Disturbance+Pair+Error(Unit/Plot_unique),data=ms23x)
summary(aov_drc23)

# Clean up 2024 df --------------------------------------------------------
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

ms24x <- ms24x %>% mutate(Unit=ifelse(Unit=="Unnamed" & Disturbance =="B","Unnamed_B",
                                      ifelse(Unit=="Unnamed" & 
                                               Disturbance == "C", "Unnamed_C",
                                             Unit)))

ms24x <- ms24x %>% mutate(Pair=case_when(
  Unit %in% c("3/16","5/15")~"1",
  Unit %in% c("20/2","3/1")~"2",
  Unit %in% c("28/2","12/2")~"3",
  Unit %in% c("Unnamed_B","Unnamed_C")~"4"))

ms24x <- ms24x %>% 
  mutate(Unit=as.factor(Unit),
         Disturbance=factor(Disturbance,levels=c("C","B")),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair),
         Survey=as.factor(Survey))

ms24x <- ms24x %>% drop_na()

ms24x <- ms24x %>% mutate(herbivory_pct=na_if(herbivory_pct,"na")) %>% 
  mutate(Pathogen_damage_pct=na_if(Pathogen_damage_pct,"na"))

ms24x$nleaves <- replace(ms24x$nleaves,ms24x$nleaves=="100+","100") %>%
  as.integer()

ms24x$Plot_unique <- with(ms24x,paste(Location, Unit, Transect, Plot, sep="_"))
ms24x$Plot_unique <- as.factor(ms24x$Plot_unique)

ms24x <- ms24x %>% filter(Survey=="2")

# ANOVA XG nleaves --------------------------------------------------------

aov_xg24 <- aov(Extension_growth_cm~Disturbance+Pair+Error(Unit),data=ms24x)
summary(aov_xg24)

aov_leaves24 <- aov(nleaves~Disturbance+Error(Unit),data=ms24x)
summary(aov_leaves24)

# LMMs --------------------------------------------------------------------

#sdlg count
library(glmmTMB)
m_count <- glmmTMB(OAKcount~Disturbance+Pair+(1|Unit),
                ziformula=~1,family=nbinom2,data=df)
summary(m_count)

library(emmeans)
emmeans(m_count,pairwise~Disturbance,type="response")
plot(emmeans(m_count,pairwise~Disturbance,type="response"))

#drc 2023 
library(lme4)
ms23x$logDRC <- log(ms23x$DRC_mm)
m_drc <- lmer(logDRC ~ Disturbance+Pair+(1|Unit/Plot_unique),data=ms23x)
summary(m_drc)

#back transform
emmeans(m_drc,~Disturbance,type="response")
#pairwise contrast
emmeans(m_drc,pairwise~Disturbance,type="response")
plot(emmeans(m_drc, ~ Disturbance, type = "response"))

#drc 2024
ms24x$logDRC <- log(ms24x$DRC_mm)
m_drc2 <- lmer(logDRC ~ Disturbance+Pair+(1|Unit/Plot_unique),data=ms24x)
summary(m_drc2)

#back transform
emmeans(m_drc2,~Disturbance,type="response")
#pairwise contrast
emmeans(m_drc2,pairwise~Disturbance,type="response")
plot(emmeans(m_drc2, ~ Disturbance, type = "response"))

#xg 2024
ms24x$logXG <- log(ms24x$Extension_growth_cm+1)
m_xg <- lmer(logXG ~ Disturbance+Pair+(1|Unit/Plot_unique),data=ms24x)
summary(m_xg)

#back transform
emm_xg <- emmeans(m_xg,~Disturbance,type="response")
emm_xg
#pairwise contrast
emmeans(m_xg,pairwise~Disturbance,type="response")
plot(emmeans(m_xg, ~ Disturbance, type = "response"))

#nleaves 2024
m_leaves <- glmmTMB(nleaves~Disturbance+Pair+(1|Unit/Plot_unique),
                    family=nbinom2, data=ms24x)
summary(m_leaves)

emmeans(m_leaves,pairwise~Disturbance,type="response")
plot(emmeans(m_leaves,pairwise~Disturbance,type="response"))
