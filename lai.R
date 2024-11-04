library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(multcomp)
library(ggpubr)
library(ggstatsplot)

lai <- read_csv("LAIR.csv")

#Add Treatment & Pair columns
lai2 <- lai %>% mutate(Treatment=case_when(Unit %in% c("3/16","5/15","28/2","12/2") 
                                           ~ "Shelterwood",
                                           Unit %in% c("20/2","3/1") 
                                           ~ "Seedtree",
                                           Unit %in% c("44B","45C","46B","46C") 
                                           ~ "Clearcut"))

lai2 <- lai2 %>% replace_na(list(Unit="Unnamed",Treatment="Unmanaged"))

lai2 <- lai2 %>% mutate(Pair=case_when(Unit %in% c("3/16","5/15")~"1",
                                       Unit %in% c("20/2","3/1")~"2",
                                       Unit %in% c("28/2","12/2")~"3",
                                       Unit=="Unnamed"~"4",
                                       Unit %in% c("44B","45C")~"5",
                                       Unit %in% c("46B","46C")~"6"))

lai2 <- lai2 %>% 
  mutate(Location=as.factor(Location),
         Unit=as.factor(Unit),
         Disturbance=as.factor(Disturbance),
         Treatment=as.factor(Treatment),
         Pair=as.factor(Pair))

#Summarize mean LAI by pair, cutting method, burn y/n
stats <- lai2 %>% group_by(Pair,Treatment,Disturbance) %>% 
  summarize(meanLAI=mean(avgLAI),
            sdLAI=sd(avgLAI))

#one away anova burn y/n
anovaBurn <- aov(avgLAI~Disturbance,data=lai2)
summary(anovaBurn)

#one way anova cutting method
anovaCut <- aov(avgLAI~Treatment,data=lai2)
summary(anovaCut)

phCut <- glht(anovaCut,
              linfct = mcp(Treatment = "Tukey"))
summary(phCut)

plot(phCut)

#one way anova location
anovaLoc <- aov(avgLAI~Location,data=lai2)
summary(anovaLoc)

phLoc <- glht(anovaLoc,
              linfct = mcp(Location = "Tukey"))
summary(phLoc)

plot(phLoc)

#one way anova pair
anovaPair <- aov(avgLAI~Pair,data=lai2)
summary(anovaPair)

phPair <- glht(anovaPair,
              linfct = mcp(Pair = "Tukey"))
summary(phPair)

plot(phPair)

#plot by pair and burn y/n
bc <- c("B"="#CC6677","C"="#88CCEE")
p6 <- c("1"="#FBE3D6","2"="#FFFFCC","3"="#E8E8E8","4"="#C2F1C8","5"="#DCEAF7","6"="#DCEAF7")

lai2 %>% 
  ggplot(aes(Pair,avgLAI))+
  geom_rect(aes(fill=Pair),alpha=0.5,
            xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_boxplot(aes(fill=Disturbance))+
  scale_fill_manual(values=c(p6,bc))+
  facet_grid(~Pair,scales='free_x')+
  theme_minimal()+
  theme(text=element_text(size=30))+
  ylab("Average LAI values")

ggbetweenstats(
  data = lai2,
  x = Disturbance,
  y = avgLAI,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  violin.args = list(width = 0, linewidth = 0),
)

#plot by pair and cutting method
lai2 %>%ggplot(aes(y=avgLAI,x=Treatment))+
  geom_boxplot()

#anova AND posthoc for cutting method
ggbetweenstats(
  data = lai2,
  x = Treatment,
  y = avgLAI,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  violin.args = list(width = 0, linewidth = 0),
)

#grouped between stats - burn by pair
windows()
grouped_ggbetweenstats(
  data=lai2,
  x=Disturbance,
  y=avgLAI,
  grouping.var = Pair,
  pairwise.comparisons = TRUE,
  pairwise.display="significant",
  p.adjust.methods="fdr",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  violin.args = list(width = 0, linewidth = 0)
)

#grouped between stats - burn by cutting method
windows()
grouped_ggbetweenstats(
  data=lai2,
  x=Disturbance,
  y=avgLAI,
  grouping.var = Treatment,
  pairwise.comparisons = TRUE,
  pairwise.display="significant",
  p.adjust.methods="fdr",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  violin.args = list(width = 0, linewidth = 0)
)

#two way anova
burncut <- aov(avgLAI~Disturbance*Treatment
               +Pair,data=lai2)
summary(burncut)

ph2w <- TukeyHSD(burncut)
ph2w

burncut2 <- aov(avgLAI~Disturbance*Treatment
               +Location,data=lai2)
summary(burncut2)

#https://www.scribbr.com/statistics/anova-in-r/
