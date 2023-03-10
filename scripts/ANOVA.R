#Set up ----

library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)

darwin <- read_csv(here("data", "darwin.csv"))
frogs <- read_csv("data/frogs_messy_data.csv")
#_______________________________________________________________----

#Simple linear model ---- 
lsmodel1 <- lm(height ~ type, data = darwin)

#_______________________________________________________________----

#ANOVA table ----
anova(lsmodel1)

#_______________________________________________________________----

#F-test ---- 
pf(5.9395, 1, 28, lower.tail=FALSE)

#The self pollinated maize plants measured an average of 17.6
#[16-19.1] (mean[95% CI]) inches high, while the cross-pollinated
#plants had a mean height of 20.2 [18.6-21.7] inches - 
#a difference of 2.6 [-0.4-4.8] inches
#(one-way ANOVA: F1,28 = 5.9, P = 0.02)

#_______________________________________________________________----

#Two-way ANOVA----
lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)

anova(lsmodel2)

#_______________________________________________________________----

#Summary ----
#ANOVA tables can be built for any linear model. 
#The tables partition the variance into signal(s) and noise,
#which can be compared using an F-test. For complex analyses 
#where many pairwise comparisons could be performed, an initial
#F-test can provide the initial evidence for whether there are 
#any differences at all, reducing the risk of over-testing and 
#the false positives that can be generated

#_______________________________________________________________----

#Activity----

##Hypothesis ----

#mean frogspawn hatching time will vary with temperature level.
#We can predict that given our temperature range, at the highest 
#temperature (25°C) hatching time will be reduced

##Tidy data----
frogs <- frogs %>% 
  rename("13" = Temperature13,
         "18" = Temperature18,
         "25" = Temperature25,
         frogspawn_id = `Frogspawn sample id`) %>% 
  pivot_longer(`13`:`25`, names_to="temperature", values_to="days") %>% 
  drop_na(days)

##Linear model----
lsmodel_frogs1 <- lm(days ~ temperature, data = frogs)

# summary(lsmodel_frogs1)

# anova(lsmodel_frogs1)

broom::tidy(lsmodel_frogs1, conf.int = T)

##Check assumptions ----
plot(lsmodel_frogs1)

##Figure----
#Struggling

##Results ----
#Increasing temperatures had a clear effect on reducing the 
#time taken for frogspawn to hatch (one-way ANOVA: F2,57 = 385.9,
#P < 0.001). At 13∘C the mean time to hatching was 26.3 days
#[25.8-26.8 95% CI], this reduced by an average of 5.3 days 
#[4.57 - 6.02] at 18∘C and by 10.1 days [9.37 - 10.82] at 25∘C

#_______________________________________________________________----

