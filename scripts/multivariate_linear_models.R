#Set up ----

library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)

biomass <- read_csv(here("data", "biomass.csv"))
pollution <- read_csv(here("data", "pollution.csv"))
#_________________________________________________________----

#Factorial linear models----

# check the structure of the data
glimpse(biomass)

# check data is in a tidy format
head(biomass)

# check variable names
colnames(biomass)

# check for duplication
biomass %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
biomass %>% 
  summarise(min=min(Biomass.m2, na.rm=TRUE), 
            max=max(Biomass.m2, na.rm=TRUE))

# check for typos by looking at distinct characters/values
biomass %>% 
  distinct(Fert)

biomass %>% 
  distinct(Light)

biomass %>% 
  distinct(FL)

# missing values
biomass %>% 
  is.na() %>% 
  sum()

# quick summary

summary(biomass)

#_______________________________________________________----

#One way ANOVA----

ls_1 <- lm(Biomass.m2 ~ FL, data = biomass)
summary(ls_1)

#We can see that the control treatment (F- L-) is the intercept
#with a mean of 356g and standard error of 23g. The Light
#treatment adds an average of 30g to the biomass (but this close 
#to the standard error of this estimated mean difference - 32.72g).
#In contrast the addition of fertiliser adds 93g to the biomass

#to visualise differences
GGally::ggcoef_model(ls_1,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)

# combine the average mean differences of the light effect and fertiliser effect
coef(ls_1)[2] + coef(ls_1)[3] 

# compare this to the average difference of the combined treatment
coef(ls_1)[4]

##In contrast to the one-way ANOVA approach, a factorial design lets us test----
#and compare additive effects and interaction effects


#_______________________________________________________----
# Testing for interactions ----

##Additive model ----
biomass %>% ggplot(aes(x=Fert, y=Biomass.m2, colour = Light, fill = Light, group = Light))+
  geom_jitter(width=0.1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 23
  )+stat_summary(
    geom = "line",
    fun = "mean",
    size = 1, linetype = "dashed"
  )

##Interactive model ----
ls_2 <- lm(Biomass.m2 ~ Fert + # main effect
             Light + # main effect
             Fert:Light, # interaction term
           data = biomass)

summary(ls_2)


#_______________________________________________________----
#Model estimates ----
# model 1
coef(ls_1)[4]

# model 2
coef(ls_2)[2] + coef(ls_2)[3] + coef(ls_2)[4]

#_______________________________________________________----
#ANOVA tables----
## F-tests----
drop1(ls_2, test = "F")

#report the size of the effect (estimate) and the uncertainty
#(confidence intervals)

## Get F values----
# we have to remove the interaction term before we can keep using drop1()

ls_3 <- lm(Biomass.m2 ~ Fert + Light, data = biomass)

drop1(ls_3, test = "F")
#_______________________________________________________----
#Unbalanced design ----
#using ANOVA, the sum of squares is calculated sequentially
#(in the order of the formula), and so we could get different 
#results depending on the order in which we assemble predictors 
#into our model

##Unbalanced dataset ----
# make three vectors and combine them into a new tibble

height <- c(50,57,91,94,102,110,57,71,85,105,120)
size <- c(rep("small", 2), rep("large", 4), rep("small", 3), rep("large", 2))
treatment <- c(rep("Control", 6), rep("Removal", 5))

unbalanced <- tibble(height, size, treatment)

unbalanced

##Activity----
#Produce a linear model testing size and treatment against height
model_1 <- lm(height ~ treatment + size, data = unbalanced)
anova(model_1)

model_2 <- lm(height ~ size + treatment, data = unbalanced)
anova(model_2)
#a different conclusion is given just because of the order 
#the terms were included in the model!

###Compare the models----
drop1(model_1)
drop1(model_2)

#_______________________________________________________----
#Post-hoc ----
#Produce pairwise comparisons
emmeans::emmeans(ls_2, specs = pairwise ~ Light + Fert + Light:Fert) %>% 
  confint()
# including the argument pairwise in front of the ~ prompts the post-hoc pairwise comparisons.
# $emmeans contains the estimate mean values for each possible combination (with confidence intervals)
# $ contrasts contains tukey test post hoc comparisons between levels

#_______________________________________________________----
#Continuous linear models ----
##Activity ----

###Explore ----
# check the structure of the data
glimpse(pollution)

# check data is in a tidy format
head(pollution)

# check variable names
colnames(pollution)

# check for duplication
pollution %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
# quick summary

summary(pollution)

# check for typos by looking at distinct characters/values
pollution %>% 
  distinct(Stress)


# missing values
pollution %>% 
  is.na() %>% 
  sum()

###Visualise ----
pollution %>% 
  ggplot(aes(x = O3, y = William))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ Stress)+
  labs(x = expression(paste(Ozone~mu~L~L^-1)),
       y = expression(paste(Log~Yield~(kg~ha^-1))))

### Model----
William_ls1 <- lm(William ~ O3 + Stress + O3:Stress, data = pollution)

William_ls1 %>% 
  broom::tidy(conf.int = T)

### Residuals ----
performance::check_model(William_ls1)

### Simplify ----
drop1(William_ls1, test = "F")

William_ls2 <- lm(William ~ O3 + Stress, data = pollution)
drop1(William_ls2, test = "F")

#report estimates and intervals
William_ls2 %>% 
  broom::tidy(conf.int = T)

### Write up----
#I hypothesised that plants under stress might react more
#negatively to pollution than non-stressed plants, however 
#when tested I found no evidence of a negative interation 
#effect between stress and Ozone levels (F1,27 = 0.4621, 
#P = 0.5). Individually however, while well-watered plants 
#had higher yields than stressed plants (mean 0.178 [95% CI: 
#0.0682 - 0.288]) (F1,28 = 11, P = 0.003), there was a much 
#larger effect of Ozone, where every unit increase (u L L-1)
#produced a mean decrease in yield of 7.14 kg ha-1 [5.13 - 9.15]
#(F1,28 = 53, P < 0.001).

#_______________________________________________________----





