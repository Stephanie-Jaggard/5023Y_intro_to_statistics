#Set up ----

library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)
library(janitor)
library(ggridges)
library(ggplot2)
library(kableExtra)

fruitfly <- read_csv(here("data", "fruitfly.csv"))

#_________________________________________________________----

#Hypothesis ----
#type - should definitely be included.
#thorax - the size of the flies could determine longevity. Carreira et al (2009)8
#sleep - sleep could easily help determine longevity. Thompson et al (2020)9
#type:sleep - the amount that sleep (rest) helps promote longevity could change 
#depending on how much activity the fly engages in when awake. Chen et al (2017)10

#_________________________________________________________----
#Tidy----
# check the structure of the data
glimpse(fruitfly)

# check data is in a tidy format
head(fruitfly)

fruitfly <- janitor::clean_names(fruitfly)

##check plot ----
GGally::ggpairs(fruitfly)

#_________________________________________________________----
#Difference in longevity----
##Density distribution for Type----
colours <- c("cyan", "darkorange", "purple")

fruitfly %>% 
  ggplot(aes(x = longevity, y = type, fill = type))+
  geom_density_ridges(alpha = 0.5)+
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(legend.position = "none")
#conclusion - type affects longevity

#_________________________________________________________
##Size----
fruitfly %>% 
  ggplot(aes(x = thorax, y = longevity))+
  geom_point()+
  theme_minimal()+
  geom_point(aes(colour = type))+
  geom_smooth(method="lm",
              se=FALSE,
              aes(colour=type)) +
  scale_color_manual(values=colours)
  #conclusion - size affects longevity
#BUT size doesn't affect longevity differently between treatment groups

#_________________________________________________________

##Sleep----
fruitfly %>% 
  ggplot(aes(x=sleep, y = longevity, group = type, colour = type))+
  geom_point( alpha = 0.6)+
  geom_smooth(method = "lm",
              se = FALSE)+
  scale_colour_manual(values = colours)+
  theme_minimal()
#conclusion -  it does look as though sleep interacts with
# treatment to affect lifespan. As the slopes of the lines are 
# very different in each group. But in order to know the strength
# of this association, and if it is significantly different from
# what we might observe under the null hypothesis, we will have 
# to build a model.

#_________________________________________________________----
#Designing a model----

# a full model
flyls1 <- lm(longevity ~ type + thorax + sleep + type:sleep, data = fruitfly)

flyls1 %>% 
  broom::tidy()

# mean longevity of a male with a 0.79mm thorax, 
#that sleeps for 22% of the day and is paired 
#with virgin females

# intercept
coef(flyls1)[1] + 
  
  # 1*coefficient for virgin treatment  
  coef(flyls1)[3] + 
  
  # 0.79 * coefficient for thorax size  
  (coef(flyls1)[4]*0.79) + 
  
  # 22 * coefficient for sleep  
  (coef(flyls1)[5]*22) + 
  # 22 * 1 * coefficient for interaction
  (coef(flyls1)[7]*22*1)
###!!! Don't get it----

#_________________________________________________________----

#Model check + Colinearity ---- 
performance::check_model(flyls1)

##Variance inflation factor summary ----
car::vif(flyls1)

#_________________________________________________________----

#Data transformations----
##1.Variable transformation e.g lm(sqrt(x) ~ y, data = data)----
#Can sometimes fix linearity
#Can sometimes fix non-normality and heteroscedasticity (i.e non-constant variance)

##2. Generalized Linear Models (GLMs) to change the error structure----

#_________________________________________________________

##BoxCox----
# run this, pick a transformation and retest the model fit
MASS::boxcox(flyls1)
###Square root transform----
flyls_sqrt <- lm(sqrt(longevity) ~ type + thorax + sleep + type:sleep, data = fruitfly)

performance::check_model(flyls_sqrt)
#conclusion - doesn't change much

#_________________________________________________________----
#Model selection----

# use drop1 function to remove top-level terms
drop1(flyls1, test = "F")

#simplify by removing the interaction
flyls2 <- lm(longevity ~ type + thorax + sleep, data = fruitfly)

drop1(flyls2, test = "F")


#_________________________________________________________----
#Post-Hoc----
emmeans::emmeans(flyls2, specs = pairwise ~ type + thorax + sleep)
# to produce the estimate mean values for different categories
# If the term pairwise is included then it will also include 
#post-hoc pairwise comparisons between all levels with a tukey test

#_________________________________________________________----

#Write up ----

#I constructed an ordinary least squares model to investigate the 
# effects of sleep, mating type and body size on longevity in adult
# Drosophila melanogaster. I also included an interaction term between 
# sleep and mating type. All Analyses and data cleaning was carried out
# in R ver 4.1.2 with the tidyverse range of packages (Wickham et al 2019), 
# model residuals were checked with the performance package (Lüdecke et al
# 2021), and summary tables produced with broom (Robinson et al 2022) and 
# kableExtra (Zhu 2020).

#_____________________________________________

##Results ----
#I tested the hypothesis that sexual activity is costly for male Drosophila 
# melanogaster fruitflies. Previous research indicated that sleep
# deprived males are less attractive to females, this would indicate that
# levels of sexual activity might be affected by sleep and impact the effect
# on longevity, as such this was included as an interaction term in the full
# model. Body size is also know to affect lifespan, as such this was included 
# as a covariate in the mode.

#There was a small interaction effect of decreased lifespan with increasing 
# sleep in the treatment groups compared to control in our samples, but this
# was not significantly different from no effect (F2,118 = 0.512, P = 0.6), 
# and was therefore dropped from the full model (Table 15.1).


flyls2 %>% broom::tidy(conf.int = T) %>% 
  select(-`std.error`) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kbl(col.names = c("Predictors",
                    "Estimates",
                    "Z-value",
                    "P",
                    "Lower 95% CI",
                    "Upper 95% CI"),
      caption = "Linear model coefficients", 
      booktabs = T) %>% 
  kable_styling(full_width = FALSE, font_size=16)

#There was a significant overall effect of treatment on male 
# longevity (Linear model: F2,120 = 30.1, P < 0.001), with males
# paired to virgin females having the lowest mean longevity 
# (48 days, [95%CI: 44.9 - 51.2]) (when holding body size and 
# sleep constant), compared to control males (61.3 days [56.8 -
# 65.8]) and males paired with inseminated females (64.9 days 
# [61.8 - 68.1 days]).

#Post hoc analysis showed that these differences were statistically 
# significant for males paired with control females compared to the
# inseminated (Tukey test: t120 = 4.8, P < 0.001) and virgin groups 
# (t120 = 7.5, P < 0.001), but there was no overall evidence of a 
# difference between inseminated and virgin groups (t120 = -1.309
# P < 0.3929) (Figure 19.4).

#Comparing the treatment effects against other predictors of 
# longevity such as body size and sleep, I found that sleep had
# a very small effect on longevity (mean change -0.05 days
# [-0.18 - 0.07]) which was not significantly different from 
# no effect (Linear model: F1,120 = 0.68, P = 0.41). Body size 
# (taken from thorax length) was a significant predictor of 
# longevity (F1,120 = 121, P < 0.001), with each 0.1 mm increase
# in body size adding 14.4 days to the individual lifespan [11.8
# - 17]. It appears as though body size has a stronger effect on
# longevity than treatment, indicating that while there is a 
# measurable cost of sexual activity to males, it may be less
# severe than in females (not compared here), and less severe 
# than other measurable predictors.

#____________________________________________________________----
#Supplementary code ----

#produce model summaries automatically

library(sjPlot)
tab_model(flyls2)

library(gtsummary)
tbl_regression(flyls2)

#____________________________________________________________----

