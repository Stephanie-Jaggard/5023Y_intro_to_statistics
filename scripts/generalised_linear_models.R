
#Set up----

library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)
library(janitor)
library(ggridges)
library(ggplot2)
library(kableExtra)

fruitfly <- read_csv(here("data", "fruitfly.csv"))

#_______________________________________________________________----


#GLM description ----
#1. Linear predictor
#2. error/variance structure
#3. link function

#_______________________________________________________________----

#Compare GLM----
flyls <- lm(longevity ~ type + thorax + sleep, data = fruitfly)
summary(flyls)

flyglm <- glm(longevity ~ type + thorax + sleep, 
              family = gaussian(link = "identity"),
              data = fruitfly)
summary(flyglm)

#_______________________________________________________________----

#Poisson regression----
#1. Discrete variable (0,1,2...)
#2. single rate parameter λ, where λ>  0
#3. Mean = λ
#4. Variance = λ

#_______________________________________________________________----

#no data



