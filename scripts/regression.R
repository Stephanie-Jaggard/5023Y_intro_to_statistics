
#Set up ----

library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)

janka <- read_csv("data/janka.csv")
#_______________________________________________________________----

#Plot for janka ----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()
#______________________________________________________________----

#Pearson's R janka ---- 

janka %>% 
  cor_test(dens, hardness)
#______________________________________________________________----

#Regression in R ---- 

janka_ls1 <- lm(hardness ~ dens, data = janka)
#the left of the 'tilde' is the response variable,
#on the right is the predictor.

# specify linear model method for line fitting
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

summary(janka_ls1)
#_______________________________________________________________----

#Mean centered regression ---- 
dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

##Confidence intervals ----
confint(janka_ls1)

#What is the minimum effect size (at 95% confidence) of density on the janka scale?
# ->  = 0.05 we think there is at least a 52.9 unit increase on the janka scale for 
#every unit increase in density (ρ). Because our 95% confidence intervals do not 
#span 0, we know that there is a significant relationship at α= 0.05.

##Effect size ----

#R squared is found in the summary of the data
summary(janka_ls1)

#_______________________________________________________________________----

#Assumptions ---- 

janka_ls1 %>% 
  broom::augment() %>% 
  head()
#make all the same assumptions as linear models
#-> unexplained variation around the regression line (the residuals) is approximately
#normally distributed, and has constant variance

#________________________________________________________________________

##Residual variance plot ----
augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")

#A perfect model would mean that all of our residual values = 0, 
#but this is incredibly unlikely to ever occur. Instead we would like to see
#that there is a 'normal distribution' to the residuals e.g. more residuals 
#close to the mean, and fewer further away in a rough z-distribution.
#We also want to see homogeneity of the residuals e.g. it would be a bad model
#if the average error was greater at one end of the model than the other. 
#This might mean we have more uncertainty in the slope of the line for large 
#values over small values or vice versa.

#___________________________________________________________________________

model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

p1+p2+p3

#____________________________________________________________

##Normal distribution janka ----
plot(janka_ls1, which=c(2,2))

##Equal variance ----
plot(janka_ls1, which=c(1,3))

##Outliers ----
#What is the outlier's positional order in the dataframe?
plot(janka_ls1, which=c(4,5))
# points should be within the cooks distance lines

#__________________________________________________________________________-----

#Prediction ----
coef(janka_ls1)
## (Intercept)        dens 
## -1160.49970    57.50667

# a + bx

-1160.49970 + 57.50667 * 65

#OR

coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65
## (Intercept) 
##    2577.434

#OR

predict(janka_ls1, newdata=list(dens=c(22,35,65)))

#____________________________________________________________________________ ----

#Add confidence intervals ---- 

##Standard error ----
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)

##95% Confidence intervals ----
broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

#_________________________________________________________________________________________----

# Plot the three new predicted values onto an existing figure----

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))

#______________________________________________________________________________________________----