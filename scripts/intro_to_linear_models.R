
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom)
library(broom.helpers)

#_______________________________________________________________

#read data
darwin <- read_csv("data/darwin.csv")

#_______________________________________________________________

#linear model function lm()
lsmodel0 <- lm(formula = height ~ 1, data = darwin)

#________________________________________________________________

#model summary
summary(lsmodel0)

mean(darwin$height)

#________________________________________________________________

#compare means 
lsmodel1 <- lm(height ~ type, data=darwin)
# note that the following is identical
# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

broom::tidy(lsmodel1)

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))

#put means in a plot
darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()
#also provides standard error but it's pooled between the two types 
#and not individual

#________________________________________________________________

#condidence intervals
confint(lsmodel1)

#________________________________________________________________

#Answering the question

#graph of the estimated mean difference with an approx 95% CI. 
#As we can see we are able to reject the null hypothesis at a 
#95% confidence level.
GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

#_________________________________________________________________

#Getting the other treatment mean and standard error
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

#emmeans
means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

#plot for means
means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

#__________________________________________________________________

#assumption checking
performance::check_model(lsmodel1)
#or
plot(lsmodel1)

#normal distribution
performance::check_model(lsmodel1, check=c("normality","qq"))
#or
plot(lsmodel1, which=c(2,2))

#_________________________________________________________________

# QQ plots
#A QQ plot is a classic way of checking whether a sample 
#distribution is the same as another (or theoretical distribution).
#The qqplot distributes your data on the y-axis, and a theoretical 
#normal distribution on the x-axis. If the residuals follow a 
#normal distribution, they should meet to produce a perfect 
#diagonal line across the plot.

#__________________________________________________________________

#Equal variance
performance::check_model(lsmodel1, check="homogeneity")
#or
plot(lsmodel1, which=c(1,3))

#lot the residuals (variance) of our data against the fitted
#(predicted) values. If the residuals were zero, this would mean 
#there is no error, and our data exactly matches our estimates. 
#In reality, there will always be residual error, but as long as it
#is evenly distributed between treatments this is ok.

#__________________________________________________________________

#Outliers
performance::check_model(lsmodel1, check="outliers")
#or
plot(lsmodel1, which=c(4,4))

#Cook's distance. 
#This is a measure of how much 'leverage' a single data point is
#exerting on the model, if it is too high, it may be having an
#outsized effect on the estimates.

#__________________________________________________________________

#Summary plot
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

