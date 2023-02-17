#Testing

#Set up ----

#packages
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom)
library(broom.helpers)
library(effectsize)
library(pwr)
library(simr)

#read data
darwin <- read_csv("data/darwin.csv")

#________________________________________________________________________________----

#Student's t-test ----

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

#one sample t-test: takes the mean of a sample and compares it with the null hypothesis of zero
#two sample t-test which compares the difference between the means of two samples against a null 
#hypothesis of no difference between the means of the two populations
#__________________________________________________________________________________

##graph for critical t values to 30 ----

df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

#__________________________________________________________________________________

##dataframe generatiom ----
lsmodel1 <- lm(height ~ type, data = darwin)

##linear model summary----
summary(lsmodel1)

##generate t value for object lsmode11 and set as object----
tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]

#________________________________________________________________________________ ----

#Paired t test ----

#pair is a factor, not numerical 
lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)

#The second row now compares the mean heights of Crossed and Selfed plants when they 
#are in the same pair

#rows three to 16 compare the average difference of each pair (Crossed and Selfed 
#combined) against pair 1

##confidece intervals ----
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows

##graph to compare uncertainties----

m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()

#________________________________________________________________________________----

#Effect sizes----

#When our 95% confidence intervals do not overlap the intercept, this indicates we
#have difference in our means which is significant at α= 0.05. More interestingly 
#than this it allows us to talk about the 'amount of difference' between our treatments, 
#the lower margin of our confidence intervals is the smallest/minimum effect size.

#_________________________________________________________________________________----

#Type 1 and 2 errors----

##type 1 error----
#The reality is that we know if we set an α= 0.05, that we run the risk of rejecting 
#the null hypothesis incorrectly in 1 in 20 of our experiments

##type 2 error----
#Keeping the null hypothesis, when we should be rejecting it? Or not finding an effect.
#The probability of making a Type 2 error is known as 1−β, where β refers to your 
#statistical 'power' (see later)

###experimental power ---- 
#this is strength of your experiment to detect a 
#statistical effect when there is one. Power is expressed as 1-β. 
#You want beta error typically to be less than 20%. So, you want a power of about 80%. 
#That is you have an 80% chance of finding an effect if it's there.

#_________________________________________________________________________________----

#Repeatability ----

set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments

#___________________________________________________________________________________----

#Activity----

##how many experiments found a significant difference and how many did not?----
y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())

##compare the estimates and confidence intervals----
y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()

#all the experiments have identical levels of uncertainty. We can clearly see that estimates
#and intervals are a substantial improvement in the way we report experiments, and that they
#make comparisons across repeated studies more valuable.

#____________________________________________________________________________________________----

#Power analysis----

##Low statistical power ----
#Large risk of committing Type II errors, e.g. a false negative

##High statistical power ----
#Small risk of committing Type II errors

#___________________________________________________________________________________

##What is statistical power?----

### Effect size ----
#The quantified magnitude of a result present in the population.
#Effect size is calculated using a specific statistical measure, 
#such as Pearson’s correlation coefficient for the relationship between 
#variables or Cohen’s d for the difference between groups

###Sample size----
#The number of observations in the sample

###Significance----
#The significance level used in the statistical test, e.g. α. Often set to 5% or 0.05

###Statistical power----
#The probability of accepting the alternative hypothesis if it is true 1−β

#_______________________________________________________________________________________

##Cohen's D----
#a standardized effect size for measuring the difference between two group means.
#So it is the effect size we use when carrying out t-tests

###Calculate Cohen's D----
#Mean difference/ standard deviation pooled

### Summarise the means, standard deviations and sample size of each cross----
summary_darwin <- darwin %>% 
  group_by(type) %>% 
  summarise(mean = mean(height),
            sd = sd(height),
            n = n())
summary_darwin

### Calculate the mean difference and the pooled standard deviation----
summary_darwin %>% 
  mutate(variance = sd^2) %>% 
  mutate(per_sample_var = variance * (n-1)) %>% 
  summarise(sd_pooled = sqrt(sum(per_sample_var)/sum(n-2)),
            mean_diff = diff(mean))

###Functions for calculating Cohen's D----

# simple t-test
lsmodel1 <- lm(height ~ type, data = darwin)

t_to_d(2.437, df_error = 28, paired = F)

# paired t-test
lsmodel2 <- lm(height ~ type +factor(pair), data = darwin)

t_to_d(2.456, df_error=27, paired=T)

#__________________________________________________________________________________

## Uses for power analysis----
#1.Working out what the statistical power of you analysis was post hoc 
#to determine how likely it was you missed an effect

#2. Working out what sample size you need before an experiment to make 
#sure you reach the desired power. Often a common βvalue is 0.8, in much 
#the same way that a common αis 0.05, it is an arbitrary target, but here 
#means we can tolerate a risk of failing to reject the null when we should 
#have in 20% of our experiments that do not produce a significant result

#_________________________________________________________________________________

#Work out the power of our two sample t-test above----

# Calculate the power of our model 
test.power <- pwr.t.test(n = 15, d = 0.92, sig.level = 0.05)

# Calculate the sample size we need to get power of 0.8 for a medium effect size (d = 0.5)

samp.size <- pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.8)
#produces none

#Call any sample size and check statistical power----
sample_size <- seq(0,1000, by=10)

output <- list(length(sample_size))


for (i in 1:length(sample_size)) { 
  
  sample <- pwr.t.test(n=sample_size[i], d=0.2, sig.level=0.05)
  output[[i]] <- sample
  
  #     if(i %% 1==0){    # The %% operator is the remainder, this handy if line prints a number every time it completes a loop
  #   print(i)
  #  }
}

sample_list <- as.list(sample_size)

names(output) <- sample_size

# output$`30`

#__________________________________________________________________________________----