#Using chinstrap penguin data create a dataset containing only the Adelie penguins:
require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))

#Histogram
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

# ---- Question 1 ---- 
#Create separate boxplots body mass for male and female Adelie penguins. boxplots
#dont have to be in same panel. Show the R-code you used to make the plots.

boxplot(body_mass_g ~ sex,
        data = dat_ade,
        main = "Body Mass of Adelie Penguin",
        xlab = "Sex",
        ylab = "Body Mass (g)")

# ---- Question 2 ----
#Perform a one-sample t-test of the alternative hypothesis that female Adelie 
#penguins have a body mass different from zero grams. Note that this is a very 
#silly alternative hypothesis. Is this a one- or two-tailed test? Show your R-code.

dat_ade_fem = droplevels(subset(penguins, species == "Adelie", sex))

t.test(dat_ade$body_mass_g, mu=0)

t.test(dat_ade_fem$body_mass_g, y = NULL, alternative = "greater", mu = 0, conf.level = 0.95)

#One Sample t-test

#data:  dat_ade_fem$body_mass_g
#t = 106.85, df = 72, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
  #3305.985 3431.687
#sample estimates:
  #mean of x 
#3368.836 


# ---- Question 3 ----
#Describe your conclusions based on the p-value of the t-test.

#Based on the results of the t test, our conclusion is that if the alternative 
#hypothesis is true, we can expect to see a penguin body mass that is zero in 
#less than 2.2e-14% of the time. In other words it would be extremely unlikely 
#that we see a penguin with a body mass that is zero


# ---- Question 4 ----
#Now, conduct a slightly less silly test: perform a one-sample t-test of the null 
#hypothesis that male Adelie penguins have a mean body mass greater than 4000 
#grams. Is this a one- or two-tailed test?

#This would be a one tailed test,  specified in R as alternative = “greater”


# ---- Question 5 ----
#Describe your conclusions based on the p-value of the t-test.

#data:  ade_male$body_mass_g
#t = 1.0715, df = 72, p-value = 0.1438
#alternative hypothesis: true mean is greater than 4000

#Based on p-value of 0.144 we would fail to reject the null hypothesis. A p value 
#this high indicates that if we were to keep pulling samples from our population ~14%
#of the time they would indicate a true population mean of less than 4000g. 


# ---- Question 6 ----
#Conduct a two-sample t-test of the alternative hypothesis that male and female 
#Adelie penguins have different mean body masses. Show your r-code.

t.test(dat_ade_fem$body_mass_g, dat_ade_male$body_mass_g, alternative = "two.sided", mu = 0, conf.level = .95)

#Welch Two Sample t-test

#data:  dat_ade_fem$body_mass_g and dat_ade_male$body_mass_g
#t = -13.126, df = 135.69, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -776.3012 -573.0139
#sample estimates:
#  mean of x mean of y 
#3368.836  4043.493 


# ---- Question 7 ----
#Describe your conclusions based on the p-value of the t-test.

#Based on the very low p value we conclude that male and females do have
#different mean body masses.


# ---- Question 8 ----
#Conduct a two-sample (one-tailed) t-test of the directional alternative hypothesis 
#that male Adelie penguins are heavier than females

t.test(dat_ade_male$body_mass_g, dat_ade_fem$body_mass_g, alternative = "greater", mu = 0, conf.level = .95)

#Welch Two Sample t-test

#data:  dat_ade_male$body_mass_g and dat_ade_fem$body_mass_g
#t = 13.126, df = 135.69, p-value < 2.2e-16
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
#  589.5351      Inf
#sample estimates:
#  mean of x mean of y 
#4043.493  3368.836 


# ---- Question 9 ----
#Conduct a two-sample (one-tailed) t-test of the directional alternative 
#hypothesis that male Adelie penguins are heavier than females.

t.test(dat_ade_male$body_mass_g, dat_ade_fem$body_mass_g, alternative = "less", mu = 0, conf.level = .95)

#Welch Two Sample t-test

#data:  dat_ade_male$body_mass_g and dat_ade_fem$body_mass_g
#t = 13.126, df = 135.69, p-value = 1
#alternative hypothesis: true difference in means is less than 0
#95 percent confidence interval:
#  -Inf 759.78
#sample estimates:
#  mean of x mean of y 
#4043.493  3368.836 


# ---- Question 10 ----
#Explain why the p-values are so drastically different in the two directions.

#The p values are so drastically different because the two inquiries are opposites.
#In the first (Q8) we are testing whether or not males are heavier than females and 
#in the second (Q9) we are testing whether or not males are lighter than females. 
#Given that we can only expect one of these to be true, if one of those has sufficient 
#evidence as supported by a low p value, we can expect that the opposite inquiry is 
#likely to lack sufficient evidence and therefore have a very high p value. Given that
#we have a decimal number (.00000000000000022) for the first p value and the highest
#possible p value for the second (1) we can see that this is the case.

.




