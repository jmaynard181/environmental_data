# ---- Question 1 ----
#Sample Standard Error Function
require(palmerpenguins)

rm(list = ls())

sse_mean = function(x)
{ na.x = is.na(x)         #Turns the numerical data into logical data
x2 = x[na.x == FALSE]     #Tells function to exclude missing data and call this new data x2
sd.x2 = sd(x2)            #Determines the standard deviation and assigns it to a new vector sd.x2
n = length(x2)            #Finds the length of the data thats available and assigns it n
sd.x2/(sqrt(n))}          #This performs the equation to determine sample standard error

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

sse_mean(penguins$bill_depth_mm)

# ---- Question 2 ----
#Two Group Resampling

#performing 2 group resample using available vector data (x) producing
#2 seperate samples from that population
two_group_resample=function(x, n_1, n_2)
  
{
  
  
  dat_1 = sample(x, n_1, replace = TRUE) #sample called dat_1 using data of x for sample 1, replacement style sampling
  dat_2 = sample(x, n_2, replace = TRUE) #sample called dat_2 using data of x for sample 2, replacement style sampling
  
  diff_simulated = #determining differnce in sample means
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
  
}

# ---- Question 3 ----

# This is monte carlo style resampling.As the columns of data are broken and not intact this supports null hypothesis.

# ---- Question 4 ----

#using the two group resample as an example
dat_pen = droplevels(subset(penguins, species != "Gentoo")) #Droplevels removes unused factors from a dataframe
                                                            #"!=" is a logical test for not being equal to
                                                            # "==" is a logical test for equality
#the 50, 50 is the sample size
two_group_resample(penguins$flipper_length_mm, 50, 50)

n = 2000                #this runs the simulation 200 times
mean_differences = c()   #creating a empty vector (holder) for the experiment results to populate
for (i in 1:n)           #This is the loop initiator and states the loop should run "n" times
{
  mean_differences = c( 
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}

hist(mean_differences)
head(mean_differences)

# ---- Question 5 ----
#Below will account for negative values to find the mean difference
#greater than 5.8 by using the absolute value. This equation then
#sums these differences
sum(abs(mean_differences) > 5.8)
#answer is 18 to question 5

# ---- Question 6 ----
#based on the low p value estimated trials would be 10 million

# ---- Question 7 ----
#how to make a boxplot example (replace flipper)
boxplot(body_mass_g ~ species, data = penguins)


# ---- Question 8 ----

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

shapiro.test(dat_pen$body_mass_g)

# ---- Question 9 ----


# ---- Question 10 ----

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

t.test(dat_pen$body_mass_g ~ dat_pen$species)

two_group_resample=function(x, n_1, n_2)
  
  n = 1000                #this runs the simulation 200 times
mean_differences = c()   #creating a empty vector (holder) for the experiment results to populate
for (i in 1:n)           #This is the loop initiator and states the loop should run "n" times
{
  mean_differences = c( 
    mean_differences,
    two_group_resample(dat_pen$body_mass_g, 68, 152)
  )
}

hist(mean_differences)
head(mean_differences)

diff_crit = sum(abs(mean_differences))
diff_crit
# ---- Question 11 ----

