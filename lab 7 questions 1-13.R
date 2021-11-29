

#49 is 1 less than the sample size, 2 is dividing the alpha value by 2 as it's
#95% confidence interval. 
crit_upper <-qt(0.05/2, 49, lower.tail=FALSE)
crit_upper
crit_lower <-qt(0.05/2, 49, lower.tail=TRUE)
crit_lower


#Review the bootstrap and parametric confidence interval materials in the lab walkthrough.

#Calculate a parametric 95% CI for mean bill length (in mm) for the Gentoo penguins in
#the penguins dataset from package palmerpenguins using your SSE function. For this 
#calculation you should use Student’s t-distribution to calculate the critical values.

# ---- Question 1 ----
#What is the sample size, n? Show the code you used for the calculation and 
#remember to check for missing data.

require(palmerpenguins)
dat_gentoo = subset(penguins, species == "Gentoo")
dat_gentoo
n_gentoo = sum(!is.na(dat_gentoo$bill_length_mm))
n_gentoo

# ---- Question 2 ----
#What is the sample standard deviation? Show the code you used for the calculation

ssd = sd(dat_gentoo$bill_length_mm, na.rm = TRUE)
ssd

# ---- Question 3 ----
#What are the critical t-values? Show the R code you used for the calculation

#This saves the qt function as a vector of data called t_crit
t_crit = qt(c(0.025, 0.975), df = n_gentoo-1)
t_crit

# ---- Question 4 ----
#What is the sample standard error? Show the R code you used for the calculation.

sse_mean = function(x)
{ na.x = is.na(x)         #Turns the numerical data into logical data
x2 = x[na.x == FALSE]     #Tells function to exclude missing data and call this new data x2
sd.x2 = sd(x2)            #Determines the standard deviation and assigns it to a new vector sd.x2
n = length(x2)            #Finds the length of the data thats available and assigns it n
sd.x2/(sqrt(n))}          #This performs the equation to determine sample standard error

sse = sse_mean(dat_gentoo$bill_length_mm)

sse

# ---- Question 5 ----
#Finally, construct the CI and show the R code you used for the calculation.

mean(dat_gentoo$bill_length_mm, na.rm = TRUE)

parametric_radius = t_crit * sse;
parametric_radius + mean(dat_gentoo$bill_length_mm, na.rm = TRUE)




#Review the bootstrap confidence interval materials in the lab walkthrough.

#Calculate a bootstrap 95% CI for mean bill length (in mm) for the Gentoo penguins
#in penguins dataset from package palmerpenguins.
#Use the boot() function from package boot()



# ---- Question 6 ----
#What is the CI?

require(boot)
#boot(data, statistic, R)

#Custom Mean Function
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = dat_gentoo$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(myboot)

#You can check which other attributes are available for retrieval 
#using the dollar sign with str()
str(myboot)

mean(dat_gentoo$bill_length_mm)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

#Lastly, we can extract our bootstrap confidence interval as follows:
quantile(
  myboot$t,
  c(0.025, 0.975))


# ---- Question 7 ----
#Show the r code you used to call the boot() function.
require(boot)
#boot(data, statistic, R)

#Custom Mean Function
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = dat_gentoo$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(myboot)

#You can check which other attributes are available for retrieval 
#using the dollar sign with str()
str(myboot)

mean(dat_gentoo$bill_length_mm)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)


# ---- Question 8 ----
#Show the r code you used to calculate the upper and lower 2.5% quantiles.
quantile(
  myboot$t,
  c(0.025, 0.975))


# ---- Question 9 ----
#Show your completed rarefaction_sampler() function.


# This clears the current R session's environment
rm(list = ls())


# Re-read my data:
require(here)
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]



rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  n = n_input_rows 
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}


 rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)


# ---- Debugging Template ----

# This clears the current R session's environment
#Show your completed rarefaction_sampler() function.


# This clears the current R session's environment
rm(list = ls())


# Re-read my data:
require(here)
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]



rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  n = n_input_rows 
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}


rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)


# ---- Question 10 ----
#What did you find most difficult about building the function?



# ---- Question 11 ----
#Show the code you used to perform the simulations and construct the curve.


# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

#For convenience, let’s bind the objects together and transpose the data frame 
#so that the columns represent the mean, 2.5% and 97.5% quantiles, and the rows
#represent sampling intensity ranging from 1 to 24.
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))


# ---- Question 12 ----
#Include your rarefaction curve plot in your report. Show the R-code you
#used to create your plot.

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

# ---- Question 13 ----
#About how many sites should you visit if you want to see all of the moth species?
#Explain your reasoning using your rarefaction curve figure.





