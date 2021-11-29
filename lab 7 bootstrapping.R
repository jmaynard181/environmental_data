# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

#Minimum and maximum values in each row
apply(dat, MARGIN = 1, FUN = min)

apply(dat, MARGIN = 1, FUN = max)

#Mean values in each column
apply(dat, MARGIN = 2, FUN = mean)

#starting an excercise
moths = read.csv(here("data", "moths.csv"))
head(moths)

# Create the results vector
m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

#perform the bootstrap
for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}

#Calculate the quantiles
mean(result)

quantile(result,c(0.025,0.975))

#Bootstrap Interval Using boot()
install.packages("boot")

#The basic syntax of boot is very simple:
require(boot)
boot(data, statistic, R)

#Custom Mean Function
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

#Now we can find the bootstrap for 10000 iterations:
myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

#You can check which other attributes are available for retrieval 
#using the dollar sign with str()
str(myboot)


mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

#Lastly, we can extract our bootstrap confidence interval as follows:
quantile(
  myboot$t,
  c(0.025, 0.975))

#Setting up the bootstrap
moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

# ---- Running the bootstrap simulation ----
#This is what it looks like all together:
n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

# ---- First draft ----
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

# ---- Second draft ----
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
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

# ---- Check in a fresh environment ---- 
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
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

# rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

# ---- Debugging template ----
#You can test your corrected function using the following template:

# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))

rarefaction_sampler = function(input_dat, n_iterations)
{
  ... paste your corrected code here ...
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)

# ---- Building the Rarefaction Curve ----
#You are now ready to continue with the rarefaction.
#Run the simulator with 10000 iterations:
# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

# ---- We can calculate the mean and 2.5% and 97.5% quantiles of the 
#bootstrapped species richness for each sampling intensity using apply() 
#function, as follows:
#For convenience, let’s bind the objects together and transpose the data frame 
#so that the columns represent the mean, 2.5% and 97.5% quantiles, and the rows
#represent sampling intensity ranging from 1 to 24.
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

# ---- Plotting the curve ----
#We can plot the rarefaction curve and the 95% confidence interval using the 
#matplot() function, which is useful for simultaneous plotting of several 
#columns of a data frame or matrix.
#We can also add a legend to make the plot complete, as follows:

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

# ---- Question 1 ----

#using the two group resample as an example
require(palmerpenguins)
dat_pen = subset(penguins, species = "Gentoo")
n = sum(!is.na(dat_pen$bill_length_mm))
n

t.test(dat_pen$body_mass_g ~ dat_pen$species)



length(dat_gentoo$bill_length_mm)









