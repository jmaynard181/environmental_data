#use the qnorm() function to find the 0.025% and 0.975% quantiles of the standard normal:
qnorm(c(0.025, 0.975))

# ---- Question 1 ----
#Calculate the critical z-values for a 90% CI of the standard normal distribution.
#Show the R-code you used to perform the calculation.
qnorm(c(0.05, 0.95))

# ---- Question 2 ----
#Consult the help entry for qt() and calculate the critical values for df = 10. 
#Show the R-code you used to perform the calculation.
qt(p = c(0.025, 0.975), df = 10)

# ---- Question 3 ----
#How many degrees of freedom are required for the 0.025% lower critical value of a
#t-distribution to match the 0.025% lower critical z-value (from the standard normal)
#to within one decimal place? Show the R-code you used to perform the calculation.
qt(p = c(0.025, 0.975), df = 61)

# ---- Question 4 ----
#How many degrees of freedom are required for the 0.025% lower critical value of a 
#t-distribution to match the 0.025% lower critical z-value (from the standard normal) 
#to within two decimal places? Show the R-code you used to perform the calculation.
qt(p = c(0.025, 0.975), df = 473)

# ---- Question 5 ----
#Suppose you know that the sample standard deviation for a group of 50 measurements is 
#3.14. The mean value is 10.0.
#What are the critical t-values you would need to know to construct a 95% CI on the mean?
qnorm(c(0.025, 0.975), mean = 10, sd = 3.14, lower.tail = TRUE, log.p = FALSE)
crit_upper <-qt(0.05/2, 49, lower.tail=FALSE)
crit_upper
crit_lower <-qt(0.05/2, 49, lower.tail=TRUE)
crit_lower



# ---- Question 6 ----
#Construct the interval. Show the R-code you used to perform the calculation.


