require(palmerpenguins)
dat_gentoo = subset(penguins, species == "Gentoo")
dat_gentoo
n = sum(!is.na(dat_gentoo$bill_depth_mm))
n


ssd = sd(dat_gentoo$bill_depth_mm, na.rm = TRUE)
print(ssd)
ssd

alpha = 0.05


t_crit = abs()

#49 is 1 less than the sample size, 2 is dividing the alpha value by 2 as it's
#95% confidence interval. 
crit_upper <-qt(0.05/2, 49, lower.tail=FALSE)
crit_upper
crit_lower <-qt(0.05/2, 49, lower.tail=TRUE)
crit_lower


# ---- Question 1 ----
hist()









