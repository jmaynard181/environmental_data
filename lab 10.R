# ---- Lab 10 ----

# ---- Code Template ----
require(here)
rm(list = ls())

#creates dataframe called rope
rope <- read.csv(here("data", "rope.csv"))

#provides data overview
summary(rope)

#Preview rows
head(rope)

#Need to factorize rope as it is currently a category
factor(rope$rope.type)

#This overrides the category type turning this column into a factor
rope$rope.type = factor(rope$rope.type)

#Displays what the possible factor levels can be
levels(rope$rope.type)

#Calculate the total number of observations and save it to a variable called                       
n_obs = nrow(rope)

#Calculate the number of groups (rope types) and save it in a variable called
n_groups = length(levels(rope$rope.type))

#This is the mean of all observations
mean(rope$p.cut)

#create a variable of mean of all observations
grand_mean = mean(rope$p.cut)

#This provides residual values
grand_mean - rope$p.cut

#create a variable for residual values
diff_resids = grand_mean - rope$p.cut

#squared residuals
diff_resids^2

#total sum of squared residuals
sum(diff_resids^2)

#Sum of squares variable
ss_tot = sum(diff_resids^2)

#
df_tot = ....

#this function calculates the residuals
aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x - mean(x))

#takes a vector of numbers and groups them by a factor (category) and a function in the group
agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x - mean(x))

#Provides summary of the object
str(agg_resids) FUN = function(x) sum((mean(x) - (x))^2)

ss_within = 

#
df_within = ....

#                      
ss_among = ....

#                       
df_among = ....

#                        
ms_within = ....

#
ms_among  = ....

#                        
f_ratio = ....

#
f_pval = ....

# ---- Question 1 ----

                        
# ---- Question 2 ----

                        
# ---- Question 3 ----
                        
                        
# ---- Question 4 ----

                        
# ---- Question 5 ----
                        
                        
# ---- Question 6 ----
                        
                        
# ---- Question 7 ----
                        
                        

                        