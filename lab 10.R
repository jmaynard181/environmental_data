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
df_tot = n_obs

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

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((mean(x) - (x))^2)
)
#
str(agg_sq_resids)

#Provides summary of the object
str(agg_resids)

ss_within = sum(agg_sq_resids$x)

#
df_within = n_obs - n_groups

#                      
ss_among = ss_tot - ss_within

#                       
df_among = n_groups-1

#                        
ms_within = ss_within/df_within

#
ms_among  = ss_among/df_among

#                        
f_ratio = ms_among/ms_within

#
f_pval = 1-pf(f_ratio, df_among, df_within)

# ---- ANOVA in R ----

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

#You can use str() to reveal the structure of the anova table object and use the subset operator $ to extract the values of interest:
  
anova_fit_1 = anova(fit_1)
str(anova_fit_1)



# ---- self check ----
# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)


                        
# ---- Question 3 ----
                        
bartlett.test(p.cut ~ rope.type, data = rope)


# ---- Question 4 ----

                        
# ---- Question 5 ----
                        
                        
# ---- Question 6 ----
                        
                        
# ---- Question 7 ----
                        
                        

                        