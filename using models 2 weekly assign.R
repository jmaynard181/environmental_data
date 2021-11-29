require(palmerpenguins)

# ---- 1 Sample T test ----
#Try running a 1-sample t-test on the Gentoo penguin flipper lengths:
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

#Instead of comparing Gentoo penguin flipper lengths to zero, let’s test whether
#they are equal to 218 mm:
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

#Let’s try a one-tailed alternative hypothesis: Gentoo penguin flippers are
#smaller than 218 mm:
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

# ---- 2 Sample T test ----
#Instead of comparing the flipper length of Gentoo penguins to a fixed value,
#I could compare the flipper lengths of two species:
t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

#---- Analysis of Variance (ANOVA) ----
#Data Exploration (Graphical)
#We can explore normality using histograms and density plots:
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")  

#Conditional boxplots are great for categorical variables:
boxplot(body_mass_g ~ species, data = penguins)

#Data Exploration (Numerical)
#Remember the assumption of normality? Let’s test whether within-group data are 
#normally-distributed. We need to do some data massaging:
#Extract the measurements for each species.
#Calculate the mean body mass for each species.
#Conduct Shapiro tests on each species’ body mass.
dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

shapiro.test(dat_chinstrap$body_mass_g)

#Here’s a cool shortcut for calculating the species mean body masses 
#using aggregate() and the formula notation:
aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

#You should try it out with the shapiro.test()
aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)

#Fit a linear model
#The syntax is easy if you use the formula notation:
fit_species = lm(body_mass_g ~ species, data = penguins)

#Then we can look at the model coefficients:
summary(fit_species)

#Conduct the ANOVA
anova(fit_species)

# ---- One-Way Anova Complete Walkthrough ----
#A simple Analysis of Variance belongs to the Group 1 analyses. It is a linear model!
#The syntax to build the model in R is easy. Use the lm() function to store the model
#in a variable:

fit_species = lm(body_mass_g ~ species, data = penguins)

#Note the use of the R formula notation.
#Then we can look at the model coefficients:
summary(fit_species)

#Conduct the ANOVA
#R makes it easy to conduct an ANOVA:
anova(fit_species)

#Two-way additive ANOVA
#Did you notice that the conditional boxplot of body mass and penguin species 
#suggested that the distributions of body mass might not be the same in each species? 
#For reference, take a look at the shape of the boxes for Chinstrap and Gentoo penguins:
boxplot(body_mass_g ~ species, data = penguins)

#Fit a 2-way, additive model.
#We’ll first fit a model with two predictors:
#Sex
#Species
#In this first 2-way model, we won’t include interaction terms.
#The syntax to fit a model with two predictors, without interaction terms, is:
fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

#Two-Way interactive (factorial) ANOVA
#Here’s our last ANOVA-style model.
#A factorial design means that all combinations of categorical variables appear as ‘groups’ in the data. We’ll use two of the penguin factor variables:
#sex
#species
#The syntax to build the model is only slightly different than that of the additive model: You just have to replace the plus symbol with a multiplication symbol to indicate an interaction (or crossing) between the two factor predictors (sex and species).
fit_interactive = lm(body_mass_g ~ sex * species, data = penguins)

# ---- Simple Linear Regression: Penguin Bills and Body Mass ---- 
lm(bill_length_mm ~ body_mass_g, data = penguins)


# ---- Question 1 ----
boxplot(body_mass_g ~ sex:species, xlab = "Sex:Species", ylab = "Body Mass (g)", data = penguins, names = c("Adelie female", "Adelie male", "Chinstrap female", "Chinstrap male", "Gentoo female", "Gentoo male"))

# ---- Question 2 ----
fit_both = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)

# ---- Question 3 ----

