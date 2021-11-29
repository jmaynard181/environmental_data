
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#Reproductive Success and Failure
#What's the evidence that reproductive success is more or less likely than 
#reproductive failure?
#We can investigate the answer with a two-sided binomial test for proportions.
#Pooling all the the data in the 14 ponds, we observed a total of 33 reproductive
#successes out of 61 total trials.
#How likely is a response of 33/61 if the reproductive success and failure are
#equally likely, i.e., Pr(success)=0.5?
#binomial test for this, specifying the number of successes (33) and the total 
#sample size (61), as follows:
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

#define variables to hold the late- and normal-filling rates:
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

#Instead of testing the null hypothesis of equal probability of success and failure
#(i.e., p=0.5), we might instead ask the question:
#What is the evidence that reproductive success is more or less frequent than the 
#normal-filling rate?
#In this scenario, we expect successful reproduction in approximately 5 of every 7 years.
#We can modify the code we used above to test the observed reproduction success 
#rate against the normal-filling rate:
binom.test(
  n_success,
  n_years,
  p = normal_fill_rate)

#In addition, note again that the default test is a two-sided alternative
#We might instead prefer the one-sided alternative hypothesis that the observed success
#rate is less than the pond normal-filling rate. We can perform the test as follows:
binom.test(
  n_success,
  n_years,
  p = normal_fill_rate,
  alternative ='less')

#Comparing two variances
#Before we can carry out a test to compare two means (see below), we need to test
#whether the sample variances are significantly different
#The simplest test is called Fisher’s F test, based on the F-statistic.
#The F-statistic represents the ratio between two variances.
#It is based on the idea that if the variances of the two samples are the same, 
#then the ratio of the variances will be 1.
#In order to be significantly different, the ratio will need to be significantly 
#smaller or larger than 1, depending on whether the smaller variance is in the 
#numerator or denominator.
veg = read.csv(here("data", "vegdata.csv"))
head(veg)

#For the moment, we’ll only consider the pine seedling count (column pine) and
#we’ll ignore the block experimental design.
#Let’s visualize the data using box plots, as follows:
boxplot(pine ~ treatment, data = veg)

#Variance test
#Let’s test whether the variance in pine seedling count differs between two treatments:
#control (do nothing) and clipped (continuous clipping of fern fronds)
var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#F-tests Assumes Normality
#Fisher’s F test for unequal variances assumes that the data are normally distributed.
#Visual inspection of the box plots indicates that this may be the case, but we might
#want to test for normality using one of the tests described previously, for example:
shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])

#Non-parametric Variance Test
#If the results indicate that the data are non-normal, then we should use a non-parametric 
#test of homogeneity of variances, such as the Fligner-Killeen test, as follows:
fligner.test(
    pine ~ treatment,
    data = veg,
    subset = treatment %in% c('control','clipped'))

#Thus far, we have been concerned with comparing the variances of two samples.
#However, there are roughly equivalent tests for k-sample problems; i.e., when there 
#are more than two groups.
#The ksample parametric test is called Bartlett’s test, which we can use to test for 
#homogeneity of variances among all four treatment levels as follows:
bartlett.test(pine ~ treatment, data=veg)

#What does Bartlett’s test say about homogeneity of variances among treatments?
#Note that Bartlett’s test, like Fisher’s F test is highly sensitive to non-normality 
#and the presence of outliers.
#The non-parametric alternative test which is largely preferred by many statisticians 
#is called the Fligner-Killeen test. We used it to test two variances above, but it can 
#test n variances as well:
fligner.test(pine ~ treatment, data = veg)

#T-test
#The Student’s t test is appropriate when the samples are independent, the variances
#constant, and the errors normally distributed. We implemented it as follows for the 
#pine seedling data in the control and clipped treatment plots:
t.test(
    pine ~ treatment,
    data = veg,
    subset = treatment %in% c('control','clipped'))

#Wilcox test
#We’ve used the Wilcox test in lecture and lab before.
#The Wilcoxon’s rank-sum test is appropriate when the samples are independent but 
#the errors are not normally distributed, and is implemented as follows:
wilcox.test(
    pine ~ treatment,
    data = veg,
    subset = treatment %in% c('control','clipped'))

#Tests for paired samples
#In the two-sample t test above, the 8 replicates in the control and 8 replicates
#in the clipped treatments were assumed to be independent samples, but in fact the
#treatments were implemented in a randomized block design
#Specifically, the experimental units were grouped together into blocks representing
#different forest stands
#It is reasonable to suspect that the stands differed somewhat in ecological conditions,
#perhaps in ways that we were not able to observe directly, but in ways that indirectly
#affect pine seedling response to the understory treatments
#If this is the case, then the samples will exhibit a positive covariance and it will 
#be to our advantage to account for this covariance by using a paired t test
#First, we need to create separate vectors for the “control” observations and “clipped”
#observations because the t.test() doesn’t accept formula’s (as above) for the paired
#option, as follows:
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

#Now we can use the t.test() function as before, but with the added argument 
#paired=TRUE, as follows:
t.test(control, clipped, paired=TRUE)

#Do the results differ from the unpaired t test?
#Recall that we decided against the Student’s t test for these data set because of failure
#to meet the underlying assumptions of constant variance and normally distributed data.
#As before, we can use the Wilcoxon’s rank-sum test when the samples are independent but
#the errors are not normally distributed, as follows:
wilcox.test(control, clipped, paired=TRUE)

#The question arises as to whether the dispersal rates for first-time breeders and 
#experienced breeders are correlated
#First, we need to read in the data and check it, as follows:
disp = read.csv(here("data", "dispersal.csv"))
disp
plot(disp$disp.rate.ftb, disp$disp.rate.eb)

#We can test the significance of the correlation using the cor.test() function,as follows:
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

#What does the correlation test say about the significance of the correlation between
#juvenile and adult dispersal-distance functions?
#The default correlation test statistic is based on Pearson’s product-moment correlation
#coefficient (r) cor(x,y) which follows a t distribution with length(x)-2 degrees of 
#freedom if the samples follow independent normal distributions
#If the data are non-normal, then a non-parametric rank-based measure of association 
#is more appropriate.
#If method is “kendall” or “spearman”, Kendall’s tau or Spearman’s rho statistic is 
#used to estimate a rank-based measure of association
#These tests may be used when data do not necessarily come from bivariate normal distribution.
#Let’s try a test of the Spearman’s rank correlation:
cor.test(
    disp$disp.rate.ftb,
    disp$disp.rate.eb,
    use='complete.obs',
    method='spearman')

#The Kolmogorov-Smirnov test works on empirical cumulative distribution functions (ecdf).
#Recall that these give the probability that a randomly selected value of X is less
#than or equal to x.
#Let’s see what the ecdf for the sample of juvenile dispersal rate looks like:
plot(
    ecdf(disp$disp.rate.ftb),
    verticals=TRUE)

#Now let’s add the ecdf for the adult dispersal rate, but change the line type 
#(lty) so that we can distinguish it from the ecdf for the juvenile dispersal rate

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

#Are these two distributions different? We can use the Kolmogorov-Smirnov test 
#(ks.test) to determine if they differ significantly in any aspect
#The test statistic is the maximum difference in value of the cumulative distribution
#functions; i.e., maximum vertical difference in the curves for a given value of X
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#The question arises as to whether the apparent positively biased mortality rate for 
#females is statistically significant, or whether this sort of difference could arise 
#through chance alone.
#This is a simple binomial proportions test, which we can easily do in R by specifying 
#two vectors:
#the number of mortalities for females and males c(4,16)
#the total number of female and male candidates: c(40,250)
prop.test(c(4,16),c(40,250))

#We can assess the significance of the differences between observed and expected 
#frequencies in a variety of ways. The usual way is with Pearson’s chi-squared test 
#(generalized linear models are an alternative).
#Do you remember how to calculate the chi-square test statistic? If not, look it up.
#In R we can compute the chi-squared test as follows:
  owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

#However, when one or more of the expected frequencies is less than 4 (or 5), 
#then it is wrong to use Pearson’s chi-squared test for your contingency table. 
#This is because small expected values inflate the value of the test statistic, 
#and it can no longer be assumed to follow the chi-square distribution
#Fisher’s Exact test
#In this case, an alternative test called Fisher’s exact test is more appropriate
#We use the function in the same way as before:
fisher.test(owls)

#Read in the raw data and create a 2x2 contingency table consisting of counts of 
#brown creeper presence (1) versus (0) absence and forest edge (E) versus interior (I):
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]
#Make sure you understand the script above:
#First, we read in the bird and habitat data and merged them into a single file 
#based on the common fields.
#Then we used the table() function to compute the cross-classified counts.
#What code converted the Brown Creeper counts to presence/absence?
#The next step simply switched the order of the columns so that the present counts
#were in the first column and the absent counts were in the second column, as this is 
#the expected order in some functions (e.g, prop.test())
#Are brown creepers present more or less frequently than expected in forest interiors 
#versus forest edges and is the difference significant?


# ---- Question 1 ----
#State the null hypothesis of the Chi-square test.
#Make sure you state the null hypothesis in terms of Brown Creeper 
#presence/absence and edge/interior habitats.

#The null hypothesis of the Chi-square test states that there is no relationship for the 
#presence/absence of Brown Creepers in edge/interior habitats.

# ---- Question 2 ----
#Consider the results of your test and explain whether you think that Brown 
#Creepers show a significant habitat preference.
#Make sure your use the output of your statistical test to support your answer.

#Based on the results of the Chi-square test I reject the null hypothesis. The
#expected versus observed values have experienced significant descrepancies 
#between their returned values.

# --- Question 3 ----
#A quick pre/re-view of build models for ANOVA
#We’ll use the lm() function, which stands for “linear model”, to build models
#that we can use for ANOVA. To build a simple model of flipper length as a function
#of species, I would use the following syntax:

#Note that I could also add a second predictor to the model by (slightly) modifying
#the formula. Re-check how to do this in the DataCamp formula tutorial.
#When you add your second factor below, make sure you use the correct formula 
#operator for crossing (hint: it’s not the plus symbol).

require(palmerpenguins)
data(package = 'palmerpenguins')
?penguins
print(penguins)

require(palmerpenguins)
fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)

#Show the R-code you can use to create a model fit (call it fit_species)
#of penguin body mass as predicted by penguin species.

fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)

# ---- Question 4 ----
#Show the R-code you can use to create a model fit (call it fit_sex) of
#penguin body mass as predicted by sex.
fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)

# ---- Question 5 ----
#Show the R-code you can use to create a model fit (call it fit_both)
#of penguin body mass as predicted by species and sex.

fit_both = 
  lm(
  formula = body_mass_g ~ sex, species,
  data = penguins)
  

# ---- Question 6 ----

boxplot(
  body_mass_g ~ species, data = penguins, ylab = "body mass (g)",
  main = "Boxplot of penguins by species")


# ---- Question 7 ----

boxplot(
  body_mass_g ~ sex, data = penguins, xlab = "sex", ylab = "body mass (g)",
  main = "Boxplot of penguins by species")


# ---- Question 8 ----

boxplot(
  body_mass_g ~ sex * species, data = penguins,las=2, ylab = "body mass (g)",
  main = "Boxplot of penguins by species",
  names = c("Adelie\nfemale",
          "Adelie\nmale",
          "Chinstrap\nfemale",
          "Chinstrap\nfemale",
          "Gentoo\nfemale",
          "Gentoo\nmale"))

# ---- Question 9 ----

#Based on the shapes of the boxes, which of the models (if any) do you think may 
#have problems fulfilling the homogeneity assumption?

Referring to shape of the boxplots and not the location, I do not believe there
are significant variance as the comparison groups are relatively similar.

# ---- Question 10 ----
#State the null hypothesis of the Bartlett test.

The variance is the same throughout the groups

# ---- Question 11  ----
#What was the p-value from the Bartlett test of homogeneity for observations 
#grouped by species? #You can round your answer to 4 decimal digits.
bartlett.test(body_mass_g ~ species, data = penguins)
p-value = 0.05005

# ---- Question 12 ----
#What was the p-value from the Bartlett test of homogeneity for observations 
#grouped by sex? You can round your answer to 4 decimal digits.
bartlett.test(body_mass_g ~ sex, data = penguins)
p-value = 0.03194

# ---- Question 13 ----
#What was the p-value from the Bartlett test of homogeneity for observations 
#grouped by both factors? You can round your answer to 4 decimal digits.

dat_groups = aggregate(
  body_mass_g ~ sex*species,
  data = penguins,
  FUN = c)
str(dat_groups)

bartlett.test(dat_groups$body_mass_g)

p-value = 0.1741

# ---- Question 14 ----
#Based on the results of the Bartlett tests, do you anticipate any issues with 
#heterogeneity in any of the models? Make sure you justify your response with 
#the results of your tests.

Based on the P value generated with the bartlet test this value supports not rejecting the null hypothesis

