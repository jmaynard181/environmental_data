require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

install.packages("simpleboot") #dont ever run this again as its installed
require(simpleboot)

head(penguin_dat)



#this creates new dataframe selecting only species that are adelie
dat_adelie = subset(penguin_dat, species == "Adelie")$flipper_length_mm
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")$flipper_length_mm


#creates a vector called pen_boot from dat_adelie and dat_chinstrap flipper dataframe
pen_boot = 
  two.boot(dat_adelie, dat_chinstrap, FUN = mean, R = 10000, na.rm = TRUE)

#breaks down the structure of pen_boot
str(pen_boot)


hist(pen_boot$t, xlab = "Differences in mean flipper length (mm) Adelie and Chinstrap Penguins", 
     ylab = "Frequency", main = "Histogram of 10000 bootstrap differences in mean penguin flipper length")

pen_boot$t

quantile(pen_boot$t, na.rm = TRUE, probs = 0.95)


ecdf(pen_boot$t)

pen_ecdf = ecdf(pen_boot$t)

pen_ecdf(-4.5)

1-pen_ecdf(-4.5)

1-pen_ecdf(-8)

mean(pen_boot$t)
  
median(pen_boot$t)


# ---- end penguin stuff ----


# ---- onto tree stuff ---- 

require(here)


veg = read.csv(
  here("data", "vegdata.csv")
)
head(veg)




     
     
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

head(dat_tree)

wilcox.test(pine ~ treatment, data = dat_tree)


boxplot(pine ~ treatment, data = veg)

require(simpleboot)

require(boot)

tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

boot.ci(tree_boot)


dat_tree = droplevels(subset(dat_veg, treatment %in% c("control", "clipped")))

str(dat_tree)


quantile(tree_boot$t, c(0.025, 0.975))






boxplot(pine ~ treatment, dat = dat_tree)





table(dat_tree$treatment)
print(dat_tree$treatment)

table(dat_tree$pine)
print(dat_tree$pine)

#Nonparametric two-sample test: Wilcoxon ranked sum test 
wilcox.test(dat_tree$pine ~ dat_tree$treatment)


tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )


require(boot)
boot.ci(tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")

quantile(tree_boot$t, 0.025)

#end tree stuff










#onto bird stuff

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))


dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

# ---- 12-17 component ----
# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd = sd(dat_all$s.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

mean(dat_all$b.sidi.standardized)
mean(dat_all$s.sidi.standardized)

sd(dat_all$b.sidi.standardized)
sd(dat_all$s.sidi.standardized)


plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)


dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)


dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)


m = 10000 
result = numeric(m) 


for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  
  result[i] = coef(fit_resampled_i)[2]
} 

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

quantile(result, c(.05))

