# ---- Step 1 creating a data frame called catrate using catrate.csv file in data folder ----
require(here)
dat_catrate <- read.csv(here("data", "catrate.csv"))
                     
head(dat_catrate)

summary(dat_catrate)

# ---- Question 1 ----
hist(dat_catrate$cat.rate, main = "histogram of catastrophe rates", xlab = "catastrophe rate", ylab = "frequency")

# ---- Question 5 ----

shapiro.test(dat_catrate$cat.rate)

t.test(x = dat_catrate$cat.rate, mu = (2/7))

# ---- question 11 ----

wilcox.test(dat_catrate$cat.rate, mu = 2/7)

# ---- Question 16 ----
require(here)
require(palmerpenguins)
head(penguins)



dat_adelie = subset(penguins, species == "Adelie")
dat_chinstrap = subset(penguins, species == "Chinstrap")

shapiro.test(dat_adelie$bill_length_mm)
shapiro.test(dat_chinstrap$bill_length_mm)

# ---- Question 17 ---- 

# ---- Question 18 ---- 
png(filename = "double_penguin_histogram.png")
par(mfrow = c(1,2))
hist(dat_adelie$bill_length_mm, main = "histogram of flipper lengths adelie penguins", xlab = "flipper lengths mm", ylab = "number of penguins")
hist(dat_chinstrap$bill_length_mm, main = "histogram of flipper lengths chinstrap penguins", xlab = "flipper lengths mm", ylab = "number of penguins")
dev.off()

# ---- Question 19 ---- 

dat_penguins = droplevels(subset(penguins, species != "Gentoo"))
dat_penguins

t.test(x = dat_penguins$bill_length_mm)

t.test(bill_length_mm ~ species, data = dat_penguins)


