# Use here() and read.csv() to read the three files into data.frame objects called:
dat_justin <- read.csv(here("data", "justin.csv"))
# catrate.csv
# delomys.csv
# rope.csv
# this loads data into my data frame as long as its saved in data folder r project
dat_catrate <- read.csv(here("data", "catrate.csv"))
dat_delomys <- read.csv(here("data", "delomys.csv"))
dat_rope <- read.csv(here("data", "rope.csv"))
head(dat_catrate)
head(dat_delomys)
head(dat_rope)
# How to create a scatterplot and label my axes
plot(x = dat_catrate$years, y = dat_catrate$cat.rate, main = "Justin Maynard", ylab = "cat.rate", xlab = "year")
hist(dat_catrate$years)
