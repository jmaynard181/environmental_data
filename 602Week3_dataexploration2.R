#I only have to run installers one time
#install.packages("here")
#to load a package must use the require function each time
require("here")
here()

# Reading from the inside out, here is looking in the data subdirectory,
#for hab.sta.csv and read.csv this reads the data from the file and turning,
#into a dataframe. Dat_habitat is the data file read in and available to R.
dat_habitat = read.csv(here("data", "hab.sta.csv"))

#This is histogram code that works
hist(dat_habitat$elev)
hist(dat_habitat$slope)
hist(dat_habitat$aspect)

#This provides data information by previewing the first few rows
head(dat_habitat)

#This is scatterplot code that works
plot(x = dat_habitat$elev, y = dat_habitat$ba.tot, main = "Sampling site Elevation and Basal Area", ylab = "elevation", xlab = "basal area")
plot(x = dat_habitat$slope, y = dat_habitat$ba.tot, main = "Sampling site Slope and Basal Area", ylab = "slope", xlab = "basal area")
plot(x = dat_habitat$aspect, y = dat_habitat$ba.tot, main = "Sampling site Aspect and Basal Area", ylab = "aspect", xlab = "basal area")


