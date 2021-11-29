#To install packages in preparation of data manipulation
#install.packages("psych")

require(here)
dat_bird = read.csv(here("data", "bird.sta.csv"))
head(dat_bird)

require(here)
dat_habitat = read.csv(here("data", "hab.sta.csv"))
head(dat_habitat)

#This will merge two types of data
dat_all = merge(dat_bird, dat_habitat)

#Tests whether data was successfully merged
plot(ba.tot ~ elev, data = dat_all)

#For sample, here are counts of Cedar Waxwings at 100 randomly sampled sites:
sample(dat_all$CEWA, 100)

dat_all["CEWA"]>=1

as.numeric(dat_all["CEWA"]>=1)

cewa_present_absent = as.numeric(dat_all["CEWA"]>=1)

head(cewa_present_absent)

plot(x = dat_all$elev, y = cewa_present_absent)

get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

#An effort to plot basal area and bird presence 

plot(x = dat_all$ba.tot, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 100, slope = 0.1), add = TRUE)

plot(x = dat_all$ba.tot, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.1), add = TRUE)

plot(x = dat_all$ba.tot, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.05), add = TRUE)



# To subset a data frame using specific column names
    dat_terrain = dat_all[c("elev", "slope", "aspect", "ba.tot")]
    head(dat_terrain)
pairs.panels(dat_terrain)

#when assigning a value to a variable, write data source location, followed
#by dollar sign which pulls the specific column info to the right of it
var1 = dat_terrain$elev
var2 = dat_terrain$slope
var3 = dat_terrain$aspect
var4 = dat_terrain$ba.tot

plot(x = dat_all$ba.tot, y = amgo_present_absent, main = "AMGO Basal Area Presence Absense Plot",
xlab = "Basal Area", ylab = "AMGO Presence Absence")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.05), add = TRUE)

dat_all$GRJA
GRJA_present_absent = as.numeric(dat_all["GRJA"]>=1)
sum(dat_all$GRJA)
sum(GRJA_present_absent)

sum(dat_all$AMCR)
sample(dat_all$AMCR, 100)

dat_all["AMCR"]>=1

as.numeric(dat_all["AMCR"]>=1)

amcr_present_absent = as.numeric(dat_all["AMCR"]>=1)

head(AMCR_present_absent)

plot(x = dat_all$elev, y = AMCR_present_absent)

plot(x = dat_all$ba.tot, y = amcr_present_absent, main = "AMCR Basal Area Presence Absense Plot",
     xlab = "Basal Area", ylab = "AMCR Presence Absence")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.05), add = TRUE)
