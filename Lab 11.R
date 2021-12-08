require(here)
rm(list = ls())

#reads data from csv file and assigns it to a variable
bird.sub = read.csv(here("data", "bird.sub.csv"))

#reads data from csv file and assigns it to a variable
hab.sub = read.csv(here("data", "hab.sub.csv"))

#takes the 2 variables above and creates 1 new dataframe
birdhab = merge(bird.sub, hab.sub)

#identifies dataframe dimensions
dim(birdhab)

#create scatterplot brown creeper abundance
plot(BRCR ~ ls, data = birdhab,
     main = "brown creeper abundance",
     xlab = "late succesional forest extent",
     ylab = "Brown Creeper Abundance")

#creates a variable using a linear model with a regression line (abline)
fit_1 = 
abline(lm(
  formula = BRCR ~ ls,
  data = birdhab))

#Let’s build an R function to calculate the value of a linear function given a 
#value of x, a slope parameter, and an intercept parameter.
#This serves as a template function that the x, y and slope can have values
#changed to produce an outcome without modifying the linear function each time.
linear = function(x, y_int, slope)
{
  return(y_int + slope*x)
} 
  
linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)

#stochastic model normal distribution


rnorm(
  n = length(x), 
  mean = linear(x = x, y_int = y_int, slope = slope), 
  sd = st_dev)

#Simulation Function
#Now you can assemble your deterministic and stochastic functions into
#a single data simulator

set.seed(1)

x
y_int
slope
st_dev

linear_simulator()






