require(here)
#Setup, i'll need this down below for questions
#code to fit a linear deterministic function
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

#creates 4 vectors of normally-distributed random numbers
norm_17 = rnorm(17, 10.4, 2.4)
norm_30 = rnorm(30, 10.4, 2.4)
norm_300 = rnorm(300, 10.4, 2.4)
norm_3000 = rnorm(3000, 10.4, 2.4)

#creates histograms of these 4 vectors
hist(norm_17, main = "17 randomly generated data points")
hist(norm_30, main = "30 randomly generated data points")
hist(norm_300, main = "300 randomly generated data points")
hist(norm_3000, main = "3000 randomly generated data points")


# creates a png file. follow with dev.off()
png(filename = here("lab_04_hist_01.png"),
    width = 1500, height = 1600, 
    res = 180, units = "px")

#creates 4 histograms arranged in a panel 2x2

par(mfrow = c(2, 2))

hist(norm_17)
hist(norm_30)
hist(norm_300)
hist(norm_3000)

dev.off()



# Generate a vector of x-values


x = seq(5, 15, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4, log = FALSE)


plot(x, y, main = "mean is 10.4 and standard deviation is 2.4",
     type = "l", xlim = c(5, 15))
abline(h = 0)

svg(filename = "norm_1.svg", width = 7, height = 7)

dev.off()

#start of question 9 and 10

set.seed(5)
n_pts = 8
x_min = 1
x_max = 10
set.seed(5)
n_pts = 8
x = rnorm(n = n_pts, mean = 10.4, sd = 2.4)
dat1 = data.frame(x = x, y_observed = rnorm(n_pts))

set.seed(2)
n_pts = 10
x = rnorm(n = n_pts, mean = 10.4, sd = 2.4)
dat2 = data.frame(x = x, y_observed = rnorm(n_pts))

set.seed(3)
n_pts = 250
x = rnorm(n = n_pts, mean = 10.4, sd = 15)
dat3 = data.frame(x = x, y_observed = rnorm(n_pts))

set.seed(9)
x = rnorm(n = n_pts, mean = 10.4, sd = 2.4)
dat4 = data.frame(x = x, y_observed = rnorm(n_pts))
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat)
head(dat)

dev.off()
svg(filename = "four_histograms.svg", width = 7, height = 7)
par(mfrow = c(2, 2))

hist(dat1$x, xlab = "x", ylab = "y",  main = "histogram 1")
hist(dat2$x, xlab = "x", ylab = "y", main = "histogram 2")
hist(dat3$x, xlab = "x", ylab = "y", main = "histogram 3")
hist(dat4$x, xlab = "x", ylab = "y", main = "histogram 4")
dev.off()
set.seed(5)
n_pts = 8
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat)
head(dat)
hist(dat4$x, xlab = "x", ylab = "y", main = "histogram 4")

dev.off()


#Example dataset from Q9 and 10 to answer q 11 and 12
set.seed(5)
n_pts = 8
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat)



guess_x = 6.5
guess_y = .4
guess_slope = -.5


curve(
  line_point_slope(
    x, 
    guess_x, 
    guess_y,
    guess_slope), 
  add = TRUE)

dev.off()

#code to fit a linear deterministic function
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
head(dat)

#used linear fitting function and the parameters I chose guess x, guess y, 
#guess slope. I used these model parameters to generate predicted y values,
#that correspond to the x values in the dataframe
line_point_slope(dat$x, guess_x, guess_y, guess_slope)
#Added these values to a new column I created in the dataframe
dat$y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope)
head(dat)

#the difference between observed and predicted y values
dat$y_observed - dat$y_predicted

#creates new column that is the vertical distance between the line and the observed points
dat$y_residuals = dat$y_observed - dat$y_predicted
head()

svg(filename = "histogram and scatterplot.svg", width = 7, height = 7)

hist(dat$y_residuals, xlab = "residuals", ylab = "frequency", main = "histogram of model residuals")
hist(dat$y_residuals)

plot(dat$y_predicted, dat$y_residuals, xlab = "predicted values", ylab = "residual", main = "scatterplot of predicted values")

svg(filename = "scatterplot and histogram", width = 7, height = 7)

png(filename = here("scatterplot and histogram.png"),
    width = 1500, height = 1600, 
    res = 180, units = "px")

par(mfrow = c(1, 2))

dev.off()
