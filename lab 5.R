# ---- Creates a linear function ----
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

# ---- Create a Ricker Function ----
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}



#plot of the shape using the simplest possible parameters: a = 1 and b = 1:
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

dev.off()


#ricker function
(a * x * exp(-b * x))

#exp_fun this will calculate the values of an exponential function
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}


#Self test for above function
curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

#choose 50 uniformly-distributed random x-values within the interval 2 to 10:

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

#choose an intercept and slope for our deterministic model and generate the ‘predicted’ y values:
param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")


#Add normally-distributed noise to generate our ‘observed’ y-values:
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

#Question 2 possibility 1
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}


#Question 2 possibility 2
curve(exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50,
      ann = FALSE, axes = TRUE, ylab = "f(x)", col=1, lty = 1)

curve(exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50, ylab = "f(x)",
      col=1, lty = 2)

curve(exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50, ylab = "f(x)",
      col=2, lty = 1)

curve(exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50, ylab = "f(x)",
      col=2, lty = 2)

#ricker function plot with 6 examples    
curve(
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 10, add = FALSE, 
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=1,
  lty=1)
  
curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 10, add = TRUE, 
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=1,
  lty=2)

curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 10, add = TRUE, 
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=1,
  lty=2)
  
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 10, add = TRUE, 
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=2,
  lty=1)
  
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 10, add = TRUE, 
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=2,
  lty=2)
  
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 10, add = TRUE, 
  main = "Single plot 6 Ricker curves",
  ylab = "f(x)", xlab = "x",
  col=2,
  lty=2)


# ---- Question 8 ----
require(here)
dat_dispersal = read.csv(here("data", "dispersal.csv"))


# ---- Runs line point slope aka linear model ---- 
plot(dat_dispersal$dist.class,dat_dispersal$disp.rate.ftb)
curve(
  line_point_slope(x, 800, 0.5, 0), 
   add = TRUE, col=2, lty=1)

# ---- Runs exponential model ----
curve(
  line_point_slope(x, 0.6, 500, 0), add = TRUE, col=2, lty=1)

plot(dat_dispersal$dist.class,dat_dispersal$disp.rate.ftb)
curve(
  exp_fun(x, 1.8, 1/250), add = TRUE)


# ---- Runs ricker model ----
plot(dat_dispersal$dist.class,dat_dispersal$disp.rate.ftb)

ricker_fun = function(x, a, b)
{return(a * x * exp(-b * x))
  }
  
curve(
  ricker_fun(x, .008, 0.007), 
  add = TRUE, 
  col=1,
  lty=1)

# ---- Calculate the residuals for your three fitted models and store them in a data.frame ----

guess_x = .007
guess_y = 0.008
guess_slope = 0

dat_dispersal$y_predicted_linear = exp_fun(dat_dispersal$dist.class, .007, .008, 0)
dat_dispersal

dat_dispersal$y_predicted_linear = line_point_slope(dat_dispersal$dist.class, 1.8, .1/250, 0)
dat_dispersal

dat_dispersal$y_predicted_linear = ricker_fun(dat_dispersal$dist.class, .5, 800)
dat_dispersal

hist(dat_dispersal$dist.class,
    main = "Residual line point slope",
    xlab = "Values",
    ylab = "Residuals",
    col = 1)

hist(dat_dispersal$resids_linear)
hist(dat_dispersal$resids_exp)
hist(dat_dispersal$resids_ricker)


line_point_slope()

# ---- creating vectors of predicted values ----



sal_resids = data.frame(x = dat_dispersal$dist.class, y_observed = dat_dispersal$disp.rate.ftb)

sal_resids$y_predicted_linear = line_point_slope(sal_resids$x, 0.5, 800, 0)
sal_resids
sal_resids$resids_linear= (sal_resids$y_observed - sal_resids$y_predicted_linear)

sal_resids$y_predicted_exp = exp_fun(sal_resids$x, 1.8, 1/250)
sal_resids
sal_resids$resids_exp = (sal_resids$y_observed - sal_resids$y_predicted_exp)

sal_resids$y_predicted_ricker = ricker_fun(sal_resids$x, 0.008, .007)
sal_resids
sal_resids$resids_ricker = (sal_resids$y_observed - sal_resids$y_predicted_ricker)

sal_resids

#histograms
par(mfrow=c3,1)
hist(sal_resids$resids_linear, 
     main = "linear histogram", 
     xlab = "resids",
     ylab = "frequency")

par(mfrow=c3,1)
hist(sal_resids$resids_exp, 
     main = "linear histogram", 
     xlab = "resids",
     ylab = "frequency")

par(mfrow=c3,1)
hist(sal_resids$resids_ricker, 
     main = "linear histogram", 
     xlab = "resids",
     ylab = "frequency")

