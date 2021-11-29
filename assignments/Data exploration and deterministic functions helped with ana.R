require(here)
dat_habitat <- read.csv(here("data", "hab.sta.csv"))
head(dat_habitat)

#creating histograms for 3 terrain variables
png(filename = "threehistogram.png")
par(mfrow = c(3, 1))
hist(dat_habitat$elev)
hist(dat_habitat$slope)
hist(dat_habitat$aspect)

dev.off()

#this plots multiple histograms as one plot
par(mfrow = c(3, 1))

#creating plots
plot(x = dat_habitat$elev, y = dat_habitat$ba.tot)
plot(x = dat_habitat$slope, y = dat_habitat$ba.tot)
plot(x = dat_habitat$aspect, y = dat_habitat$ba.tot)

#creating new point and drawing a line through it
##Physically create a center for the data
data_center_x_aspect = mean(dat_habitat$aspect)
data_center_y = mean(dat_habitat$ba.tot)


data_center_x_slope = mean(dat_habitat$slope)
data_center_y = mean(dat_habitat$ba.tot)


data_center_x_elev = mean(dat_habitat$elev)
data_center_y = mean(dat_habitat$ba.tot)


##Now I'm going to plot it on the plot
points(x = data_center_x_aspect, y = data_center_y, col = "red")

# ---- Now a line will be drawn through the center plot point i created
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
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    0.1), 
  add = TRUE)

#####################################

#creating plots
png(filename = "threescatterplots.png")

par(mfrow = c(3, 1))

# everything related to elevation scatterplot
plot(x = dat_habitat$elev, y = dat_habitat$ba.tot, xlab = "elevation in feet",
     ylab = "Basal Area in sq ft", main = "elevation vs basal area for habitat")

points(x = data_center_x_elev, y = data_center_y, col = "red")

  
curve(
  line_point_slope(
    x, 
    data_center_x_elev, 
    data_center_y,
    0.1), 
  add = TRUE)

# everything related to slope scatterplot
plot(x = dat_habitat$slope, y = dat_habitat$ba.tot, xlab = "slope in percent",
     ylab = "Basal Area in sq ft", main = "slope vs basal area for habitat")

points(x = data_center_x_slope, y = data_center_y, col = "red")

curve(
  line_point_slope(
    x, 
    data_center_x_slope, 
    data_center_y,
    0.1), 
  add = TRUE)

# everything related to aspect scatterplot
plot(x = dat_habitat$aspect, y = dat_habitat$ba.tot, xlab = "Degrees of
     aspect", ylab = "Basal Area in sq ft", main = "aspect vs basal area for habitat")

points(x = data_center_x_aspect, y = data_center_y, col = "red")

curve(
  line_point_slope(
    x, 
    data_center_x_aspect, 
    data_center_y,
    0.1), 
  add = TRUE)

dev.off()

#####################################


#creating new point and drawing a line through it
##Physically create a center for the data


data_center_x = mean(dat_habitat$slope)
data_center_y = mean(dat_habitat$ba.tot)
c(data_center_x, data_center_y)

data_center_x = mean(dat_habitat$elev)
data_center_y = mean(dat_habitat$ba.tot)
c(data_center_x, data_center_y)
