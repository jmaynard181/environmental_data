#loading in a dataset
data(iris)
#Previewing a dataset - use function head()
head(iris)
#to get entries in the column
iris$Sepal.Width
#to calculate the mean and standard deviation of vectors
mean(iris$Sepal.Length)
sd(iris$Sepal.Width)
#to build a scatterplot of iris sepal width and length
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
#to find center of the data
data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)
#to add to an existing plot
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")


line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)

#to add a custom function into r workspace
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
#to add a curve to an existing plot
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.1), 
  add = TRUE)


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


data(iris)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)

plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")

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

# This code works


plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.1), 
  add = TRUE)

plot(x = iris$Sepal.Length, y = iris$Sepal.Width)
points(x =data_center_x, y = data_center_y, main = "justins plot", xlab = "sepal length", ylab = "sepal width", col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    3), 
  add = TRUE)
