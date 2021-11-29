

# Creates a scatter plot
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)

# Adds a red dot to the center of the plot at the coordinates
# at x and y
points(x = data_center_x, y = data_center_y, col = "red")

#This draws a red dot at the coordinates 2 and 5
points(x = 2, y = 5, col = "red")

# Created a variable trial_x valued at 3.6
trial_x = 3.6

# Created a variable trial_y valued at 6.9
trial_y = 6.9

# My next task is to describe what is in here
points(x = trial_x, y = trial_y, col = "red")

#find the difference between line 11 and 20

# Drew a linear curve through the center of the plot
# It knew to go through the red dot
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.1), 
  add = TRUE)


