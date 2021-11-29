# ---- Question 1 ----
#Run the following code to create a large vector containing randomly generated integers between 1 and 12:
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

# Test for equality for which elements are equal to 3
vec_1==3

# This characteristic is now assigned to vec_2
vec_2=(vec_1==3)

# This is the self test to print all 3's
vec_1[vec_2]

#Run the following code to create a large vector containing randomly generated integers between 1 and 12:
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

# ---- Question 6 ----
#This is the example code for loop function

for (i in 1:10)
{
  print(paste ("This is loop iteration", i))
}


# ---- Question 7 ----
# This loop sequence uses a variable to dictate times loop is ran
n=10
for (i in 1:n)
{
  print(paste("This is loop iteration", i))
}
  
# ---- Question 8 ----
#Creating variables for min and max values
#Creating a variable opposed to hard numbers increases,
#usefullness of this code and allows for multiple uses later on

n = 17
min_x = 1
max_x = 10

vec_1 = sample(x = min_x : max_x, size = n, replace = TRUE)

for (i in 1:n)
{
  print(paste("The element of vec_1 at index", i, "is", vec_1[i]))
}




#Now use the code you used to create a loop the executes n times to
#print out each element of vec_1. Each iteration through the loop 
#should print the corresponding element of vec_1.

Your output should look something like this:
  
  ## The element of vec_1 at index 1 is 7.
  ## The element of vec_1 at index 2 is 7.
  ## The element of vec_1 at index 3 is 8.
  ## The element of vec_1 at index 4 is 1.
  ## The element of vec_1 at index 5 is 1.
  ## The element of vec_1 at index 6 is 7.
