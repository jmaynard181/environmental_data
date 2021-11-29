a <- "Justin"
b1 <- 45.6
b2 = "45.6"
c1 = c(1:3)
class(a)
class(b1)
class(b2)
b1 + b2
v1 = c(-2:2)
v2 = 3*v1
v2 
sum(v2)
vec_4 = c(1:12)


mat_1 <- matrix(vec_4, nrow = 3, byrow = TRUE)
mat_1

mat_2 = matrix(vec_4, nrow = 3, byrow = FALSE)
mat_2


my_list_1 = list(5.2, "five point two", 1:5)
names(my_list_1) = c("two", "one", "three")
my_list_1[[3]]

my_list_1$three

my_vec = rep(1:3, 5)
my_vec

my_bool_vec == my_vec


data.frame(my_vec, my_bool_vec)
my_bool_vec[3]
