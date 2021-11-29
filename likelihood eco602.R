#warblers observed at two sites, 2 and 6 quantities
x_observed = c(2, 6)
print(x_observed)

#think count of 2 is low, I decide propose Poisson dist with λ=4.5 
#as a model of the counts of Wilson’s Warblers on my study sites.
#I can use dpois() to calculate the probability mass for the two counts 
#given a Poisson distribution with λ=4.5
dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)

#I know the likelihood of observing those particular counts together is 
#the product of the individual likelihoods:
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

#I can take advantage of vectorization in R by storing the counts in a 
#single vector:
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)

#I can more easily calculate the product now:
prod(dpois(x = wiwa_counts, lambda = 4.5))

#And the sum of log-likelihoods like:
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

#Now let’s say I want to find the value of that maximizes the likelihood
#for the counts of Wilson’s Warblers.
#I first need to load the bird data:
dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

#Next I need to do some numerical and graphical data exploration.
#I’ll start with a 5-number summary, and then plot a histogram.
#The summary:
summary(dat_all$WIWA)

#Next, we’ll plot a histogram of census counts.
#Here’s the histogram that R produces with default settings:
hist(dat_all$WIWA)

#try setting the breaks argument to 7. This will suggest to R that it 
#should create 7 bins (corresponding to observations between 0 and 7 wrens):
hist(dat_all$WIWA, breaks = 7)

#Need to un-group the zero and one counts so that we see them as distinct bars 
#in the histogram. We used a single number for the breaks argument to tell to 
#try to automatically figure out how to divide the counts into 7 bins.
#You can use a vector for the breaks argument. R attempts to estimate bin 
#breakpoints when breaks is a single number, but it will honor your input
#if you provide a vector of breakpoints:
hist(dat_all$WIWA, breaks = 0:7)

#We can trick R into only counting the lower endpoint if we cleverly manipulate 
#the sequence that we give to bins.
#If we write code that subtracts single value (a scalar in linear algebra lingo) 
#from a vector R subtracts number from each element in vector and return the output.
#We can trick R into only counting the lower endpoint if we cleverly manipulate 
#the sequence that we give to bins.
hist(dat_all$WIWA, breaks = 0:7 - .5)

#This works because the data were discrete counts. It also looks nicer because
#the bars are now centered over the census counts.

#This trick doesn’t work as well for continuous data.
#Histograms with (discrete) count data
#If we wanted to use code like this with data for which we didn’t know the
#maximum value ahead of time we could write:
par(mfrow = c(1, 2))

dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

#I’ll try a Poisson distribution with lambda = 1.0:
sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))


