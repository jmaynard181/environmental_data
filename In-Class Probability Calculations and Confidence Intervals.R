# ---- binomial probability

#Q2: 6 plots, observe birds in about 2/3, (observe 4 presence in 6 plots)
dbinom(4, 6, 2/3)

#Q3: no birds in my plots
dbinom(0, 6, 2/3)

# ---- cumulative probability: the p functions

#observe count 7 or fewer if have poisson dist. pop w lamda 10.4
ppois(q = 7, lambda = 10.4)

#Q4: calculate the probability of observing four or fewer presences in the 6 plots
#pbinom is cumulative density
pbinom(4, 6, 2/3)

#Q5: pbinom & law of total probability to calculate probability observing 4 or more presences in the 6 plots.
#tells us probability of 3 or fewer
pbinom(3, 6, 2/3)

dbinom(4, 6, 2/3)+
dbinom(5, 6, 2/3)+
dbinom(6, 6, 2/3)




