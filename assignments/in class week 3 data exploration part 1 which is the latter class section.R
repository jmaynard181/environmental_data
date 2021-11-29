dat_birds = read.csv("https://michaelfrancenelson.github.io/environmental_data/data/bird.sta.csv")
dat_habitat = read.csv("https://michaelfrancenelson.github.io/environmental_data/data/hab.sta.csv")
head(dat_birds)
head(dat_habitat)
hist(AMDI)
pairs(dat_habitat)
pairs(dat_birds)
head(dat_birds)
hist(dat_birds$GCKI)

hist(dat_birds$GCKI, breaks = 0:5 - 0.5, xlab = "Number of birds counted")


pairs(dat_habitat[, c()])
pairs(dat_habitat[, c("elev", "slope", "aspect")])
hist(AMCR)
