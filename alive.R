# 2/23/16
# alive dead elevation aspect
# and other bioclim variables (precip and temp etc.)


setwd('~/Documents/Thesis')
d <- read.csv('alive.csv')
d <- d[1:1244,1:4]
#d[,2] <- as.numeric(as.character(d[,2]))

stand <- substr(d[,1], 0, 1)
elev <- (d[,3])
aspect <- (d[,4])
# add other bioclim variables here


d[,2] <- as.numeric(as.character(d[,2]))
summary(glm(d[,2]~stand+elev, family = binomial, na.action = na.omit))
confint(glm(d[,2]~stand+elev, family = binomial, na.action = na.omit))

summary(glm(d[,2]~stand, family = binomial, na.action = na.omit))
confint(glm(d[,2]~stand, family = binomial, na.action = na.omit))

standmean <- tapply(d[,2], stand, mean)
#boxplot(d[,4]~stand)


par(family='Avenir')
standplot <- barplot(standmean, 
	ylim=c(0, 1), 
	xlab = 'Pine Population', 
	names = c('SCI Schist (C)', 'Rincon Formation (K)', 'SCI Volcanics (P)', 'Monterey Shale (R)'),
	ylab = 'Proportion of Living Pines'
	)

#display significance
text(c(0.71, 1.91, 3.11, 4.31), c(0.44, 0.34, 0.27, 0.74), c('a', 'a', 'a', 'b'))

glmfit <-glm(d[,2]~elev, family = binomial, na.action = na.omit)

summary(glm(d[,2]~ elev, family = binomial, na.action = na.omit))
confint(glm(d[,2]~ elev, family = binomial, na.action = na.omit))


# Loess fit - dead or alive across continuous variable below
predvar <- elev

plot(predvar, d[,2])
linefit <- loess(d[,2]~ predvar)
lines(seq(0, 500, 1), predict(linefit,seq(0,500,1)))


par(mfrow=c(2,1))
hist(aspect[d[,2]==0], breaks=30)
hist(aspect[d[,2]==1], breaks = 30)
