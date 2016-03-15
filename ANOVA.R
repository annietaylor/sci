# 2/3/16
# ANOVA test SCI data for 4 stands

setwd('~/Documents/Thesis')
d <- read.csv('SCIDataforR.csv', skip = 1)
d <- d[1:477,1:8]
# numcols <- c('Diam') #, 'Age', 'Beetle', 'NeedleD')
# d[numcols] <- apply(d[numcols], 2, as.double())
d[,7] <- as.numeric(as.character(d[,7]))

stand <- substr(d[,1], 0, 1)

# Age box plot

par(family='Avenir')
summary(aov(d[,7]~stand))
# pdf('Rplot2.pdf', family='Avenir', 7, 7)
boxplot(d[,7]~stand, xlab = 'Pine Population', 
		ylab = 'Estimated Age (years)', 
		names = c('Christy', 'Kinton', 'Pelican', 'China'))
text(c(1, 2, 3, 4), c(37, 33.5, 46, 53), c('a', 'a', 'b', 'c'))
# dev.off()
TukeyHSD(aov(d[,7]~stand))

#Age structure alive/dead
par(family='Avenir')
summary(aov(d[,7]~stand))
# pdf('Rplot2.pdf', family='Avenir', 7, 7)
boxplot(d[,7]~stand, xlab = 'Pine Population', 
		ylab = 'Estimated Age (years)', 
		names = c('Christy', 'Kinton', 'Pelican', 'China'))
text(c(1, 2, 3, 4), c(37, 33.5, 46, 53), c('a', 'a', 'b', 'c'))
# dev.off()
TukeyHSD(aov(d[,7]~stand))




# Beetle = binomial logistic regression

d[,4] <- as.numeric(as.character(d[,4]))
summary(glm(d[,4]~stand, family = binomial, na.action = na.omit))
confint(glm(d[,4]~stand, family = binomial, na.action = na.omit))
# Beetle barplot
standmean <- tapply(d[,4], stand, mean)
#boxplot(d[,4]~stand)
par(family='Avenir')
standplot <- barplot(standmean, 
	ylim=c(0, 1), 
	xlab = 'Pine Population', 
	names = c('Christy', 'Kinton', 'Pelican', 'China'),
	ylab = 'Proportion of Pines with Bark Beetle'
	)
#display significance
text(c(0.71, 1.91, 3.11, 4.31), c(0.73, 0.70, 0.82, 0.42), c('a', 'a', 'a', 'b'))



# Age histograms

whichStand <- 'R'

par(family='Avenir')
hist(d$Age[stand==whichStand],
		breaks = seq(from=0, to=70, by=2.5), 
		xlim=c(10,max(d$Age, na.rm = TRUE)),
		ylim=c(0,60), 
		xlab = 'Estimated Age (years)', 
		main = 'China Pines')
hist(d$Age[stand== whichStand & d$Alive==0],
		breaks = seq(from=0, to=70, by=2.5),
		add= TRUE, col = 'gray'

)		
		
