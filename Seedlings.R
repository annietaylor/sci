# 2/3/16
# GML test SCI seedling data for 4 stands

setwd('~/Documents/Thesis')
d <- read.csv('SeedlingsforR.csv')
d <- d[1:133,1:3]
d[,2] <- as.numeric(as.character(d[,2]))

stand <- substr(d[,1], 0, 1)

summary(glm(d[,2]~stand, family = poisson, na.action = na.omit))
confint(glm(d[,2]~stand, family = poisson, na.action = na.omit))


boxplot(d[,2]~stand, xlab = 'Pine Population', 
		ylab = 'Seedling Count', 
		names = c('Christy', 'Kinton', 'Pelican', 'China'))
text(c(1, 2, 3, 4), c(37, 33.5, 46, 53), c('a', 'a', 'b', 'b'))

# give means to barplot
# SEEDLING BARPLOT

standmean <- tapply(d[,2], stand, mean)
stderror <- tapply(d[,2], stand, function(x) sd(x)/(length(x)^(0.5)))

par(family='Avenir')
standplot <- barplot(standmean, 
	ylim=c(0,7.0), 
	xlab = 'Pine Population', 
	names = c('Christy', 'Kinton', 'Pelican', 'China'),
	ylab = 'Mean Seedling Count (+/- SE)'
	)
arrows(x0=standplot, y1=standmean-stderror, y0=standmean+stderror, angle=90, code = 3)

#display significance
text(c(0.71, 1.91, 3.11, 4.31), c(6.55, 4.1, 1, 2.6), c('a', 'b', 'c', 'd'))
