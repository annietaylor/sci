# 3.7.16
# run RF classifier

require(randomForest)

setwd('~/Documents/Thesis')
naipGee <- read.csv('NAIPValuesFinal.csv')
naipGee <- naipGee[1:418, 1:9]


# topredict has to be a factor
topredict <- naipGee
# print(topredict)


NAIPclassified <- randomForest(as.factor(Class) ~ R + G + B + N + NDVI + GRVI, 
	data=naipGee, importance=TRUE, ntree=2000, na.action=na.omit, keep.forest=TRUE)
varImpPlot(NAIPclassified)

partialPlot(x=NAIPclassified, pred.data=subset(naipGee,is.finite(naipGee$NDVI)), x.var = 'R',which.class='2')
#  confusion matrix? specificity/sensitivity

bands <- list('R', 'G', 'B', 'N', 'NDVI', 'GRVI')
classes <- list('0', '1', '2')

#print all on one plot
png('/Users/annietaylor/Documents/Thesis/test.png', 1000, 800, family='Avenir', units = "px")
par(mfrow=c(6,3), mar=c(0.2,0.2,0.2,0.2), omi=c(0.3,0.6,0.25,0.25), bty='n', cex.axis=0.75, mgp=c(3,0.75,0))
     
for(i in 1:6)  
{
  for(j in 1:3) 
  {
     partialPlot(x=NAIPclassified, pred.data=naipGee, 
     			x.var = bands[[i]], which.class=classes[[j]],
     			xlab = paste(band, class, sep=' effect on '))
  }
}
dev.off()


#print and save each separately

for(i in 1:6)  
{
  for(j in 1:3) 
  {
     print(bands[[i]])
     print(classes[[j]])
     band <- bands[[i]]
     class <- classes[[j]]
     filename <- paste('pdplot', band, class, '.png', sep='')
     print(filename)
     png(filename, 700, 700, family='Avenir', units = "px")
     par(mfrow=c(6,3)), mar=c(0.2,0.2,0.2,0.2), omi=c(0.3,0.6,0.25,0.25), bty='n', cex.axis=0.75, mgp=c(3,0.75,0))
     partialPlot(x=NAIPclassified, pred.data=naipGee, 
     			x.var = bands[[i]], which.class=classes[[j]],
     			xlab = paste(band, class, sep=' effect on '),
     			main = 'Partial Dependence Plot')
     dev.off()
  }
}




