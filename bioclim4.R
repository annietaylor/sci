# analyze bioclim variables by 4 measured trees

setwd('~/Documents/Thesis')
d <- read.csv('BioClimByPlot.csv')
d <- d[1:477,1:27]

stand <- substr(d[,2], 0, 1)

# Vars

# 1: Mean annual temp (°C)
# 2: Mean diurnal range (Mean of monthly (max temp - min temp)) (°C)
# 4: Temperature Seasonality (standard deviation *100) 
# 7: Temperature Annual Range (max monthly temp - min monthly temp) (°C)
# 8: Mean Temperature of Wettest Quarter (°C)
# 9: Mean Temperature of Driest Quarter (°C)
# 12: Annual Precipitation (mm)
# 15: Precipitation Seasonality (coefficient of variation)

anntemp <- (d[,19])
precip <- (d[,25])



# Loess fit - dead or alive across continuous variable below
predvar <- anntemp

plot(predvar, d[,4])
linefit <- loess(d[,4]~ predvar)
lines(seq(0, 600, 1), predict(linefit,seq(0,600,1)))

