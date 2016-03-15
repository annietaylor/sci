# 2/23/16
# Chi Square for A/D on each stand


setwd('~/Documents/Thesis')
tbl <- read.csv('chi.csv')
tbl <- tbl[,1:2] # exclude labels
chisq.test(tbl)
