#playing with kernow ordered logit
library(rethinking)

setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/")

pdRatings <- read.csv("pdRatings.csv")

#for reference: the prestige and dominance items are:
#PrestigeRatings <- subset(pdRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
#DominanceRatings <- subset(pdRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))

colnames(pdRatings)
pdSub <- pdRatings[,3:32]
colnames(pdSub)
#get rid of unused non-reversed items:
pdSub <- pdSub[,-c(5,9,13,15,20)]
colnames(pdSub)

prestige <- c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")
?reshape
pdSubLong <- reshape(pdSub, times = prestige,
                     varying = prestige,
                     v.names = c("prestigeRatings"), 
                     direction = "long")

pdSubLong <- pdSubLong[order(pdSubLong$Group),]
colnames(pdSubLong)[17] <- "presItem"

dominance <- c("R3", "R5", "R7", "R9", "R10rev", "R11", "R12rev", "R16")

#hmmm this didn't work with pdSubLong instead of pdSub... 
pdSubLong2 <- reshape(pdSub, times = dominance,
                      varying = dominance,
                      v.names = c("dominanceRatings"),
                      direction = "long")

#now two diff datasets created, one with the prestige ratings, another with the dominance. Maybe that's fine...
#delete unwated bits:

colnames(pdSubLong)
pdSubLong[,4:16] <- NULL

colnames(pdSubLong2)
pdSubLong2[,4:17] <- NULL

#cool, should be able to do some ordinal with these 