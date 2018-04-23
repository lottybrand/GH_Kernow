#######################
#######################
######################## Community ratings script


#### Preparing commRatings file 
#commRatings <- read.delim("comm_influence.txt")


### Calculate p&d scores
### Reverse score items 2,6,10,12,17 as according to Cheng et al. 2013 (should really write a function for this..!!)

#commRatings$R2rev <- ifelse((commRatings$R2==1),7,
#                          ifelse((commRatings$R2==2),6,
#                                 ifelse((commRatings$R2==3),5,
#                                        ifelse((commRatings$R2==4),4,
#                                               ifelse((commRatings$R2==5),3,
#                                                      ifelse((commRatings$R2==6),2,
#                                                             ifelse((commRatings$R2==7),1,NA)))))))
# 
# 
# commRatings$R6rev <- ifelse((commRatings$R6==1),7,
#                           ifelse((commRatings$R6==2),6,
#                                  ifelse((commRatings$R6==3),5,
#                                         ifelse((commRatings$R6==4),4,
#                                                ifelse((commRatings$R6==5),3,
#                                                       ifelse((commRatings$R6==6),2,
#                                                              ifelse((commRatings$R6==7),1,NA)))))))
# 
# commRatings$R10rev <- ifelse((commRatings$R10==1),7,
#                           ifelse((commRatings$R10==2),6,
#                                  ifelse((commRatings$R10==3),5,
#                                         ifelse((commRatings$R10==4),4,
#                                                ifelse((commRatings$R10==5),3,
#                                                       ifelse((commRatings$R10==6),2,
#                                                              ifelse((commRatings$R10==7),1,NA)))))))
#  
# commRatings$R12rev <- ifelse((commRatings$R12==1),7,
#                            ifelse((commRatings$R12==2),6,
#                                   ifelse((commRatings$R12==3),5,
#                                          ifelse((commRatings$R12==4),4,
#                                                 ifelse((commRatings$R12==5),3,
#                                                        ifelse((commRatings$R12==6),2,
#                                                               ifelse((commRatings$R12==7),1,NA)))))))
#  
# commRatings$R17rev <- ifelse((commRatings$R17==1),7,
#                            ifelse((commRatings$R17==2),6,
#                                   ifelse((commRatings$R17==3),5,
#                                          ifelse((commRatings$R17==4),4,
#                                                 ifelse((commRatings$R17==5),3,
#                                                        ifelse((commRatings$R17==6),2,
#                                                               ifelse((commRatings$R17==7),1,NA)))))))
#  
# 
# commRatings[commRatings == 999] <- NA
# commRatings <- na.omit(commRatings)
# 
# 
# commRatings$dominanceMean <- rowMeans(commRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
# commRatings$prestigeMean <- rowMeans(commRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)
# 
# 
# commRatings$Psum <- rowSums(commRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)
# commRatings$Pprop <- (commRatings$Psum - 9)/54
#  
# commRatings$Dsum <- rowSums(commRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
# commRatings$Dprop <-(commRatings$Dsum -8)/48
# 
# write.csv(commRatings, "commRatings.csv")


#####################
######################
######################

library(dplyr)
library(psy)
library(rethinking)

commRatings <- read.csv("commRatings.csv")

#cronbach test stuff
PrestigeRatings <- subset(commRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
DominanceRatings <- subset(commRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))

cronbach(PrestigeRatings)
cronbach(DominanceRatings)

hist(commRatings$prestigeMean)
hist(commRatings$dominanceMean)

SirDave <- commRatings[commRatings$Name == "David Attenborough",]
hist(SirDave$prestigeMean)
hist(SirDave$dominanceMean)

TheQueen <- commRatings[commRatings$Name == "The Queen",]
hist(TheQueen$prestigeMean)
hist(TheQueen$dominanceMean)

TMay <- commRatings[commRatings$Name == "Theresa May",]
hist(TMay$prestigeMean)
hist(TMay$dominanceMean)

####################################
#####################################
####################################
#ordinal version: PRESTIGE FIRST
#################################
#################################


colnames(commRatings)

commRatings <- commRatings[,1:25]
colnames(commRatings)
presComm <- commRatings[,c(1,2,3,4,21,7,22,11,16,17,18,25)]


prestige <- c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")

presCommLong <- reshape(presComm, times = prestige,
                     varying = prestige,
                     v.names = c("prestigeRatings"), 
                     direction = "long")

presCommLong <- presCommLong[order(presCommLong$Rater_ID),]
colnames(presCommLong)[4] <- "presItem"
colnames(presCommLong)[2] <- "Name"
presComm <- presCommLong

#histPlots
presCommPlot <- simplehist(presComm$prestigeRatings, xlim = c(1,7), xlab = "response")

SirDave <- presComm[presComm$Name == "David Attenborough",]
daveHist <- simplehist(SirDave$prestigeRatings, xlim = c(1,7), xlab = "response")

TMay <- presComm[presComm$Name == "Theresa May",]
TMay <- simplehist(TMay$prestigeRatings, xlim = c(1,7), xlab = "response")

table(presComm$Name)
queenNames <- c("The Queen", "Queen Elizabeth", "Queen")
Queen <- presComm[presComm$Name %in% queenNames,]

Qplot <- simplehist(Queen$prestigeRatings, xlim = c(1,7), xlab = "response")


####Comm Prest Ordered 

presComm$itemID <- coerce_index(presComm$presItem)
presComm$raterID <- coerce_index(presComm$Rater_ID)

commPrestMod <- map2stan(
  alist(
    prestigeRatings ~ dordlogit(phi, cutpoints),
    phi <- aR[raterID]*sigmaR + aItem[itemID]*sigmaItem,
    aR[raterID] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=presComm, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(commPrestMod, depth = 2)



###################
###################
########### Dom Ratings
###################
##################

colnames(commRatings)
domComm <- commRatings[,c(2,3,4,7,9,11,13,24,15,25,20)]

dominance <- c("R3", "R5","R7","R9","R10rev","R11","R12rev","R16")

domCommLong <- reshape(domComm, times = dominance,
                        varying = dominance,
                        v.names = c("dominanceRatings"), 
                        direction = "long")

domCommLong <- domCommLong[order(domCommLong$Rater_ID),]
colnames(domCommLong)[4] <- "domItem"
domComm <- domCommLong

#hists
domCommPlot <- simplehist(domComm$dominanceRatings, xlim = c(1,7), xlab = "response")

SirDaveD <- domComm[domComm$Name == "David Attenborough",]
daveDHist <- simplehist(SirDaveD$dominanceRatings, xlim = c(1,7), xlab = "response")

TMay <- domComm[domComm$Name == "Theresa May",]
TMay <- simplehist(TMay$dominanceRatings, xlim = c(1,7), xlab = "response")

table(presComm$Name)
queenNames <- c("The Queen", "Queen Elizabeth", "Queen")
Queen <- presComm[presComm$Name %in% queenNames,]

Qplot <- simplehist(Queen$prestigeRatings, xlim = c(1,7), xlab = "response")

##### cumulative likelihood plot more useful here than model?
