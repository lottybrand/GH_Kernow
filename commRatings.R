#######################
#######################
######################## Community ratings script


#### Preparing commRatings file 
#commRatings <- read.delim("comm_influence.txt")

#####
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

#setwd("~/Desktop/Postdoc/CornwallCommunityStudy/Results/Kernow/DataFiles")

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
commRatings$X <- NULL
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


#####
#####Comm Prest Ordered logit
#####

commRatings<- read.csv("commRatings.csv")
#might need to do own indexing the old way:


NItems = length(unique(presComm$presItem))
OldItems <- presComm$presItem
ItemID <- array(0,length(presComm$presItem))
for (index in 1:NItems){
  ItemID[OldItems == unique(OldItems)[index]] = index
}
presComm$itemID <- ItemID

Nrater = length(unique(presComm$Rater_ID))
OldRater <- presComm$Rater_ID
RaterID <- array(0,length(presComm$Rater_ID))
for (index in 1:Nrater){
  RaterID[OldRater == unique(OldRater)[index]] = index
}
presComm$raterID <- RaterID

#presComm$itemID <- coerce_index(as.factor(presComm$presItem))
#presComm$raterID <- coerce_index(presComm$Rater_ID)

presComm$propD <- commRatings$Dprop[match(presComm$Rater_ID, commRatings$Rater_ID)]

commPrestMod <- map2stan(
  alist(
    prestigeRatings ~ dordlogit(phi, cutpoints),
    phi <- bd*propD + aR[raterID]*sigmaR + aItem[itemID]*sigmaItem,
    bd ~ dnorm(0,1),
    aR[raterID] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=presComm, 
  constraints = list(sigmaR = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(commPrestMod)

# #mean   sd  5.5% 94.5% n_eff Rhat
# bd        -3.07 0.34 -3.62 -2.54   293    1
# sigmaR     0.92 0.06  0.84  1.02   495    1
# sigmaItem  0.31 0.05  0.23  0.39   725    1

#precis(commPrestMod, depth=2)
  #              #mean   sd  5.5% 94.5% n_eff Rhat
  # cutpoints[1] -4.97 0.27 -5.40 -4.55   311    1
  # cutpoints[2] -3.91 0.26 -4.34 -3.52   298    1
  # cutpoints[3] -3.23 0.25 -3.64 -2.85   293    1
  # cutpoints[4] -2.10 0.24 -2.48 -1.73   287    1
  # cutpoints[5] -1.25 0.24 -1.63 -0.88   291    1
  # cutpoints[6]  0.25 0.23 -0.11  0.61   290    1
  # bd           -3.05 0.37 -3.64 -2.51   301    1

pC <- ggplot(data=presComm) +
  geom_bar(aes(x=prestigeRatings), fill="seagreen") + theme_bw() +
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_x_continuous(breaks=1:7, labels=c("1","2","3","4","5","6","7")) +
  scale_y_continuous(limits=c(0,400)) +
  xlab("Prestige Rating") + ylab("Total Count")
pC




#####################################
#####################################
########### Dominance Ratings
#####################################
#####################################

colnames(commRatings)
commRatings$X <- NULL
commRatings <- commRatings[,1:25]
colnames(commRatings)

colnames(commRatings)
domComm <- commRatings[,c(1,2,3,6,8,10,12,23,14,24,19)]

dominance <- c("R3", "R5","R7","R9","R10rev","R11","R12rev","R16")

domCommLong <- reshape(domComm, times = dominance,
                        varying = dominance,
                        v.names = c("dominanceRatings"), 
                        direction = "long")

domCommLong <- domCommLong[order(domCommLong$Rater_ID),]
colnames(domCommLong)[4] <- "domItem"
domComm <- domCommLong


commRatings<- read.csv("commRatings.csv")
#might need to do own indexing the old way:


NItems = length(unique(domComm$domItem))
OldItems <- domComm$domItem
ItemID <- array(0,length(domComm$domItem))
for (index in 1:NItems){
  ItemID[OldItems == unique(OldItems)[index]] = index
}
domComm$itemID <- ItemID

Nrater = length(unique(domComm$Rater_ID))
OldRater <- domComm$Rater_ID
RaterID <- array(0,length(domComm$Rater_ID))
for (index in 1:Nrater){
  RaterID[OldRater == unique(OldRater)[index]] = index
}
domComm$raterID <- RaterID

#presComm$itemID <- coerce_index(as.factor(presComm$presItem))
#presComm$raterID <- coerce_index(presComm$Rater_ID)

domComm$propP <- commRatings$Pprop[match(domComm$Rater_ID, commRatings$Rater_ID)]

commDomMod <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
    phi <- bp*propP + aR[raterID]*sigmaR + aItem[itemID]*sigmaItem,
    bp ~ dnorm(0,1),
    aR[raterID] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=domComm, 
  constraints = list(sigmaR = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(commDomMod)

# #mean   sd  5.5% 94.5% n_eff Rhat
# bp        -3.22 0.38 -3.86 -2.61   289    1
# sigmaR     0.85 0.05  0.76  0.94   652    1
# sigmaItem  0.45 0.05  0.38  0.54   465    1

precis(commDomMod, depth = 2)
#               Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# cutpoints[1] -3.95   0.32      -4.46      -3.42   350 1.00
# cutpoints[2] -3.03   0.32      -3.53      -2.50   351 1.00
# cutpoints[3] -2.47   0.32      -3.04      -2.03   341 1.00
# cutpoints[4] -1.60   0.31      -2.15      -1.16   351 1.00
# cutpoints[5] -0.97   0.31      -1.49      -0.54   340 1.00
# cutpoints[6]  0.20   0.31      -0.30       0.68   352 1.00
# bp           -3.20   0.40      -3.81      -2.55   369 1.00

# plot posterior predictions? (rethinking p.341)
post <- extract.samples(commDomMod)

plot(1, 1, type = "n", xlab = "proportion prestige", ylab = "probability", xlim = c(0,1), ylim = c(0,1), xaxp = c(0,1,1), yaxp = c(0,1,2), main = "influence ratings")

#kP <- 0:1 # values of propP to calculate over
kP <- seq(0,1,by = 0.01) # values of propP to calculate over

for( s in 1:100) {
  p <- as.data.frame(post)[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bp*kP
  pk <- pordlogit( 1:6, a=ak, phi=phi)
  for ( i in 1:6)
    lines( kP, pk[,i], col=col.alpha(rangi2,0.1))
}

# hard to distinguish lines given they're all blue, use different colors
color_list <- c("red","orange","yellow","green","blue","violet")
plot(1, 1, type = "n", xlab = "proportion prestige", ylab = "probability", xlim = c(0,1), ylim = c(0,1), xaxp = c(0,1,1), yaxp = c(0,1,2), main = "influence ratings")

for( s in 1:100) {
  p <- as.data.frame(post)[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bp*kP
  pk <- pordlogit( 1:6, a=ak, phi=phi)
  for ( i in 1:6)
    lines( kP, pk[,i], col=col.alpha(color_list[i]))
}

# hard to see given uncertainty, so try s=1
plot(1, 1, type = "n", xlab = "proportion prestige", ylab = "probability", xlim = c(0,1), ylim = c(0,1), xaxp = c(0,1,1), yaxp = c(0,1,2), main = "influence ratings")

for( s in 1:1) {
  p <- as.data.frame(post)[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bp*kP
  pk <- pordlogit( 1:6, a=ak, phi=phi)
  for ( i in 1:6)
    lines( kP, pk[,i], col=color_list[i])
}

#hists
domCommPlot <- simplehist(domComm$dominanceRatings, xlim = c(1,7), xlab = "response")

dC <- ggplot(data=domComm) +
  geom_bar(aes(x=dominanceRatings), fill="seagreen") + theme_bw() +
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_x_continuous(breaks=1:7, labels=c("1","2","3","4","5","6","7")) +
  scale_y_continuous(limits=c(0,400)) +
  xlab("Dominance Rating") + ylab("Total Count")
dC



SirDaveD <- domComm[domComm$Name == "David Attenborough",]
daveDHist <- simplehist(SirDaveD$dominanceRatings, xlim = c(1,7), xlab = "response")

TMay <- domComm[domComm$Name == "Theresa May",]
TMay <- simplehist(TMay$dominanceRatings, xlim = c(1,7), xlab = "response")

table(presComm$Name)
queenNames <- c("The Queen", "Queen Elizabeth", "Queen")
Queen <- presComm[presComm$Name %in% queenNames,]

Qplot <- simplehist(Queen$prestigeRatings, xlim = c(1,7), xlab = "response")

##### cumulative likelihood plot more useful here than model?

####hist plots for presentation


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
