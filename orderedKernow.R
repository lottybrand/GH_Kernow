#playing with kernow ordered logit
library(rethinking)

setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/")

pdRatings <- read.csv("pdRatings.csv")
kernowResults <- read.csv("kernowResults.csv")
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
colnames(pdSubLong2)[4] <- "domItem"

#cool, should be able to do some ordinal with these 

gPresD <- pdSubLong 
gDomD <- pdSubLong2

#histPlots
presPlot <- simplehist(gPresD$prestigeRatings, xlim = c(1,7), xlab = "response")
domPlot <- simplehist(gDomD$dominanceRatings, xlim = c(1,7), xlab = "response")
domPlot


#okay let's try and add the other info in from the other datasets for predictors.
#for Full prestige model, we need, score, overconfidence, influence, likeability, sex, age. 
#and nominated tooo..

#using match from the full kernowResults file: 
gPresD$Age <- kernowResults$Age[match(gPresD$rated_ID, kernowResults$ID)]
#check using order:
gPresD <- gPresD[order(gPresD$rated_ID),]
#GREAT! OKay, this is lovely. 
gPresD$Sex <- kernowResults$Sex[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$Score <- kernowResults$IndividScore[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$OverConf <- kernowResults$Overconfidence[match(gPresD$rated_ID,kernowResults$ID)]
gPresD$nominated <- kernowResults$Nominated[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$initInf <- kernowResults$initial_influential[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$initLrn <- kernowResults$intial_learn[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$Liked <- kernowResults$aveLik[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$Inflntl <- kernowResults$aveInf[match(gPresD$rated_ID, kernowResults$ID)]



#coerce_index 
gPresD$raterId <- coerce_index(gPresD$rater_id)
gPresD$ratedID <- coerce_index(gPresD$rated_ID)
gPresD$grpID <- coerce_index(gPresD$Group)
gPresD$itemID <- coerce_index(gPresD$presItem)

#centre score:
gPresD$ScoreC <- gPresD$Score - mean(gPresD$Score)

#Null
pM_null <- map2stan(
  alist(
    prestigeRatings ~ dordlogit(phi, cutpoints),
    phi <- aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gPresD, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(pM_null)

# initial ratings? (Expl)

pM_exp <- map2stan(
  alist(
    prestigeRatings ~ dordlogit(phi, cutpoints),
    phi <-  bIn*initInf + bInl*initLrn +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    c(bIn, bInl) ~ dnorm(0,10),
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gPresD, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(pM_exp)

# A Priori Model
pM_Apriori <- map2stan(
  alist(
    prestigeRatings ~ dordlogit(phi, cutpoints),
    phi <- bS*ScoreC + bI*Inflntl + bL*Liked +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    c(bS, bI, bL) ~ dnorm(0,10),
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gPresD, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(pM_Apriori)

#try full multi level: 
pMFull <- map2stan(
  alist(
    prestigeRatings ~ dordlogit(phi, cutpoints),
    phi <- bS*ScoreC + bI*Inflntl + bL*Liked + bN*nominated + 
      bIn*initInf + bInl*initLrn +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    c(bS, bI, bL, bN, bIn, bInl) ~ dnorm(0,10),
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gPresD, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(pMFull)
plot(precis(pMFull))
compare(pM_null, pM_Apriori, pM_exp, pMFull, refresh=0.1)

############################
############################
############################
############################
#### Dominance Models ######
############################
###########################
############################
###########################

#and what about adding gDomD to gPresD now using match?
###CAN'T DO THAT IT's A DIFF NUMBER OF ITEMS. ??RIGHT?

#So need to go back through with each like for gPresD:


#using match from the full kernowResults file: 
gDomD$Age <- kernowResults$Age[match(gDomD$rated_ID, kernowResults$ID)]
#check using order:
gDomD <- gDomD[order(gDomD$rated_ID),]
#GREAT! OKay, this is lovely. 
gDomD$Sex <- kernowResults$Sex[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$Score <- kernowResults$IndividScore[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$OverConf <- kernowResults$Overconfidence[match(gDomD$rated_ID,kernowResults$ID)]
gDomD$nominated <- kernowResults$Nominated[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$initInf <- kernowResults$initial_influential[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$initLrn <- kernowResults$intial_learn[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$Liked <- kernowResults$aveLik[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$Inflntl <- kernowResults$aveInf[match(gDomD$rated_ID, kernowResults$ID)]



#coerce_index 
gDomD$raterId <- coerce_index(gDomD$rater_id)
gDomD$ratedID <- coerce_index(gDomD$rated_ID)
gDomD$grpID <- coerce_index(gDomD$Group)
gDomD$itemID <- coerce_index(gDomD$domItem)

#centre score:
gDomD$ScoreC <- gDomD$Score - mean(gDomD$Score)
gDomD$OverConfC <- gDomD$OverConf - mean(gDomD$OverConf)

#Null
dM_null <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
    phi <- aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(dM_null)

# A Priori
dM_aPriori <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
    phi <- bO*OverConfC + bI*Inflntl +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(bO, bI) ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(dM_aPriori)
