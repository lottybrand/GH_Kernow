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

hist(kernowResults$Age, xlim = c(0,100), ylim = c(0,40), xlab = "Age")

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

#centre score & overconf
gPresD$ScoreC <- gPresD$Score - mean(gPresD$Score)
gPresD$overconf <- gPresD$OverConf+40
meanOConf <- mean(gPresD$overconf, na.rm = TRUE)
gPresD$OverConfC <- gPresD$overconf - meanOConf

gPresD_NA <- na.omit(gPresD)

#coerce_index 
gPresD_NA$raterId <- coerce_index(gPresD_NA$rater_id)
gPresD_NA$ratedID <- coerce_index(gPresD_NA$rated_ID)
gPresD_NA$grpID <- coerce_index(gPresD_NA$Group)
gPresD_NA$itemID <- coerce_index(gPresD_NA$presItem)



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
  data=gPresD_NA, 
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
  data=gPresD_NA, 
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
  data=gPresD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(pM_Apriori)
plot(precis(pM_Apriori))
#Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
#bS        -0.02   0.02      -0.05       0.01   279 1.00
#bI         3.24   0.41       2.61       3.90   291 1.00
#bL         7.12   0.70       6.04       8.23   242 1.00
#sigmaID    0.53   0.06       0.44       0.61   359 1.00
#sigmaR     1.07   0.08       0.92       1.18   228 1.00
#sigmaG     0.18   0.12       0.00       0.34    84 1.05
#sigmaItem  1.30   0.36       0.79       1.79   217 1.0

#try full multi level: 
pMFull <- map2stan(
  alist(
    prestigeRatings ~ dordlogit(phi, cutpoints),
    phi <- bS*ScoreC + b_o*OverConf + bI*Inflntl + bL*Liked + bN*nominated + 
      bIn*initInf + bInl*initLrn + bSx*Sex + bA*Age +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    c(bS, b_o, bI, bL, bN, bIn, bInl, bSx, bA) ~ dnorm(0,10),
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gPresD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(pMFull)
plot(precis(pMFull))
compare(pM_null, pM_Apriori, pM_exp, pMFull, refresh=0.1)
#WAIC pWAIC dWAIC weight     SE   dSE
#pM_Apriori 11034.1 201.9   0.0   0.81 123.54    NA
#pMFull     11037.0 202.8   2.8   0.19 123.97  4.43
#pM_null    11054.7 224.5  20.6   0.00 123.63 10.64
#pM_exp     11058.2 224.9  24.0   0.00 123.88 10.89



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


#centre score & over confidence:
gDomD$ScoreC <- gDomD$Score - mean(gDomD$Score)
gDomD$overconf <- gDomD$OverConf+40
meanOConf <- mean(gDomD$overconf, na.rm = TRUE)
gDomD$OverConfC <- gDomD$overconf - meanOConf

gDomD_NA <- na.omit(gDomD)

#coerce_index 
gDomD_NA$raterId <- coerce_index(gDomD_NA$rater_id)
gDomD_NA$ratedID <- coerce_index(gDomD_NA$rated_ID)
gDomD_NA$grpID <- coerce_index(gDomD_NA$Group)
gDomD_NA$itemID <- coerce_index(gDomD_NA$domItem)

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
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(dM_null)

# A Priori
dM_aPriori <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
    phi <- b_o*OverConfC + bI*Inflntl +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(b_o, bI) ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(dM_aPriori)
plot(precis(dM_aPriori))

# initial ratings? (Expl)

dM_exp <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
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
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(dM_exp)

#Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
#bIn       0.72   0.30       0.24       1.20  1000    1
#bInl      0.13   0.42      -0.56       0.77  1000    1
#sigmaID   1.10   0.09       0.97       1.24   424    1
#sigmaR    1.08   0.09       0.94       1.22   405    1
#sigmaG    0.19   0.15       0.00       0.40   134    1
#sigmaItem 1.60   0.45       0.94       2.17   501    1

dM_FULL <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
    phi <- bs*Score + b_o*OverConfC + bI*Inflntl + bL*Liked + b_n*nominated +
      bIn*initInf + bInlrn*initLrn + bsx*Sex + ba*Age +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(bs, b_o, bI, bL, bIn, bInlrn, bsx, ba, b_n) ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ dcauchy(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 1, cores = 1)

precis(dM_FULL)
plot(precis(dM_FULL))
#Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
#bs        -0.01   0.03      -0.05       0.03   877    1
#b_o        0.02   0.01       0.00       0.05  1000    1
#bI         2.31   0.57       1.30       3.13   810    1
#bL        -2.29   0.75      -3.41      -1.04  1000    1
#bIn        0.55   0.26       0.15       0.95  1000    1
#bInlrn    -0.11   0.35      -0.67       0.43  1000    1
#bsx        0.27   0.22      -0.12       0.57  1000    1
#ba         0.02   0.01       0.00       0.02   591    1
#sigmaID    0.90   0.08       0.78       1.04   253    1
#sigmaR     1.09   0.08       0.98       1.24   520    1
#sigmaG     0.24   0.18       0.00       0.48   110    1
#sigmaItem  1.60   0.48       0.99       2.26   559    1

compare(dM_null, dM_aPriori, dM_exp, dM_FULL)
#WAIC pWAIC dWAIC weight     SE  dSE
#dM_FULL    10374.7 228.7   0.0   0.78 114.56   NA
#dM_aPriori 10379.1 231.5   4.4   0.09 114.51 3.28
#dM_exp     10379.2 231.8   4.5   0.08 114.68 4.01
#dM_null    10380.1 232.8   5.4   0.05 114.63 4.62
