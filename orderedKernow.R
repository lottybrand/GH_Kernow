#playing with kernow ordered logit
library(rethinking)

setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/")

pdRatings <- read.csv("pdRatings.csv")
kernowResults <- read.csv("kernowResults.csv")

###### RESHAPING BELOW FOR ORDERED CATEGORICAL ANALYSIS OF ORDINAL RATINGS
#####



#for reference: the prestige and dominance items are:
#PrestigeRatings <- subset(pdRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
#DominanceRatings <- subset(pdRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))
# 
# colnames(pdRatings)
# pdSub <- pdRatings[,3:32]
# colnames(pdSub)
# #get rid of unused non-reversed items:
# pdSub <- pdSub[,-c(5,9,13,15,20)]
# colnames(pdSub)
# 
# prestige <- c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")
# ?reshape
# pdSubLong <- reshape(pdSub, times = prestige,
#                      varying = prestige,
#                      v.names = c("prestigeRatings"), 
#                      direction = "long")
# 
# pdSubLong <- pdSubLong[order(pdSubLong$rater_id),]
# colnames(pdSubLong)[17] <- "presItem"
# 
# dominance <- c("R3", "R5", "R7", "R9", "R10rev", "R11", "R12rev", "R16")
# 
# pdSubLong2 <- reshape(pdSub, times = dominance,
#                       varying = dominance,
#                       v.names = c("dominanceRatings"),
#                       direction = "long")
# 
# #Two diff datasets created, one with the prestige ratings, another with the dominance
# (There are more items in the prestige than the dominance scales)
#
# #delete unwated bits:
# colnames(pdSubLong)
# pdSubLong[,4:16] <- NULL
# 
# colnames(pdSubLong2)
# pdSubLong2[,4:17] <- NULL
# colnames(pdSubLong2)[4] <- "domItem"
# 
# #cool, should be able to do some ordinal with these 
# 
# gPresD <- pdSubLong 
# gDomD <- pdSubLong2
# 
# write.csv(gPresD, "gPresD.csv")
# write.csv(gDomD, "gDomD.csv")
#####
gPresD <- read.csv("gPresD.csv")
gDomD <- read.csv("gDomD.csv")

# Now we need to add the other info in from the other datasets for predictors.
# We need, score, overconfidence, influence, likeability, sex, age, nominated 

##########
########## PRESTIGE FILE FIRST
##########

#using match from the kernowResults file: 
gPresD$Age <- kernowResults$Age[match(gPresD$rated_ID, kernowResults$ID)]
#check using order:
gPresD <- gPresD[order(gPresD$rated_ID),]
#GREAT! OKay, this is lovely. 
gPresD$Sex <- kernowResults$Sex[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$nominated <- kernowResults$Nominated[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$initInf <- kernowResults$initial_influential[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$initLrn <- kernowResults$initial_learn[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$Liked <- kernowResults$aveLik[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$Inflntl <- kernowResults$aveInf[match(gPresD$rated_ID, kernowResults$ID)]

#May as well add centred and scaled versions straight away as these done previously:
gPresD$ScoreCS <- kernowResults$ScoreCS[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$AgeCS <- kernowResults$AgeCS[match(gPresD$rated_ID, kernowResults$ID)]
gPresD$OverCS <- kernowResults$OverCS[match(gPresD$rated_ID,kernowResults$ID)]

#now centre and scale liked and inflntl too: 
gPresD$Liked <- scale(gPresD$Liked, center = TRUE, scale = TRUE)
gPresD$Inflntl <- scale(gPresD$Inflntl, center = TRUE, scale = TRUE)

#Overconfidence Binned
gPresD$OConfBIN <- kernowResults$OConfBIN[match(gPresD$rated_ID,kernowResults$ID)]

#remove NAs for Stan Models
gPresD_NA <- na.omit(gPresD)

#coerce_index for random effecs 
gPresD_NA$raterId <- coerce_index(gPresD_NA$rater_id)
gPresD_NA$ratedID <- coerce_index(gPresD_NA$rated_ID)
gPresD_NA$grpID <- coerce_index(gPresD_NA$Group)
gPresD_NA$itemID <- coerce_index(gPresD_NA$presItem)

#####
###now ready to save! Can jump straight to here in future:
#####
write.csv(gPresD_NA, "gPresD_NA.csv")
gPresD_NA <- read.csv("gPresD_NA.csv")

#removing those extra weird dot columns (leftover from aggregate/match functions earlier on?)
gPresD_NA$X.1 <- NULL
gPresD_NA$X <- NULL
#####
#### On with the models!! 
#####

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
  chains = 1, cores = 1, iter=800)

precis(pM_null)
#Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# sigmaID   1.02   0.08       0.90       1.16   203 1.00
# sigmaR    1.10   0.09       0.96       1.24   317 1.00
# sigmaG    0.32   0.19       0.00       0.57    54 1.00
# sigmaItem 1.36   0.37       0.81       1.85   210 1.02

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
# Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# bIn       0.79   0.27       0.36       1.20   223 1.00
# bInl      0.79   0.37       0.21       1.36   267 1.00
# sigmaID   0.96   0.08       0.84       1.10   268 1.00
# sigmaR    1.09   0.08       0.95       1.21   241 1.00
# sigmaG    0.31   0.19       0.00       0.55    67 1.01
# sigmaItem 1.29   0.33       0.81       1.75   272 1.00


##### A Priori Model
pM_Apriori <- map2stan(
  alist(
    prestigeRatings ~ dordlogit(phi, cutpoints),
    phi <- bS*ScoreCS + bI*Inflntl + bL*Liked +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    c(bS, bI, bL) ~ dnorm(0,1),
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gPresD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"), 
  control=list(adapt_delta=0.99, max_treedepth=13), 
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  chains = 3, cores = 3, iter=800)

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
    phi <- bS*ScoreCS + b_o*OConfBIN + bI*Inflntl + bL*Liked + bN*nominated + 
      bIn*initInf + bInl*initLrn + bSx*Sex + bA*AgeCS +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    c(bS, b_o, bI, bL, bN, bIn, bInl, bSx, bA) ~ dnorm(0,1),
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gPresD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(pMFull)
plot(precis(pMFull))

saveRDS(pMFull, file = "SAVED_pMFULL.rds")
save(pMFull, file = "plainSave_pMFULL")
pMFULL <- readRDS("SAVED_pMFULL.rds")

# Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# bS        -0.06   0.06      -0.16       0.02   790 1.00
# b_o       -0.06   0.14      -0.27       0.18   948 1.00
# bI         0.45   0.06       0.36       0.55   792 1.00
# bL         0.67   0.06       0.56       0.76   939 1.00
# bN        -0.10   0.13      -0.30       0.12   960 1.00
# bIn        0.31   0.15       0.05       0.52  1211 1.00
# bInl       0.39   0.20       0.11       0.74  1293 1.00
# bSx        0.17   0.12      -0.03       0.36   854 1.00
# bA         0.15   0.08       0.02       0.29   526 1.01
# sigmaID    0.39   0.04       0.33       0.46   774 1.00
# sigmaR     0.77   0.04       0.70       0.84   710 1.00
# sigmaG     0.10   0.07       0.00       0.19   288 1.01
# sigmaItem  0.55   0.05       0.47       0.62   954 1.00

plot(precis(pMFull))
compare(pM_null, pM_Apriori, pM_exp, pMFull, refresh=0.1)
# WAIC pWAIC dWAIC weight     SE   dSE
# pMFull     11034.1 202.0   0.0   0.65 124.13    NA
# pM_Apriori 11035.3 202.4   1.2   0.35 123.65  4.40
# pM_null    11053.3 222.8  19.2   0.00 123.63 11.45
# pM_exp     11054.7 223.4  20.6   0.00 123.92 10.74



############################
############################
#### Dominance Models ######
############################
###########################

#Need to match with the other factors like for gPresD above:

gDomD$Age <- kernowResults$Age[match(gDomD$rated_ID, kernowResults$ID)]
#check using order:
gDomD <- gDomD[order(gDomD$rated_ID),]
#GREAT! OKay, this is lovely. 
gDomD$Sex <- kernowResults$Sex[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$nominated <- kernowResults$Nominated[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$initInf <- kernowResults$initial_influential[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$initLrn <- kernowResults$initial_learn[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$Liked <- kernowResults$aveLik[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$Inflntl <- kernowResults$aveInf[match(gDomD$rated_ID, kernowResults$ID)]

#May as well add centred and scaled versions straight away as these done previously:
gDomD$ScoreCS <- kernowResults$ScoreCS[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$AgeCS <- kernowResults$AgeCS[match(gDomD$rated_ID, kernowResults$ID)]
gDomD$OverCS <- kernowResults$OverCS[match(gDomD$rated_ID,kernowResults$ID)]

#now centre and scale liked and inflntl too: 
gDomD$Liked <- scale(gDomD$Liked, center = TRUE, scale = TRUE)
gDomD$Inflntl <- scale(gDomD$Inflntl, center = TRUE, scale = TRUE)

#Overconfidence Binned
gDomD$OConfBIN <- kernowResults$OConfBIN[match(gDomD$rated_ID,kernowResults$ID)]

#remove NAs for Stan Models
gDomD_NA <- na.omit(gDomD)

#coerce_index for random effecs 
gDomD_NA$raterId <- coerce_index(gDomD_NA$rater_id)
gDomD_NA$ratedID <- coerce_index(gDomD_NA$rated_ID)
gDomD_NA$grpID <- coerce_index(gDomD_NA$Group)
gDomD_NA$itemID <- coerce_index(gDomD_NA$domItem)

#####
##### Now you can write and skip to this for model running next time:
write.csv(gDomD_NA, "gDomD_NA.csv")
gDomD_NA <- read.csv("gDomD_NA.csv")

##### remove those extra columns from processing:
gDomD_NA$X.1 <- NULL
gDomD_NA$X <- NULL

#####
#####
#### On to the models!
#####

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
dM_aPriori2 <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
    phi <- b_o*OverCS + bI*Inflntl +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(b_o, bI) ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(dM_aPriori2)

# Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# b_o       0.12   0.07       0.01       0.25   539 1.00
# bI        0.46   0.07       0.35       0.58   748 1.00
# sigmaID   0.72   0.05       0.64       0.79   799 1.00
# sigmaR    0.77   0.04       0.71       0.84   984 1.00
# sigmaG    0.09   0.07       0.00       0.18   398 1.00
# sigmaItem 0.58   0.05       0.50       0.66   708 1.01

precis(dM_aPriori2)
plot(precis(dM_aPriori2))

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
    phi <- bs*ScoreCS + b_o*OverCS + bI*Inflntl + bL*Liked + b_n*nominated +
      bIn*initInf + bInlrn*initLrn + bsx*Sex + ba*AgeCS +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(bs, b_o, bI, bL, bIn, bInlrn, bsx, ba, b_n) ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter = 1200)

precis(dM_FULL)
plot(precis(dM_FULL))

# Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# bs        -0.11   0.08      -0.25       0.01   709 1.00
# b_o        0.09   0.07      -0.03       0.21   748 1.01
# bI         0.55   0.08       0.43       0.69   735 1.00
# bL        -0.48   0.08      -0.60      -0.36   863 1.00
# bIn        0.49   0.20       0.16       0.80   623 1.00
# bInlrn    -0.08   0.27      -0.51       0.33   750 1.00
# bsx        0.26   0.17       0.00       0.52   533 1.01
# ba         0.26   0.10       0.10       0.42   564 1.00
# b_n        0.04   0.19      -0.25       0.34   860 1.00
# sigmaID    0.62   0.05       0.55       0.69   821 1.01
# sigmaR     0.78   0.04       0.71       0.85   773 1.00
# sigmaG     0.12   0.08       0.00       0.23   260 1.00
# sigmaItem  0.59   0.05       0.51       0.67  1258 1.00

compare(dM_null, dM_aPriori, dM_exp, dM_FULL)
# WAIC pWAIC dWAIC weight     SE  dSE
# dM_FULL    10373.9 227.4   0.0   0.59 114.43   NA
# dM_aPriori 10375.4 229.1   1.5   0.28 114.52 3.41
# dM_null    10377.0 232.0   3.1   0.12 114.77 4.88
# dM_exp     10383.4 234.6   9.5   0.01 114.75 4.10

saveRDS(dM_FULL, file = "dMFULL.rds")

dM_FULL <- readRDS("SAVED_dMFULL.rds")


#histPlots
presPlot <- simplehist(gPresD$prestigeRatings, xlim = c(1,7), xlab = "response")
presPlot
domPlot <- simplehist(gDomD$dominanceRatings, xlim = c(1,7), xlab = "response")
domPlot
agePlot <- simplehist(kernowResults$Age, xlab = "Age")