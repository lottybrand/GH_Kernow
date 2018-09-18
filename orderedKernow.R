# THESE ARE THE ORDERED CATEGORICAL VERSIONS OF THE MODELS 
library(rethinking)

setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/DataFiles")

#pdRatings <- read.csv("pdRatings.csv")
#kernowResults <- read.csv("kernowResults.csv")

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
#write.csv(gPresD_NA, "gPresD_NA.csv", row.names=FALSE)
gPresD_NA <- read.csv("gPresD_NA.csv")

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
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gPresD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

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
    c(bIn, bInl) ~ dnorm(0,1),
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
  chains = 3, cores = 3, iter=1200)

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
    phi <- bS*ScoreCS + b_o*OverCS + bI*Inflntl + bL*Liked + bN*nominated + 
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

plot(precis(pMFull), pars=c("bI","bL","bIn","bInl","bA","bSx","bS","b_o","bN"), labels=c("Nominated","Confidence","Score","Sex","Age","Learning model","Initially influential","Likeability","Influence"))

# mean   sd  5.5% 94.5% n_eff Rhat
# bS        -0.09 0.06 -0.19  0.01   939 1.00
# b_o       -0.07 0.06 -0.16  0.02   993 1.00
# bI         0.46 0.06  0.37  0.56   777 1.00
# bL         0.67 0.06  0.57  0.77  1206 1.00
# bN        -0.08 0.14 -0.31  0.14   886 1.00
# bIn        0.29 0.15  0.05  0.55  1347 1.00
# bInl       0.42 0.20  0.11  0.73  1364 1.00
# bSx        0.16 0.12 -0.04  0.35   835 1.00
# bA         0.16 0.08  0.03  0.29   795 1.00
# sigmaID    0.39 0.04  0.32  0.46   913 1.00
# sigmaR     0.77 0.04  0.70  0.84   999 1.00
# sigmaG     0.09 0.07  0.01  0.22   261 1.01
# sigmaItem  0.54 0.05  0.47  0.62  1133 1.00

plot(precis(pMFull))
compare(pM_null, pM_Apriori, pM_exp, pMFull, refresh=0.1)
# WAIC pWAIC dWAIC weight     SE   dSE
# pMFull     11055.4 177.0   0.0   0.63 119.61    NA
# pM_Apriori 11056.4 176.5   1.0   0.37 118.95  5.28
# pM_exp     11081.7 198.1  26.4   0.00 118.09 13.80
# pM_null    11084.2 199.2  28.8   0.00 117.62 14.57

### trying the posterior prediction plots

### Influence
post <- extract.samples(pMFull)

kI <- seq(-3,3,by=0.1) #values of influence

color_list <- c("red","orange","yellow","green","blue","violet")
plot(1, 1, type = "n", xlab = "Influential", ylab = "Cumulative Probability", xlim = c(-3,3), ylim = c(0,1), xaxp = c(-3,3,4), yaxp = c(0,1,2), main = "Prestige Ratings")

pI_means <- matrix(0, nrow = 61, ncol = 6) 

for( s in 1:100) {
  p <- as.data.frame(post)[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bI*kI
  pI <- pordlogit( 1:6, a=ak, phi=phi)
  pI_means <- pI_means + pI
  for ( i in 1:6)
    lines( kI, pI[,i], col=col.alpha(color_list[i], alpha = 0.05))
}

# add thick lines for means
for (i in 1:6)
  lines( kI, pI_means[,i]/101, col = color_list[i], lwd = 3)

# add labels for numbers
text(-2.4, 0.01, labels = "1")
text(-2, 0.03, labels = "2")
text(-1.6, 0.06, labels = "3")
text(-0.6, 0.1, labels = "4")
text(0.2, 0.2, labels = "5")
text(0.9, 0.5, labels = "6")
text(1.6, 0.8, labels = "7")


# Liked
post <- extract.samples(pMFull)

kL <- seq(-3,3,by = 0.1) # values of Liked to calculate over

color_list <- c("red","orange","yellow","green","blue","violet")
plot(1, 1, type = "n", xlab = "Liked", ylab = "Cumulative Probability", xlim = c(-3,3), ylim = c(0,1), xaxp = c(-3,3,4), yaxp = c(0,1,2), main = "Prestige Ratings")

pL_means <- matrix(0, nrow = 61, ncol = 6) 

for( s in 1:100) {
  p <- as.data.frame(post)[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bL*kL
  pL <- pordlogit( 1:6, a=ak, phi=phi)
  pL_means <- pL_means + pL
  for ( i in 1:6)
    lines( kL, pL[,i], col=col.alpha(color_list[i], alpha = 0.05))
}

# add thick lines for means
for (i in 1:6)
  lines( kL, pL_means[,i]/101, col = color_list[i], lwd = 3)

# add labels for numbers
text(-2.4, 0.01, labels = "1")
text(-2, 0.04, labels = "2")
text(-1.6, 0.07, labels = "3")
text(-0.6, 0.1, labels = "4")
text(0.2, 0.2, labels = "5")
text(0.9, 0.5, labels = "6")
text(1.6, 0.8, labels = "7")


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
#write.csv(gDomD_NA, "gDomD_NA.csv", row.names = FALSE)
gDomD_NA <- read.csv("gDomD_NA.csv")


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
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(dM_null)
# #mean   sd 5.5% 94.5% n_eff Rhat
# sigmaID   0.80 0.04 0.73  0.87   889    1
# sigmaR    0.76 0.04 0.69  0.83   989    1
# sigmaG    0.08 0.06 0.01  0.19   479    1
# sigmaItem 0.59 0.05 0.51  0.66  1366    1

# A Priori
dM_aPriori <- map2stan(
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

precis(dM_aPriori)

# mean   sd 5.5% 94.5% n_eff Rhat
# b_o       0.12 0.07 0.01  0.24   730 1.00
# bI        0.46 0.07 0.34  0.57   587 1.00
# sigmaID   0.72 0.04 0.66  0.79   824 1.00
# sigmaR    0.77 0.04 0.70  0.85  1252 1.00
# sigmaG    0.09 0.07 0.01  0.22   371 1.01
# sigmaItem 0.59 0.05 0.51  0.67   955 1.00

plot(precis(dM_aPriori))

# initial ratings? (Expl)

dM_exp <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
    phi <-  bIn*initInf + bInl*initLrn +
      aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    c(bIn, bInl) ~ dnorm(0,1),
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(dM_exp)

# mean   sd  5.5% 94.5% n_eff Rhat
# bIn       0.66 0.21  0.34  0.99   798 1.00
# bInl      0.11 0.30 -0.37  0.58   829 1.00
# sigmaID   0.77 0.05  0.70  0.85  1247 1.00
# sigmaR    0.76 0.05  0.69  0.83   819 1.00
# sigmaG    0.09 0.07  0.01  0.21   481 1.01
# sigmaItem 0.58 0.05  0.51  0.67   949 1.00

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

plot(precis(dM_FULL), pars=c("bI","bIn","ba","bsx","bs","b_o","b_n","bInlrn","bL"), labels=c("Likeability","Learning model","Nominated","Confidence","Score","Sex","Age","Initially influential","Influence"))


# mean   sd  5.5% 94.5% n_eff Rhat
# bs        -0.11 0.09 -0.24  0.03   797 1.00
# b_o        0.08 0.08 -0.05  0.20   574 1.00
# bI         0.55 0.08  0.41  0.68   740 1.00
# bL        -0.48 0.08 -0.61 -0.35   741 1.00
# bIn        0.50 0.20  0.17  0.84   828 1.00
# bInlrn    -0.09 0.27 -0.52  0.37   701 1.00
# bsx        0.25 0.17 -0.02  0.51   716 1.00
# ba         0.26 0.10  0.10  0.42   693 1.00
# b_n        0.04 0.19 -0.27  0.34   757 1.00
# sigmaID    0.63 0.05  0.56  0.70   898 1.00
# sigmaR     0.78 0.04  0.71  0.86   785 1.00
# sigmaG     0.12 0.08  0.01  0.26   288 1.02
# sigmaItem  0.59 0.05  0.51  0.66  1079 1.00

compare(dM_null, dM_aPriori, dM_exp, dM_FULL)
# WAIC pWAIC dWAIC weight     SE  dSE
# dM_FULL    10394.3 196.7   0.0   0.75 109.21   NA
# dM_aPriori 10396.6 198.1   2.3   0.23 108.76 7.05
# dM_exp     10403.0 199.6   8.8   0.01 108.40 8.41
# dM_null    10403.7 199.9   9.4   0.01 108.29 9.17

saveRDS(dM_FULL, file = "dMFULL.rds")

dM_FULL <- readRDS("SAVED_dMFULL.rds")


### trying posterior prediction plots

# Liked
post <- extract.samples(dM_FULL)

kL <- seq(-3,3,by = 0.1) # values of Liked to calculate over

color_list <- c("red","orange","yellow","green","blue","violet")
plot(1, 1, type = "n", xlab = "Liked", ylab = "Cumulative Probability", xlim = c(-3,3), ylim = c(0,1), xaxp = c(-3,3,4), yaxp = c(0,1,2), main = "Dominance Ratings")

pL_means <- matrix(0, nrow = 61, ncol = 6) 

for( s in 1:100) {
  p <- as.data.frame(post)[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bL*kL
  pL <- pordlogit( 1:6, a=ak, phi=phi)
  pL_means <- pL_means + pL
  for ( i in 1:6)
    lines( kL, pL[,i], col=col.alpha(color_list[i], alpha = 0.05))
}

# add thick lines for means
for (i in 1:6)
  lines( kL, pL_means[,i]/101, col = color_list[i], lwd = 3)

# add labels for numbers
text(1.5, 0.2, labels = "1")
text(0, 0.45, labels = "2")
text(-1.5, 0.6, labels = "3")
text(-2, 0.78, labels = "4")
text(-2.2, 0.88, labels = "5")
text(-2.6, 0.94, labels = "6")
text(-2.8, 0.99, labels = "7")

### Influence
post <- extract.samples(dM_FULL)

kI <- seq(-3,3,by=0.1) #values of influence

color_list <- c("red","orange","yellow","green","blue","violet")
plot(1, 1, type = "n", xlab = "Influential", ylab = "Cumulative Probability", xlim = c(-3,3), ylim = c(0,1), xaxp = c(-3,3,4), yaxp = c(0,1,2), main = "Dominance Ratings")

pI_means <- matrix(0, nrow = 61, ncol = 6) 

for( s in 1:100) {
  p <- as.data.frame(post)[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bI*kI
  pI <- pordlogit( 1:6, a=ak, phi=phi)
  pI_means <- pI_means + pI
  for ( i in 1:6)
    lines( kI, pI[,i], col=col.alpha(color_list[i], alpha = 0.05))
}

# add thick lines for means
for (i in 1:6)
  lines( kI, pI_means[,i]/101, col = color_list[i], lwd = 3)

# add labels for numbers
text(0, 0.1, labels = "1")
text(0.9, 0.4, labels = "2")
text(1.2, 0.6, labels = "3")
text(2, 0.75, labels = "4")
text(2.2, 0.88, labels = "5")
text(2.6, 0.94, labels = "6")
text(2.8, 1, labels = "7")


###### is dominance predicted by prestige score?
gDomD_NA$aveP <- kernowResults$aveP[match(gDomD_NA$rated_ID, kernowResults$ID)]

#dominance - prestige independence model
dM_prest <- map2stan(
  alist(
    dominanceRatings ~ dordlogit(phi, cutpoints),
    phi <- bp*aveP + aID[ratedID]*sigmaID + aR[raterId]*sigmaR +
      aG[grpID]*sigmaG + aItem[itemID]*sigmaItem,
    bp ~ dnorm(0,1),
    aG[grpID] ~ dnorm(0,1),
    aID[ratedID]  ~ dnorm(0,1),
    aR[raterId] ~ dnorm(0,1),
    aItem[itemID] ~ dnorm(0,1),
    c(sigmaID, sigmaR, sigmaG, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=gDomD_NA, 
  constraints = list(sigmaID = "lower=0", sigmaR = "lower=0", sigmaG = "lower=0", sigmaItem = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(dM_prest)
# mean   sd  5.5% 94.5% n_eff Rhat
# bp        0.71 0.62 -0.27  1.69   535 1.01
# sigmaID   0.79 0.05  0.72  0.86   589 1.00
# sigmaR    0.76 0.04  0.69  0.84   970 1.01
# sigmaG    0.08 0.06  0.00  0.19   392 1.01
# sigmaItem 0.59 0.05  0.52  0.67   862 1.01

#precis with cutpoints (depth = 2) but minus the varying intercepts (=129!!)
# mean   sd  5.5% 94.5% n_eff Rhat
# cutpoints[1] -0.31 0.54 -1.17  0.56   797 1.01
# cutpoints[2]  1.09 0.54  0.24  1.96   799 1.01
# cutpoints[3]  1.99 0.54  1.14  2.87   800 1.01
# cutpoints[4]  3.06 0.54  2.20  3.95   802 1.01
# cutpoints[5]  4.00 0.54  3.15  4.89   816 1.01
# cutpoints[6]  5.28 0.55  4.42  6.16   828 1.01
# bp            0.67 0.63 -0.33  1.67   873 1.00

########
##### PLOTTING for public engagement event
###### experimental plotting

#histPlots
presPlot <- simplehist(gPresD$prestigeRatings, xlim = c(1,7), xlab = "response")
presPlot
domPlot <- simplehist(gDomD$dominanceRatings, xlim = c(1,7), xlab = "response")
domPlot
agePlot <- hist(kernowResults$Age, xlab = "Age", ylab = "Total number of Participants", xlim = c(0,100), 
                main = "Age Range", ylim = c(0,35),col = "seagreen", cex.lab = 1.4, cex.axis = 1, cex.main =2)

sexPlot <- plot(kernowResults$Gender, xlab = "Gender", ylab = "Total number of Participants", col = "seagreen",
                main = "Gender Split", ylim = c(0,100), cex.lab = 1.4, cex.axis = 1, cex.main =2)


plot(kernowResults$Gender)

nomPlot <- ggplot(kernowResults, aes(avePcs, Nominated)) +
  stat_summary(fun.y=mean, geom = "point", size = 2, color = 222) +
  theme_bw() + theme(text = element_text(size=10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=12)) + 
  ylab("Proportion of Nominations") +
  xlab("perc D") +
  scale_y_continuous(limits=c(0,1))
nomPlot

nom1<- ggplot(data = kernowResults) + 
  geom_smooth(mapping = aes(x = avePcs, y = Nominated)) +
  scale_y_continuous(limits=c(0,1))
nom1

nom2<- ggplot(data = kernowResults) + 
  geom_smooth(mapping = aes(x = aveDcs, y = Nominated)) +
  scale_y_continuous(limits=c(0,1))
nom2

nom3<- ggplot(data = kernowResults) + 
  geom_smooth(mapping = aes(x = ScoreCS, y = Nominated)) +
  scale_y_continuous(limits=c(0,1))
nom3

presPlot <- ggplot(kernowResults, aes(aveP, initial_influential)) +
  stat_summary(fun.y=mean, position= position_dodge(0.3), geom = "point", size = 4, color = "red") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  ylab("Prestige Score") +
  xlab("Influential Group Member") +
  scale_y_continuous(limits=c(0,1))
presPlot

presPlot2 <- ggplot(data = kernowResults) + 
  stat_summary(
    mapping = aes(x = initial_influential, y = aveP),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
presPlot2

kernowResults$influential_grp <- ifelse(kernowResults$initial_influential==1,"yes","no")

presPlot3 <- ggplot(data = kernowResults, mapping = aes(x=influential_grp,y = aveP)) + 
  geom_boxplot()
presPlot3


presPlot4 <- ggplot(gPresD_NA, aes(prestigeRatings, initInf)) +
  stat_summary(fun.y=mean, position= position_dodge(0.3), geom = "point", size = 4, color = "red") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  ylab("Voted influential to the Group") +
  xlab("Prestige Score") + 
  scale_y_continuous(limits=c(0,1))
presPlot4



meanPres <- tapply(gPresD_NA$prestigeRatings, list(gPresD_NA$initInf),mean)
table(meanPres)
table(gPresD_NA$initInf)

#####
## Violins
kernowResults$percP <- kernowResults$aveP*100
kernowResults$percD <- kernowResults$aveD*100

p <- ggplot(kernowResults, aes(factor(influential_grp), percP)) +  geom_violin(fill="seagreen") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  ylab("Prestige Score %") +
  xlab("Influential group member to begin with") + 
  scale_y_continuous(limits=c(0,100))
p

d <- ggplot(kernowResults, aes(factor(influential_grp), percD)) +  geom_violin(fill="seagreen") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  ylab("Dominance Score %") +
  xlab("Influential group member to begin with") + 
  scale_y_continuous(limits=c(0,100))
d

#violin from all Raw (Not using)
p1 <- ggplot(gPresD_NA, aes(factor(initInf), prestigeRatings)) +  geom_violin(fill="seagreen") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  ylab("Prestige Score") +
  xlab("Influential group member to begin with") 
p1

plot(kernowResults$aveP ~ kernowResults$Age)

#influential plots:
#Prestige:

initInf <- gPresD_NA[gPresD_NA$initInf == 1,]
hist(initInf$prestigeRatings, main = paste("Influential to begin with"), col = "blue", labels = FALSE, xlab = "Prestige Rating", ylab = "Total")
NinitInf <- gPresD_NA[gPresD_NA$initInf ==0,]
hist(NinitInf$prestigeRatings, main = paste("NOT influential to begin with"), col = "blue", labels = FALSE, xlab = "Prestige Rating", ylab = "Total")

#Dominance:
initInfD <- gDomD_NA[gDomD_NA$initInf == 1,]
hist(initInfD$dominanceRatings, main = paste("Influential to begin with"), col = "blue", labels = FALSE, xlab = "Dominance Rating", ylab = "Total")
NinitInfD <- gDomD_NA[gDomD_NA$initInf ==0,]
hist(NinitInfD$dominanceRatings, main = paste("NOT influential to begin with"), col = "blue", labels = FALSE, xlab = "Dominance Rating", ylab = "Total")

hist(kernowResults$percD, main = paste("Dominance Ratings"), col = "seagreen", labels = FALSE, xlab = "Dominance %", ylab = "Total people", xlim = c(0,100), ylim = c(0,50), freq = TRUE)
hist(kernowResults$percP, main = paste("Prestige Ratings"), col = "seagreen", labels = FALSE, xlab = "Prestige %", ylab = "Total people", xlim = c(0,100), ylim = c(0,35), freq = TRUE)

likPPlot <- ggplot(gPresD_NA, aes(Liked, prestigeRatings)) +
  stat_summary(fun.y=mean, position= position_dodge(0.3), geom = "point", size = 4, color = "red") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  scale_y_continuous(limits=c(0,7))
likPPlot

likDplot <- ggplot(gDomD_NA, aes(Liked, dominanceRatings)) +
  stat_summary(fun.y=mean, position= position_dodge(0.3), geom = "point", size = 4, color = "red") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  scale_y_continuous(limits=c(0,7))
likDplot

#####
###### Looking at per group stuff 

#### Highest Dominance score of any individual? look at their group:
mostDom <- kernowResults[order(kernowResults$aveD, decreasing = TRUE), ]
mostPres <- kernowResults[order(kernowResults$aveP, decreasing = TRUE),]

grp18 <- kernowResults[kernowResults$Group==18,]
plot(grp18$aveD ~ grp18$ID, ylim = c(0,1))

grp16 <- kernowResults[kernowResults$Group==16,]
plot(grp16$aveD ~ grp16$ID, ylim = c(0,1))

# order via dominance within group 
perGroup <- kernowResults[order(kernowResults$Group,kernowResults$percD),]
perGroup$ID_G<-sequence(rle(perGroup$Group)$length) 

perGroup$Group <- as.factor(perGroup$Group)
highestDomPlot <- ggplot(data = perGroup, mapping = aes(x = ID_G, y = percD)) + 
  geom_smooth(mapping = aes(color = Group)) + theme_bw() 
highestDomPlot

#order for Prestige
perGroupP <- kernowResults[order(kernowResults$Group,kernowResults$percP),]
perGroupP$ID_G<-sequence(rle(perGroupP$Group)$length) 

perGroupP$Group <- as.factor(perGroupP$Group)
highestPresPlot <- ggplot(data = perGroupP, mapping = aes(x = ID_G, y = percP)) + 
  geom_smooth(mapping = aes(color = Group)) + theme_bw() 
highestPresPlot

#plotting men and women's overconfidence
confSexPlot <- ggplot(kernowResults, aes(factor(Gender), Overconfidence)) +  geom_violin(fill="seagreen") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  ylab("Conf") +
  xlab("Gender") + 
  scale_y_continuous(limits=c(-40,40))
confSexPlot

#plotting men and women's score
scoreSexPlot <- ggplot(kernowResults, aes(factor(Gender), IndividScore)) +  geom_violin(fill="seagreen") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  ylab("Score") +
  xlab("Gender") + 
  scale_y_continuous(limits=c(0,40))
scoreSexPlot

#plotting men and women's likeability
likeSexPlot <- ggplot(kernowResults, aes(factor(Gender), aveLik)) +  geom_violin(fill="seagreen") +
  theme_bw() + theme(text = element_text(size=20), axis.text.x = element_text(colour="blue",size=15), axis.text.y = element_text(colour="blue",size=15)) + 
  ylab("Liked") +
  xlab("Gender") + 
  scale_y_continuous(limits=c(0,1))
likeSexPlot

