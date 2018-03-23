######## Kernow Study Analysis
library(dplyr)
library(psy)
setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/")

#pdRatings <- read.csv("kernowResults220318.csv")

pdRatings <- read.csv("pdRatings.csv")

### Calculate p&d scores
### Reverse score items 2,6,10,12,17 as according to Cheng et al. 2013 (should really write a function for this..!!)

##pdRatings$R2rev <- ifelse((pdRatings$R2==1),7,
#                          ifelse((pdRatings$R2==2),6,
#                                 ifelse((pdRatings$R2==3),5,
#                                        ifelse((pdRatings$R2==4),4,
#                                               ifelse((pdRatings$R2==5),3,
#                                                      ifelse((pdRatings$R2==6),2,
#                                                             ifelse((pdRatings$R2==7),1,NA)))))))
##pdRatings$R6rev <- ifelse((pdRatings$R6==1),7,
#                          ifelse((pdRatings$R6==2),6,
#                                 ifelse((pdRatings$R6==3),5,
#                                        ifelse((pdRatings$R6==4),4,
#                                               ifelse((pdRatings$R6==5),3,
#                                                      ifelse((pdRatings$R6==6),2,
#                                                             ifelse((pdRatings$R6==7),1,NA)))))))
##pdRatings$R10rev <- ifelse((pdRatings$R10==1),7,
#                          ifelse((pdRatings$R10==2),6,
#                                 ifelse((pdRatings$R10==3),5,
#                                        ifelse((pdRatings$R10==4),4,
#                                               ifelse((pdRatings$R10==5),3,
#                                                      ifelse((pdRatings$R10==6),2,
#                                                             ifelse((pdRatings$R10==7),1,NA)))))))
# 
# pdRatings$R12rev <- ifelse((pdRatings$R12==1),7,
#                           ifelse((pdRatings$R12==2),6,
#                                  ifelse((pdRatings$R12==3),5,
#                                         ifelse((pdRatings$R12==4),4,
#                                                ifelse((pdRatings$R12==5),3,
#                                                       ifelse((pdRatings$R12==6),2,
#                                                              ifelse((pdRatings$R12==7),1,NA)))))))
# 
# pdRatings$R17rev <- ifelse((pdRatings$R17==1),7,
#                           ifelse((pdRatings$R17==2),6,
#                                  ifelse((pdRatings$R17==3),5,
#                                         ifelse((pdRatings$R17==4),4,
#                                                ifelse((pdRatings$R17==5),3,
#                                                       ifelse((pdRatings$R17==6),2,
#                                                              ifelse((pdRatings$R17==7),1,NA)))))))
# 
# pdRatings[pdRatings == 999] <- NA
 

#pdRatings$dominanceMean <- rowMeans(pdRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
#pdRatings$prestigeMean <- rowMeans(pdRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)


#pdCols <- colnames(pdRatings)
#prestigeCols <- c(pdRatings$R1, pdRatings$R2rev, pdRatings$R4, pdRatings$R6rev, pdRatings$R8, pdRatings$R13, 
#                  pdRatings$R14, pdRatings$R15, pdRatings$R17rev)
# 
# pdRatings$Psum <- rowSums(pdRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)
# 
# pdRatings <- na.omit(pdRatings)
# pdRatings$Pprop <- (pdRatings$Psum - 9)/54
# 
# pdRatings$Dsum <- rowSums(pdRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
# pdRatings$Dprop <-(pdRatings$Dsum -8)/48

#write.csv(pdRatings, "pdRatings.csv")


#cronbach test stuff
PrestigeRatings <- subset(pdRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
DominanceRatings <- subset(pdRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))

cronbach(PrestigeRatings)
cronbach(DominanceRatings)

library(rethinking)

#try single level first

pdCorrelation <- map(
  alist(
    Pprop ~ dnorm(mu, sigma),
    mu <- a + dprop*Dprop,  
    a ~ dnorm(0,10),
    dprop ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=pdRatings )

precis(pdCorrelation)

#okay now try adding in random effects

pdCorrMulti <- map2stan(
  alist(
    Pprop ~ dnorm(mu, sigma),
    mu <- a + dprop*Dprop + 
      a_p[rater_id]*sigma_p + a_g[Group]*sigma_g,
      a ~ dnorm(0,10),
      dprop ~ dnorm(0,4),
      a_p[rater_id] ~ dnorm(0,1),
      a_g[Group] ~ dnorm(0,1),
      sigma ~ dunif(0,10),
      sigma_p ~ dcauchy(0,1),
      sigma_g ~ dcauchy(0,1)
    ),
  data=pdRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

#error, think it is something to do with indexing values for participant random effect, may have to renumber due to nas?
#try removing that random effect. 


pdCorrMulti1 <- map2stan(
  alist(
    Pprop ~ dnorm(mu, sigma),
    mu <- a + dprop*Dprop + 
      a_g[GroupID]*sigma_g,
      a ~ dnorm(0,10),
      dprop ~ dnorm(0,4),
      a_g[GroupID] ~ dnorm(0,1),
      sigma ~ dunif(0,10),
      sigma_g ~ dcauchy(0,1)
  ),
  data=pdRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(pdCorrMulti1)
#same problem with group. Will try and renumber group numbers using James' Secret Code and then try again:

#James' Secret Code:
NGroups = length(unique(pdRatings$Group))
OldGroupID <- pdRatings$Group
GroupID <- array(0,length(pdRatings$Group))
for (index in 1:NGroups){
  GroupID[OldGroupID == unique(OldGroupID)[index]] = index
}
pdRatings$GroupID <- GroupID

#######*******************IMPORTANT*****************************************
#######*********************************************************************
#That worked! IMPORTANT: will have to be careful doing this as all id and group numbers have to be relative
#to the originals for the other analyses too. Need to think about this carefully. 
#Also not entirely sure if we do the same process for ppt ID numbers, will this still be relative to the correct group IDs?
#Will try anyway for now and check with someone else later. 
#FOR NOW: If we do the same process for the other dataset, once we have FIRST attributed the average pd ratings to the correct ID, then it
#should all be relative within that dataset? 

Nraters = length(unique(pdRatings$rater_id))
OldRaterID <- pdRatings$rater_id
RaterID <- array(0,length(pdRatings$rater_id))
for (index in 1:Nraters){
  RaterID[OldRaterID == unique(OldRaterID)[index]] = index
}
pdRatings$RaterID <- RaterID



pdCorrMulti <- map2stan(
  alist(
    Pprop ~ dnorm(mu, sigma),
    mu <- a + dprop*Dprop + 
      a_p[RaterID]*sigma_p + a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    dprop ~ dnorm(0,4),
    a_p[RaterID] ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=pdRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(pdCorrMulti)

plot(pdRatings$Dprop ~ pdRatings$Pprop)
cor.test(pdRatings$Dprop, pdRatings$Pprop)
cor(pdRatings$Dprop, pdRatings$Pprop)

### First need to make an average column for the pd ratings for each rated ppt. 
### Then use the match function to assign these to the other dataframe

aveP <- aggregate(pdRatings$Pprop, list(pdRatings$rated_ID), mean)
aveD <- aggregate(pdRatings$Dprop, list(pdRatings$rated_ID), mean)

#using Match!
pdRatings$aveP <- aveP$x[match(pdRatings$rated_ID, aveP$Group.1)]
pdRatings$aveD <- aveD$x[match(pdRatings$rated_ID, aveD$Group.1)]

kernowResults <- read.delim("kernow_results_16_03_18_IDS.txt")

kernowResults[kernowResults == "na"] <- NA
kernowResults <- na.omit(kernowResults)

kernowResults$aveP <- aveP$x[match(kernowResults$ID, aveP$Group.1)]
kernowResults$aveD <- aveD$x[match(kernowResults$ID, aveD$Group.1)]

#hmmm what to do about scaling overconfidence... 
kernowResults$Overconfidence <- as.numeric(levels(kernowResults$Overconfidence))[as.integer(kernowResults$Overconfidence)]
#kernowResults$oConf <- (kernowResults$Overconfidence + 40)/80 
kernowResults$oConf <- scale(kernowResults$Overconfidence)
kernowResults$sScore <- scale(kernowResults$IndividScore)

#single level first:

votedMod_s <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + score*sScore + oconf*oConf + 
          prestige*aveP + Dominance*aveD,
        a ~ dnorm(0,10),
        c(score, oconf, prestige, Dominance) ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)


precis(votedMod_s)

#trying multilevel
#gets annoyed with group indexing again: 
#will change BUT NEED TO THINK ABOUT THIS:

NGroups = length(unique(kernowResults$Group))
OldGroupID <- kernowResults$Group
GroupID <- array(0,length(kernowResults$Group))
for (index in 1:NGroups){
  GroupID[OldGroupID == unique(OldGroupID)[index]] = index
}
kernowResults$GroupID <- GroupID

kernowResults$Age <- as.numeric(levels(kernowResults$Age))[as.integer(kernowResults$Age)]
kernowResults$ageS <- scale(kernowResults$Age)
kernowResults$Sex <- as.numeric(levels(kernowResults$Sex))[as.integer(kernowResults$Sex)]


votedMod_m <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + score*sScore + oconf*oConf + 
          prestige*aveP + Dominance*aveD + infl*initial_influential +
          age*ageS + sex*Sex + 
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        c(score, oconf, prestige, Dominance, infl, age, sex) ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(votedMod_m)
cor(kernowResults$IndividScore, kernowResults$Overconfidence)
cor.test(kernowResults$IndividScore, kernowResults$Overconfidence)

####Dominance? Full so far...
DomMod <- map2stan(
  alist(
    aveD ~ dnorm(mu, sigma),
    mu <- a + oconf*oConf + age*ageS + sex*Sex + score*sScore + infl*initial_influential +
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    c(oconf, age, sex, score, infl) ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(DomMod)

####compare to predictions
DomModPriori <- map2stan(
  alist(
    aveD ~ dnorm(mu, sigma),
    mu <- a + oconf*oConf + infl*initial_influential +
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    c(oconf, infl) ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(DomModPriori)

####compare to Null
DomModNull <- map2stan(
  alist(
    aveD ~ dnorm(mu, sigma),
    mu <- a + 
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(DomModNull)

compare(DomMod,DomModPriori, DomModNull)

####Prestige? Full so far...
PresMod <- map2stan(
  alist(
    aveP ~ dnorm(mu, sigma),
    mu <- a + oconf*oConf + age*ageS + sex*Sex + score*sScore + infl*initial_influential +
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    c(oconf, age, sex, score, infl) ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(PresMod)

#A Priori

PresModPriori <- map2stan(
  alist(
    aveP ~ dnorm(mu, sigma),
    mu <- a + score*IndividScore +
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    score ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(PresModPriori)


#Null 

PresModNull <- map2stan(
  alist(
    aveP ~ dnorm(mu, sigma),
    mu <- a +
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(PresModNull)

compare(PresModNull,PresMod,PresModPriori)
