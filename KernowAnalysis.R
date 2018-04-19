######## Kernow Study Analysis
library(dplyr)
library(psy)
setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/")

################ FIRST REVERSE CODED THE P & D RATINGS ACCORDING TO CHENG ET AL. 2013 
################ THEN RE-SAVED AS pdRatings.csv so this is commented out and no longer needed

#pdRatings <- read.csv("kernowResults220318.csv")

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
 

# pdRatings$dominanceMean <- rowMeans(pdRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
# pdRatings$prestigeMean <- rowMeans(pdRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)

# pdRatings <- na.omit(pdRatings)

# pdRatings$Psum <- rowSums(pdRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)
# pdRatings$Pprop <- (pdRatings$Psum - 9)/54
# 
# pdRatings$Dsum <- rowSums(pdRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
# pdRatings$Dprop <-(pdRatings$Dsum -8)/48

# write.csv(pdRatings, "pdRatings.csv")


#subsets and cronbach test stuff
PrestigeRatings <- subset(pdRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
DominanceRatings <- subset(pdRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))

cronbach(PrestigeRatings)
cronbach(DominanceRatings)


################# Now for combining with Kernow Results (IDS etc)
#################
#################

pdRatings <- read.csv("pdRatings.csv")
kernowResults <- read.delim("kernow_results_26_03_18_IDS.txt")

kernowResults[kernowResults == "na"] <- NA


### First need to make an average column for the pd ratings for each rated ppt. 
### Then use the match function to assign these to the other dataframe

aveP <- aggregate(pdRatings$Pprop, list(pdRatings$rated_ID), mean)
aveD <- aggregate(pdRatings$Dprop, list(pdRatings$rated_ID), mean)

#using Match
kernowResults$aveP <- aveP$x[match(kernowResults$ID, aveP$Group.1)]
kernowResults$aveD <- aveD$x[match(kernowResults$ID, aveD$Group.1)]

#and for the ordinal...

meanP <- aggregate(pdRatings$prestigeMean, list(pdRatings$rated_ID), mean)
meanD <- aggregate(pdRatings$dominanceMean, list(pdRatings$rated_ID), mean)

kernowResults$meanP <- meanP$x[match(kernowResults$ID, meanP$Group.1)]
kernowResults$meanD <- meanD$x[match(kernowResults$ID, meanD$Group.1)]

#and for influence ratings:

pdRatings$Infsum <- rowSums(pdRatings[,c("R18", "R19", "R20")], na.rm = TRUE)
pdRatings$Infprop <- (pdRatings$Infsum -3)/18

aveInf <- aggregate(pdRatings$Infprop, list(pdRatings$rated_ID), mean)
kernowResults$aveInf <- aveInf$x[match(kernowResults$ID, aveInf$Group.1)]

#and for likeability ratings:

pdRatings$Likesum <- rowSums(pdRatings[,c("R21", "R22")], na.rm = TRUE)
pdRatings$Likeprop <-(pdRatings$Likesum -2)/12

aveLik <- aggregate(pdRatings$Likeprop, list(pdRatings$rated_ID), mean)
kernowResults$aveLik <- aveLik$x[match(kernowResults$ID, aveLik$Group.1)]
 

#need to decide best way to scale overconfidence.. and score?

kernowResults$Overconfidence <- as.numeric(levels(kernowResults$Overconfidence))[as.integer(kernowResults$Overconfidence)]
kernowResults$Overconfidence <- (kernowResults$Overconfidence + 40)/80
kernowResults$oConf <- scale(kernowResults$Overconfidence)
kernowResults$sScore <- scale(kernowResults$IndividScore)

#Do we need to scale Age as well? 

kernowResults$Age <- as.numeric(levels(kernowResults$Age))[as.integer(kernowResults$Age)]
kernowResults$ageS <- scale(kernowResults$Age)
kernowResults$Sex <- as.numeric(levels(kernowResults$Sex))[as.integer(kernowResults$Sex)]

##Now write this to file
write.csv(kernowResults, "kernowResults.csv")

#some plotting
hist(pdRatings$prestigeMean)
hist(pdRatings$dominanceMean)

hist(kernowResults$Age)
hist(kernowResults$Overconfidence)
hist(kernowResults$IndividScore)

############
########### NOW YOU CAN RE-ORDER INDECES
##########

kernowResults <- read.csv("kernowResults.csv")
pdRatings <- read.csv("pdRatings.csv")

################### REMEMBER HOW THIS AFFECTS THE OTHER DATASET. 
################## IF WE DO FOR GROUP AND ID, WILL THEY STAY THE SAME RELATIVE TO EACH OTHER?
#################

####### should be able to use coerce index instead ####
##### First for the PD Ratings: 

#James' Secret Code:
NGroups = length(unique(pdRatings$Group))
OldGroupID <- pdRatings$Group
GroupID <- array(0,length(pdRatings$Group))
for (index in 1:NGroups){
  GroupID[OldGroupID == unique(OldGroupID)[index]] = index
}
pdRatings$GroupID <- GroupID

### FOR PD ratings Rater ID's: 

Nraters = length(unique(pdRatings$rater_id))
OldRaterID <- pdRatings$rater_id
RaterID <- array(0,length(pdRatings$rater_id))
for (index in 1:Nraters){
  RaterID[OldRaterID == unique(OldRaterID)[index]] = index
}
pdRatings$RaterID <- RaterID

############# FOR KERNOW RESULTS GROUPS

NGroups = length(unique(kernowResults$Group))
OldGroupID <- kernowResults$Group
GroupID <- array(0,length(kernowResults$Group))
for (index in 1:NGroups){
  GroupID[OldGroupID == unique(OldGroupID)[index]] = index
}
kernowResults$GroupID <- GroupID


kernowResults <- na.omit(kernowResults)
colnames(kernowResults)
colnames(kernowResults)[17] <- "initial_learn"


######################### PRESTIGE & DOM MODELS. METRIC FIRST #########################

library(rethinking)


############################################################
############################################################
######## COMMUNITY INFLUENCE PRESTIGE AND DOMINANCE RATINGS: 
############################################################
############################################################

commRatings <- read.csv("commRatings.csv")
commRatings$X.1 <- NULL

presComm <- map2stan(
  alist(
    Pprop ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=commRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(presComm)


domComm <- map2stan(
  alist(
    Dprop ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=commRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(domComm)

#### Prestige predicted by Dominance?

domPrestComm <- map2stan(
  alist(
    Dprop ~ dnorm(mu, sigma),
    mu <- a + b_pres*Pprop,
    a ~ dnorm(0,10),
    b_pres ~ dnorm(0,4),
    sigma ~ dunif(0,10)
  ),
  data=commRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(domPrestComm)

plot(commRatings$Dprop ~ commRatings$Pprop)
cor.test(commRatings$Dprop, commRatings$Pprop)
cor(commRatings$Dprop, commRatings$Pprop)



############################################################
############################################################
######## COMMUNITY LEARN FROM PRESTIGE AND DOMINANCE RATINGS: 
############################################################
############################################################

learnRatings <- read.csv("learnRatings.csv")

presLearn <- map2stan(
  alist(
    Pprop ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=learnRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(presLearn)


domLearn <- map2stan(
  alist(
    Dprop ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=learnRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(domLearn)

#### Prestige predicted by Dominance?

domPrestLearn <- map2stan(
  alist(
    Dprop ~ dnorm(mu, sigma),
    mu <- a + b_pres*Pprop,
    a ~ dnorm(0,10),
    b_pres ~ dnorm(0,4),
    sigma ~ dunif(0,10)
  ),
  data=learnRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(domPrestLearn)

plot(learnRatings$Dprop ~ learnRatings$Pprop)
cor.test(learnRatings$Dprop, learnRatings$Pprop)
cor(learnRatings$Dprop, learnRatings$Pprop)



############################################################
############################################################
######## GROUP PRESTIGE AND DOMINANCE, DO THEY CORRELATE?
############################################################
############################################################


presMod <- map2stan(
  alist(
    Pprop ~ dnorm(mu, sigma),
    mu <- a +  
      a_p[RaterID]*sigma_p + a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    a_p[RaterID] ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=pdRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(presMod)

#### Dominance null model:

domMod <- map2stan(
  alist(
    Dprop ~ dnorm(mu, sigma),
    mu <- a +  
      a_p[RaterID]*sigma_p + a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    a_p[RaterID] ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=pdRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(domMod)

#### Prestige predicted by Dominance?

domPrest <- map2stan(
  alist(
    Dprop ~ dnorm(mu, sigma),
    mu <- a + b_pres*Pprop + 
      a_p[RaterID]*sigma_p + a_g[GroupID]*sigma_g,
      a ~ dnorm(0,10),
      b_pres ~ dnorm(0,4),
      a_p[RaterID] ~ dnorm(0,1),
      a_g[GroupID] ~ dnorm(0,1),
      sigma ~ dunif(0,10),
      sigma_p ~ dcauchy(0,1),
      sigma_g ~ dcauchy(0,1)
  ),
  data=pdRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(domPrest)


plot(pdRatings$Dprop ~ pdRatings$Pprop)
cor.test(pdRatings$Dprop, pdRatings$Pprop)
cor(pdRatings$Dprop, pdRatings$Pprop)

############################################################
############################################################
##################### What predicts group prestige ratings? ###############
############################################################
############################################################

#Full 

PresFull<- map2stan(
  alist(
    aveP ~ dnorm(mu, sigma),
    mu <- a + score*IndividScore + o_conf*Overconfidence + infR*aveInf + lik*aveLik +
      sex*Sex + age*ageS +
      a_g[GroupID]*sigma_g,
      a ~ dnorm(0,10),
      c(score, o_conf, infR, lik, sex, age) ~ dnorm(0,1),
      a_g[GroupID] ~ dnorm(0,1),
      sigma ~ dunif(0,10),
      sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(PresFull)

#A Priori

PresAPriori <- map2stan(
  alist(
    aveP ~ dnorm(mu, sigma),
    mu <- a + score*IndividScore + infR*aveInf + lik*aveLik +
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    c(score, infR, lik) ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(PresAPriori)


#Null 

PresNull <- map2stan(
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

precis(PresNull)

compare(PresNull,PresFull,PresAPriori)

##### (This is 'Nominated' from the OSF 'full model'? 'inital influential?' #####


PresInitial <- map2stan(
  alist(
    aveP ~ dnorm(mu, sigma), 
    mu <- a + infl*initial_influential + infLearn*initial_learn +
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    c(infl, infLearn) ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(PresInitial)

compare(PresFull, PresNull, PresAPriori, PresInitial)


############################################################
############################################################
##################### What predicts group Dominance ratings? ###############
############################################################
############################################################


DomFull <- map2stan(
  alist(
    aveD ~ dnorm(mu, sigma),
    mu <- a + score*sScore + oconf*oConf +  infR*aveInf + lik*aveLik + 
      age*ageS + sex*Sex + 
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    c(score, oconf, infR, lik, age, sex) ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(DomFull)

####compare to predictions
DomAPriori <- map2stan(
  alist(
    aveD ~ dnorm(mu, sigma),
    mu <- a + oconf*oConf + infR*aveInf +
      a_g[GroupID]*sigma_g,
      a ~ dnorm(0,10),
      c(oconf, infR) ~ dnorm(0,1),
      a_g[GroupID] ~ dnorm(0,1),
      sigma ~ dunif(0,10),
      sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(DomAPriori)

####compare to Null
DomNull <- map2stan(
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

precis(DomNull)

compare(DomFull,DomAPriori, DomNull)

############# Exploratory: Do initial ratings predict Dominance?


DomInit <- map2stan(
  alist(
    aveD ~ dnorm(mu, sigma),
    mu <- a + infl*initial_influential + infLearn*initial_learn +  
      a_g[GroupID]*sigma_g,
    a ~ dnorm(0,10),
    c(infl, infLearn) ~ dnorm(0,1),
    a_g[GroupID] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(DomInit)

compare(DomFull, DomNull, DomAPriori, DomInit)


##################################################
##########################################################################
################ NOMINATIONS MODELS ################################################
##########################################################################
##################################################


##### Full Model: 

nominatedFull <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + score*sScore + oconf*oConf + 
          prestige*aveP + Dominance*aveD + infR*aveInf + lik*aveLik + 
          infl*initial_influential + inLearn*initial_learn + 
          age*ageS + sex*Sex + 
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        c(score, oconf, prestige, Dominance, infR, lik, infl, inLearn, age, sex) ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nominatedFull)


####### Null Model: 


nominatedNull <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + 
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nominatedNull)


######## Prestige predicts nominations:


nomPres <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + 
          prestive*aveP +
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        prestige ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nomPres)

######### Dominance predicts nominations:


nomDom <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + 
          Dominance*aveD +
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        Dominance ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nomDom)

########### Likeability predicts nominations: 


nomLik <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + lik*aveLik + 
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        lik ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nomLik)

######## Influence on the task: 


nomInf <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + infR*aveInf + 
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        infR ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nomInf)


########## Previously influential:


nomPrevious <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + infl*initial_influential + inLearn*initial_learn + 
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        c(infl, inLearn) ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nomPrevious)

compare(nominatedNull, nominatedFull, nomDom, nomPres, nomInf, nomLik, nomPrevious)

############# EXPLORATORY: Score & Overconf? 


nomSCORE <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + score*sScore + oconf*oConf + 
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        c(score, oconf) ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nomSCORE)
compare(nominatedNull, nominatedFull, nomDom, nomPres, nomInf, nomLik, nomPrevious, nomSCORE)

##############################################################
##############################################################
######################### PLOTS ##############################
##############################################################
##############################################################
##############################################################

hist(commRatings$)