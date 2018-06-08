
######################### PRESTIGE & DOM MODELS. METRIC FIRST #########################

library(rethinking)


############################################################
############################################################
######## COMMUNITY INFLUENCE PRESTIGE AND DOMINANCE RATINGS: 
############################################################
############################################################

commRatings <- read.csv("commRatings.csv")
commRatings[,1:7] <- NULL
write.csv(commRatings, "commRatings.csv")

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
    a ~ dnorm(0,1),
    b_pres ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data=commRatings, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)

precis(domPrestComm)

#Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
#a       0.82   0.06       0.73       0.91   335    1
#b_pres -0.52   0.08      -0.66      -0.39   276    1
#sigma   0.21   0.01       0.19       0.23   632    1
?gsub
colnames(commRatings)[2] <- "Name"
commRatings$name <- gsub("999", 'na', commRatings$Name)
commRatings$Name <- commRatings$name
commRatings$name <- NULL
write.csv(commRatings, "commRatings.csv")
commRatings <- read.csv("commRatings.csv")

inflNames <- c("The Queen", "Queen", "Queen Elizabeth", "Theresa May", "David Attenborough",
               "Jeremy Corbyn", "Donald Trump", "JK Rowling", "Ozzy Osbourne",
               "Boris Johnson", "Prince Charles")
infLabels <- commRatings[commRatings$Name %in% inflNames,]


namePlot <- ggplot(commRatings, aes(Pprop, Dprop, label=Name)) +
  geom_point(size = 0.5) +
  theme_bw() +
  geom_text(data=infLabels, position = position_jitter(), size = 5)
namePlot

cor.test(commRatings$Dprop, commRatings$Pprop)
cor(commRatings$Dprop, commRatings$Pprop)
#[1] -0.4868556


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

# Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
#a       0.48   0.12       0.28       0.65   296 1.00
#b_pres -0.20   0.15      -0.41       0.05   297 1.00
#sigma   0.20   0.01       0.18       0.22   371 1.01

plot(learnRatings$Dprop ~ learnRatings$Pprop)
cor.test(learnRatings$Dprop, learnRatings$Pprop)
cor(learnRatings$Dprop, learnRatings$Pprop)
#[1] -0.1110871


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
# Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# a        0.31   0.04       0.24       0.39   569 1.00
# b_pres  -0.04   0.06      -0.12       0.06   572 1.00
# sigma    0.16   0.01       0.16       0.17   520 1.00
# sigma_p  0.06   0.01       0.04       0.08   232 1.00
# sigma_g  0.00   0.04      -0.05       0.05    20 1.01

plot(pdRatings$Dprop ~ pdRatings$Pprop)
cor.test(pdRatings$Dprop, pdRatings$Pprop)
cor(pdRatings$Dprop, pdRatings$Pprop)
#[1] -0.05316162
#p-value = 0.2262

############################################################
############################################################
##################### What predicts group prestige ratings? ###############
############################################################
############################################################

#Full 

PresFull<- map2stan(
  alist(
    aveP ~ dnorm(mu, sigma),
    mu <- a + score*sScore + oConf*o_conf + infR*aveInf + lik*aveLik +
      sex*Sex + age*ageS + initL*initial_learn + initInf*initial_influential + 
      a_g[GroupID]*sigma_g,
      a ~ dnorm(0,10),
      c(score, oConf, infR, lik, sex, age, initL, initInf) ~ dnorm(0,1),
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

#####
#### centre and scale aveInf aveLik aveP and aveD too
#####

kernowResults$aveDcs <- scale(kernowResults$aveD, center = TRUE, scale = TRUE)
kernowResults$avePcs <- scale(kernowResults$aveP, center = TRUE, scale = TRUE)
kernowResults$aveInfCS <- scale(kernowResults$aveInf, center = TRUE, scale = TRUE)
#crap forgot to add CS to end, fix later:
kernowResults$aveLik <- scale(kernowResults$aveLik, center = TRUE, scale = TRUE)

kernowResults <- na.omit(kernowResults)
##### Full Model: 

nominatedFull <- map2stan(
  alist(Nominated ~ dbinom(1,p),
        logit(p) <- a + score*ScoreCS + conf*OverCS + 
          prestige*avePcs + Dominance*aveDcs + infR*aveInfCS + lik*aveLik + 
          infl*initial_influential + inLearn*initial_learn + 
          age*AgeCS + sex*Sex + a_g[GroupID]*sigma_g,
        a ~ dnorm(0,1),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ normal(0,0.1),
        c(score, conf, prestige, Dominance, infR, lik, infl, inLearn, age, sex) ~ dnorm(0,1)
  ),
  data=kernowResults, 
  constraints = list(sigma_g = "lower=0"),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter = 1200)


precis(nominatedFull)
# Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# a         -1.55   0.39      -2.17      -0.96  1800    1
# sigma_g    0.08   0.06       0.00       0.15  1800    1
# score      0.80   0.33       0.27       1.32  1800    1
# conf       0.26   0.28      -0.19       0.72  1800    1
# prestige  -0.23   0.40      -0.86       0.40  1800    1
# Dominance  0.25   0.30      -0.19       0.75  1800    1
# infR       1.49   0.38       0.94       2.15  1800    1
# lik       -0.12   0.35      -0.66       0.46  1800    1
# infl      -0.26   0.66      -1.36       0.74  1800    1
# inLearn    0.37   0.69      -0.64       1.47  1800    1
# age        0.00   0.28      -0.46       0.43  1800    1
# sex       -0.99   0.51      -1.79      -0.21  1800    1

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
          prestige*aveP +
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
        logit(p) <- a + score*sScore + oconf*o_conf + 
          a_g[GroupID]*sigma_g,
        a ~ dnorm(0,10),
        a_g[GroupID] ~ dnorm(0,1),
        sigma_g ~ dcauchy(0,1),
        c(score, oconf) ~ dnorm(0,1)
  ),
  data=kernowResults, warmup = 1000, iter=2000, chains=1, cores = 1)

precis(nomSCORE)
compare(nominatedNull, nominatedFull, nomDom, nomPres, nomInf, nomLik, nomPrevious, nomSCORE)


#WAIC pWAIC dWAIC weight    SE   dSE
#nominatedFull 121.1   7.6   0.0   0.82 14.42    NA
#nomSCORE      125.0   4.5   4.0   0.11 14.93  3.28
#nomInf        126.2   2.1   5.1   0.06 11.56 10.10
#nomPrevious   140.7   3.7  19.6   0.00 13.47  9.82
#nomDom        141.0   2.3  19.9   0.00 13.12 10.89
#nomPres       142.1   2.1  21.0   0.00 13.21 10.57
#nominatedNull 142.4   2.1  21.3   0.00 13.22 10.58
#nomLik        143.0   2.5  22.0   0.00 13.26 10.63

plot(precis(nominatedFull, pars = c("a", "score", "oconf", "prestige", "Dominance", "infR","lik","infl","inLearn","age","sex")))

