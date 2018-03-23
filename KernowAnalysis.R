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

#That worked! IMPORTANT: will have to be careful doing this as all id and group numbers have to be relative
#to the originals for the other analyses too. Need to think about this carefully. 






