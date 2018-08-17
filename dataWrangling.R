######## Kernow Study Analysis
library(dplyr)
library(psy)
setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/")

######## First block of data wrangling with raw data file: 
################ FIRST REVERSE CODED THE P & D RATINGS ACCORDING TO CHENG ET AL. 2013 
################ THEN RE-SAVED AS pdRatings.csv so this is commented out and no longer needed
#####

#pdRatings <- read.csv("kernowResults_ratingsOnly.csv")

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
#PrestigeRatings <- subset(pdRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
#DominanceRatings <- subset(pdRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))

#cronbach(DominanceRatings)
#cronbach(PrestigeRatings)


################# Now for combining with Kernow Results (IDS etc)
#################
#################

pdRatings <- read.csv("pdRatings.csv")

##### WARNING: column "group affiliation" removed for GitHub version to preserve anonymity
##### thus some column numbers may not match up to the RAW file. 

kernowResults <- read.delim("kernow_results_RAW.txt")

kernowResults[kernowResults == "na"] <- NA


### First need to make an average column for the pd ratings for each rated ppt. 
### Then use the match function to assign these to the other dataframe

aveP <- aggregate(pdRatings$Pprop, list(pdRatings$rated_ID), mean)
aveD <- aggregate(pdRatings$Dprop, list(pdRatings$rated_ID), mean)

#using Match
kernowResults$aveP <- aveP$x[match(kernowResults$ID, aveP$Group.1)]
kernowResults$aveD <- aveD$x[match(kernowResults$ID, aveD$Group.1)]

# and for the ordinal... 
##(Don't think this is needed since the orderedKernow file handles all ordinal now)

#meanP <- aggregate(pdRatings$prestigeMean, list(pdRatings$rated_ID), mean)
#meanD <- aggregate(pdRatings$dominanceMean, list(pdRatings$rated_ID), mean)
#kernowResults$meanP <- meanP$x[match(kernowResults$ID, meanP$Group.1)]
#kernowResults$meanD <- meanD$x[match(kernowResults$ID, meanD$Group.1)]

#and for influence ratings:

pdRatings$Infsum <- rowSums(pdRatings[,c("R18", "R19", "R20")], na.rm = TRUE)
pdRatings$Infprop <- (pdRatings$Infsum -3)/18

aveInf <- aggregate(pdRatings$Infprop, list(pdRatings$rated_ID), mean)
kernowResults$aveInf <- aveInf$x[match(kernowResults$ID, aveInf$Group.1)]

#and for likeability ratings:

pdRatings$Likesum <- rowSums(pdRatings[,c("R21", "R22")], na.rm = TRUE)
pdRatings$Likeprop <-(pdRatings$Likesum -2)/12
pdRatings$LikeAve <- (pdRatings$Likesum/2)

aveLik <- aggregate(pdRatings$Likeprop, list(pdRatings$rated_ID), mean)
kernowResults$aveLik <- aveLik$x[match(kernowResults$ID, aveLik$Group.1)]


#scale and centre overconfidence, score, age, etc. 
#(Overconfidence coming out as factor because weird Rstudio/excel stuff, need to use as.character as below:
kernowResults <- na.omit(kernowResults)
kernowResults$oc <- as.numeric(as.character(kernowResults$Overconfidence))
kernowResults$Overconfidence <- kernowResults$oc
kernowResults$oc <- NULL
kernowResults$OverCS <- scale(kernowResults$Overconfidence, center = TRUE, scale = TRUE)

#as binary:
kernowResults$OConfBIN <- ifelse(kernowResults$Overconfidence >= 1, 1, 0)

#scale and center rest of predictors too:
kernowResults$ScoreCS <- scale(kernowResults$IndividScore, center = TRUE, scale = TRUE)

kernowResults$Age <- as.numeric(as.character(kernowResults$Age))
kernowResults$AgeCS <- scale(kernowResults$Age, center = TRUE, scale = TRUE)

#misspelt "initial" somewhere along the line..!
#colnames(kernowResults)
#colnames(kernowResults)[17] <- "initial_learn"

####### Use coerce index for group IDs etc  ####
##### First for the PD Ratings: 
pdRatings$GroupID <- coerce_index(pdRatings$Group)

### FOR PD ratings Rater ID's: 
pdRatings$RaterID <- coerce_index(pdRatings$rater_id)


############# FOR KERNOW RESULTS GROUPS
kernowResults$GroupID <- coerce_index(kernowResults$Group)

#### NOW write to file.... 

write.csv(kernowResults, "kernowResults.csv")
kernowResults <- read.csv("kernowResults.csv")

