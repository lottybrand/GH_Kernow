######## Kernow Study Analysis
library(dplyr)
library(psy)
setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/")

pdRatings <- read.csv("kernowResults220318.csv")

### Calculate p&d scores
### Reverse score items 2,6,10,12,17 as according to Cheng et al. 2013 (should really write a function for this..!!)

pdRatings$R2rev <- ifelse((pdRatings$R2==1),7,
                          ifelse((pdRatings$R2==2),6,
                                 ifelse((pdRatings$R2==3),5,
                                        ifelse((pdRatings$R2==4),4,
                                               ifelse((pdRatings$R2==5),3,
                                                      ifelse((pdRatings$R2==6),2,
                                                             ifelse((pdRatings$R2==7),1,NA)))))))
pdRatings$R6rev <- ifelse((pdRatings$R6==1),7,
                          ifelse((pdRatings$R6==2),6,
                                 ifelse((pdRatings$R6==3),5,
                                        ifelse((pdRatings$R6==4),4,
                                               ifelse((pdRatings$R6==5),3,
                                                      ifelse((pdRatings$R6==6),2,
                                                             ifelse((pdRatings$R6==7),1,NA)))))))
pdRatings$R10rev <- ifelse((pdRatings$R10==1),7,
                          ifelse((pdRatings$R10==2),6,
                                 ifelse((pdRatings$R10==3),5,
                                        ifelse((pdRatings$R10==4),4,
                                               ifelse((pdRatings$R10==5),3,
                                                      ifelse((pdRatings$R10==6),2,
                                                             ifelse((pdRatings$R10==7),1,NA)))))))

pdRatings$R12rev <- ifelse((pdRatings$R12==1),7,
                          ifelse((pdRatings$R12==2),6,
                                 ifelse((pdRatings$R12==3),5,
                                        ifelse((pdRatings$R12==4),4,
                                               ifelse((pdRatings$R12==5),3,
                                                      ifelse((pdRatings$R12==6),2,
                                                             ifelse((pdRatings$R12==7),1,NA)))))))

pdRatings$R17rev <- ifelse((pdRatings$R17==1),7,
                          ifelse((pdRatings$R17==2),6,
                                 ifelse((pdRatings$R17==3),5,
                                        ifelse((pdRatings$R17==4),4,
                                               ifelse((pdRatings$R17==5),3,
                                                      ifelse((pdRatings$R17==6),2,
                                                             ifelse((pdRatings$R17==7),1,NA)))))))

write.csv(pdRatings, file="pdRatings.csv")

#something fishy happening when trying to get the means, argggggh
#pdRatings$prestigeMean <- ave(pdRatings$R1,pdRatings$R2rev, na.rm=TRUE)
#pdRatings$prestigeMean <- rowMeans(pdRatings[,c("R1","R2rev","R4")], na.rm = TRUE)
#try the dplyr way?? 
#pdRatings %>%
#  rowwise() %>%
#  mutate(c=mean(c(R1,R2rev,R4)))

#cronbach test stuff
PrestigeRatings <- subset(pdRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
DominanceRatings <- subset(pdRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))
#Pratings <- na.omit(PrestigeRatings)
#let's see if it's to do with nas
#PratingsNA <- Pratings[1:3,]
cronbach(PrestigeRatings)
cronbach(DominanceRatings)


