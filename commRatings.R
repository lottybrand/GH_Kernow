#######################
#######################
######################## Community ratings script

commRatings <- read.delim("comm_influence.txt")


### Calculate p&d scores
### Reverse score items 2,6,10,12,17 as according to Cheng et al. 2013 (should really write a function for this..!!)

commRatings$R2rev <- ifelse((commRatings$R2==1),7,
                          ifelse((commRatings$R2==2),6,
                                 ifelse((commRatings$R2==3),5,
                                        ifelse((commRatings$R2==4),4,
                                               ifelse((commRatings$R2==5),3,
                                                      ifelse((commRatings$R2==6),2,
                                                             ifelse((commRatings$R2==7),1,NA)))))))


commRatings$R6rev <- ifelse((commRatings$R6==1),7,
                          ifelse((commRatings$R6==2),6,
                                 ifelse((commRatings$R6==3),5,
                                        ifelse((commRatings$R6==4),4,
                                               ifelse((commRatings$R6==5),3,
                                                      ifelse((commRatings$R6==6),2,
                                                             ifelse((commRatings$R6==7),1,NA)))))))

commRatings$R10rev <- ifelse((commRatings$R10==1),7,
                          ifelse((commRatings$R10==2),6,
                                 ifelse((commRatings$R10==3),5,
                                        ifelse((commRatings$R10==4),4,
                                               ifelse((commRatings$R10==5),3,
                                                      ifelse((commRatings$R10==6),2,
                                                             ifelse((commRatings$R10==7),1,NA)))))))
 
commRatings$R12rev <- ifelse((commRatings$R12==1),7,
                           ifelse((commRatings$R12==2),6,
                                  ifelse((commRatings$R12==3),5,
                                         ifelse((commRatings$R12==4),4,
                                                ifelse((commRatings$R12==5),3,
                                                       ifelse((commRatings$R12==6),2,
                                                              ifelse((commRatings$R12==7),1,NA)))))))
 
commRatings$R17rev <- ifelse((commRatings$R17==1),7,
                           ifelse((commRatings$R17==2),6,
                                  ifelse((commRatings$R17==3),5,
                                         ifelse((commRatings$R17==4),4,
                                                ifelse((commRatings$R17==5),3,
                                                       ifelse((commRatings$R17==6),2,
                                                              ifelse((commRatings$R17==7),1,NA)))))))
 
commRatings <- read.csv("commRatings.csv")
commRatings[commRatings == 999] <- NA
commRatings <- na.omit(commRatings)


commRatings$dominanceMean <- rowMeans(commRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
commRatings$prestigeMean <- rowMeans(commRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)


commRatings$Psum <- rowSums(commRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)
commRatings$Pprop <- (commRatings$Psum - 9)/54
 
commRatings$Dsum <- rowSums(commRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
commRatings$Dprop <-(commRatings$Dsum -8)/48

write.csv(commRatings, "commRatings.csv")


#cronbach test stuff
PrestigeRatings <- subset(commRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
DominanceRatings <- subset(commRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))

cronbach(PrestigeRatings)
cronbach(DominanceRatings)

hist(commRatings$prestigeMean)
hist(commRatings$dominanceMean)
