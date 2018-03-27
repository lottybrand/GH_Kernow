#######################
#######################
######################## learn from ratings script

learnRatings <- read.delim("learn_from.txt")


### Calculate p&d scores
### Reverse score items 2,6,10,12,17 as according to Cheng et al. 2013 (should really write a function for this..!!)

learnRatings$R2rev <- ifelse((learnRatings$R2==1),7,
                            ifelse((learnRatings$R2==2),6,
                                   ifelse((learnRatings$R2==3),5,
                                          ifelse((learnRatings$R2==4),4,
                                                 ifelse((learnRatings$R2==5),3,
                                                        ifelse((learnRatings$R2==6),2,
                                                               ifelse((learnRatings$R2==7),1,NA)))))))


learnRatings$R6rev <- ifelse((learnRatings$R6==1),7,
                            ifelse((learnRatings$R6==2),6,
                                   ifelse((learnRatings$R6==3),5,
                                          ifelse((learnRatings$R6==4),4,
                                                 ifelse((learnRatings$R6==5),3,
                                                        ifelse((learnRatings$R6==6),2,
                                                               ifelse((learnRatings$R6==7),1,NA)))))))

learnRatings$R10rev <- ifelse((learnRatings$R10==1),7,
                             ifelse((learnRatings$R10==2),6,
                                    ifelse((learnRatings$R10==3),5,
                                           ifelse((learnRatings$R10==4),4,
                                                  ifelse((learnRatings$R10==5),3,
                                                         ifelse((learnRatings$R10==6),2,
                                                                ifelse((learnRatings$R10==7),1,NA)))))))

learnRatings$R12rev <- ifelse((learnRatings$R12==1),7,
                             ifelse((learnRatings$R12==2),6,
                                    ifelse((learnRatings$R12==3),5,
                                           ifelse((learnRatings$R12==4),4,
                                                  ifelse((learnRatings$R12==5),3,
                                                         ifelse((learnRatings$R12==6),2,
                                                                ifelse((learnRatings$R12==7),1,NA)))))))

learnRatings$R17rev <- ifelse((learnRatings$R17==1),7,
                             ifelse((learnRatings$R17==2),6,
                                    ifelse((learnRatings$R17==3),5,
                                           ifelse((learnRatings$R17==4),4,
                                                  ifelse((learnRatings$R17==5),3,
                                                         ifelse((learnRatings$R17==6),2,
                                                                ifelse((learnRatings$R17==7),1,NA)))))))

#learnRatings <- read.csv("learnRatings.csv")
learnRatings[learnRatings == 999] <- NA
learnRatings <- na.omit(learnRatings)


learnRatings$dominanceMean <- rowMeans(learnRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
learnRatings$prestigeMean <- rowMeans(learnRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)


learnRatings$Psum <- rowSums(learnRatings[,c("R1", "R2rev", "R4", "R6rev", "R8", "R13", "R14", "R15", "R17rev")], na.rm = TRUE)
learnRatings$Pprop <- (learnRatings$Psum - 9)/54

learnRatings$Dsum <- rowSums(learnRatings[,c("R3", "R5", "R7", "R9","R10rev","R11","R12rev","R16")], na.rm = TRUE)
learnRatings$Dprop <-(learnRatings$Dsum -8)/48

write.csv(learnRatings, "learnRatings.csv")


#cronbach test stuff
PrestigeRatings <- subset(learnRatings, select = c(R1, R2rev, R4, R6rev, R8, R13, R14, R15, R17rev))
DominanceRatings <- subset(learnRatings, select = c(R3, R5, R7, R9, R10rev, R11, R12rev, R16))

cronbach(PrestigeRatings)
cronbach(DominanceRatings)

hist(learnRatings$prestigeMean)
hist(learnRatings$dominanceMean)
