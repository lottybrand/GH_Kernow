######## Kernow Study Analysis
library(dplyr)
setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/")

pdRatings <- read.delim("kernow_results_16_03_18_pds.txt")

### Calculate p&d scores
### Reverse score items 2,6,10,12,17 as according to Cheng et al. 2013 (should really write a function for this..!!)

pdRatings$R2rev <- ifelse((pdRatings$R2==1),7,
                          ifelse((pdRatings$R2==2),6,
                                 ifelse((pdRatings$R2==3),5,
                                        ifelse((pdRatings$R2==4),4,
                                               ifelse((pdRatings$R2==5),3,
                                                      ifelse((pdRatings$R2==6),2,
                                                             ifelse((pdRatings$R2==7),1,"NA")))))))
pdRatings$R6rev <- ifelse((pdRatings$R6==1),7,
                          ifelse((pdRatings$R6==2),6,
                                 ifelse((pdRatings$R6==3),5,
                                        ifelse((pdRatings$R6==4),4,
                                               ifelse((pdRatings$R6==5),3,
                                                      ifelse((pdRatings$R6==6),2,
                                                             ifelse((pdRatings$R6==7),1,"NA")))))))
pdRatings$R10rev <- ifelse((pdRatings$R10==1),7,
                          ifelse((pdRatings$R10==2),6,
                                 ifelse((pdRatings$R10==3),5,
                                        ifelse((pdRatings$R10==4),4,
                                               ifelse((pdRatings$R10==5),3,
                                                      ifelse((pdRatings$R10==6),2,
                                                             ifelse((pdRatings$R10==7),1,"NA")))))))

pdRatings$R12rev <- ifelse((pdRatings$R12==1),7,
                          ifelse((pdRatings$R12==2),6,
                                 ifelse((pdRatings$R12==3),5,
                                        ifelse((pdRatings$R12==4),4,
                                               ifelse((pdRatings$R12==5),3,
                                                      ifelse((pdRatings$R12==6),2,
                                                             ifelse((pdRatings$R12==7),1,"NA")))))))

pdRatings$R17rev <- ifelse((pdRatings$R17==1),7,
                          ifelse((pdRatings$R17==2),6,
                                 ifelse((pdRatings$R17==3),5,
                                        ifelse((pdRatings$R17==4),4,
                                               ifelse((pdRatings$R17==5),3,
                                                      ifelse((pdRatings$R17==6),2,
                                                             ifelse((pdRatings$R17==7),1,"NA")))))))


pdRatings$prestigeMean <- ave(pdRatings$R1,pdRatings$R2rev, na.rm=TRUE)

pdRatings$prestigeMean <- rowMeans(pdRatings[,c("R1","R2rev","R4")], na.rm = TRUE)


pdRatings %>%
  rowwise() %>%
  mutate(c=mean(c(R1,R2rev,R4)))
