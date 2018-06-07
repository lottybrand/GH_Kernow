#### Trying to figure out what all these old data files are:

setwd("~/Desktop/Postdoc/CornwallCommunityStudy/results/Kernow/DataFiles")

#this is final processed kernow results after the dataWrangling.R file
#change these names to reflect raw, ratings only, etc, once you're sure. 
kernow1 <- read.csv("kernowResults.csv")
#this is pure ratings only
kernow2 <- read.csv("kernowResults220318.csv")
#this is the raw original file
kernow3 <- read.delim("kernow_results_26_03_18_IDS.txt")

#this is final processed ratings file with all the additions after the dataWrangling.R file
pdRatings1 <- read.csv("pdRatings.csv") 

#these are formatted for ordinal analyses in orderedKernow.R
gDom <- read.csv("gDomD.csv")
gPres <- read.csv("gPresD.csv")


