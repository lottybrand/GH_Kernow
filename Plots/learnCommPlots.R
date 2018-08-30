#making scatter + hist plots for learning and comm ratings

install.packages("ggstatsplot")
library(ggstatsplot)

#setwd("~/Desktop/Postdoc/CornwallCommunityStudy/Results/Kernow/DataFiles")
learnRatings <- read.csv("learnRatings.csv", stringsAsFactors=FALSE)

hist(learnRatings$dominanceMean)
hist(learnRatings$prestigeMean)

plot(learnRatings$dominanceMean, learnRatings$prestigeMean, ylim = c(1,7), xlim = c(1,7))

abline( lm(learnRatings$prestigeMean ~ learnRatings$dominanceMean))
learnPlot <- 
ggscatterstats(
  data = learnRatings,                                          
  x = dominanceMean,                                                  
  y = prestigeMean,
  xlab = "Mean Dominance Rating",
  ylab = "Mean Prestige Rating",
  axes.range.restrict = TRUE,
  results.subtitle = FALSE,
  marginal = TRUE,
  marginal.type = "histogram",
  margins = "both",
  title = "Community Learning Model Ratings",
  messages = FALSE
)
learnPlot
?ggscatterstats

mean(learnRatings$prestigeMean)
sd(learnRatings$prestigeMean)
mean(learnRatings$dominanceMean)
sd(learnRatings$dominanceMean)

Mode(learnRatings$prestigeMean)
Mode(learnRatings$dominanceMean)

commRatings <- read.csv("commRatings.csv", stringsAsFactors = FALSE)

hist(commRatings$dominanceMean)
hist(commRatings$prestigeMean)

plot(commRatings$dominanceMean, commRatings$prestigeMean, ylim = c(1,7), xlim = c(1,7))

abline( lm(commRatings$prestigeMean ~ commRatings$dominanceMean))

commPlot <- 
ggscatterstats(
  data = commRatings,                                          
  x = dominanceMean,                                                  
  y = prestigeMean,
  xlab = "Mean Dominance Rating",
  ylab = "Mean Prestige Rating",
  axes.range.restrict = TRUE,
  results.subtitle = FALSE,
  marginal = TRUE,
  marginal.type = "histogram",
  margins = "both",
  title = "Community Influence Ratings",
  messages = FALSE
)
commPlot

mean(commRatings$prestigeMean)
sd(commRatings$prestigeMean)
mean(commRatings$dominanceMean)
sd(commRatings$dominanceMean)
