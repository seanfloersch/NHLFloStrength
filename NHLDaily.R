##########################################################################################################
#################################################
# Set Up
#################################################

library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console

##########################################################################################################
#################################################
# Completing yesterday's odds
#################################################
toddate <- getDate(x)
yestdate <- getyestDate(x)
todgameID <- getGameID(x)
yestgameID <- getyestGameID(x)
NHLCompleteOddsPO <- CompleteTheOddsNHL(yestdate, yestgameID)

write_csv(NHLCompleteOddsPO, "/Users/seanfloersch/FloStrength/NHLFloStrength/NHLCompleteOddsPO")
#################################################
# Add yesterday games to NHL Game Dataset
#################################################
NHLGames <- AddYestfun(yestdate)

write_csv(NHLGames, "/Users/seanfloersch/FloStrength/NHLFloStrength/NHLGames2122")
#################################################
# Team Stats
#################################################
yestteam <- teamyestfun(yestdate)
#FloPlayer <- NHLPlayerFun(x, NHLCompleteOdds)
#write_csv(FloPlayer, "/Users/seanfloersch/FloStrength/NHLFloStrength/FloPlayer22")
FloPlayer <- read.csv("~/FloStrength/NHLFloStrength/FloPlayer22")
NHLMaster <- NHLMasterfun(NHLGames, FloPlayer)
#################################################
# Daily DataFrame 
#################################################
NHLOdds <- DailyNHL(toddate, NHLMaster, yestteam, todgameID)
write_csv(NHLOdds, "/Users/seanfloersch/FloStrength/NHLFloStrength/NHLOddsyesterday")
NHLCompleteOdds <- read.csv("~/FloStrength/NHLFloStrength/NHLCompleteOdds3.0")

linmod<- LinMod(NHLCompleteOdds)
dfy <- WinAnalysis(NHLCompleteOdds)
NHLOddsNew <- UpdDailyOdds(NHLOdds, linmod, dfy)
write_csv(NHLOddsNew, "/Users/seanfloersch/FloStrength/NHLFloStrength/NHLOddsyesterday")
linmod
mean(NHLCompleteOdds$FloCorrect)
mean(NHLCompleteOdds$BookCorrect)
mean(dfy$SpreadCorrect)
mean(NHLCompleteOdds$BookSpreadCorrect)



