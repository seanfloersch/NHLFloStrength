library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console


weekschedfun <- function(url)  {
  h <- read_html(url) 
  date <- url %>% str_remove("https://www.espn.com/nhl/scoreboard/_/date/")
  ATeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
  HTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
  Scores <- html_nodes(h, ".ScoreCell_Score--scoreboard") %>% html_text
  AScore <- c()
  for (i in c(1:(length(Scores)/2))){
    y <- 2 *i -1
    AScore[i] <- Scores[y]
  }
  HScore <- c()
  for (i in c(1:(length(Scores)/2))){
    y <- 2 *i
    HScore[i] <- Scores[y]
  }
  if (length(HScore) != length(HTeam)){
    x <- length(HScore)
    HTeam <- HTeam[1:x]
    ATeam <- ATeam[1:x]
  }
  df <- data.frame(ATeam, HTeam, AScore, HScore) %>%
    mutate(AScore = as.numeric(AScore))%>%
    mutate(HScore = as.numeric(HScore)) %>%
    mutate(PD = AScore - HScore) %>%
    mutate(gameID = str_c(date, c(1:length(AScore)), by = "")) %>%
    mutate(date = date)
  return(df)
}


octurls <- c()
octdates <- c(11:15, 17:31)
for (i in octdates){
  octurls[i] <- str_c("https://www.espn.com/nhl/scoreboard/_/date/202210", i, sep = "")  
}
octurls <- na.omit(octurls)
oct <- map_df(.x= octurls, .f=weekschedfun)  

novurls <- c()
novdates <- c("01", "02", "03","04", "05", "06", "07", "08","09", 10:19)
for (i in novdates){
  novurls[i] <- str_c("https://www.espn.com/nhl/scoreboard/_/date/202211", i, sep = "")   
}
nov <- map_df(.x= novurls, .f=weekschedfun)  

test<- rbind(oct, nov)

NHLGames<- test %>% mutate(PD = abs(PD))


Teams <- c("Ducks" = "ANA","Coyotes" = "ARI","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
NHLGames$ATeam <-as.character(Teams[NHLGames$ATeam])
NHLGames$HTeam <-as.character(Teams[NHLGames$HTeam])

write_csv(NHLGames, "/Users/seanfloersch/FloStrength/NHLFloStrength/NHLGames2223")

