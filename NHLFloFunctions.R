library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
rm(list=ls(all=TRUE)) # clear local memory
graphics.off() # close figures
cat("\014") # clear console
getDate <- function(x){
  x <- Sys.Date()
  x <- str_remove_all(x, "-")
  return(x)
}
getyestDate <- function(x){
  x <- Sys.Date() -1
  x <- str_remove_all(x, "-")
  return(x)
}
getNHLPlayer <- function(x){
  h <- read_html("https://www.hockey-reference.com/leagues/NHL_2023_skaters.html") 
  stats <- html_nodes(h, "td") %>% html_text
  len <- length(stats) / 27
  df<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:27)){
      marker = j + (i -1)* 27
      df[i,j]<- stats[marker]
    }
  }
  colnames(df)<- html_nodes(h, ".right+ .poptip , .poptip.right , .center+ .poptip , .left+ .poptip , .poptip.left") %>% html_text
  df <- df[,c(1:15,19:27)]
  df <- df %>% select(-PS, -EV,-PP,-SH,-GW, -`S%`, -ATOI)
  h <- read_html("https://www.hockey-reference.com/leagues/NHL_2023_skaters-advanced.html") 
  stats <- html_nodes(h, "td") %>% html_text
  len <- length(stats) / 25
  dfadv<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:25)){
      marker = j + (i -1)* 25
      dfadv[i,j]<- stats[marker]
    }
  }
  colnames(dfadv)<- html_nodes(h, ".right+ .poptip , .center+ .poptip , .left+ .poptip , .poptip.left") %>% html_text
  dfadv <- dfadv %>% select(Player,Tm,`CF%`, "OISH"=`oiSH%`, "OISV"=`oiSV%`, `PDO`, `dZS%`, TK,GV,`SAtt.`)
  dfplay <- left_join(df, dfadv, by = c("Player", "Tm"))
  for (i in c(2,5:25)) {
    dfplay[i] <- as.numeric(unlist(dfplay[i]))
  }
  ind <- which(dfplay$Pos == "LW" | (dfplay$Pos == "RW" | dfplay$Pos == "F"))
  dfplay$Pos[ind] = "W"
  dfplay <- dfplay %>%
    select(-`FO%`) %>%
    mutate(FOScore = FOW*FOW / (FOL + 1)) %>%
    mutate(PTSpm =(.0001 + ( G + .5 * A)/TOI)) %>%
    na.omit() %>%
    mutate(TORatio = (TK - GV)/ TOI) %>%
    mutate(Shooting = (((S*S/SAtt.) + 0.00001))) %>%
    mutate(TOIpg = TOI / GP) %>%
    mutate(Def = ((1.5 * BLK + HIT) / TOI)) %>%
    mutate(Def1 = OISV) %>%
    filter(GP > 4 & TOIpg>5) %>%
    mutate(yearID = 2022) %>%
    group_by(Pos) %>%
    mutate(PtScore = (PTSpm - mean(PTSpm)) / sd(PTSpm))%>%
    mutate(ShotScore = (Shooting - mean(Shooting)) / sd(Shooting))%>%
    mutate(TOScore = (TORatio - mean(TORatio)) / sd(TORatio))%>%
    mutate(DEFScore = (Def - mean(Def)) / sd(Def)) %>%
    mutate(DEFScore1 = (Def1 - mean(Def1)) / sd(Def1)) %>%
    mutate(FOScore = (FOScore - mean(FOScore)) / sd(FOScore)) %>%
    ungroup %>%
    mutate(FloStrength = NA)
  dfC <- dfplay %>%
    filter(Pos == "C") %>%
    mutate(FloStrength = (2*PtScore +.7 * ShotScore + .4 * TOScore+ .4*DEFScore1+ .5 * FOScore)/4)%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (TOI/500)) %>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  dfD <- dfplay %>%
    filter(Pos == "D") %>%
    mutate(FloStrength = (.6* PtScore + .4* TOScore+3 *DEFScore1+ 2*DEFScore)/6)%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (TOI/500))%>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  
  dfW <- dfplay %>%
    filter(Pos == "W") %>%
    mutate(FloStrength = (3* PtScore + .6* TOScore+.4 *DEFScore1+ 2*ShotScore)/6)%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (TOI/500))%>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  dfplayer <- rbind(dfC, dfD, dfW)
  
  h <- read_html("https://www.hockey-reference.com/leagues/NHL_2023_goalies.html") 
  stats <- html_nodes(h, "td") %>% html_text
  len <- length(stats) / 25
  dfgoal<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:25)){
      marker = j + (i -1)* 25
      dfgoal[i,j]<- stats[marker]
    }
  }
  colnames(dfgoal) <- (html_nodes(h, ".left+ .poptip , .sort_default_asc.left , .center+ .poptip") %>% html_text)[1:25]
  for (i in c(2,4:25)) {
    dfgoal[i] <- as.numeric(unlist(dfgoal[i]))
  }
  dfgoal1<-dfgoal %>%
    na.omit %>%
    mutate(Pos = "G") %>%
    mutate(TOI = MIN) %>%
    mutate(FloStrength = GSAA+ 30*(SO/GP) - 30*(RBS/GP)- 200 * (GA/TOI)) %>%
    select(Player, Age, Tm, Pos, GP, TOI,FloStrength) %>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (GP/ max(GP))) %>%
    mutate(G = 0)%>%
    mutate(A = 0)%>%
    mutate(`+/-` = 0)%>%
    mutate(FOScore = 0)%>%
    mutate(ShotScore = 0)%>%
    mutate(PtScore = 0)%>%
    mutate(TOScore = 0)%>%
    mutate(DEFScore = 0)%>%
    mutate(DEFScore1 = 0)
  dfplayer <- rbind(dfplayer, dfgoal1)
  return(dfplayer)
}

getNHLMaster <- function(NHLGames, FloPlayer){
  NHLGames <- NHLGames
  FloPlayer1 <- FloPlayer
  #################################################
  # Team Stats
  #################################################
  
  h <- read_html("https://www.espn.com/nhl/stats/team/_/season/2023/seasontype/2/table/offensive/sort/avgGoals/dir/desc") 
  Team <- html_nodes(h, ".TeamLink__Logo+ .AnchorLink") %>% html_text
  GP <- html_nodes(h, ".Table__TD:nth-child(1) div") %>% html_text%>% as.numeric
  GF <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(2) div") %>% html_text %>% as.numeric
  Assists <- html_nodes(h, ".Table__TD:nth-child(3) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PPGoals <- html_nodes(h, ".Table__TD:nth-child(5) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PPSucRate <- html_nodes(h, ".Table__TD:nth-child(6) div") %>% html_text %>% str_remove(",")%>% as.numeric
  SHG <- html_nodes(h, ".Table__TD:nth-child(7) div") %>% html_text %>% str_remove(",")%>% as.numeric
  Shots <- html_nodes(h, ".Table__TD:nth-child(8) div") %>% html_text %>% str_remove(",")%>% as.numeric
  GoalPer <- html_nodes(h, ".Table__TD:nth-child(9) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PenMin <- html_nodes(h, ".Table__TD:nth-child(10) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PenKill <- html_nodes(h, ".Table__TD:nth-child(11) div") %>% html_text %>% str_remove(",")%>% as.numeric
  SOA <- html_nodes(h, ".Table__TD:nth-child(12) div") %>% html_text %>% str_remove(",")%>% as.numeric
  SOG <- html_nodes(h, ".Table__TD:nth-child(13) div") %>% html_text %>% str_remove(",")%>% as.numeric
  SOP <- html_nodes(h, ".Table__TD:nth-child(14) div") %>% html_text %>% str_remove(",")%>% as.numeric
  
  dft <- data.frame(Team, GP, GF, Assists, PPGoals, PPSucRate, SHG, Shots, GoalPer, PenMin, PenKill, SOA, SOG, SOP)
  
  h <- read_html("https://www.espn.com/nhl/stats/team/_/view/goaltending/season/2023/seasontype/2/table/defensive/sort/avgGoalsAgainst/dir/asc") 
  Team <- html_nodes(h, ".TeamLink__Logo+ .AnchorLink") %>% html_text
  GAPG <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(2)") %>% html_text %>% as.numeric
  Wins <- html_nodes(h, ".Table__TD:nth-child(3)") %>% html_text %>% as.numeric
  Losses <- html_nodes(h, ".Table__TD:nth-child(4)") %>% html_text %>% as.numeric
  OTLoss <- html_nodes(h, ".Table__TD:nth-child(5)") %>% html_text %>% as.numeric
  SA <- html_nodes(h, ".Table__TD:nth-child(6)") %>% html_text %>% str_remove(",")%>%as.numeric
  GA <- html_nodes(h, ".Table__TD:nth-child(7)") %>% html_text %>% as.numeric
  Saves <- html_nodes(h, ".Table__TD:nth-child(8)") %>% html_text %>% str_remove(",")%>%as.numeric
  SavePer <- html_nodes(h, ".Table__TD:nth-child(9)") %>% html_text %>% as.numeric
  ShutOuts <- html_nodes(h, ".Table__TD:nth-child(10)") %>% html_text %>% as.numeric
  SOSA <- html_nodes(h, ".Table__TD:nth-child(11)") %>% html_text %>% as.numeric
  SOS <- html_nodes(h, ".Table__TD:nth-child(12)") %>% html_text %>% as.numeric
  SOSPer <- html_nodes(h, ".Table__TD:nth-child(13)") %>% html_text %>% as.numeric
  dfd <- data.frame(Team, GAPG, Wins, Losses, OTLoss, SA, GA, Saves, SavePer, ShutOuts, SOSA, SOS, SOSPer)
  
  
  TeamStats <- merge(dft, dfd, by = "Team")
  
  TeamStats <- TeamStats %>%
    mutate(wpct = (2* Wins + 1 * OTLoss) / (2*GP))
  
  NHLMaster <- TeamStats %>%
    mutate(GS = round(GF * GP)) %>%
    mutate(GoalRatio = GS / GA) %>%
    mutate(SPG = SA /GP) %>%
    mutate(WinPct = (Wins + .5 * OTLoss) / GP)%>%
    mutate(ShotRatio = Shots / SA) %>%
    mutate(Defense = GAPG + 6 * SavePer) %>%
    mutate(Discapline = (1-PPSucRate) - PenKill) %>%
    mutate(Offense = (.25 * Assists + GS + .25 * Shots + PPSucRate) / GP) %>%
    select(Team,WinPct,Offense, Defense, Discapline) %>%
    mutate(Defense = -1 *((Defense - mean(Defense)) / sd(Defense))) %>%
    mutate(Offense = (Offense - mean(Offense)) / sd(Offense)) %>%
    mutate(Discapline = -1*((Discapline - mean(Discapline)) / sd(Discapline))) %>%
    mutate(TeamScore = .5 + 0.06256* Offense + 0.05065* Defense + 0.01751 *Discapline)
  
  Teams <- c("Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG")
  NHLMaster$Team <-as.character(Teams[NHLMaster$Team])
  h <- read_html("https://www.espn.com/nhl/injuries") 
  Injured <- html_nodes(h, ".Table__TD .AnchorLink") %>% html_text 
  x <- setdiff(FloPlayer1$Player, Injured)
  FloPlayer <- FloPlayer1 %>%
    filter(Player %in% x)
  
  FPC <- FloPlayer %>%
    filter(Pos == "C" & Tm != "TOT") %>%
    group_by(Tm) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(CenScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, CenScore)
  FPD <- FloPlayer %>%
    filter(Pos == "D" & Tm != "TOT") %>%
    group_by(Tm) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(DefScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, DefScore)
  FPW <- FloPlayer %>%
    filter(Pos == "W" & Tm != "TOT") %>%
    group_by(Tm) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(WingScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, WingScore)
  FPG <- FloPlayer %>%
    filter(Pos == "G" & Tm != "TOT") %>%
    group_by(Tm) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(GScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, GScore)
  NHLTP <- left_join(FPC, FPD, by = "Tm")
  NHLTP <- left_join(NHLTP, FPG, by = "Tm")
  NHLTP <- left_join(NHLTP, FPW, by = "Tm") %>%
    mutate(PlayerScore = WingScore + GScore+CenScore+DefScore) %>%
    mutate(yearID = 2022) %>%
    group_by(yearID) %>%
    mutate(PlayerScore = (3 +((PlayerScore - mean(PlayerScore))/ sd(PlayerScore)))/6) %>%
    mutate(Team = Tm) %>%
    ungroup %>%
    select(-Tm)
  
  NHLMaster <- left_join(NHLMaster, NHLTP, by = "Team")
  
  #################################################
  # Massey Method
  #################################################
  
  AFS <- NHLMaster %>%
    select("ATeam"=Team, "ATS"=TeamScore)
  HFS <- NHLMaster %>%
    select("HTeam"=Team,"HTS"= TeamScore)
  NHLGames1 <- left_join(NHLGames, AFS, by = "ATeam")
  NHLGames1 <- left_join(NHLGames1, HFS, by = "HTeam") %>%
    mutate(AFS = NA) %>%
    mutate(HFS = NA)
  ind <- which(NHLGames1$AScore > NHLGames1$HScore )
  NHLGames1$AFS[ind] = NHLGames1$HTS[ind] * NHLGames1$PD[ind]
  NHLGames1$HFS[ind] = (1- NHLGames1$ATS[ind]) * NHLGames1$PD[ind] * -1 
  ind <- which(NHLGames1$AScore < NHLGames1$HScore)
  NHLGames1$HFS[ind] = NHLGames1$ATS[ind] * NHLGames1$PD[ind]
  NHLGames1$AFS[ind] = (1- NHLGames1$HTS[ind]) * NHLGames1$PD[ind] * -1 
  AS <- NHLGames1 %>%
    group_by(ATeam) %>%
    mutate(ASc = sum(AFS)) %>%
    mutate(AGP = length(AFS)) %>%
    slice(1) %>%
    select("Team" = ATeam, ASc, AGP)
  HS <- NHLGames1 %>%
    group_by(HTeam) %>%
    mutate(HSc = sum(HFS)) %>%
    mutate(HGP = length(HFS)) %>%
    slice(1) %>%
    select("Team" = HTeam, HSc, HGP)
  SS <- left_join(AS, HS, by = "Team") %>%
    mutate(SchedScore = (ASc + HSc) / (AGP+HGP)) %>%
    mutate(yearID = 2022) %>%
    group_by(yearID) %>%
    mutate(SchedScore = (3 +((SchedScore - mean(SchedScore))/ sd(SchedScore)))/6) %>%
    ungroup %>%
    select(Team, SchedScore)
  NHLMaster<- merge(NHLMaster, SS, by = "Team") %>%
    mutate(FloStrength = (TeamScore + PlayerScore + SchedScore)/ 3)
  NHLGames1 <-NHLGames %>%
    mutate(daynum = NA)
  gamedays <- unique(NHLGames1$date)
  for (i in c(1:length(gamedays))) {
    ind <- which(NHLGames1$date == gamedays[i])
    NHLGames1$daynum[ind] = i
  }
  AFS <- NHLMaster %>%
    select("ATeam"=Team, "ATS"=TeamScore)
  HFS <- NHLMaster %>%
    select("HTeam"=Team,"HTS"= TeamScore)
  NHLGames1 <- left_join(NHLGames1, AFS, by = "ATeam")
  NHLGames1 <- left_join(NHLGames1, HFS, by = "HTeam") %>%
    mutate(AFS = NA) %>%
    mutate(HFS = NA)
  ind <- which(NHLGames1$AScore > NHLGames1$HScore )
  NHLGames1$AFS[ind] = NHLGames1$HTS[ind] * NHLGames1$PD[ind]
  NHLGames1$HFS[ind] = (1- NHLGames1$ATS[ind]) * NHLGames1$PD[ind] * -1 
  ind <- which(NHLGames1$AScore < NHLGames1$HScore)
  NHLGames1$HFS[ind] = NHLGames1$ATS[ind] * NHLGames1$PD[ind]
  NHLGames1$AFS[ind] = (1- NHLGames1$HTS[ind]) * NHLGames1$PD[ind] * -1 
  away <- NHLGames1 %>%
    select("Team" = ATeam,"Sched"= AFS, daynum)
  home <- NHLGames1 %>%
    select("Team" = HTeam, "Sched" =HFS,daynum)
  sched3 <- rbind(away,home) %>%
    arrange(-daynum) %>%
    group_by(Team) %>%
    slice(1:3) %>%
    mutate(last3games = sum(Sched)/3) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last3games = ((last3games-mean(last3games))/ sd(last3games))/6) %>%
    mutate(last3games = last3games + .5) %>%
    ungroup()%>%
    select(Team, last3games)
  sched5 <- rbind(away,home) %>%
    arrange(daynum) %>%
    group_by(Team) %>%
    slice(1:5) %>%
    mutate(last5games = sum(Sched)/5) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last5games = ((last5games-mean(last5games))/ sd(last5games))/6) %>%
    mutate(last5games = last5games + .5) %>%
    ungroup()%>%
    select(Team, last5games)
  sched7 <- rbind(away,home) %>%
    arrange(daynum) %>%
    group_by(Team) %>%
    slice(1:7) %>%
    mutate(last7games = sum(Sched)/7) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last7games = ((last7games-mean(last7games))/ sd(last7games))/6) %>%
    mutate(last7games = last7games + .5) %>%
    ungroup()%>%
    select(Team, last7games)
  sched10 <- rbind(away,home) %>%
    arrange(daynum) %>%
    group_by(Team) %>%
    slice(1:10) %>%
    mutate(last10games = sum(Sched)/10) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last10games = ((last10games-mean(last10games))/ sd(last10games))/6) %>%
    mutate(last10games = last10games + .5) %>%
    ungroup()%>%
    select(Team, last10games)
  lastx <- left_join(sched3,sched5, by = "Team")
  lastx <- left_join(lastx,sched7, by = "Team")
  lastx <- left_join(lastx,sched10, by = "Team")
  yesterday <- max(NHLGames1$daynum)
  yesteam<- rbind(away,home) %>%
    mutate(wasyest = ifelse(daynum == yesterday, 1,0)) %>%
    filter(wasyest == 1) %>%
    select(Team) %>%
    mutate(PlayedYest = 1)
  lastx <- left_join(lastx,yesteam, by = "Team")
  ind <- which(is.na(lastx$PlayedYest) == TRUE)
  lastx$PlayedYest[ind] = 0
  NHLMaster <- left_join(NHLMaster, lastx, by = "Team")
  return(NHLMaster)
}  
getNHLResults <- function(yestdate){
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
      mutate(PD = abs(AScore - HScore)) %>%
      mutate(gameID = str_c(date, c(1:length(AScore)), by = "")) %>%
      mutate(date = date)
    Teams <- c("Ducks" = "ANA","Coyotes" = "ARI","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
    df$ATeam <-as.character(Teams[df$ATeam])
    df$HTeam <-as.character(Teams[df$HTeam])
    comp <- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLGames2223")
    df <- rbind(comp, df)
    return(df)
  }
  df <- weekschedfun(str_c("https://www.espn.com/nhl/scoreboard/_/date/", yestdate, sep = "") )
  
  df <- unique(df)
  
  return(df)  
}  
getPredictions <- function(toddate, NHLMaster){
  h <- read_html(str_c("https://www.espn.com/nhl/scoreboard/_/date/", toddate,sep = ""))
  AwayTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
  HomeTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
  toddf <- data.frame(AwayTeam, HomeTeam)
  Teams <- c("Ducks" = "ANA","Coyotes" = "ARI","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
  toddf$AwayTeam <-as.character(Teams[toddf$AwayTeam])
  toddf$HomeTeam <-as.character(Teams[toddf$HomeTeam])
  teamdf <- NHLMaster %>%
    rename("AwayTeam"="Team")
  toddf <- left_join(toddf, teamdf, by = "AwayTeam")
  for (i in c(3:20)) {
    colnames(toddf)[i]<- str_c("Away",colnames(toddf)[i], sep = "")
  }
  teamdf <- NHLMaster %>%
    rename("HomeTeam"="Team")
  toddf <- left_join(toddf, teamdf, by = "HomeTeam")
  for (i in c(21:38)) {
    colnames(toddf)[i]<- str_c("Home",colnames(toddf)[i], sep = "")
  }
  DailyOdds<- function(x) {
    x <- date() %>% str_extract("\\d{1,2}") %>% as.numeric
    h <- read_html("https://www.sportsline.com/nhl/odds/money-line/") 
    BovOddAway <- html_nodes(h, ".away-team td:nth-child(5) .primary") %>% html_text
    BovOddAway <- str_replace_all(BovOddAway, "\\+", "") %>% as.numeric
    BovOddHome <- html_nodes(h, ".home-team td:nth-child(5) .primary") %>% html_text
    BovOddHome <- str_replace_all(BovOddHome, "\\+", "") %>% as.numeric
    Team <- html_nodes(h, ".away-team .cfYQTQ") %>% html_text
    Opponent <- html_nodes(h, ".home-team .cfYQTQ") %>% html_text
    det <- html_nodes(h, ".game-details") %>% html_text
    day <- str_extract(det, "\\d{1,2}") %>% as.numeric
    
    h <- read_html("https://www.sportsline.com/nhl/odds/picks-against-the-spread/") 
    SpreadAway <- html_nodes(h, ".away-team td:nth-child(5) .primary") %>% html_text
    SpreadAway <- str_replace_all(SpreadAway, "\\+", "") %>% as.numeric
    SpreadHome <- html_nodes(h, ".home-team td:nth-child(5) .primary") %>% html_text
    SpreadHome <- str_replace_all(SpreadHome, "\\+", "") %>% as.numeric
    SpreadAwayOdd<- html_nodes(h, ".away-team td:nth-child(5) .secondary") %>% html_text
    SpreadAwayOdd <- str_replace_all(SpreadAwayOdd, "\\+", "") %>% as.numeric
    SpreadHomeOdd<- html_nodes(h, ".home-team td:nth-child(5) .secondary") %>% html_text
    SpreadHomeOdd <- str_replace_all(SpreadHomeOdd, "\\+", "") %>% as.numeric
    
    BookOdds <- data.frame(Team[1:length(SpreadHome)],BovOddAway[1:length(SpreadHome)], Opponent[1:length(SpreadHome)],BovOddHome[1:length(SpreadHome)] ,SpreadAway, SpreadAwayOdd, SpreadHome, SpreadHomeOdd)
    colnames(BookOdds) <- c("ATeam", "Aodd", "HTeam", "Hodd","ASpread", "SpreadAwayOdd", "HSpread", "SpreadHomeOdd")
    BookOdds <-BookOdds %>%
      mutate(BookWin = NA) %>%
      mutate(BookSpread = NA)
    Teams <- c("Ducks" = "ANA","Coyotes" = "ARI","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
    BookOdds$ATeam <-as.character(Teams[BookOdds$ATeam])
    BookOdds$HTeam <-as.character(Teams[BookOdds$HTeam])
    ind <- which(BookOdds$ASpread < 0)
    BookOdds$BookWin[ind] <- BookOdds$ATeam[ind]
    ind <- which(BookOdds$ASpread > 0)
    BookOdds$BookWin[ind] <- BookOdds$HTeam[ind]
    ind <- which(BookOdds$SpreadAwayOdd < 0)
    BookOdds$BookSpread[ind] <- BookOdds$ASpread[ind]
    ind <- which(BookOdds$SpreadAwayOdd > 0)
    BookOdds$BookSpread[ind] <- -1 * BookOdds$ASpread[ind]
    BookOdds <- BookOdds 
    return(BookOdds)
  }
  BookOdds <- DailyOdds(x) %>%
    mutate(gameID = str_c(ATeam, HTeam, toddate,sep = "")) %>%
    select(-ATeam, -HTeam)
  BookOdds <- BookOdds[1:length(toddf$AwayTeam),]
  toddf <- toddf %>%
    mutate(gameID = str_c(AwayTeam, HomeTeam, toddate,sep = ""))
  toddf1 <- left_join(toddf, BookOdds, by = "gameID") %>%
    mutate(predPD = -.4 + 10*AwayFloStrength -10 *HomeFloStrength) %>%
    mutate(FloWin = ifelse(predPD >0, AwayTeam, HomeTeam))
  return(toddf1)
}
updYestNHL <- function(NHLGames, yestdate){
  NHLGames1 <- NHLGames %>%
    filter(date == yestdate) %>%
    mutate(gameID = str_c(ATeam, HTeam, date)) %>%
    mutate(WTeam = ifelse(AScore > HScore, ATeam, HTeam))
  yestdf <- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLPredyest")
  yestcom <- left_join(yestdf, NHLGames1, by = "gameID") %>%
    mutate(FloCorr = ifelse(WTeam == FloWin, 1,0))%>%
    mutate(BookCorr = ifelse(WTeam == BookWin, 1,0))
  commod <- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLModelComp")
  commod <- rbind(commod, yestcom)
  return(commod)
}

yestdate <- getyestDate(x)
toddate <- getDate(x)

NHLPlayers <- getNHLPlayer(x)

NHLGames <- getNHLResults(yestdate)
write_csv(NHLGames, "/Users/seanfloersch/FloStrength/NHLFloStrength/NHLGames2223")

NHLModdf <- updYestNHL(NHLGames, yestdate)
write_csv(NHLModdf, "/Users/seanfloersch/FloStrength/NHLFloStrength/NHLModelComp")

NHLMaster <- getNHLMaster(NHLGames, NHLPlayers)

NHLPredTod <- getPredictions(toddate,NHLMaster)
write_csv(NHLPredTod, "/Users/seanfloersch/FloStrength/NHLFloStrength/NHLPredyest")

NHLPR <- NHLMaster %>%
  arrange(-FloStrength) %>%
  mutate(Rank = c(1:32)) %>%
  select(Rank, Team, FloStrength)
