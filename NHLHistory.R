library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
getNHLPlayer <- function(year){
  h <- read_html(str_c("https://www.hockey-reference.com/leagues/NHL_",year,"_skaters.html")) 
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
  if (year<2015){
  h <- read_html(str_c("https://www.hockey-reference.com/leagues/NHL_",year,"_skaters-advanced.html")) 
  stats <- html_nodes(h, "td") %>% html_text
  len <- length(stats) / 24
  dfadv<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:24)){
      marker = j + (i -1)* 24
      dfadv[i,j]<- stats[marker]
    }
  }
  }else{
    h <- read_html(str_c("https://www.hockey-reference.com/leagues/NHL_",year,"_skaters-advanced.html")) 
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(stats) / 25
    dfadv<- data.frame()
    for (i in c(1:len)) {
      for (j in c(1:25)){
        marker = j + (i -1)* 25
        dfadv[i,j]<- stats[marker]
      }
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
    mutate(Value = FloStrength * (TOI/340)) %>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A,S,SAtt.,TK,GV,BLK,HIT, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  dfD <- dfplay %>%
    filter(Pos == "D") %>%
    mutate(FloStrength = (2* PtScore + 1* TOScore+3 *DEFScore1)/6)%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (TOI/1060))%>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A,S,SAtt.,TK,GV,BLK,HIT, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  
  dfW <- dfplay %>%
    filter(Pos == "W") %>%
    mutate(FloStrength = (3* PtScore + .6* TOScore+.4 *DEFScore1+ 2*ShotScore)/6)%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (TOI/304))%>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A,S,SAtt.,TK,GV,BLK,HIT, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  dfplayer <- rbind(dfC, dfD, dfW)%>%
    mutate(SV=0)
  h <- read_html(str_c("https://www.hockey-reference.com/leagues/NHL_",year,"_goalies.html")) 
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
    select(Player, Age, Tm, Pos, GP,SV, TOI,FloStrength) %>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (GP/ 22)) %>%
    mutate(G = 0)%>%
    mutate(A = 0)%>%
    mutate(`+/-` = 0)%>%
    mutate(FOScore = 0)%>%
    mutate(ShotScore = 0)%>%
    mutate(PtScore = 0)%>%
    mutate(TOScore = 0)%>%
    mutate(DEFScore = 0)%>%
    mutate(DEFScore1 = 0)%>%
    mutate(S = 0)%>%
    mutate(SAtt. = 0)%>%
    mutate(TK = 0)%>%
    mutate(GV = 0)%>%
    mutate(BLK = 0)%>%
    mutate(HIT = 0)
  dfplayer <- rbind(dfplayer, dfgoal1) %>%
     mutate(yearID = year)
  return(dfplayer)
}
x<-map_df(.x=c(2008:2012),.f=getNHLPlayer)
y<-map_df(.x=c(2013:2017),.f=getNHLPlayer)
z<-map_df(.x=c(2018:2022),.f=getNHLPlayer)
df <- rbind(x,y,z) %>%
  filter((TOI>650&yearID!=2013)|(TOI>350&yearID==2013))


#################################################################################
# Team Stats
#################################################
playerdf<-df
getNHLTeam<-function(year){
  h <- read_html(str_c("https://www.espn.com/nhl/stats/team/_/season/",year,"/seasontype/2/table/offensive/sort/avgGoals/dir/desc"))
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
  
  h <- read_html(str_c("https://www.espn.com/nhl/stats/team/_/view/goaltending/season/",year,"/seasontype/2/table/defensive/sort/avgGoalsAgainst/dir/asc")) 
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
    mutate(SavePer = 1 - SavePer)%>%
    mutate(Defense = GAPG + 25 * SavePer) %>%
    mutate(Discapline = (1-PPSucRate) - PenKill) %>%
    mutate(Offense = (.25 * Assists + GS + .25 * Shots + PPSucRate) / GP) %>%
    select(Team,GP,WinPct,Offense, Defense, Discapline,GS,GA) %>%
    mutate(Defense = -1 *((Defense - mean(Defense)) / sd(Defense))) %>%
    mutate(Offense = (Offense - mean(Offense)) / sd(Offense)) %>%
    mutate(Discapline = -1*((Discapline - mean(Discapline)) / sd(Discapline))) %>%
    mutate(TeamScore = .5 + 0.06256* Offense + 0.05065* Defense + 0.01751 *Discapline) %>%
    mutate(yearID = year)
  
  Teams <- c("Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG","Atlanta Thrashers"="ATL","Phoenix Coyotes"="PHX")
  NHLMaster$Team <-as.character(Teams[NHLMaster$Team])
  playerdf<-playerdf %>%
    filter(yearID == year)
  FPC <- playerdf %>%
    filter(Pos == "C" & Tm != "TOT") %>%
    group_by(Tm,yearID) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(CenScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, CenScore)
  FPD <- playerdf %>%
    filter(Pos == "D" & Tm != "TOT") %>%
    group_by(Tm,yearID) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(DefScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, DefScore)
  FPW <- playerdf %>%
    filter(Pos == "W" & Tm != "TOT") %>%
    group_by(Tm,yearID) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(WingScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, WingScore)
  FPG <- playerdf %>%
    filter(Pos == "G" & Tm != "TOT") %>%
    group_by(Tm,yearID) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(GScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, GScore)
  NHLTP <- left_join(FPC, FPD, by = "Tm")
  NHLTP <- left_join(NHLTP, FPG, by = "Tm")
  NHLTP <- left_join(NHLTP, FPW, by = "Tm") %>%
    mutate(GScore= ifelse(is.na(GScore)==TRUE,-1.5,GScore)) %>%
    mutate(PlayerScore = WingScore + GScore+CenScore+DefScore) %>%
    mutate(yearID = year) %>%
    group_by(yearID) %>%
    mutate(PlayerScore = (3 +((PlayerScore - mean(PlayerScore))/ sd(PlayerScore)))/6) %>%
    mutate(Team = Tm) %>%
    ungroup %>%
    select(-Tm)
  PlayWL<- playerdf %>%
    group_by(Tm,yearID) %>%
    mutate(PlayWL = sum((Value))) %>%
    slice(1) %>%
    ungroup() %>%
    select("Team"="Tm",yearID, PlayWL)
  NHLMaster <- left_join(NHLMaster, NHLTP, by = c("Team","yearID"))
  NHLMaster <- left_join(NHLMaster, PlayWL, by = c("Team","yearID"))
  return(NHLMaster)
}
x<-map_df(.x=c(2008:2012),.f=getNHLTeam)
y<-map_df(.x=c(2013:2017),.f=getNHLTeam)
z<-map_df(.x=c(2018:2022),.f=getNHLTeam)
teams = rbind(x,y,z) 
linmod <- lm(WinPct~Offense+Defense+Discapline, data = teams)
teams<- teams%>%
  mutate(TeamScore = predict(linmod)) %>%
  mutate(TeamPts = 2*WinPct*GP)%>%
  group_by(yearID)%>%
  mutate(PlayWL = mean(TeamPts)+ PlayWL-22*(min(GP)/82)) %>%
  mutate(PlayWL = PlayWL / (min(GP)*2)) %>%
  ungroup()
linmod<-lm(WinPct~PlayWL+TeamScore,data = teams)
teams<- teams %>%
  mutate(FloStrength = predict(linmod))

getNHLSched<- function(year){
  h <- read_html(str_c("https://www.hockey-reference.com/leagues/NHL_",year,"_games.html")) 
  away <- html_nodes(h, ".left+ td.left") %>% html_text 
  home <- html_nodes(h, "td:nth-child(4)") %>% html_text
  as <- html_nodes(h, ".right:nth-child(3)") %>% html_text%>% as.numeric()
  hs <- html_nodes(h, ".right~ .left+ .right") %>% html_text%>% as.numeric()
  date <- html_nodes(h, "th a") %>% html_text
  sched <- data.frame(away,home,as,hs) %>%
    na.omit() %>%
    mutate(yearID = year)%>%
    group_by(away) %>%
    mutate(ag = c(1:length(away))) %>%
    ungroup()%>%
    group_by(home) %>%
    mutate(hg = c(1:length(home))) %>%
    ungroup()
  return(sched)
}
x<-map_df(.x=c(2008:2022),.f=getNHLSched)
regszn <- x %>% 
  group_by(yearID) %>%
  slice(1:(length(unique(away))*41)) %>%
  ungroup()
regszn20 <- regszn %>%
  filter(yearID == 2020) %>%
  slice(1:1082)
regszn21 <- regszn%>%
  filter(yearID == 2021)  %>%
  slice(1:868)
regszn13 <- regszn %>%
  filter(yearID == 2013) %>%
  slice(1:720)
filtregszn<- regszn %>%
  filter(yearID != 2013 & yearID != 2020)%>%
  filter(yearID != 2021)
regszn <- rbind(regszn13,regszn20,regszn21,filtregszn)
Teams <- c("Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG","Atlanta Thrashers"="ATL","Phoenix Coyotes"="PHX")
regszn$away <-as.character(Teams[regszn$away])
regszn$home <-as.character(Teams[regszn$home])
awayte<- teams %>%
  select("away"=Team, yearID,"AFS"=FloStrength)
regszn1 <- left_join(regszn, awayte, by = c("away","yearID"))
homete<- teams %>%
  select("home"=Team, yearID,"HFS"=FloStrength)
regszn1 <- left_join(regszn1, homete, by = c("home","yearID"))
fssched <- regszn1 %>%
  mutate(PD = as - hs) %>%
  mutate(AFSS = ifelse(PD>0,HFS*PD,(1-HFS)*PD))%>%
  mutate(PD = hs - as) %>%
  mutate(HFSS = ifelse(PD>0,AFS*PD,(1-AFS)*PD))
afs <- fssched %>%
  group_by(away,yearID) %>%
  mutate(AFS = sum(AFSS)) %>%
  mutate(agp = length(away)) %>%
  slice(1)%>%
  ungroup() %>%
  select("Team"=away,yearID, agp,AFS)
hfs <- fssched %>%
  group_by(home,yearID) %>%
  mutate(HFS = sum(HFSS)) %>%
  mutate(hgp = length(home)) %>%
  slice(1)%>%
  ungroup() %>%
  select("Team"=home,yearID, hgp,HFS)
fssched <- left_join(afs,hfs, by = c("Team", "yearID")) %>%
  mutate(SchedFS = (AFS+HFS)/ (agp+hgp)) %>% 
  group_by(yearID) %>%
  mutate(SchedFS = (SchedFS - mean(SchedFS))/ sd(SchedFS)) %>%
  mutate(SchedFS = (5+SchedFS)/10) %>%
  ungroup() %>%
  select(Team,yearID, SchedFS)
teams<- left_join(teams, fssched, by = c("Team", "yearID"))
lin<- lm(WinPct ~ SchedFS+PlayWL+TeamScore, data = teams)
Master<- teams %>%
  mutate(FloStrength = predict(lin))


PO <-map_df(.x=c(2008:2022),.f=getNHLSched) 
PlayOffCom <- PO %>%
  filter(yearID != 2013 & yearID != 2020)%>%
  filter(yearID != 2021) %>%
  filter(ag>41 &hg>41) 
po20 <- PO %>%
  filter(yearID == 2020) %>%
  slice(1083:length(yearID))
po21 <- PO%>%
  filter(yearID == 2021)  %>%
  slice(869:length(yearID))
po13 <- PO %>%
  filter(yearID == 2013) %>%
  slice(721:length(yearID))
playoffs = rbind(PlayOffCom,po13,po20,po21)
playoff<- playoffs %>%
  filter(yearID == 2008)
at<-last(playoff$away)
ht<-last(playoff$home)
as<-last(playoff$as)
hs<-last(playoff$hs)
df <- data.frame(at,ht,as,hs) %>%
  mutate(Champ= ifelse(as>hs,at,ht)) %>%
  mutate(ConfChamp= ifelse(as>hs,ht,at)) %>%
  mutate(yearID = 2008) %>%
  select(Champ, ConfChamp,yearID)
x<-playoff %>% filter((away == df$Champ & home!=df$ConfChamp)|(home == df$Champ & away!=df$ConfChamp))
at<-last(x$away)
ht<-last(x$home)
y<-playoff %>% filter((away != df$Champ & home==df$ConfChamp)|(home != df$Champ & away==df$ConfChamp))
aty<-last(y$away)
hty<-last(y$home)
df <- df %>%
  mutate(DivChamp1 = ifelse(at==Champ, ht, at))%>%
  mutate(DivChamp2 = ifelse(aty==ConfChamp, hty, aty))
for (i in c(2009: max(playoffs$yearID))){
  playoff<- playoffs %>%
    filter(yearID == i)
  at<-last(playoff$away)
  ht<-last(playoff$home)
  as<-last(playoff$as)
  hs<-last(playoff$hs)
  df1<- data.frame(at,ht,as,hs) %>%
    mutate(Champ= ifelse(as>hs,at,ht)) %>%
    mutate(ConfChamp= ifelse(as>hs,ht,at)) %>%
    mutate(yearID = i) %>%
    select(Champ, ConfChamp,yearID)
  x<-playoff %>% filter((away == df1$Champ & home!=df1$ConfChamp)|(home == df1$Champ & away!=df1$ConfChamp))
  at<-last(x$away)
  ht<-last(x$home)
  y<-playoff %>% filter((away != df1$Champ & home==df1$ConfChamp)|(home != df1$Champ & away==df1$ConfChamp))
  aty<-last(y$away)
  hty<-last(y$home)
  df1 <- df1 %>%
    mutate(DivChamp1 = ifelse(at==Champ, ht, at))%>%
    mutate(DivChamp2 = ifelse(aty==ConfChamp, hty, aty))
  
  df<- rbind(df,df1)
}
poteams1 <- playoffs %>%
  select("Team"="away", yearID)
poteams2 <- playoffs %>%
  select("Team"="home", yearID)
poteams<- rbind(poteams1,poteams2) %>%
  group_by(Team, yearID) %>%
  slice(1) %>%
  ungroup()
poteams<- left_join(poteams, df, by = "yearID") 
pot<- poteams%>%
  mutate(POSuc = ifelse(Team ==Champ, 4,NA))%>%
  mutate(POSuc = ifelse(Team ==ConfChamp & is.na(POSuc)==TRUE, 3,POSuc))%>%
  mutate(POSuc = ifelse(Team ==DivChamp1 & is.na(POSuc)==TRUE, 2,POSuc))%>%
  mutate(POSuc = ifelse(Team ==DivChamp2 & is.na(POSuc)==TRUE, 2,POSuc))%>%
  mutate(POSuc = ifelse(is.na(POSuc)==TRUE, 1,POSuc)) %>%
  select(yearID,Team,POSuc)
Teams <- c("Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG","Atlanta Thrashers"="ATL","Phoenix Coyotes"="PHX")
pot$Team <-as.character(Teams[pot$Team])
Master <- left_join(Master, pot, by=c("Team", "yearID"))  %>%
  mutate(POSuc = ifelse(is.na(POSuc)==TRUE, 0,POSuc))
Master <- Master %>%
  mutate(resid1 = TeamScore-WinPct)%>%
  mutate(resid2 = PlayWL-WinPct)%>%
  mutate(SCWin = ifelse(POSuc==4|POSuc==3,1,0))
logmod<- glm(SCWin~(PlayWL+SchedFS+Offense+Defense+Discapline+resid1+resid2), data = Master, family = binomial)
Master <- Master %>%
  mutate(FloStrength =predict(logmod)) %>%
  group_by(yearID) %>%
  mutate(FloStrength = (FloStrength-mean(FloStrength))/ sd(FloStrength)) %>%
  ungroup() %>%
  mutate(FloStrength = (5+FloStrength)/10)
write_csv(Master, "/Users/seanfloersch/FloStrength/FloStrengthApp/NHLTeamsAT")
write_csv(playerdf, "/Users/seanfloersch/FloStrength/FloStrengthApp/NHLPlayersAT")
careers <- playerdf %>%
  mutate(Player = str_remove(Player, "\\*")) %>%
  mutate(birth = yearID - Age) %>%
  group_by(Player, birth) %>%
  mutate(Value = sum(Value)) %>%
  mutate(GP = sum(GP)) %>%
  mutate(G = sum(G)) %>%
  mutate(A = sum(A)) %>%
  mutate(SV = sum(SV)) %>%
  mutate(HIT = sum(HIT)) %>%
  mutate(BLK = sum(BLK)) %>%
  mutate(TK = sum(TK)) %>%
  mutate(GV = sum(GV)) %>%
  mutate(FloStrength = sum(FloStrength * TOI/sum(TOI))) %>%
  mutate(TOScore = sum(TOScore * TOI/sum(TOI))) %>%
  mutate(ShotScore = sum(ShotScore * TOI/sum(TOI))) %>%
  mutate(PtScore = sum(PtScore * TOI/sum(TOI))) %>%
  mutate(DEFScore = sum(DEFScore1 * TOI/sum(TOI))) %>%
  slice(1) %>%
  ungroup() %>%
  select(Player, Pos, GP, G, A,S,HIT,BLK,TK,GV,SV, DEFScore,ShotScore,PtScore,TOScore,FloStrength, Value) %>%
  filter(GP>99)
write_csv(careers, "/Users/seanfloersch/FloStrength/FloStrengthApp/NHLPlayerCarAT")

