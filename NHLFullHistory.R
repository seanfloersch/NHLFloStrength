getOldNHLPlayer <- function(year){
  if(year<2008){
    h <- read_html(str_c("https://www.hockey-reference.com/leagues/NHL_",year,"_skaters.html")) 
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(stats) / 22
    df<- data.frame()
    for (i in c(1:len)) {
      for (j in c(1:22)){
        marker = j + (i -1)* 22
        df[i,j]<- stats[marker]
      }
    }
    colnames(df)<- html_nodes(h, ".right+ .poptip , .poptip.right , .center+ .poptip , .left+ .poptip , .poptip.left") %>% html_text
    df<-df[,c(1:11,15,19:21)]
    dfplay<- df %>%
      group_by(Player)%>%
      mutate(Tm = last(Tm))%>%
      slice(1)
    for (i in c(2,5:15)) {
      dfplay[i] <- as.numeric(unlist(dfplay[i]))
    }
  }else{
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
    df <- df[,c(1:11,15,19:21)]
    dfplay<- df %>%
      group_by(Player)%>%
      mutate(Tm = last(Tm))%>%
      slice(1)
    for (i in c(2,5:15)) {
      dfplay[i] <- as.numeric(unlist(dfplay[i]))
    }
  }
  ind <- which(dfplay$Pos == "LW" | (dfplay$Pos == "RW" | dfplay$Pos == "F"))
  dfplay$Pos[ind] = "W"
  dfplay <- dfplay %>%
    mutate(PTSpm =(.0001 + ( G + .5 * A)/GP)) %>%
    mutate(Def = .00001+(`+/-`/GP)-PTSpm)%>%
    mutate(Shooting = (((G*G/(S+.00001)) + 0.00001))) %>%
    filter(GP>10) %>%
    mutate(yearID = year) %>%
    group_by(Pos) %>%
    mutate(PtScore = (PTSpm - mean(PTSpm)) / sd(PTSpm))%>%
    mutate(DefScore = (Def - mean(Def)) / sd(Def))%>%
    mutate(ShotScore = (Shooting - mean(Shooting)) / sd(Shooting))%>%
    ungroup %>%
    group_by(Tm) %>%
    mutate(DefScore = (DefScore - mean(DefScore)) / sd(DefScore))%>%
    ungroup %>%
    mutate(FloStrength = NA)
  dfC <- dfplay %>%
    filter(Pos == "C"|Pos == "W") %>%
    mutate(FloStrength = (2*PtScore +.7 * ShotScore))%>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    mutate(Value = FloStrength * (GP/20))
  dfD <- dfplay %>%
    filter(Pos == "D") %>%
    mutate(PtScore = (6+PtScore)^.15)%>%
    mutate(PtScore = (PtScore - mean(PtScore)) / sd(PtScore))%>%
    mutate(FloStrength = (3* PtScore +2 *DefScore))%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (GP/20))
  dfplayer <- rbind(dfC, dfD)%>%
    select(Player,Age,Tm,Pos, GP,G,A,"+/-",S,PtScore,DefScore,ShotScore,FloStrength,Value)%>%
    mutate(SV=0)%>%
    mutate(SA=0)
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
  dfgoal<- dfgoal %>%
    group_by(Player)%>%
    mutate(Tm = last(Tm))%>%
    slice(1)%>%
    ungroup()
  dfgoal1<-dfgoal %>%
    mutate(Pos = "G") %>%
    mutate(FloStrength = (SV/SA)) %>%
    filter(SA>4*max(GP))%>%
    select(Player, Age, Tm, Pos, GP,SV,SA,FloStrength) %>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (SA/ 300)) %>%
    mutate(G = 0)%>%
    mutate(A = 0)%>%
    mutate(`+/-` = 0)%>%
    mutate(DefScore = 0)%>%
    mutate(ShotScore = 0)%>%
    mutate(PtScore = 0)%>%
    mutate(S = 0)
  dfplayer <- rbind(dfplayer, dfgoal1) %>%
    mutate(yearID = year)
  return(dfplayer)
}
v<-map_df(.x=c(2018:2023),.f=getOldNHLPlayer)
w<-map_df(.x=c(2008:2017),.f=getOldNHLPlayer)
x<-map_df(.x=c(1998:2004,2006,2007),.f=getOldNHLPlayer)
z<-map_df(.x=c(1988:1997),.f=getOldNHLPlayer)
y<-map_df(.x=c(1978:1987),.f=getOldNHLPlayer)
u<-map_df(.x=c(1968:1977),.f=getOldNHLPlayer)
df<-rbind(u,v,w,x,y,z)
  
df<-df%>%
  mutate(Tm=ifelse(Tm=="WIN","WPG",Tm))%>%
  mutate(Player = str_remove(Player, "\\*"))%>%
  mutate(Value = ifelse(Pos=="G",.7*Value,Value))%>%
  mutate(Value = ifelse(Pos=="W",Value*.65,Value))%>%
  mutate(Value = ifelse(Pos=="D",Value*.7,Value))%>%
  mutate(Value = ifelse(Pos=="C",Value*.65,Value))
careers <- df %>%
  group_by(Player) %>%
  mutate(FloStrength = sum(GP*FloStrength)/sum(GP))%>%
  mutate(DefScore = sum(GP*DefScore)/sum(GP))%>%
  mutate(ShotScore = sum(GP*ShotScore)/sum(GP))%>%
  mutate(PtScore = sum(GP*PtScore)/sum(GP))%>%
  mutate(Value = sum(Value))%>%
  mutate(GP = sum(GP))%>%
  mutate(G = sum(G))%>%
  mutate(A = sum(A))%>%
  mutate(S = sum(S))%>%
  mutate(PlusMinus= sum(`+/-`))%>%
  mutate(SV= sum(SV))%>%
  mutate(SA= sum(SA))%>%
  slice(1)%>%
  ungroup()%>%
  select(-`+/-`)%>%
  filter(GP>100)
write_csv(df, "/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NHLPlayersAT")
write_csv(careers, "/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NHLPlayerCarAT")

###########################################################
#################Teams#####################################
###########################################################

NHLTeamHistory <- read_excel("~/Downloads/NHLTeamHistory.xlsx")%>%
  mutate(Team = str_remove(Team, "\\*"))%>%
  mutate(SOW=NA)%>%
  mutate(SOL=NA)
NHLTeamHistory1 <- read_excel("~/Downloads/NHLModernTeams.xlsx")%>%
  mutate(Team = str_remove(Team, "\\*"))%>%
  mutate(T=NA)
nhlteams<-rbind(NHLTeamHistory,NHLTeamHistory1)
Teams <- c("Mighty Ducks of Anaheim"="MDA","Minnesota North Stars" = "MNS","Oakland Seals"="OAK","Chicago Black Hawks"="CBH","California Golden Seals"="CGS","Atlanta Flames" ="ATF","Kansas City Scouts"="KCS","Cleveland Barons"="CLE","Colorado Rockies"="CLR","Hartford Whalers"="HAR","Quebec Nordiques"="QUE","Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG","Atlanta Thrashers"="ATL","Phoenix Coyotes"="PHX")
nhlteams$Team <-as.character(Teams[nhlteams$Team])
NHLMaster <- nhlteams %>%
  mutate(GS = GF) %>%
  mutate(SPG = S/GP)%>%
  mutate(GoalRatio = GS / GA) %>%
  mutate(WinPct = `PTS%`)%>%
  mutate(ShotRatio = S / SA) %>%
  mutate(SavePer = 1 - `SV%`)%>%
  mutate(Defense = `GA/G` + 25 * SavePer) %>%
  mutate(Discapline = (1-`PP%`) - `PK%`) %>%
  mutate(Offense = (GS + .25 * S + `PP%`) / GP) %>%
  select(Team,yearID,GP,WinPct,Offense, Defense, Discapline,GS,GA,SPG) %>%
  group_by(yearID)%>%
  mutate(Defense = -1 *((Defense - mean(Defense)) / sd(Defense))) %>%
  mutate(Offense = (Offense - mean(Offense)) / sd(Offense)) %>%
  mutate(Discapline = -1*((Discapline - mean(Discapline)) / sd(Discapline))) %>%
  mutate(TeamScore = 0.52606 + 0.04984* Offense + 0.05690* Defense + 0.01373 *Discapline)%>%
  ungroup()
linny<-lm(WinPct~Offense+Defense+Discapline, data = NHLMaster)

testdf<- df%>% group_by(Tm,Pos,yearID)%>% mutate(mvp= sum(Value))%>% slice(1) %>% ungroup()%>% filter(Tm!="TOT")%>% select(Tm,Pos,mvp,yearID)
C<-testdf%>% filter(Pos == "C")%>% mutate(CMVP=mvp)%>%select(Tm,yearID, CMVP)
D<-testdf%>% filter(Pos == "D")%>% mutate(DMVP=mvp)%>%select(Tm,yearID, DMVP)
G<-testdf%>% filter(Pos == "G")%>% mutate(GMVP=1.728*mvp)%>%select(Tm,yearID, GMVP)
W<-testdf%>% filter(Pos == "W")%>% mutate(WMVP=mvp)%>%select(Tm,yearID, WMVP)
testdf<- left_join(C,D,by = c("Tm","yearID"))
testdf<- left_join(testdf,G,by = c("Tm","yearID"))
testdf<- left_join(testdf,W,by = c("Tm","yearID"))%>%
  mutate(GMVP = ifelse(is.na(GMVP)==TRUE,-5,GMVP))%>%
  na.omit%>%
  mutate(PlayerScore = CMVP+DMVP+GMVP+WMVP)%>%
  group_by(yearID)%>%
  mutate(PlayerScore = (PlayerScore-mean(PlayerScore))/sd(PlayerScore))%>%
  ungroup()%>%
  select("Team"="Tm",yearID, PlayerScore)
NHLMaster<- left_join(NHLMaster, testdf, by = c("Team","yearID"))%>%
  mutate(PlayerScore = 0.52606+0.09086*PlayerScore)%>%
  na.omit()

linny<-lm(WinPct~PlayerScore, data = NHLMaster)
linmod<-lm(WinPct~PlayerScore+TeamScore,data = NHLMaster)
NHLMaster<- NHLMaster %>%
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
t<-map_df(.x=c(2022:2023),.f=getNHLSched)
x<-map_df(.x=c(2008:2021),.f=getNHLSched)
y<-map_df(.x=c(1992:2004,2006,2007),.f=getNHLSched)
u<-map_df(.x=c(1968:1977),.f=getNHLSched)
z<-map_df(.x=c(1978:1991),.f=getNHLSched)

sched<- rbind(t,u,x,y,z)
Teams <- c("Mighty Ducks of Anaheim"="MDA","Minnesota North Stars" = "MNS","Oakland Seals"="OAK","Chicago Black Hawks"="CBH","California Golden Seals"="CGS","Atlanta Flames" ="ATF","Kansas City Scouts"="KCS","Cleveland Barons"="CLE","Colorado Rockies"="CLR","Hartford Whalers"="HAR","Quebec Nordiques"="QUE","Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG","Atlanta Thrashers"="ATL","Phoenix Coyotes"="PHX")
sched$away <-as.character(Teams[sched$away])
sched$home <-as.character(Teams[sched$home])
awayfs<- NHLMaster %>%
  select("away"="Team", yearID, "awayFS"="FloStrength", "awayGP"="GP")
homefs<- NHLMaster %>%
  select("home"="Team", yearID, "homeFS"="FloStrength", "homeGP"="GP")
sched<- left_join(sched, awayfs, by = c("away","yearID"))
sched<- left_join(sched, homefs, by = c("home","yearID"))
completeyears <- sched%>%
  na.omit()%>%
  mutate(yrtest = ifelse(awayGP ==homeGP,0,1))%>%
  group_by(yearID)%>%
  mutate(yrtest = sum(yrtest))%>%
  slice(1)%>%
  ungroup()%>%
  filter(yrtest==0|yearID==2023)
yerp<-unique(completeyears$yearID)
schedcom<- sched %>%
  filter(yearID %in% yerp)%>%
  group_by(yearID)%>%
  slice(1:(length(unique(away))*(last(awayGP)/2)))%>%
  ungroup()
sched2020<-sched%>%
  filter(yearID == 2020) %>%
  slice(1:1082)
regszn<-rbind(schedcom, sched2020)
fssched <- regszn %>%
  na.omit()%>%
  mutate(PD = as - hs) %>%
  mutate(AFSS = ifelse(PD>0,homeFS*PD,(1-homeFS)*PD))%>%
  mutate(PD = hs - as) %>%
  mutate(HFSS = ifelse(PD>0,awayFS*PD,(1-awayFS)*PD))
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
  ungroup() %>%
  select(Team,yearID, SchedFS)
NHLMaster<- left_join(NHLMaster,fssched, by = c("Team","yearID"))
linny <- lm(WinPct~SchedFS, data = NHLMaster)
NHLMaster<- NHLMaster %>%
  mutate(SchedFS=0.5261+0.1013*SchedFS)
yerp<-unique(completeyears$yearID)
poszn<- sched %>%
  filter(yearID %in% yerp)%>%
  group_by(yearID)%>%
  slice(((length(unique(away))*(last(awayGP)/2))+1):length(yearID))%>%
  ungroup()
po2020<-sched%>%
  filter(yearID == 2020) %>%
  slice(1083:length(yearID))
poszn<-rbind(poszn, po2020)
champions <- poszn %>%
  group_by(yearID) %>%
  slice(length(yearID))%>%
  mutate(SCChamp= ifelse(as>hs,away,home))%>%
  select(yearID,"Team"=SCChamp) %>% 
  mutate(SCChamp = 1)
NHLMaster<-left_join(NHLMaster,champions, by = c("Team","yearID"))
NHLMaster<- NHLMaster %>%
  mutate(SCChamp = ifelse(is.na(SCChamp)==TRUE, 0,1))
NHLMaster <- NHLMaster %>%
  mutate(resid1= TeamScore-WinPct)%>%
  mutate(resid2= PlayerScore-WinPct)%>%
  mutate(resid3= SchedFS-WinPct)
best<-glm(SCChamp~TeamScore+PlayerScore+SchedFS+resid1+resid2+resid3, data = NHLMaster, family = "binomial")
NHLMaster1<- NHLMaster%>%
  mutate(FloStrength=predict(best))%>%
  group_by(yearID)%>%
  mutate(FloStrength= (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
  ungroup%>%
  mutate(FloStrength = (5+FloStrength)/10)
NHLMaster <- NHLMaster1 %>%
  select(Team,yearID, GP,WinPct,Offense,Defense,Discapline, GS,GA,SPG,TeamScore,PlayerScore,"SchedScore"="SchedFS","FloStrength", "SCChamp")%>%
  mutate(Offense = round(Offense,3))%>%
  mutate(Defense = round(Defense,3))%>%
  mutate(Discapline = round(Discapline,3))%>%
  mutate(SPG = round(SPG,3))%>%
  mutate(TeamScore = round(TeamScore,3))%>%
  mutate(SchedScore = round(SchedScore,3))%>%
  mutate(PlayerScore = round(PlayerScore,3))%>%
  mutate(FloStrength = round(FloStrength,3))

write_csv(NHLMaster, "/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NHLTeamsAT")




