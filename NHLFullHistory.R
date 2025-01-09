getOldNHLPlayer <- function(year){
  if(year>2007){
    h <- read_html(str_c("https://www.hockey-reference.com/leagues/NHL_",year,"_skaters.html")) 
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(stats) / 30
    df<- data.frame()
    for (i in c(1:len)) {
      for (j in c(1:30)){
        marker = j + (i -1)* 30
        df[i,j]<- stats[marker]
      }
    }
    colnames(df)<- (html_nodes(h, ".right+ .poptip , .poptip.right , .center+ .poptip , .left+ .poptip , .poptip.left") %>% html_text)[7:36]
    dfplay<- df %>%
      group_by(Player)%>%
      mutate(Team = last(Team))%>%
      slice(1)%>%
      ungroup()%>%
      select(Player,Age,Team,Pos,GP,G,A,PTS,`+/-`,"S"=SOG,ATOI)%>%
      mutate(Pos1=Pos)
    for (i in c(2,5:10)) {
      dfplay[i] <- as.numeric(unlist(dfplay[i]))
    }
    ind <- which(dfplay$Pos == "LW" | (dfplay$Pos == "RW" | dfplay$Pos == "F"))
    dfplay$Pos1[ind] = "W"
    dfplay <- dfplay %>%
      mutate(PTSpm =(.0001 + ( G + .5 * A)/GP)) %>%
      mutate(Def = .00001+(`+/-`/GP)-PTSpm)%>%
      mutate(Shooting = (((G*G/(S+.00001)) + 0.00001))) %>%
      mutate(ATOI1=as.numeric(str_split(ATOI, "\\:", simplify=T)[,1]))%>%
      mutate(ATOI2=as.numeric(str_split(ATOI, "\\:", simplify=T)[,2])/60)%>%
      mutate(ATOI=ATOI1+ATOI2)%>%
      select(-ATOI1,-ATOI2)%>%
      filter(GP>8 &ATOI>12) %>%
      filter(Pos != "G")%>%
      mutate(yearID = year) %>%
      group_by(Pos1) %>%
      mutate(PtScore = (PTSpm - mean(PTSpm)) / sd(PTSpm))%>%
      mutate(DefScore = (Def - mean(Def)) / sd(Def))%>%
      mutate(ShotScore = (Shooting - mean(Shooting)) / sd(Shooting))%>%
      ungroup %>%
      group_by(Team) %>%
      mutate(DefScore = (DefScore - mean(DefScore)) / sd(DefScore))%>%
      ungroup %>%
      mutate(FloStrength = NA)
    dfC <- dfplay %>%
      filter(Pos1 == "C"|Pos1=="W") %>%
      mutate(FloStrength = (2*PtScore +.7 * ShotScore))%>%
      mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
      mutate(Value = FloStrength * (GP/31))
    dfD <- dfplay %>%
      filter(Pos1 == "D") %>%
      mutate(PtScore = (6+PtScore)^.15)%>%
      mutate(PtScore = (PtScore - mean(PtScore)) / sd(PtScore))%>%
      mutate(FloStrength = (3* PtScore +2 *DefScore))%>%
      mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
      mutate(Value = FloStrength * (GP/29))
    dfplayer <- rbind(dfC, dfD)%>%
      mutate(PTS=G+A)%>%
      mutate(SV=NA)%>%
      mutate(SavePer=NA)%>%
      select(Player,Age,Team,Pos, GP,ATOI,G,A,PTS,S,"+/-",SV,SavePer,FloStrength,Value)
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
      mutate(Team = last(Tm))%>%
      slice(1)%>%
      ungroup()
    dfgoal1<-dfgoal %>%
      mutate(Pos = "G") %>%
      mutate(FloStrength = (SV/SA)) %>%
      filter(SA>4*max(GP))%>%
      mutate(SavePer=SV/SA)%>%
      group_by(Pos) %>%
      mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
      ungroup() %>%
      mutate(Value = FloStrength * (SA/ 428)) %>%
      mutate(ATOI=(MIN/GP))%>%
      select(Player, Age, Team, Pos, GP,SV,SavePer,"ATOI",FloStrength,Value) %>%
      mutate(G = NA)%>%
      mutate(A = NA)%>%
      mutate(`+/-` = NA)%>%
      mutate(S = NA)%>%
      mutate(PTS = NA)
    dfplayer <- rbind(dfplayer, dfgoal1) %>%
      mutate(yearID = year)%>%
      mutate(FloStrength=round(FloStrength,3))%>%
      mutate(SavePer=round(SavePer,3))%>%
      mutate(Value=round(Value,2))%>%
      mutate(ATOI=round(ATOI,2))
  }else{
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
    colnames(df)<- (html_nodes(h, ".right+ .poptip , .poptip.right , .center+ .poptip , .left+ .poptip , .poptip.left") %>% html_text)[5:26]
    dfplay<- df %>%
      group_by(Player)%>%
      mutate(Team = last(Team))%>%
      slice(1)%>%
      ungroup()%>%
      select(Player,Age,Team,Pos,GP,G,A,PTS,`+/-`,"S"=SOG,ATOI)%>%
      mutate(Pos1=Pos)
    for (i in c(2,5:10)) {
      dfplay[i] <- as.numeric(unlist(dfplay[i]))
    }
    ind <- which(dfplay$Pos == "LW" | (dfplay$Pos == "RW" | dfplay$Pos == "F"))
    dfplay$Pos1[ind] = "W"
    dfplay <- dfplay %>%
      filter(Pos!="G")%>%
      mutate(PTSpm =(.0001 + ( G + .5 * A)/GP)) %>%
      mutate(Def = .00001+(`+/-`/GP)-PTSpm)%>%
      mutate(Shooting = (((G*G/(S+.00001)) + 0.00001))) %>%
      mutate(ATOI1=ifelse(is.na(as.numeric(ATOI))==TRUE,0,as.numeric(str_split(ATOI, "\\:", simplify=T)[,1])))%>%
      mutate(ATOI2=ifelse(is.na(as.numeric(ATOI))==TRUE,0,as.numeric(str_split(ATOI, "\\:", simplify=T)[,2])/60))%>%
      mutate(ATOI=ATOI1+ATOI2)%>%
      select(-ATOI1,-ATOI2)%>%
      filter(GP>11) %>%
      mutate(yearID = year) %>%
      group_by(Pos1) %>%
      mutate(PtScore = (PTSpm - mean(PTSpm)) / sd(PTSpm))%>%
      mutate(DefScore = (Def - mean(Def)) / sd(Def))%>%
      mutate(ShotScore = (Shooting - mean(Shooting)) / sd(Shooting))%>%
      ungroup %>%
      group_by(Team) %>%
      mutate(DefScore = (DefScore - mean(DefScore)) / sd(DefScore))%>%
      ungroup %>%
      mutate(FloStrength = NA)
    dfC <- dfplay %>%
      filter(Pos1 == "C"|Pos1=="W") %>%
      mutate(FloStrength = (2*PtScore +.7 * ShotScore))%>%
      mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
      mutate(Value = FloStrength * (GP/31))
    dfD <- dfplay %>%
      filter(Pos1 == "D") %>%
      mutate(PtScore = (6+PtScore)^.15)%>%
      mutate(PtScore = (PtScore - mean(PtScore)) / sd(PtScore))%>%
      mutate(FloStrength = (3* PtScore +2 *DefScore))%>%
      mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
      mutate(Value = FloStrength * (GP/29))
    dfplayer <- rbind(dfC, dfD)%>%
      mutate(PTS=G+A)%>%
      mutate(SV=NA)%>%
      mutate(SavePer=NA)%>%
      select(Player,Age,Team,Pos, GP,ATOI,G,A,PTS,S,"+/-",SV,SavePer,FloStrength,Value)
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
      mutate(Team = last(Tm))%>%
      slice(1)%>%
      ungroup()
    dfgoal1<-dfgoal %>%
      mutate(Pos = "G") %>%
      mutate(FloStrength = (SV/SA)) %>%
      filter(SA>4*max(GP))%>%
      mutate(SavePer=SV/SA)%>%
      group_by(Pos) %>%
      mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
      ungroup() %>%
      mutate(Value = FloStrength * (SA/ 428)) %>%
      mutate(ATOI=(MIN/GP))%>%
      select(Player, Age, Team, Pos, GP,SV,SavePer,"ATOI",FloStrength,Value) %>%
      mutate(G = NA)%>%
      mutate(A = NA)%>%
      mutate(`+/-` = NA)%>%
      mutate(S = NA)%>%
      mutate(PTS = NA)
    dfplayer <- rbind(dfplayer, dfgoal1) %>%
      mutate(yearID = year)%>%
      mutate(FloStrength=round(FloStrength,3))%>%
      mutate(SavePer=round(SavePer,3))%>%
      mutate(Value=round(Value,2))%>%
      mutate(ATOI=round(ATOI,2))
  }

  return(dfplayer)
}
v<-map_df(.x=c(2017:2024),.f=getOldNHLPlayer)
w<-map_df(.x=c(2009:2016),.f=getOldNHLPlayer)
x<-map_df(.x=c(1998:2004,2006:2008),.f=getOldNHLPlayer)
z<-map_df(.x=c(1988:1997),.f=getOldNHLPlayer)
y<-map_df(.x=c(1978:1987),.f=getOldNHLPlayer)
u<-map_df(.x=c(1968:1977),.f=getOldNHLPlayer)
df<-rbind(u,v,w,x,y,z)%>%
  mutate(Value=ifelse(Pos=="G",round(Value*.7,2),Value))
  
df<-df%>%
  mutate(Team=ifelse(Team=="WIN","WPG",Team))%>%
  mutate(Player = str_remove(Player, "\\*"))%>%
  mutate(Team=ifelse(Team=="MNS","DAL",Team))%>%
  mutate(Team=ifelse(Team=="OAK","CLE",Team))%>%
  mutate(Team=ifelse(Team=="CBH","CHI",Team))%>%
  mutate(Team=ifelse(Team=="CGS","CLE",Team))%>%
  mutate(Team=ifelse(Team=="ATF","CGY",Team))%>%
  mutate(Team=ifelse(Team=="KCS","NJD",Team))%>%
  mutate(Team=ifelse(Team=="CLR","NJD",Team))%>%
  mutate(Team=ifelse(Team=="ATL","WPG",Team))%>%
  mutate(Team=ifelse(Team=="PHX","ARI",Team))%>%
  mutate(Team=ifelse(Team=="MDA","ANA",Team))%>%
  mutate(Team=ifelse(Team=="HAR","CAR",Team))%>%
  mutate(Team=ifelse(Team=="QUE","COL",Team))

write_csv(df, "/Users/seanfloersch/FloStrength/HockeyApp/NHLPlayersAT.csv")

###########################################################
#################Teams#####################################
###########################################################

NHLTeamHistory <- read_excel("~/Downloads/NHLTeamHistory.xlsx")%>%
  mutate(Team = str_remove(Team, "\\*"))%>%
  mutate(SOW=NA)%>%
  mutate(SOL=NA)
NHLTeamHistory1 <- read_excel("~/Downloads/NHLModernTeam1.xlsx")%>%
  mutate(Team = str_remove(Team, "\\*"))%>%
  mutate(T=0)
nhlteams<-rbind(NHLTeamHistory,NHLTeamHistory1)
Teams <- c("Mighty Ducks of Anaheim"="ANA","Minnesota North Stars" = "DAL","Oakland Seals"="CLE","Chicago Black Hawks"="CHI","California Golden Seals"="CLE","Atlanta Flames" ="CGY","Kansas City Scouts"="NJD","Cleveland Barons"="CLE","Colorado Rockies"="NJD","Hartford Whalers"="CAR","Quebec Nordiques"="COL","Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG","Atlanta Thrashers"="WPG","Phoenix Coyotes"="ARI")
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
  mutate("OTL/T"=OL+T)%>%
  select(Team,yearID,GP,W,L,"OTL/T",PTS,WinPct,Offense, Defense, Discapline,GS,GA,"SHO"=SO,"Pen"=PPOA) %>%
  group_by(yearID)%>%
  mutate(Defense = -1 *((Defense - mean(Defense)) / sd(Defense))) %>%
  mutate(Offense = (Offense - mean(Offense)) / sd(Offense)) %>%
  mutate(Discapline = -1*((Discapline - mean(Discapline)) / sd(Discapline))) %>%
  mutate(TeamScore = 0.52606 + 0.04984* Offense + 0.05690* Defense + 0.01373 *Discapline)%>%
  mutate(Defense=round(Defense,3))%>%
  mutate(Offense=round(Offense,3))%>%
  mutate(Discapline=round(Discapline,3))%>%
  mutate(TeamScore=round(TeamScore,3))%>%
  ungroup()
linny<-lm(WinPct~Offense+Defense+Discapline, data = NHLMaster)

testdf<- df%>% group_by(Team,Pos,yearID)%>% mutate(mvp= sum(Value))%>% slice(1) %>% ungroup()%>% filter(Team!="TOT")%>% select(Team,Pos,mvp,yearID)
C<-testdf%>% filter(Pos == "C")%>% mutate(CMVP=mvp)%>%select(Team,yearID, CMVP)
D<-testdf%>% filter(Pos == "D")%>% mutate(DMVP=mvp)%>%select(Team,yearID, DMVP)
G<-testdf%>% filter(Pos == "G")%>% mutate(GMVP=mvp)%>%select(Team,yearID, GMVP)
W<-testdf%>% filter(!Pos %in% c("C","D","G"))%>% group_by(Team,yearID)%>%mutate(WMVP=sum(mvp))%>%slice(1)%>%ungroup()%>%select(Team,yearID, WMVP)
testdf<- left_join(C,D,by = c("Team","yearID"))
testdf<- left_join(testdf,G,by = c("Team","yearID"))
testdf<- left_join(testdf,W,by = c("Team","yearID"))%>%
  mutate(GMVP = ifelse(is.na(GMVP)==TRUE,-5,GMVP))%>%
  na.omit%>%
  mutate(PlayerScore = CMVP+DMVP+GMVP+WMVP)%>%
  group_by(yearID)%>%
  mutate(PlayerScore = (PlayerScore-mean(PlayerScore))/sd(PlayerScore))%>%
  ungroup()%>%
  select("Team",yearID, PlayerScore)
NHLMaster<- left_join(NHLMaster, testdf, by = c("Team","yearID"))%>%
  mutate(PlayerScore = round(.527398+0.087875*PlayerScore,3))

linny<-lm(WinPct~PlayerScore, data = NHLMaster)
linmod<-lm(WinPct~PlayerScore+TeamScore,data = NHLMaster)
NHLMaster<- NHLMaster %>%
  mutate(FloStrength = round(-.05664+.75494*TeamScore+.35434*PlayerScore,3))
getNHLSched<- function(year){
  h <- read_html(str_c("https://www.hockey-reference.com/leagues/NHL_",year,"_games.html")) 
  away <- html_nodes(h, "td.left+ .left") %>% html_text %>%na.omit()
  home <- html_nodes(h, "td:nth-child(5)") %>% html_text%>%na.omit()
  as <- html_nodes(h, ".right:nth-child(4)") %>% html_text%>% as.numeric()%>%na.omit()
  hs <- html_nodes(h, ".right~ .left+ .right") %>% html_text%>% as.numeric()%>%na.omit()
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
t<-map_df(.x=c(2022:2024),.f=getNHLSched)
x<-map_df(.x=c(2008:2021),.f=getNHLSched)
y<-map_df(.x=c(1992:2004,2006,2007),.f=getNHLSched)
u<-map_df(.x=c(1968:1977),.f=getNHLSched)
z<-map_df(.x=c(1978:1991),.f=getNHLSched)

sched<- rbind(t,u,x,y,z)
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
  select(-resid1,-resid2,-resid3)%>%
  mutate(Offense = round(Offense,3))%>%
  mutate(Defense = round(Defense,3))%>%
  mutate(Discapline = round(Discapline,3))%>%
  mutate(TeamScore = round(TeamScore,3))%>%
  mutate(SchedFS = round(SchedFS,3))%>%
  mutate(PlayerScore = round(PlayerScore,3))%>%
  mutate(FloStrength = round(FloStrength,3))

write_csv(NHLMaster, "/Users/seanfloersch/FloStrength/HockeyApp/NHLTeamsAT.csv")




