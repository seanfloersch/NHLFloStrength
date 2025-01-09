
getNHLMaster <- function(x){
  getNHLPlayer <- function(x){
    year = 2025
    
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
    dfplayna<-dfplay
    dfplay <- dfplay %>%
      mutate(PTSpm =(.0001 + ( G + .5 * A)/GP)) %>%
      mutate(Def = .00001+(`+/-`/GP)-PTSpm)%>%
      mutate(Shooting = (((G*G/(S+.00001)) + 0.00001))) %>%
      mutate(ATOI1=as.numeric(str_split(ATOI, "\\:", simplify=T)[,1]))%>%
      mutate(ATOI2=as.numeric(str_split(ATOI, "\\:", simplify=T)[,2])/60)%>%
      mutate(ATOI=ATOI1+ATOI2)%>%
      select(-ATOI1,-ATOI2)%>%
      filter((GP*ATOI)>100) %>%
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
    len <- length(stats) / 28
    dfgoal<- data.frame()
    for (i in c(1:len)) {
      for (j in c(1:28)){
        marker = j + (i -1)* 28
        dfgoal[i,j]<- stats[marker]
      }
    }
    colnames(dfgoal) <- (html_nodes(h, "thead .over_header+ tr .center+ .center") %>% html_text)[1:28]
    for (i in c(2,5:15,17:28)) {
      dfgoal[i] <- as.numeric(unlist(dfgoal[i]))
    }
    dfgoal<- dfgoal %>%
      group_by(Player)%>%
      mutate(Team = last(Team))%>%
      slice(1)%>%
      ungroup()%>%
      filter(Player!="League Average")
    dfgoal1<-dfgoal %>%
      mutate(Pos = "G") %>%
      mutate(FloStrength = (SV/Shots)) %>%
      filter(Shots>60)%>%
      mutate(SavePer=SV/Shots)%>%
      group_by(Pos) %>%
      mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
      ungroup() %>%
      mutate(Value = FloStrength * (Shots/ 428))  %>%
      mutate(ATOI1=as.numeric(str_split(MIN, "\\:", simplify=T)[,1]))%>%
      mutate(ATOI2=as.numeric(str_split(MIN, "\\:", simplify=T)[,2])/60)%>%
      mutate(ATOI=(ATOI1+ATOI2)/GP)%>%
      select(Player, Age, Team, Pos, GP,SV,SavePer,"ATOI",FloStrength,Value) %>%
      mutate(G = NA)%>%
      mutate(A = NA)%>%
      mutate(`+/-` = NA)%>%
      mutate(S = NA)%>%
      mutate(PTS = NA)
    dfgoalna<-dfgoal %>%
      mutate(Pos = "G") %>%
      mutate(FloStrength = (SV/Shots)) %>%
      filter(Shots<=60)%>%
      filter(Shots>19)%>%
      mutate(SavePer=SV/Shots)%>%
      group_by(Pos) %>%
      mutate(FloStrength = NA) %>%
      ungroup() %>%
      mutate(Value = NA) %>%
      mutate(ATOI1=as.numeric(str_split(MIN, "\\:", simplify=T)[,1]))%>%
      mutate(ATOI2=as.numeric(str_split(MIN, "\\:", simplify=T)[,2])/60)%>%
      mutate(ATOI=(ATOI1+ATOI2)/GP)%>%
      select(Player, Age, Team, Pos, GP,SV,SavePer,"ATOI",FloStrength,Value) %>%
      mutate(G = NA)%>%
      mutate(A = NA)%>%
      mutate(`+/-` = NA)%>%
      mutate(S = NA)%>%
      mutate(PTS = NA)
    
    dfplayna <- dfplayna %>%
      mutate(PTSpm =(.0001 + ( G + .5 * A)/GP)) %>%
      mutate(Def = .00001+(`+/-`/GP)-PTSpm)%>%
      mutate(Shooting = (((G*G/(S+.00001)) + 0.00001))) %>%
      mutate(ATOI1=as.numeric(str_split(ATOI, "\\:", simplify=T)[,1]))%>%
      mutate(ATOI2=as.numeric(str_split(ATOI, "\\:", simplify=T)[,2])/60)%>%
      mutate(ATOI=ATOI1+ATOI2)%>%
      select(-ATOI1,-ATOI2)%>%
      filter((GP*ATOI)<100) %>%
      filter(Pos != "G")%>%
      mutate(yearID = year)%>%
      mutate(FloStrength = NA)%>%
      mutate(PTS=G+A)%>%
      mutate(SV=NA)%>%
      mutate(SavePer=NA)%>%
      mutate(Value=NA)%>%
      select(Player,Age,Team,Pos, GP,ATOI,G,A,PTS,S,"+/-",SV,SavePer,FloStrength,Value)%>%
      filter(ATOI>5 &GP>3)
    dfplayer <- rbind(dfplayer, dfgoal1,dfplayna,dfgoalna) %>%
      mutate(yearID = year)%>%
      mutate(FloStrength=round(FloStrength,3))%>%
      mutate(SavePer=round(SavePer,3))%>%
      mutate(Value=round(Value,2))%>%
      mutate(ATOI=round(ATOI,2))%>%
      rename("X..."=`+/-`)
    
    full<-read.csv("/Users/seanfloersch/FloStrength/HockeyApp/NHLPlayersAT.csv")%>%
      filter(yearID!=2025)
    careers=rbind(full,dfplayer) %>%
      group_by(Player) %>%
      mutate(FloStrength=ifelse(is.na(FloStrength)==TRUE,0,FloStrength))%>%
      mutate(Value=ifelse(is.na(Value)==TRUE,0,Value))%>%
      mutate(FloStrength = round(sum(GP*FloStrength)/sum(GP),3))%>%
      mutate(ATOI = round(sum(GP*ATOI)/sum(GP),2))%>%
      mutate(Value = sum(Value))%>%
      mutate(GP = sum(GP))%>%
      mutate(G = sum(G))%>%
      mutate(A = sum(A))%>%
      mutate(PTS = sum(PTS))%>%
      mutate(S = sum(S))%>%
      mutate(PlusMinus= sum(`X...`))%>%
      mutate(SV= sum(SV))%>%
      mutate(SavePer = round(sum(SV*SavePer)/sum(SV),2))%>%
      slice(1)%>%
      ungroup()%>%
      select(-`X...`)
    write_csv(careers, "/Users/seanfloersch/FloStrength/HockeyApp/NHLPlayerCarAT.csv")
    careers=rbind(full,dfplayer)
    write_csv(careers, "/Users/seanfloersch/FloStrength/HockeyApp/NHLPlayersAT.csv")
    return(dfplayer)
  }
  getNHLResults <- function(x){
    h <- read_html("https://www.hockey-reference.com/leagues/NHL_2025_games.html") 
    Date <- html_nodes(h, ".left:nth-child(1)") %>% html_text
    Date<-Date[-which(Date=="Date")]
    ATeam <- html_nodes(h, "td.left+ .left") %>% html_text
    HTeam <- html_nodes(h, "td:nth-child(5)") %>% html_text
    AScore <- html_nodes(h, "td:nth-child(4)") %>% html_text%>%as.numeric()
    HScore <- html_nodes(h, "td:nth-child(6)") %>% html_text%>%as.numeric()
    df<-data.frame(Date,ATeam,HTeam,AScore,HScore)%>%
      na.omit()%>%
      mutate(gameID=str_remove_all(Date,"-"))
    Teams <- c("Utah Hockey Club"="UTA","Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG")
    df$ATeam <-as.character(Teams[df$ATeam])
    df$HTeam <-as.character(Teams[df$HTeam])
    df<-df%>%
      mutate(gameID=str_c(ATeam,HTeam,gameID))
    return(df)  
  }  
  NHLGames <- getNHLResults()
  FloPlayer1 <- getNHLPlayer()
  #################################################
  # Team Stats
  #################################################
  
  h <- read_html("https://www.cbssports.com/nhl/stats/team/team/scoring/nhl/regular/") 
  Team <- html_nodes(h, ".TeamName a") %>% html_text
  GP <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(2)") %>% html_text%>% as.numeric
  GF <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(3)") %>% html_text %>% as.numeric
  Assists <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(4)") %>% html_text %>% str_remove(",")%>% as.numeric
  Shots <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(6)") %>% html_text %>% str_remove(",")%>% as.numeric
  PPG <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(8)") %>% html_text %>% str_remove(",")%>% as.numeric
  dft <- data.frame(Team, GP, GF, Assists, Shots,PPG)
  
  h <- read_html("https://www.cbssports.com/nhl/stats/team/opponent/scoring/nhl/regular/") 
  Team <- html_nodes(h, ".TeamName a") %>% html_text
  GA <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(3)") %>% html_text %>% as.numeric
  oppAst <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(4)") %>% html_text %>% str_remove(",")%>% as.numeric
  oppSh <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(6)") %>% html_text %>% str_remove(",")%>% as.numeric
  oppPPG <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(8)") %>% html_text %>% str_remove(",")%>% as.numeric
  dfo <- data.frame(Team, GA, oppAst, oppSh,oppPPG)
  
  h <- read_html("https://www.cbssports.com/nhl/stats/team/team/penalties/nhl/regular/") 
  Team <- html_nodes(h, ".TeamName a") %>% html_text
  PM <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(3)") %>% html_text %>% as.numeric
  dfp <- data.frame(Team, PM)
  
  h <- read_html("https://www.cbssports.com/nhl/stats/team/opponent/penalties/nhl/regular/") 
  Team <- html_nodes(h, ".TeamName a") %>% html_text
  oppPM <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(3)") %>% html_text %>% as.numeric
  dfpo <- data.frame(Team, oppPM)
  
  h <- read_html("https://www.cbssports.com/nhl/stats/team/team/goaltending/nhl/regular/") 
  Team <- html_nodes(h, ".TeamName a") %>% html_text
  Wins <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(3)") %>% html_text %>% as.numeric
  Losses <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(4)") %>% html_text %>% as.numeric
  OTLoss <- html_nodes(h, ".TableBase-bodyTd--number:nth-child(5)") %>% html_text %>% as.numeric
  dfs <- data.frame(Team, Wins, Losses, OTLoss)
  
  
  TeamStats <- merge(dft, dfo, by = "Team")
  TeamStats <- merge(dfs, TeamStats, by = "Team")
  TeamStats <- merge(TeamStats, dfp, by = "Team")
  TeamStats <- merge(TeamStats, dfpo, by = "Team")
  
  NHLMaster <- TeamStats %>%
    mutate(PPG=PPG/(oppPM/3))%>%
    mutate(oppPPG=oppPPG/(PM/3))%>%
    mutate(WinPct = (Wins + .5 * OTLoss) / GP)%>%
    mutate(Defense = (.25 * oppAst + GA + .25 * oppSh) / GP) %>%
    mutate(Discapline = PPG- oppPPG) %>%
    mutate(Offense = (.25 * Assists + GF + .25 * Shots) / GP) %>%
    select(Team,WinPct,GP,"W"="Wins","L"="Losses","OTL"="OTLoss",GF,GA,Shots,oppSh,PM,oppPM,,Offense, Defense, Discapline) %>%
    mutate(Defense = -1 *((Defense - mean(Defense)) / sd(Defense))) %>%
    mutate(Offense = (Offense - mean(Offense)) / sd(Offense)) %>%
    mutate(Discapline = -1*((Discapline - mean(Discapline)) / sd(Discapline))) %>%
    mutate(TeamScore = .5 + .08* Offense + .08* Defense + .02 *Discapline)
  
  Teams <- c("Utah"="UTA","Anaheim" = "ANA","Arizona" = "ARI","Boston" = "BOS","Buffalo" = "BUF","Calgary" = "CGY","Carolina" = "CAR","Chicago" = "CHI","Colorado" = "COL","Columbus" = "CBJ","Dallas" = "DAL","Detroit" = "DET","Edmonton" = "EDM","Florida" = "FLA","Los Angeles" = "LAK","Minnesota" = "MIN","Montreal" = "MTL","Nashville" = "NSH","New Jersey" = "NJD","N.Y. Islanders" = "NYI","N.Y. Rangers" = "NYR","Ottawa" = "OTT","Philadelphia" = "PHI","Pittsburgh" = "PIT","San Jose" = "SJS","Seattle" = "SEA","St. Louis" = "STL","Tampa Bay" = "TBL","Toronto" = "TOR","Vancouver" = "VAN","Vegas"="VEG","Washington" = "WSH", "Winnipeg"="WPG")
  NHLMaster$Team <-as.character(Teams[NHLMaster$Team])
  h <- read_html("https://www.espn.com/nhl/injuries") 
  Injured <- html_nodes(h, ".Table__TD .AnchorLink") %>% html_text 
  x <- setdiff(FloPlayer1$Player, Injured)
  FloPlayer <- FloPlayer1 %>%
    filter(Player %in% x)
  
  FPC <- FloPlayer %>%
    filter(is.na(FloStrength)!=TRUE)%>%
    filter(Pos == "C" & Team != "TOT") %>%
    group_by(Team) %>%
    mutate(WFS = FloStrength * (GP / sum(GP)))%>%
    mutate(CenScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Team, CenScore)
  FPD <- FloPlayer %>%
    filter(is.na(FloStrength)!=TRUE)%>%
    filter(Pos == "D" & Team != "TOT") %>%
    group_by(Team) %>%
    mutate(WFS = FloStrength * (GP / sum(GP)))%>%
    mutate(DefScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Team, DefScore)
  FPW <- FloPlayer %>%
    filter(is.na(FloStrength)!=TRUE)%>%
    filter(Pos %in% c("W","LW","RW") & Team != "TOT") %>%
    group_by(Team) %>%
    mutate(WFS = FloStrength * (GP / sum(GP)))%>%
    mutate(WingScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Team, WingScore)
  FPG <- FloPlayer %>%
    filter(is.na(FloStrength)!=TRUE)%>%
    filter(Pos == "G" & Team != "TOT") %>%
    group_by(Team) %>%
    mutate(WFS = FloStrength * (GP / sum(GP)))%>%
    mutate(GScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Team, GScore)
  NHLTP <- full_join(FPC, FPD, by = "Team")
  NHLTP <- full_join(NHLTP, FPG, by = "Team")
  NHLTP <- full_join(NHLTP, FPW, by = "Team") %>%
    mutate(GScore= ifelse(is.na(GScore)==TRUE,-1.5,GScore)) %>%
    mutate(WingScore= ifelse(is.na(WingScore)==TRUE,CenScore-1,WingScore)) %>%
    mutate(PlayerScore = WingScore + GScore+CenScore+DefScore) %>%
    mutate(yearID = 2025) %>%
    group_by(yearID) %>%
    mutate(PlayerScore = (3 +((PlayerScore - mean(PlayerScore))/ sd(PlayerScore)))/6) %>%
    ungroup 
  
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
    mutate(HFS = NA)%>%
    mutate(PD=abs(AScore-HScore))
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
    mutate(yearID = 2024) %>%
    group_by(yearID) %>%
    mutate(SchedScore = (3 +((SchedScore - mean(SchedScore))/ sd(SchedScore)))/6) %>%
    ungroup %>%
    select(Team, SchedScore)
  NHLMaster<- merge(NHLMaster, SS, by = "Team") %>%
    mutate(FloStrength = (TeamScore + PlayerScore + SchedScore)/ 3)
  NHLGames1 <-NHLGames %>%
    mutate(daynum = NA)
  gamedays <- unique(NHLGames1$Date)
  for (i in c(1:length(gamedays))) {
    ind <- which(NHLGames1$Date == gamedays[i])
    NHLGames1$daynum[ind] = i
  }
  AFS <- NHLMaster %>%
    select("ATeam"=Team, "ATS"=TeamScore)
  HFS <- NHLMaster %>%
    select("HTeam"=Team,"HTS"= TeamScore)
  NHLGames1 <- left_join(NHLGames1, AFS, by = "ATeam")
  NHLGames1 <- left_join(NHLGames1, HFS, by = "HTeam") %>%
    mutate(AFS = NA) %>%
    mutate(HFS = NA) %>%
    mutate(PD=abs(AScore-HScore))
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
    arrange(-daynum) %>%
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
    arrange(-daynum) %>%
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
    arrange(-daynum) %>%
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
  NHLMaster <- left_join(NHLMaster, lastx, by = "Team")%>%
    mutate(FloStrength = .5+2*TeamScore+PlayerScore+.25*SchedScore+.05*last3games+.1*last5games+.15*last7games+.2*last10games)%>%
    mutate(FloStrength=ifelse(is.na(PlayerScore)==TRUE,.5+2*TeamScore+.25*SchedScore+.05*last3games+.1*last5games+.15*last7games,FloStrength))%>%
    mutate(FloStrength= (FloStrength-mean(FloStrength))/sd(FloStrength))%>%
    mutate(FloStrength = (3+FloStrength)/6)
  for (i in c(2,13:28)){
    NHLMaster[,i]=round(NHLMaster[,i],3)
  }
  general<-read.csv("~/FloStrength/NHLFloStrength/TeamGeneral.csv")%>%
    mutate(Team=ifelse(Team=="ARI","UTA",Team))
  NHLMaster<-left_join(NHLMaster,general,by="Team")
  write_csv(NHLMaster, "/Users/seanfloersch/FloStrength/HockeyApp/NHLMaster.csv")
  
}  
getNHLPredictions <- function(yay){
  NHLMaster<-read.csv("/Users/seanfloersch/FloStrength/HockeyApp/NHLMaster.csv")%>%
    select(Team,Offense,Defense,Discapline,TeamScore,CenScore,DefScore,GScore,WingScore,PlayerScore,SchedScore,FloStrength,last3games,last5games,last7games,last10games,PlayedYest,GF,GA,GP)
  toddate <- str_remove_all(Sys.Date(), "-")
  h <- read_html("https://www.hockey-reference.com/leagues/NHL_2025_games.html") 
  Date <- html_nodes(h, ".left:nth-child(1)") %>% html_text
  Date<-Date[-which(Date=="Date")]
  AwayTeam <- html_nodes(h, ".left+ .left a") %>% html_text
  HomeTeam <- html_nodes(h, "td:nth-child(5)") %>% html_text
  Time <- html_nodes(h, "th+ td") %>% html_text
  df<-data.frame(Date,Time, AwayTeam,HomeTeam)
  Teams <- c("Anaheim Ducks" = "ANA","Utah Hockey Club" = "UTA","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG")
  df$AwayTeam <-as.character(Teams[df$AwayTeam])
  df$HomeTeam <-as.character(Teams[df$HomeTeam])
  today=as.character(Sys.Date())
  toddf<-df%>%
    filter(Date==today)%>%
    arrange(Time)
  Teams <- c("Ducks" = "ANA","Utah Hockey Club" = "UTA","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
  teamdf <- NHLMaster %>%
    rename("AwayTeam"="Team")
  for (i in c(2:20)) {
    colnames(teamdf)[i]<- str_c("Away",colnames(teamdf)[i], sep = "")
  }
  toddf <- left_join(toddf, teamdf, by = "AwayTeam")
  
  teamdf <- NHLMaster %>%
    rename("HomeTeam"="Team")
  for (i in c(2:20)) {
    colnames(teamdf)[i]<- str_c("Home",colnames(teamdf)[i], sep = "")
  }
  toddf <- left_join(toddf, teamdf, by = "HomeTeam")
  
  DailyOdds<- function(x) {
    x <- date() %>% str_extract("\\d{1,2}") %>% as.numeric
    h <- read_html("https://www.sportsline.com/nhl/odds/money-line/") 
    BovOddAway <- html_nodes(h, ".away-team td:nth-child(3) .primary") %>% html_text
    BovOddAway <- str_replace_all(BovOddAway, "\\+", "") %>% as.numeric
    BovOddHome <- html_nodes(h, ".home-team td:nth-child(3) .primary") %>% html_text
    BovOddHome <- str_replace_all(BovOddHome, "\\+", "") %>% as.numeric
    Team <- html_nodes(h, ".away-team .kLcZjE .dEoYge") %>% html_text
    Opponent <- html_nodes(h, ".home-team .kLcZjE .dEoYge") %>% html_text
    det <- html_nodes(h, ".game-details") %>% html_text
    day <- str_extract(det, "\\d{1,2}") %>% as.numeric
    
    h <- read_html("https://www.sportsline.com/nhl/odds/picks-against-the-spread/") 
    SpreadAway <- html_nodes(h, ".away-team td:nth-child(3) .primary") %>% html_text
    SpreadAway <- str_replace_all(SpreadAway, "\\+", "") %>% as.numeric
    SpreadHome <- html_nodes(h, ".home-team td:nth-child(3) .primary") %>% html_text
    SpreadHome <- str_replace_all(SpreadHome, "\\+", "") %>% as.numeric
    SpreadAwayOdd<- html_nodes(h, ".away-team td:nth-child(3) .secondary") %>% html_text
    SpreadAwayOdd <- str_replace_all(SpreadAwayOdd, "\\+", "") %>% as.numeric
    SpreadHomeOdd<- html_nodes(h, ".home-team td:nth-child(3) .secondary") %>% html_text
    SpreadHomeOdd <- str_replace_all(SpreadHomeOdd, "\\+", "") %>% as.numeric
    
    h <- read_html("https://www.sportsline.com/nhl/odds/over-under/") 
    ou <- html_nodes(h, ".away-team .projected-score+ td") %>% html_text
    ou<-substr(ou,2,4)
    ou<-str_remove(ou,"-")
    ou<-str_remove(ou,"\\+")
    filt<-which(as.numeric(ou)>10)
    ou[filt]<-substr(ou[filt],1,1)
    BookOdds <- data.frame(Team[1:length(SpreadHome)], Opponent[1:length(SpreadHome)] ,SpreadAway, SpreadAwayOdd, SpreadHome, SpreadHomeOdd,ou[1:length(SpreadHome)])
    colnames(BookOdds) <- c("ATeam", "HTeam","ASpread", "SpreadAwayOdd", "HSpread", "SpreadHomeOdd","OU")
    BookOdds <-BookOdds %>%
      mutate(BookWin = NA) %>%
      mutate(BookSpread = NA)
    Teams <- c("Ducks" = "ANA","Hockey Club" = "UTA","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
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
    BookOdds <- BookOdds %>%
      mutate(OU=as.numeric(OU))
    return(BookOdds)
  }
  
  BookOdds <- DailyOdds(x) %>%
    mutate(gameID = str_c(ATeam, HTeam, toddate,sep = "")) %>%
    select(-ATeam, -HTeam)
  BookOdds <- BookOdds[1:length(toddf$AwayTeam),]
  toddf <- toddf %>%
    mutate(gameID = str_c(AwayTeam, HomeTeam, toddate,sep = ""))
  toddf1 <- left_join(toddf, BookOdds, by = "gameID") %>%
    mutate(AwayBookOdds= ifelse(ASpread==-1.5, SpreadAwayOdd-220,SpreadHomeOdd-220))%>%
    mutate(AwayBookOdds = ifelse(AwayBookOdds< -220,AwayBookOdds+220,AwayBookOdds))%>%
    mutate(AwayBookOdds = ifelse(AwayBookOdds> 220,AwayBookOdds-220,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds<= 0,AwayBookOdds-100,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds> 0,AwayBookOdds+200,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds<0&AwayBookOdds > -110,-1*SpreadAwayOdd+80,AwayBookOdds)) %>%
    mutate(AwayBookOdds= ifelse(SpreadAwayOdd< -250, -110,AwayBookOdds))%>%
    mutate(HomeBookOdds=round(abs(AwayBookOdds)*.9))%>%
    mutate(HomeBookOdds= ifelse(AwayBookOdds== -110, -110,HomeBookOdds))%>%
    mutate(spacer=AwayBookOdds)%>%
    mutate(AwayBookOdds=ifelse(ASpread==1.5,HomeBookOdds,spacer))%>%
    mutate(HomeBookOdds=ifelse(ASpread==1.5,spacer,HomeBookOdds))%>%
    select(-spacer)%>%
    mutate(AOdd1=ifelse(AwayBookOdds<0,.9090909*abs(AwayBookOdds)/(.9090909*abs(AwayBookOdds)+100),100/(abs(AwayBookOdds)+100)))%>%
    na.omit()%>%
    mutate(PD1=AwayOffense-HomeDefense)%>%
    mutate(PD2=AwayDefense-HomeOffense)%>%
    mutate(OU1=AwayOffense-HomeDefense)%>%
    mutate(OU2=HomeOffense-AwayDefense)%>%
    mutate(AH=AwayPlayerScore-AwayTeamScore)%>%
    mutate(HH=HomePlayerScore-HomeTeamScore)%>%
    mutate(PD3=AwayDiscapline-HomeDiscapline)%>%
    mutate(HS=Homelast5games-HomeSchedScore)%>%
    mutate(AS=Awaylast7games-AwaySchedScore)%>%
    mutate(Str=AS-HS)%>%
    mutate(PY=HomePlayedYest-AwayPlayedYest)%>%
    mutate(BookSpr=-3.41+6.747*AOdd1)%>%
    mutate(FloSpr=-.2+.28*PY+.234*PD1+.336*PD2-.92*AH-.96*HH)%>%
    mutate(SprDiff=FloSpr-BookSpr)%>%
    mutate(Diff=ifelse(SprDiff< -.22,1,0))%>%
    mutate(Diff=ifelse(SprDiff> .25,2,Diff))%>%
    mutate(TeamDiff=AwayFloStrength-HomeFloStrength)%>%
    mutate(FloOU=6.16+.29*OU1+.245*OU2)%>%
    mutate(OUDiff=FloOU-OU)%>%
    mutate(DiffOU=ifelse(OUDiff< -.17,1,0))%>%
    mutate(DiffOU=ifelse(OUDiff> .2,2,DiffOU))
  
  modeldf<-read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLMLData2324.csv")%>%
    mutate(AWin = ifelse(WTeam == AwayTeam,1,0)) %>%
    mutate(PD = ifelse(WTeam == AwayTeam,abs(PD),-1*abs(PD))) %>%
    mutate(BookWin = ifelse(BookWin == AwayTeam,1,0)) %>%
    mutate(daynum = NA)%>%
    mutate(date=as.numeric(substr(gameID,7,14)))%>%
    filter(date>20240000)%>%
    mutate(AwayBookOdds= ifelse(ASpread==-1.5, SpreadAwayOdd-220,SpreadHomeOdd-220))%>%
    mutate(AwayBookOdds = ifelse(AwayBookOdds< -220,AwayBookOdds+220,AwayBookOdds))%>%
    mutate(AwayBookOdds = ifelse(AwayBookOdds> 220,AwayBookOdds-220,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds<= 0,AwayBookOdds-100,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds> 0,AwayBookOdds+200,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds<0&AwayBookOdds > -110,-1*SpreadAwayOdd+80,AwayBookOdds)) %>%
    mutate(AwayBookOdds= ifelse(SpreadAwayOdd< -250, -110,AwayBookOdds))%>%
    mutate(HomeBookOdds=round(abs(AwayBookOdds)*.9))%>%
    mutate(HomeBookOdds= ifelse(AwayBookOdds== -110, -110,HomeBookOdds))%>%
    mutate(spacer=AwayBookOdds)%>%
    mutate(AwayBookOdds=ifelse(ASpread==1.5,HomeBookOdds,spacer))%>%
    mutate(HomeBookOdds=ifelse(ASpread==1.5,spacer,HomeBookOdds))%>%
    select(-spacer)%>%
    mutate(AOdd1=ifelse(AwayBookOdds<0,.9090909*abs(AwayBookOdds)/(.9090909*abs(AwayBookOdds)+100),100/(abs(AwayBookOdds)+100)))%>%
    select(-daynum)%>%
    na.omit()%>%
    mutate(PD1=AwayOffense-HomeDefense)%>%
    mutate(PD2=AwayDefense-HomeOffense)%>%
    mutate(OU1=AwayOffense-HomeDefense)%>%
    mutate(OU2=HomeOffense-AwayDefense)%>%
    mutate(AH=AwayPlayerScore-AwayTeamScore)%>%
    mutate(HH=HomePlayerScore-HomeTeamScore)%>%
    mutate(PD3=AwayDiscapline-HomeDiscapline)%>%
    mutate(HS=Homelast5games-HomeSchedScore)%>%
    mutate(AS=Awaylast7games-AwaySchedScore)%>%
    mutate(Str=AS-HS)%>%
    mutate(PY=HomePlayedYest-AwayPlayedYest)%>%
    mutate(BookSpr=-3.41+6.747*AOdd1)%>%
    mutate(FloSpr=-.2+.28*PY+.234*PD1+.336*PD2-.92*AH-.96*HH)%>%
    mutate(SprDiff=FloSpr-BookSpr)%>%
    mutate(Diff=ifelse(SprDiff< -.22,1,0))%>%
    mutate(Diff=ifelse(SprDiff> .25,2,Diff))%>%
    mutate(TeamDiff=AwayFloStrength-HomeFloStrength)%>%
    mutate(FloOU=6.16+.29*OU1+.245*OU2)%>%
    mutate(OUDiff=FloOU-OU)%>%
    mutate(DiffOU=ifelse(OUDiff< -.17,1,0))%>%
    mutate(DiffOU=ifelse(OUDiff> .2,2,DiffOU))
  oumod <- lm(TotGl~factor(DiffOU)*(OU1+OU2), data = modeldf)
  oupred1 <- predict(oumod, newdata = toddf1)
  
  linmod <- lm(PD~factor(PY)*(AH+HH)+PD1 + PD2, data = modeldf)
  linpreds=predict(linmod,newdata = modeldf)
  linpreds1=predict(linmod,newdata = toddf1)
  
  modeldf<-modeldf%>%
    mutate(PredPD=linpreds)
  toddf1<-toddf1%>%
    mutate(PredPD=linpreds1)
  logmod <- glm(AWin~PredPD, data = modeldf,family = binomial)
  logmod$coefficients[1]=0
  logpred <- predict(logmod, newdata = toddf1, type = "response")
  toddf1<-toddf1%>%
    mutate(FloProb=logpred)%>%
    mutate(PredOU=oupred1)
  full=read.csv("/Users/seanfloersch/FloStrength/HockeyApp/GamePreds.csv")
  df<-rbind(toddf1,full)
  write_csv(df,"/Users/seanfloersch/FloStrength/HockeyApp/GamePreds.csv")
  todsched<-toddf1%>%
    mutate(FloWin=ifelse(PredPD>0,AwayTeam,HomeTeam))%>%
    mutate(FloSpr=ifelse(BookWin==FloWin&abs(PredPD)>1.5,-1.5,"+1.5"))%>%
    mutate(FloOU=ifelse(PredOU>OU,str_c("O",OU),str_c("U",OU)))%>%
    mutate(BookSpread=ifelse(BookSpread>0,str_c("+",BookSpread),BookSpread))%>%
    mutate(Book=str_c(BookWin,"; ",BookSpread,"; O/U ",OU))%>%
    mutate(Model=str_c(FloWin,"; ",FloSpr,"; O/U ",FloOU))%>%
    mutate(FloProb=ifelse(FloWin==AwayTeam,FloProb,1-FloProb))%>%
    mutate(WinProb=round(FloProb,3))%>%
    mutate(PredPD=round(abs(PredPD),2))%>%
    mutate(PredOU=round(PredOU,2))%>%
    select("Away"=AwayTeam,"Home"=HomeTeam,"Time [ET]"=Time,Book,Model,"Winner"=FloWin,WinProb,PredPD,PredOU)
  write_csv(todsched,"/Users/seanfloersch/FloStrength/HockeyApp/TodSched.csv")
}
analyzePreds=function(vale){
  full=read.csv("/Users/seanfloersch/FloStrength/HockeyApp/GamePreds.csv")
  getNHLResults <- function(x){
    h <- read_html("https://www.hockey-reference.com/leagues/NHL_2025_games.html") 
    Date <- html_nodes(h, ".left:nth-child(1)") %>% html_text
    Date<-Date[-which(Date=="Date")]
    ATeam <- html_nodes(h, "td.left+ .left") %>% html_text
    HTeam <- html_nodes(h, "td:nth-child(5)") %>% html_text
    AScore <- html_nodes(h, "td:nth-child(4)") %>% html_text%>%as.numeric()
    HScore <- html_nodes(h, "td:nth-child(6)") %>% html_text%>%as.numeric()
    df<-data.frame(Date,ATeam,HTeam,AScore,HScore)%>%
      na.omit()%>%
      mutate(gameID=str_remove_all(Date,"-"))
    Teams <- c("Utah Hockey Club"="UTA","Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG")
    df$ATeam <-as.character(Teams[df$ATeam])
    df$HTeam <-as.character(Teams[df$HTeam])
    df<-df%>%
      mutate(gameID=str_c(ATeam,HTeam,gameID))
    return(df)  
  }  
  NHLGames <- getNHLResults()%>%
    select(AScore,HScore,gameID)
  fulldf=left_join(full,NHLGames,by=c("gameID"))%>%na.omit()%>%
    mutate(TotGl=AScore+HScore)%>%
    mutate(ResOU=ifelse(TotGl>OU,"O","U"))%>%
    mutate(ResOU=ifelse(TotGl==OU,NA,ResOU))%>%
    mutate(FloOU=ifelse(PredOU>OU,"O","U"))
  ou<-fulldf%>%filter(is.na(ResOU)==FALSE)%>%select(TotGl,ResOU,FloOU)
  
}
getNHLMLRes<-function(yay){
  yestdate <- str_remove_all(Sys.Date()-1, "-")
  h <- read_html(str_c("https://www.espn.com/nhl/scoreboard/_/date/", yestdate,sep = ""))
  AwayTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
  HomeTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
  AScore <- html_nodes(h, ".ScoreboardScoreCell__Item--away .ScoreCell_Score--scoreboard") %>% html_text%>%as.numeric()
  HScore <- html_nodes(h, ".ScoreboardScoreCell__Item--home .ScoreCell_Score--scoreboard") %>% html_text%>%as.numeric()
  
  toddf <- data.frame(AwayTeam, HomeTeam,AScore,HScore)
  Teams <- c("Ducks" = "ANA","Coyotes" = "ARI","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
  toddf$AwayTeam <-as.character(Teams[toddf$AwayTeam])
  toddf$HomeTeam <-as.character(Teams[toddf$HomeTeam])
  toddf<-toddf%>%
    mutate(WTeam=ifelse(AScore>HScore,AwayTeam,HomeTeam))%>%
    mutate(TotGl=AScore+HScore)%>%
    mutate(PD=abs(AScore-HScore))%>%
    mutate(gameID=str_c(AwayTeam,HomeTeam,yestdate))%>%
    select(-AwayTeam,-HomeTeam)
  yest<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/NHLBettingTod.csv")
  df<-left_join(yest,toddf,by="gameID")%>%
    mutate(MLCorr=ifelse(WTeam==FloWin,1,0))%>%
    mutate(ActSpr=ifelse(WTeam==BookWin &PD>1.5,-1.5,1.5))%>%
    mutate(ActOU=ifelse(TotGl>OU,"Over","Under"))%>%
    mutate(OUCorr=ifelse(ActOU==FloOU,1,0))%>%
    mutate(SprCorr=ifelse(FloSpr==ActSpr,1,0))%>%
    mutate(MLNet=ifelse(MLCorr==1,WLWinnings,-1))%>%
    mutate(SprNet=ifelse(OUCorr==1,OUWinnings,-1))%>%
    mutate(OUNet=ifelse(SprCorr==1,SprWinnings,-1))
  comp<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/NHLBettingHist.csv")
  df1<-unique(rbind(comp,df))
  write_csv(df1,"/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/NHLBettingHist.csv")
  
  comp<-read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLMLData2324.csv")
  df1<-unique(rbind(comp,df))%>%na.omit
  write_csv(df1,"/Users/seanfloersch/FloStrength/NHLFloStrength/NHLMLData2324.csv")
  
}
predictNHLModel<- function(nada){
  modeldf<-read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLMLData2324.csv")%>%
    mutate(AWin = ifelse(WTeam == AwayTeam,1,0)) %>%
    mutate(PD = ifelse(WTeam == AwayTeam,abs(PD),-1*abs(PD))) %>%
    mutate(BookWin = ifelse(BookWin == AwayTeam,1,0)) %>%
    mutate(daynum = NA)%>%
    mutate(date=as.numeric(substr(gameID,7,14)))%>%
    filter(date>20240000)%>%
    mutate(AwayBookOdds= ifelse(ASpread==-1.5, SpreadAwayOdd-220,SpreadHomeOdd-220))%>%
    mutate(AwayBookOdds = ifelse(AwayBookOdds< -220,AwayBookOdds+220,AwayBookOdds))%>%
    mutate(AwayBookOdds = ifelse(AwayBookOdds> 220,AwayBookOdds-220,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds<= 0,AwayBookOdds-100,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds> 0,AwayBookOdds+200,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds<0&AwayBookOdds > -110,-1*SpreadAwayOdd+80,AwayBookOdds)) %>%
    mutate(AwayBookOdds= ifelse(SpreadAwayOdd< -250, -110,AwayBookOdds))%>%
    mutate(HomeBookOdds=round(abs(AwayBookOdds)*.9))%>%
    mutate(HomeBookOdds= ifelse(AwayBookOdds== -110, -110,HomeBookOdds))%>%
    mutate(spacer=AwayBookOdds)%>%
    mutate(AwayBookOdds=ifelse(ASpread==1.5,HomeBookOdds,spacer))%>%
    mutate(HomeBookOdds=ifelse(ASpread==1.5,spacer,HomeBookOdds))%>%
    select(-spacer)%>%
    mutate(AOdd1=ifelse(AwayBookOdds<0,.9090909*abs(AwayBookOdds)/(.9090909*abs(AwayBookOdds)+100),100/(abs(AwayBookOdds)+100)))%>%
    select(-daynum)%>%
    na.omit()%>%
    mutate(PD1=AwayOffense-HomeDefense)%>%
    mutate(PD2=AwayDefense-HomeOffense)%>%
    mutate(OU1=AwayOffense-HomeDefense)%>%
    mutate(OU2=HomeOffense-AwayDefense)%>%
    mutate(AH=AwayPlayerScore-AwayTeamScore)%>%
    mutate(HH=HomePlayerScore-HomeTeamScore)%>%
    mutate(PD3=AwayDiscapline-HomeDiscapline)%>%
    mutate(HS=Homelast5games-HomeSchedScore)%>%
    mutate(AS=Awaylast7games-AwaySchedScore)%>%
    mutate(Str=AS-HS)%>%
    mutate(PY=HomePlayedYest-AwayPlayedYest)%>%
    mutate(BookSpr=-3.41+6.747*AOdd1)%>%
    mutate(FloSpr=-.2+.28*PY+.234*PD1+.336*PD2-.92*AH-.96*HH)%>%
    mutate(SprDiff=FloSpr-BookSpr)%>%
    mutate(Diff=ifelse(SprDiff< -.22,1,0))%>%
    mutate(Diff=ifelse(SprDiff> .25,2,Diff))%>%
    mutate(TeamDiff=AwayFloStrength-HomeFloStrength)%>%
    mutate(FloOU=6.16+.29*OU1+.245*OU2)%>%
    mutate(OUDiff=FloOU-OU)%>%
    mutate(DiffOU=ifelse(OUDiff< -.17,1,0))%>%
    mutate(DiffOU=ifelse(OUDiff> .2,2,DiffOU))
  oumod <- lm(TotGl~factor(DiffOU)*(OU1+OU2+OU), data = modeldf)
  oupred1 <- predict(oumod, newdata = preddf)
  
  linmod <- lm(PD~factor(PY)*(AH+HH)+PD1 + PD2 +AOdd1+factor(Diff), data = modeldf)
  linpreds=predict(linmod,newdata = modeldf)
  modeldf<-modeldf%>%
    mutate(PredPD=linpreds)
  logmod <- glm(AWin~PredPD, data = modeldf,family = binomial)
  logpred <- predict(logmod, newdata = preddf, type = "response")
  modeldf<-modeldf%>%
    mutate(FloProb=logpred)
  
  preddf<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/NHL/NHLBettingTod.csv") %>%
    na.omit()%>%
    mutate(AwayBookOdds= ifelse(ASpread==-1.5, SpreadAwayOdd-220,SpreadHomeOdd-220))%>%
    mutate(AwayBookOdds = ifelse(AwayBookOdds< -220,AwayBookOdds+220,AwayBookOdds))%>%
    mutate(AwayBookOdds = ifelse(AwayBookOdds> 220,AwayBookOdds-220,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds<= 0,AwayBookOdds-100,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds> 0,AwayBookOdds+200,AwayBookOdds))%>%
    mutate(AwayBookOdds= ifelse(AwayBookOdds<0&AwayBookOdds > -110,-1*SpreadAwayOdd+80,AwayBookOdds)) %>%
    mutate(AwayBookOdds= ifelse(SpreadAwayOdd< -250, -110,AwayBookOdds))%>%
    mutate(HomeBookOdds=round(abs(AwayBookOdds)*.9))%>%
    mutate(HomeBookOdds= ifelse(AwayBookOdds== -110, -110,HomeBookOdds))%>%
    mutate(spacer=AwayBookOdds)%>%
    mutate(AwayBookOdds=ifelse(ASpread==1.5,HomeBookOdds,spacer))%>%
    mutate(HomeBookOdds=ifelse(ASpread==1.5,spacer,HomeBookOdds))%>%
    select(-spacer)%>%
    mutate(AOdd1=ifelse(AwayBookOdds<0,.9090909*abs(AwayBookOdds)/(.9090909*abs(AwayBookOdds)+100),100/(abs(AwayBookOdds)+100)))%>%
    mutate(PD1=AwayOffense-HomeDefense)%>%
    mutate(PD2=AwayDefense-HomeOffense)%>%
    mutate(AH=AwayPlayerScore-AwayTeamScore)%>%
    mutate(HH=HomePlayerScore-HomeTeamScore)%>%
    mutate(PD3=AwayDiscapline-HomeDiscapline)
  linmod <- lm(PD~AOdd1+PD1+PD2+PD3+AH+HH+Awaylast3games+Homelast3games+HomePlayedYest+AwayPlayedYest, data = modeldf)
  
  logmod <- glm(AWin~PD1+PD2+AwayDiscapline+HomeDiscapline+AwayPlayerScore+HomePlayerScore+AwaySchedScore+HomeSchedScore+Awaylast3games+Homelast3games+HomePlayedYest+AwayPlayedYest, data = modeldf,family = binomial)
  logpred <- predict(logmod, newdata = preddf, type = "response")
  logwin <- ifelse(logpred>.5,1,0)
  
  svmmod <- svm(AWin~AOdd1+AwayOffense+HomeOffense+AwayDefense+HomeDefense+AwayDiscapline+HomeDiscapline+AwayPlayerScore+HomePlayerScore+AwaySchedScore+HomeSchedScore+Awaylast3games+Homelast3games+HomePlayedYest+AwayPlayedYest, data = modeldf,probability = TRUE)
  svmpred <- predict(svmmod, newdata = preddf, probability = TRUE)
  svmwin <- as.vector(ifelse(svmpred>.5,1,0))
  
  set.seed(1212)
  rfmod <- randomForest(AWin~AOdd1+AwayOffense+HomeOffense+AwayDefense+HomeDefense+AwayDiscapline+HomeDiscapline+AwayPlayerScore+HomePlayerScore+AwaySchedScore+HomeSchedScore+Awaylast3games+Homelast3games+HomePlayedYest+AwayPlayedYest, data = modeldf)
  rfpred<-as.vector(predict(rfmod, newdata = preddf))
  rfwin=ifelse(rfpred>0.5,1,0)
  
  nnmod  <- neuralnet(AWin~AOdd1+AwayOffense+HomeOffense+AwayDefense+HomeDefense+AwayDiscapline+HomeDiscapline+AwayPlayerScore+HomePlayerScore+AwaySchedScore+HomeSchedScore+Awaylast3games+Homelast3games+HomePlayedYest+AwayPlayedYest, data = modeldf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  nnpred<-as.vector((compute(nnmod,preddf))$net.result)
  nnwin <- as.vector(ifelse(nnpred>0.5, 1, 0))
  
  gbmmod <- gbm(AWin~AOdd1+AwayOffense+HomeOffense+AwayDefense+HomeDefense+AwayDiscapline+HomeDiscapline+AwayPlayerScore+HomePlayerScore+AwaySchedScore+HomeSchedScore+Awaylast3games+Homelast3games+HomePlayedYest+AwayPlayedYest, data = modeldf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 100) 
  gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, preddf, type = "response")
  gbmwin=ifelse(gbmpred>0.5,1,0)
  
  linmod <- lm(PD~AOdd1+PD1+PD2+AwayDiscapline+HomeDiscapline+AwayPlayerScore+HomePlayerScore+AwaySchedScore+HomeSchedScore+Awaylast3games+Homelast3games+HomePlayedYest+AwayPlayedYest, data = modeldf)
  linpred1 <- predict(linmod, newdata = preddf)
  oumod <- lm(TotGl~OU+AwayOffense+HomeOffense+AwayDefense+HomeDefense+AwayDiscapline+HomeDiscapline+AwayPlayerScore+HomePlayerScore+HomePlayedYest+AwayPlayedYest, data = modeldf)
  oupred1 <- predict(oumod, newdata = preddf)
  predact <- preddf %>%
    mutate(predPD = linpred1) %>%
    mutate(predOU = oupred1) %>%
    mutate(log = logwin) %>%
    mutate(logprob = logpred) %>%
    mutate(svm = svmwin) %>%
    mutate(svmprob = svmpred) %>%
    mutate(gbm = gbmwin) %>%
    mutate(gbmprob = gbmpred) %>%
    mutate(nn = nnwin) %>%
    mutate(nnprob = nnpred) %>%
    mutate(rf = rfwin) %>%
    mutate(rfprob = rfpred) %>%
    mutate(ensprob = (svmprob+nnprob+logprob)/3)%>%
    mutate(ens = ifelse(ensprob>.5, 1, 0))%>%
    mutate(book = ifelse(BookWin==AwayTeam,1,0))%>%
    mutate(predML=ifelse(ens==1,AwayTeam,HomeTeam))%>%
    mutate(WLWinnings=ifelse(ens==1,AwayBookOdds,HomeBookOdds))%>%
    mutate(WLWinnings=ifelse(WLWinnings<0,100/abs(WLWinnings),WLWinnings/100))%>%
    mutate(FloOU=ifelse(predOU>OU,"Over","Under"))%>%
    mutate(FloSpr=ifelse(predPD>0,1,0))%>%
    mutate(FloSpr=ifelse(FloSpr==book &abs(predPD)>1.5,-1.5,1.5))%>%
    mutate(SprWinnings=ifelse(FloSpr==ASpread, SpreadAwayOdd,SpreadHomeOdd))%>%
    mutate(SprWinnings=ifelse(SprWinnings<0,100/abs(SprWinnings),SprWinnings/100))%>%
    mutate(SprWinnings=round(SprWinnings,2))%>%
    mutate(Odds=ifelse(SprWinnings>=.91,(.91)/(.91+SprWinnings),(SprWinnings)/(.91+SprWinnings)))%>%
    mutate(Spr=ifelse(SprWinnings>=.91,-1*FloSpr,FloSpr))%>%
    mutate(Odds=1-Odds)%>%
    mutate(Spr1=ifelse(book==1,1.5,-1.5))%>%
    mutate(diff=abs(Spr1-predPD))%>%
    mutate(prob=pnorm(diff,sd = 2))%>%
    mutate(Odds=ifelse(Spr!=FloSpr,1-Odds,Odds))%>%
    mutate(SprDiff=prob-Odds)%>%
    mutate(SprConf=ifelse(SprDiff<0,1,NA))%>%
    mutate(SprConf=ifelse(SprDiff>=0&SprDiff<.05,2,SprConf))%>%
    mutate(SprConf=ifelse(SprDiff>=.05&SprDiff<.12,3,SprConf))%>%
    mutate(SprConf=ifelse(SprDiff>=.12&SprDiff<.22,4,SprConf))%>%
    mutate(SprConf=ifelse(SprDiff>=.22,5,SprConf))%>%
    select(-SprDiff,-Odds,-prob,-Spr,-Spr1,-diff)%>%
    mutate(oudiff=abs(OU-predOU))%>%
    mutate(OUConf=ifelse(oudiff<0.2,1,NA))%>%
    mutate(OUConf=ifelse(oudiff>=.2&oudiff<.4,2,OUConf))%>%
    mutate(OUConf=ifelse(oudiff>=.4&oudiff<.7,3,OUConf))%>%
    mutate(OUConf=ifelse(oudiff>=.7&oudiff<1.1,4,OUConf))%>%
    mutate(OUConf=ifelse(oudiff>=1.1,5,OUConf))%>%
    select(-oudiff)%>%
    mutate(odds=ifelse(WLWinnings>.91,WLWinnings/(WLWinnings+1),0.9090909/(WLWinnings+0.9090909)))%>%
    mutate(odds=ifelse(BookWin==AwayTeam,odds,1-odds))%>%
    mutate(oudiff=abs(odds-ensprob))%>%
    mutate(MLConf=ifelse(oudiff<.05,1,NA))%>%
    mutate(MLConf=ifelse(oudiff>=.05&oudiff<.09,2,MLConf))%>%
    mutate(MLConf=ifelse(oudiff>=.09&oudiff<.135,3,MLConf))%>%
    mutate(MLConf=ifelse(oudiff>=.135&oudiff<.2,4,MLConf))%>%
    mutate(MLConf=ifelse(oudiff>=.2,5,MLConf))%>%
    select(-oudiff)
    
  write_csv(predact,"/Users/seanfloersch/FloStrength/FloStrengthFuture/Betting/NHLBettingTod.csv")
}
updNHLBets <- function(nada){
  modeldf<-read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLMLData2324.csv")%>%
    select(gameID,WTeam,PD,TotGl)
  preddf<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/Betting/NHLBettingTod.csv")%>%
    select(AwayTeam,HomeTeam,gameID,OU,BookWin,predML,predPD,predOU,FloOU,FloSpr,SprWinnings,OUWinnings,WLWinnings,log,svm,rf,gbm,nn,book,ens,logprob,svmprob,rfprob,gbmprob,nnprob)
  df<-left_join(modeldf,preddf,by="gameID")%>%
    na.omit()%>%
    mutate(ActOU=ifelse(TotGl>OU,"Over","Under"))%>%
    mutate(ActSpr=ifelse(WTeam==BookWin &abs(PD)>1.5,-1.5,1.5))%>%
    mutate(MLNet=ifelse(WTeam==predML,WLWinnings,-1))%>%
    mutate(OUNet=ifelse(ActOU==FloOU,OUWinnings,-1))%>%
    mutate(SprNet=ifelse(ActSpr==FloSpr,SprWinnings,-1))
  
  compdf<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/Betting/NHLBettingComp.csv")
  df<-unique(rbind(compdf,df))%>%na.omit()%>%
    mutate(OUNet=ifelse(OU==TotGl,0,OUNet))
  write_csv(df,"/Users/seanfloersch/FloStrength/FloStrengthFuture/Betting/NHLBettingComp.csv")
  
}
#analyze
analyzeBets<-function(nada){
  df<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthFuture/Betting/NHLBettingComp.csv")
  ml<-df%>%mutate(prob=(logprob+nnprob+svmprob)/3)%>%mutate(ActW=ifelse(WTeam==AwayTeam,1,0))%>%select(prob,WLWinnings,predML,BookWin,AwayTeam,HomeTeam,WTeam)%>%mutate(PredWin=ifelse(prob>.5,AwayTeam,HomeTeam))%>%mutate(odds=ifelse(WLWinnings>.91,WLWinnings/(WLWinnings+1),0.9090909/(WLWinnings+0.9090909)))%>%mutate(odds=ifelse(BookWin==AwayTeam,odds,1-odds))%>%mutate(idk=ifelse(PredWin==AwayTeam,odds,1-odds))%>%mutate(WLWinnings=.91*(100-idk*100)/(idk*100))%>%select(-idk)%>%mutate(WLWinnings=ifelse(PredWin==WTeam,WLWinnings,-1))%>%mutate(diff=ifelse(prob>.5,prob-odds,odds-prob))
  quantile(ml$diff, probs = seq(0, 1, 0.2))
  ml<-ml%>%mutate(MLConf=ifelse(diff<0,1,NA))%>%mutate(MLConf=ifelse(diff>=0&diff<.06,2,MLConf))%>%mutate(MLConf=ifelse(diff>=.06&diff<.11,3,MLConf))%>%mutate(MLConf=ifelse(diff>=.11&diff<.17,4,MLConf))%>%mutate(MLConf=ifelse(diff>=.17,5,MLConf))%>%group_by(MLConf)%>%mutate(met=mean(WLWinnings))%>%slice(1)
  #4
  ou<-df%>%select(OU,predOU,OUNet)%>%mutate(oudiff=predOU-OU)
  quantile(ou$oudiff, probs = seq(0, 1, 1/6))
  ou<-ou%>%mutate(OUConf=ifelse(oudiff< -.7,1,NA))%>%mutate(OUConf=ifelse(oudiff>= -.7&oudiff< -.3,2,OUConf))%>%mutate(OUConf=ifelse(oudiff>= -.3&oudiff<0,3,OUConf))%>%mutate(OUConf=ifelse(oudiff>=0&oudiff<.23,4,OUConf))%>%mutate(OUConf=ifelse(oudiff>=.23&oudiff<.7,5,OUConf))%>%mutate(OUConf=ifelse(oudiff>=.7,6,OUConf))%>%group_by(OUConf)%>%mutate(met=mean(OUNet))%>%slice(1)
  #1,6
  sp<-df%>%select(book,SprWinnings,FloSpr,predPD,ActSpr)%>%mutate(Odds=ifelse(SprWinnings>=.91,(.91)/(.91+SprWinnings),(SprWinnings)/(.91+SprWinnings)))%>%mutate(Spr=ifelse(SprWinnings>=.91,-1*FloSpr,FloSpr))%>%mutate(Odds=1-Odds)%>%mutate(Spr1=ifelse(book==1,1.5,-1.5))%>%mutate(diff=abs(Spr1-predPD))%>%mutate(prob=pnorm(diff,sd = 2))%>%select(FloSpr,prob,Spr,Odds,ActSpr,SprWinnings)%>%mutate(Odds=ifelse(Spr!=FloSpr,1-Odds,Odds))%>%mutate(SprDiff=(prob-Odds))
  quantile(sp$SprDiff, probs = seq(0, 1, 0.2))
  sp<-sp%>%mutate(SprConf=ifelse(SprDiff< 0,1,NA))%>%mutate(SprConf=ifelse(SprDiff>= 0&SprDiff< .05,2,SprConf))%>%mutate(SprConf=ifelse(SprDiff>= .05&SprDiff< .1,3,SprConf))%>%mutate(SprConf=ifelse(SprDiff>=.1&SprDiff<.18,4,SprConf))%>%mutate(SprConf=ifelse(SprDiff>=.18,5,SprConf))%>%mutate(Winnings=ifelse(FloSpr==ActSpr,SprWinnings,-1))%>%group_by(SprConf)%>%mutate(met=mean(Winnings))%>%slice(1)%>%select(SprConf,met)
  #1,2
}




