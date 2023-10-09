library(rvest)
library(tidyverse)
library(optmatch)
library(stringr)
########################################################################################
skate<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NHLPlayersAT")%>%
  filter(yearID>2000)%>%
  select(Player,yearID,Age,Tm,FloStrength,GP)
lyskate<-skate%>%
  mutate(yearID=yearID+1)%>%
  select(Player,yearID,"LYFS"=FloStrength,"LYGP"=GP)
tyskate<-skate%>%
  mutate(yearID=yearID+2)%>%
  select(Player,yearID,"TYFS"=FloStrength,"TYGP"=GP)
skate<-left_join(skate,lyskate,by=c("Player","yearID"))
skate<-left_join(skate,tyskate,by=c("Player","yearID"))

comp<-skate%>%
  filter(is.na(TYFS)==F&is.na(LYFS)==F)
lincomGP<-lm(GP~LYGP+TYGP+LYFS+Age,data = comp)
lincomFS<-lm(FloStrength~LYFS+TYFS+Age,data = comp)
predGP<-predict(lincomGP,newdata = comp)
predFS<-predict(lincomFS,newdata = comp)
comp<-comp%>%
  mutate(GPPred=predGP)%>%
  mutate(FSPred=predFS)
ml<-skate%>%
  filter(is.na(TYFS)==F&is.na(LYFS)==T)
linmlGP<-lm(GP~TYGP+TYFS+Age,data = ml)
linmlFS<-lm(FloStrength~TYFS+Age,data = ml)
predGP<-predict(linmlGP,newdata = ml)
predFS<-predict(linmlFS,newdata = ml)
ml<-ml%>%
  mutate(GPPred=predGP)%>%
  mutate(FSPred=predFS)

sy<-skate%>%
  filter(is.na(TYFS)==T&is.na(LYFS)==F)
linsyGP<-lm(GP~LYGP+LYFS+Age,data = sy)
linsyFS<-lm(FloStrength~LYGP+LYFS+Age,data = sy)
predGP<-predict(linsyGP,newdata = sy)
predFS<-predict(linsyFS,newdata = sy)
sy<-sy%>%
  mutate(GPPred=predGP)%>%
  mutate(FSPred=predFS)

fy<-skate%>%
  filter(is.na(TYFS)==T&is.na(LYFS)==T)
linfyGP<-mean(fy$GP)
linfyFS<-mean(fy$FloStrength)
fy<-fy%>%
  mutate(GPPred=linfyGP)%>%
  mutate(FSPred=linfyFS)
skatepred<-rbind(fy,sy,comp,ml)

skateteam<-skatepred%>%
  group_by(yearID,Tm,FSPred)%>%
  slice(1:2)%>%
  ungroup()%>%
  group_by(yearID,Tm)%>%
  mutate(WFS=FSPred*(GPPred/sum(GPPred)))%>%
  arrange(-WFS)%>%
  slice(1:20)%>%
  mutate(WFS=sum(WFS))%>%
  slice(1)%>%
  ungroup()%>%
  group_by(yearID)%>%
  mutate(PlayPred=(WFS-mean(WFS))/sd(WFS))%>%
  ungroup%>%
  select("Team"="Tm",yearID,PlayPred)
  
########################################################################################
teams<- read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NHLTeamsAT")%>%
  filter(yearID>2000)%>%
  select(Team, yearID, FloStrength,WinPct)
lagyear<-teams%>%
  mutate(yearID=yearID+1)%>%
  select(Team,yearID,"LYFS"=FloStrength)
predteam<-left_join(teams,lagyear,by=c("Team","yearID"))%>%na.omit()
predteam<-left_join(predteam,skateteam,by=c("Team","yearID"))%>%na.omit()
linmod<-lm(WinPct~LYFS+PlayPred,data = predteam)
########################################################################################
TeamPred<-teams%>%
  filter(yearID==2023)%>%
  select(Team,yearID,"LYFS"=FloStrength)
getRoster <- function(team){
  url <- str_c("https://www.hockey-reference.com/teams/",team,"/index.html")
  h <- read_html(url) 
  Player <- html_nodes(h, ".iz+ .left") %>% html_text
  Age<-html_nodes(h,".center+ td.center")%>%html_text%>%as.numeric
  df <- data.frame(Player,Age) %>%
    mutate(Team = team) %>%
    mutate(yearID = 2023)
  return(df)
}
teams<-unique(TeamPred$Team)
roster1 <- map_df(.x= teams[1:15], .f=getRoster)
roster2 <- map_df(.x= teams[16:32], .f=getRoster)
roster<-rbind(roster1,roster2)%>%
  mutate(Player = str_remove(Player, "\\s\\(\\C\\)"))


lyskate<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NHLPlayersAT")%>%
  filter(yearID==2023)%>%
  select(Player,"LYFS"=FloStrength,"LYGP"=GP)
tyskate<-read.csv("/Users/seanfloersch/FloStrength/FloStrengthHistoryApp/NHLPlayersAT")%>%
  filter(yearID==2022)%>%
  select(Player,"TYFS"=FloStrength,"TYGP"=GP)
skatepred<-left_join(roster,lyskate,by="Player")
skatepred<-left_join(skatepred,tyskate,by="Player")

comp<-skatepred%>%
  filter(is.na(TYFS)==F&is.na(LYFS)==F)
predGP<-predict(lincomGP,newdata = comp)
predFS<-predict(lincomFS,newdata = comp)
comp<-comp%>%
  mutate(GPPred=predGP)%>%
  mutate(FSPred=predFS)

ml<-skatepred%>%
  filter(is.na(TYFS)==F&is.na(LYFS)==T)
predGP<-predict(linmlGP,newdata = ml)
predFS<-predict(linmlFS,newdata = ml)
ml<-ml%>%
  mutate(GPPred=predGP)%>%
  mutate(FSPred=predFS)

sy<-skatepred%>%
  filter(is.na(TYFS)==T&is.na(LYFS)==F)
predGP<-predict(linsyGP,newdata = sy)
predFS<-predict(linsyFS,newdata = sy)
sy<-sy%>%
  mutate(GPPred=predGP)%>%
  mutate(FSPred=predFS)

fy<-skatepred%>%
  filter(is.na(TYFS)==T&is.na(LYFS)==T)
fy<-fy%>%
  mutate(GPPred=linfyGP)%>%
  mutate(FSPred=linfyFS)
skatepred<-rbind(fy,sy,comp,ml)
skateteam<-skatepred%>%
  group_by(yearID,Team,FSPred)%>%
  slice(1:2)%>%
  ungroup()%>%
  group_by(yearID,Team)%>%
  mutate(WFS=FSPred*(GPPred/sum(GPPred)))%>%
  arrange(-WFS)%>%
  slice(1:20)%>%
  mutate(WFS=sum(WFS))%>%
  slice(1)%>%
  ungroup()%>%
  group_by(yearID)%>%
  mutate(PlayPred=(WFS-mean(WFS))/sd(WFS))%>%
  ungroup%>%
  select("Team",yearID,PlayPred)
TeamPred<-left_join(TeamPred,skateteam,by=c("Team","yearID"))
preds<-predict(linmod,newdata = TeamPred)
TeamPred<-TeamPred%>%
  mutate(PredFS=preds)%>%
  mutate(PredPts=round(164*PredFS,2))%>%
  mutate(PredFS=round(PredFS,3))
TeamGen<-TeamPred%>%
  arrange(Team)
Team<-TeamGen$Team
Conf<-c(2,2,1,1,1,1,2,2,2,2,1,2,1,2,2,1,1,2,1,1,1,1,1,2,2,2,1,1,2,2,2,1)
Div<-c(4,3,1,1,2,2,4,3,3,3,1,4,1,4,3,1,2,3,2,2,1,2,2,4,4,3,1,1,4,4,3,2)
TeamGen<-data.frame(Team,Conf,Div)%>%
  mutate(Conf=ifelse(Conf==2,"West","East"))%>%
  mutate(Div=ifelse(Div==1,"Atlantic",Div))%>%
  mutate(Div=ifelse(Div==2,"Metro",Div))%>%
  mutate(Div=ifelse(Div==3,"Central",Div))%>%
  mutate(Div=ifelse(Div==4,"Metropolitan",Div))
TeamPred<-left_join(TeamPred,TeamGen,by = "Team")
skatepred<-left_join(skatepred,TeamGen,by = "Team")%>%
  filter(is.na(LYFS)==FALSE |is.na(TYFS)==FALSE)%>%
  mutate(FSPred=round(FSPred,3))%>%
  mutate(GPPred=round(GPPred,0))%>%
  mutate(ValPred=round((GPPred/27)*FSPred,2))%>%
  select(Player,Age,Team,GPPred,FSPred,ValPred,Conf,Div)
write_csv(skatepred,"~/FloStrength/FloStrengthFuture/NHL/SkatePred23.csv")
write_csv(TeamPred,"~/FloStrength/FloStrengthFuture/NHL/TeamPred23.csv")

write_csv(skatepred,"~/FloStrength/NFLFloStrength/SkatePred23.csv")
write_csv(TeamPred,"~/FloStrength/NFLFloStrength/TeamPred23.csv")
write_csv(TeamGen,"~/FloStrength/NHLFloStrength/TeamGeneral.csv")
