library(tidyverse)
library(stringr)
library(rsconnect)
library(shinythemes)
library(bslib)
library(shinydashboard)
library(htmlwidgets)
library(shinybrowser)
library(rvest)
library(plotly)
options(DT.options = list(pageLength = 50, language = list(search = 'Filter:')))
nhlplayersfut<-read.csv("SkatePred23.csv")

NHLTeams <-read.csv("NHLMaster.csv")
NHLPlayers<-read.csv("NHLPlayers.csv")
nhlplayers<-read.csv("NHLPlayersAT.csv")
nhlteams<-read.csv("NHLTeamsAT.csv")
nhlwsodds<-read.csv("StanleyCupOdds.csv")
nhlplayersfut<-read.csv("SkatePred23.csv")
###########################UI#######################################

ui<-fluidPage(theme=shinytheme("united"),
              navbarPage("FloStrength Hockey",id = "nav",
                         #####Home#####
                         tabPanel("Home",
                                  fluidPage(
                                    fluidRow(
                                      column(5),
                                      column(2,selectizeInput("HomeSelect",label=h3("Ranking"),choices=c("NHL T10","NHL MVP"))),
                                      column(5)
                                    ),
                                    fluidRow(
                                      column(1),
                                      column(10,DT::dataTableOutput("HomeData")),
                                      column(1)
                                    )
                                  )
                         ),
                         tabPanel("Schedule",
                                  fluidPage(
                                    fluidRow(
                                      DT::dataTableOutput("NHLSched")
                                    )
                                  )),
                                    tabPanel("Teams",
                                             fluidPage(
                                               fluidRow(
                                                 column(3,
                                                        radioButtons("NHLTeamStat", label = h3("Stat"),
                                                                     choices = list("Flo Stats"=1,"Basic" = 2,"Standings"=3), 
                                                                     selected = 1)),
                                                 column(3,
                                                        selectizeInput("NHLTeamTeamFilt",label=h3("Team(s)"),choices=c(unique(NHLTeams$Team)),multiple=TRUE)),
                                                 column(3,
                                                        selectizeInput("NHLTeamDivisionFilt",label=h3("Division"),choices=c("",unique(NHLTeams$Div)))),
                                                 column(3,
                                                        radioButtons("NHLTeamLeagueFilt", label = h3("Conference"),
                                                                     choices = list("Both"=1,"West" = 2,"East" = 3), 
                                                                     selected = 1))),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("NHLTeam"))
                                               )
                                             )),
                                    tabPanel("Players",
                                             fluidPage(
                                               fluidRow(
                                                 column(2),
                                                 column(2,selectizeInput("NHLPlayerPosFilt",label=h3("Position"),choices=c("",unique(NHLPlayers$Pos)))),
                                                 column(3,selectizeInput("NHLPlayerTeamFilt",label=h3("Team(s)"),choices=c(unique(NHLTeams$Team)),multiple=TRUE)),
                                                 column(3,selectizeInput("NHLPlayerNameFilt",label=h3("Players(s)"),choices=c(unique(NHLPlayers$Player)),multiple=TRUE)),
                                                 column(2)
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("NHLPlayers")
                                                 )
                                               )
                                             )),
                                    tabPanel("Team Search",
                                             fluidPage(
                                               fluidRow(
                                                 column(3),
                                                 column(3,selectizeInput("NHLHistYear",label=h3("Year"),choices=c("",unique(nhlteams$yearID)))),
                                                 column(3,selectizeInput("NHLHistTeam",label=h3("Team"),choices=c("",unique(nhlteams$franchID)))),
                                                 column(3)
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("NHLTeamHist")),
                                               )
                                             )),
                                    tabPanel("Player Search",
                                             fluidPage(
                                               fluidRow(
                                                 column(4),
                                                 column(4,selectizeInput("NHLHistPlayName",label=h3("Player:"),choices=c("",unique(nhlplayers$Player)))),
                                                 column(4)
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("NHLPlayerSearch1")),
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("NHLPlayerSearch2")),
                                               )
                                             )),
                                    tabPanel("Player Lists",
                                             fluidPage(
                                               fluidRow(
                                                 column(1),
                                                 column(2,radioButtons("NHLListType", label = h3("List"),
                                                                       choices = list("Seasons" = 1,"Careers"=2), 
                                                                       selected = 1)),
                                                 column(2,radioButtons("NHLListPos", label = h3("Position"),
                                                                       choices = list("Ice" = 1,"GK"=2), 
                                                                       selected = 1)),
                                                 column(3,selectizeInput("NHLListYear",label=h3("Year"),choices=c("",unique(nhlplayers$yearID)))),
                                                 column(3,selectizeInput("NHLListTeam",label=h3("Team"),choices=c("",unique(nhlplayers$Team)))),
                                                 column(1)
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("NHLList")),
                                               )
                                             )),
                                    tabPanel("Future",
                                             fluidPage(
                                               tabsetPanel(
                                                 tabPanel("Stanley Cupp Odds",
                                                          fluidPage(
                                                            fluidRow(h3("2023-24 NHL Stanley Cup Odds",align="center")),
                                                            fluidRow(column(3,selectizeInput("NHLSCDivFilt", label = h3("Division:"),choices = c("All",unique(nhlplayersfut$Div)))),
                                                                     column(4,selectizeInput("NHLSCTeamFilt", label = h3("Team(s):"), choices=c(unique(nhlplayersfut$Team)),multiple=TRUE)),
                                                                     column(3,radioButtons("NHLSCPOFilt", label = h3("Playoff Projected:"),
                                                                                           choices = list("Yes"=1,"No" = 2), 
                                                                                           selected = 2)),
                                                                     column(2,radioButtons("NHLSCConfFilt", label = h3("Conference:"),
                                                                                           choices = list("Both"=1,"West" = 2,"East"=3), 
                                                                                           selected = 1))),
                                                            fluidRow(plotlyOutput("NHLSCPlotly"))
                                                          )),
                                                 tabPanel("Team Win OU",
                                                          fluidPage(
                                                            fluidRow(h3("2023-24 Team Over Under Wins",align="center")),
                                                            fluidRow(column(3,selectizeInput("NHLOUDivFilt", label = h3("Division:"),choices = c("All",unique(nhlplayersfut$Div)))),
                                                                     column(4,selectizeInput("NHLOUTeamFilt", label = h3("Team(s):"), choices=c(unique(nhlplayersfut$Team)),multiple=TRUE)),
                                                                     column(3,radioButtons("NHLOUPOFilt", label = h3("Playoff Projection:"),
                                                                                           choices = list("Yes"=1,"No" = 2), 
                                                                                           selected = 2)),
                                                                     column(2,radioButtons("NHLOUConfFilt", label = h3("Conference:"),
                                                                                           choices = list("Both"=1,"West" = 2,"East"=3), 
                                                                                           selected = 1))),
                                                            fluidRow(DT::dataTableOutput("NHLOU"))
                                                          )),
                                                 tabPanel("Player Value",
                                                          fluidPage(
                                                            fluidRow(h3("2023-24 Player Predictions",align="center")),
                                                            fluidRow(column(2,radioButtons("NHLPlayConfFilt", label = h3("Conference:"),
                                                                                           choices = list("Both"=1,"West" = 2,"East"=3), 
                                                                                           selected = 1)),
                                                                     column(4,selectizeInput("NHLPlayTeamFilt", label = h3("Team(s):"), choices=c(unique(nhlplayersfut$Team)),multiple=TRUE)),
                                                                     column(4,selectizeInput("NHLNameFilt", label = h3("Player(s):"), choices=c(unique(nhlplayersfut$Player)),multiple=TRUE)),
                                                                     column(2,selectizeInput("NHLPlayDivFilt", label = h3("Division:"),choices = c("All",unique(nhlplayersfut$Div))))),
                                                            fluidRow(DT::dataTableOutput("NHLPlayerRanks"))
                                                          ))
                                               )
                                             )
                                    )
                         #############
              )
              
)
###########################SERVER#######################################
server <- function(input, output) {
  ######Home######
  output$NHLSched<- DT::renderDataTable({
    df<- read.csv("TodSched.csv")%>%
      rename("Time [ET]"=Time..ET.)
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$HomeData <- DT::renderDataTable({
    df<-data.frame(data=c(NA))
    if(input$HomeSelect=="NHL T10"){
      df<-read.csv("NHLMaster.csv")%>%
        select(Team,FloStrength,"Wpct"=WinPct)%>%
        arrange(-FloStrength)%>%
        slice(1:10)
    }
    if(input$HomeSelect=="NHL MVP"){

      df<-read.csv("NHLPlayers.csv")%>%
        select(Player,Tm,Pos,FloStrength,Value)%>%
        arrange(-Value)%>%
        slice(1:20)%>%
        mutate(FloStrength=round(FloStrength,3))%>%
        mutate(Value=round(Value,3))
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLTeam <- DT::renderDataTable({
    df <-read.csv("NHLMaster.csv")%>%
      arrange(-FloStrength)
    if(length(input$NHLTeamTeamFilt)!= 0){
      df<-df%>%
        filter(Team %in% input$NHLTeamTeamFilt)
    }
    if(input$NHLTeamDivisionFilt != ""){
      df<-df%>%
        filter(Div == input$NHLTeamDivisionFilt)
    }
    if(input$NHLTeamLeagueFilt== 2){
      df<-df%>%
        filter(Conf == "West")
    }
    if(input$NHLTeamLeagueFilt== 3){
      df<-df%>%
        filter(Conf == "East")
    }
    if(input$NHLTeamStat==1){
      df<-df%>%
        select(Team,GP,WinPct,FloStrength,Offense,Defense,Discapline, TeamScore,PlayerScore,SchedScore,"last3"=last3games,"last5"=last5games,"last7"=last7games,"last10"=last10games)
    }
    if(input$NHLTeamStat==2){
      df<-df%>%
        select(Team,GP,WinPct,FloStrength,GF,GA,Shots,oppSh,PM,oppPM)
    }
    if(input$NHLTeamStat==3){
      df<-df%>%
        mutate(Pts=2*W+OTL)%>%
        select(Team,Conf,Div,GP,FloStrength,WinPct,W,L,OTL,Pts,GF,GA)%>%
        arrange(-WinPct)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLPlayers <- DT::renderDataTable({
    df<- read.csv("NHLPlayersAT.csv")%>%
      filter(yearID==2025)%>%
      rename("PlusMinus"="X...","Shots"="S")
    if(length(input$NHLPlayerTeamFilt)!= 0){
      df<-df%>%
        filter(Team %in% input$NHLPlayerTeamFilt)
    }
    if(length(input$NHLPlayerNameFilt)!= 0){
      df<-df%>%
        filter(Player %in% input$NHLPlayerNameFilt)
    }
    if(input$NHLPlayerPosFilt!= ""){
      if(input$NHLPlayerPosFilt== "W"){
        df<-df%>%
          filter(!Pos %in%c("C","D","G"))
      }else{
        df<-df%>%
          filter(Pos == input$NHLPlayerPosFilt)
      }
    }
    df<-df%>%
      arrange(-Value)
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLTeamHist<-DT::renderDataTable({
    df<-read.csv("NHLTeamsAT.csv")%>%
      mutate(rank=FloStrength+.2*SCChamp)%>%
      arrange(-rank)%>%
      mutate(GS=round(GS/GP,2))%>%
      mutate(GA=round(GA/GP,2))%>%
      select(Team,"Year"=yearID,SCChamp,GP,W,L,"OTL"=OTL.T,PTS,WinPct, FloStrength, Offense, GS, Defense, GA, TeamScore, PlayerScore,"SchedScore"=SchedFS)
    if(input$NHLHistTeam!=""){
      df<-df%>%
        filter(Team==input$NHLHistTeam)
    }
    if(input$NHLHistYear!=""){
      df<-df%>%
        filter(Year==input$NHLHistYear)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLPlayerSearch1<- DT::renderDataTable({
    if(input$NHLHistPlayName==""){
      df<-data.frame()
      }else{
        df<-read.csv("NHLPlayersAT.csv")%>%
          filter(yearID==2025)%>%
          rename("PlusMinus"="X...","Shots"="S")%>%
          filter(Player==input$NHLHistPlayName)
        if(length(df$Player!=0)){
          if(df$Pos[1]=="G"){
            df<-df%>%
              mutate(GAA= round(((SV/SavePer)-SV)/GP,2))%>%
              filter(Pos=="G")%>%
              select(Player,Age,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,SV,GAA,SavePer)
          }else{
            df<-df%>%
              mutate(Pts=G+A)%>%
              filter(Pos !="G")%>%
              select(Player,Age,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,G,A,Pts,Shots)
            }
        }else{
          df<-data.frame()
        }
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLPlayerSearch2<- DT::renderDataTable({
    if(input$NHLHistPlayName==""){
      df<-data.frame()
    }else{
      df<-read.csv("NHLPlayersAT.csv")%>%
        filter(Player==input$NHLHistPlayName)%>%
        arrange(yearID)
      if(df$Pos[1]!="G"){
        df<-df %>%
          mutate(Pts=G+A)%>%
          select(Player,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,G,A,Pts,Shots="S")
        df1<-read_csv("NHLPlayerCarAT.csv")%>%
          filter(Player==input$NHLHistPlayName)%>%
          mutate(Pts=G+A)%>%
          select(Player,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,G,A,Pts,Shots="S")%>%
          mutate(Year="")%>%
          mutate(Team="Career")%>%
          mutate(Pos="")
        df<-rbind(df,df1)%>%
          mutate(FloStrength=round(FloStrength,3))%>%
          mutate(Value=round(Value,3))
      }else{
        df<-df%>%
          mutate(GAA= round(((SV/SavePer)-SV)/GP,2))%>%
          filter(Pos=="G")%>%
          select(Player,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,SV,GAA,SavePer)
        df1<-read_csv("NHLPlayerCarAT.csv")%>%
          filter(Player==input$NHLHistPlayName)%>%
          mutate(GAA= round(((SV/SavePer)-SV)/GP,2))%>%
          filter(Pos=="G")%>%
          select(Player,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,SV,GAA,SavePer)%>%
          mutate(Year="")%>%
          mutate(Team="Career")%>%
          mutate(Pos="")
        df<-rbind(df,df1)%>%
          mutate(FloStrength=round(FloStrength,3))%>%
          mutate(Value=round(Value,3))
      }
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLList<- DT::renderDataTable({
    df<-data.frame()
    if(input$NHLListPos==1 & input$NHLListType==1){
      df<-read.csv("NHLPlayersAT.csv")%>%
        mutate(Pts=G+A)%>%
        filter(Pos !="G")%>%
        select(Player,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,G,A,Pts,Shots="S")
    }
    if(input$NHLListPos==2 & input$NHLListType==1){
      df<-read.csv("NHLPlayersAT.csv")%>%
        mutate(GAA= round(((SV/SavePer)-SV)/GP,2))%>%
        filter(Pos=="G")%>%
        select(Player,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,SV,GAA,SavePer)
    }
    if(input$NHLListPos==1 & input$NHLListType==2){
      df <- read_csv("NHLPlayerCarAT.csv")%>%
        filter(Pos!="G")%>%
        mutate(Pts=G+A)%>%
        select(Player,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,G,A,Pts,Shots="S")%>%
        mutate(Year="")%>%
        mutate(Team="Career")
    }
    if(input$NHLListPos==2 & input$NHLListType==2){
      df <- read_csv("NHLPlayerCarAT.csv")%>%
        filter(Pos=="G")%>%
        mutate(GAA= round(((SV/SavePer)-SV)/GP,2))%>%
        select(Player,"Year"=yearID,"Team",Pos,GP,FloStrength, Value,SV,GAA,SavePer)%>%
        mutate(Year="")%>%
        mutate(Team="Career")
    }
    if(input$NHLListTeam!=""&input$NHLListType==1){
      df<-df%>%
        filter(Team==input$NHLListTeam)
    }
    if(input$NHLListYear!=""&input$NHLListType==1){
      df<-df%>%
        filter(Year==input$NHLListYear)
    }
    df<-df%>%
      arrange(-Value)%>%
      mutate(FloStrength=round(FloStrength,3))%>%
      mutate(Value=round(Value,3))
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLOU<- DT::renderDataTable({
    df<- read.csv("TeamPred24.csv")%>%
      arrange(-PredPts)
    if(length(input$NHLOUTeamFilt)!=0){
      df<-df%>%
        filter(Team%in%input$NHLOUTeamFilt)
    }
    if(input$NHLOUConfFilt==2){
      df<-df%>%
        filter(Conf=="West")
    }
    if(input$NHLOUConfFilt==3){
      df<-df%>%
        filter(Conf=="East")
    }
    if(input$NHLOUPOFilt==1){
      df<-df%>%
        group_by(Conf)%>%
        slice(1:8)%>%
        ungroup()
    }
    if(input$NHLOUDivFilt!="All"){
      df<-df%>%
        filter(Div==input$NHLOUDivFilt)
    }
    df<-df%>%
      select(Team, PredFS,PredPts,OU,PredOU)
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLPlayerRanks<- DT::renderDataTable({
    df<- read.csv("SkatePred24.csv")%>%
      arrange(-ValPred)
    if(length(input$NHLPlayTeamFilt)!=0){
      df<-df%>%
        filter(Team%in%input$NHLPlayTeamFilt)
    }
    if(length(input$NHLNameFilt)!=0){
      df<-df%>%
        filter(Player %in% input$NHLNameFilt)
    }
    if(input$NHLPlayConfFilt==2){
      df<-df%>%
        filter(Conf=="West")
    }
    if(input$NHLPlayConfFilt==3){
      df<-df%>%
        filter(Conf=="East")
    }
    if(input$NHLPlayDivFilt!="All"){
      df<-df%>%
        filter(Div==input$NHLPlayDivFilt)
    }
    df<-df%>%
      select(-Conf,-Div)
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$NHLSCPlotly <- renderPlotly({
    df<-read.csv("StanleyCupOdds24.csv")
    if(length(input$NHLSCTeamFilt)!=0){
      df<-df%>%
        filter(Team%in%input$NHLSCTeamFilt)
    }
    if(input$NHLSCConfFilt==2){
      df<-df%>%
        filter(Conf=="West")
    }
    if(input$NHLSCConfFilt==3){
      df<-df%>%
        filter(Conf=="East")
    }
    if(input$NHLSCPOFilt==1){
      dffilt<-df%>%
        mutate(date=as.character(Date))%>%
        mutate(date=as.numeric(str_remove_all(date,"-")))%>%
        filter(date==max(date))%>%
        group_by(Conf)%>%
        slice(1:8)%>%
        ungroup()
      filtteams<-dffilt$Team
      df<-df%>%
        filter(Team%in%filtteams)
    }
    if(input$NHLSCDivFilt!="All"){
      df<-df%>%
        filter(Div==input$NHLSCDivFilt)
    }
    plot_ly(df, x = ~Date, y = ~Probability, type = 'scatter',color = ~Team,   mode = "lines+markers")%>%
      layout(yaxis= list(range = c(0,.15)),showlegend = F )
  })
}


shinyApp(ui, server)