library(RJSONIO);library(matrixStats);library(data.table);library(dplyr);library(zoo)
options(stringsAsFactors = F)

#setwd("~/RAPM/nba-pbp-data")

####ENTER SEASON AND NAME TO SAVE FILE AS####
season<-"2016-17" #season must be of form 20XX-(XX+1)
fileName<-"PBP17" #save as "PBP(XX+1)" if planning to run RAPM analysis

##SCRAPE TEAM-GAME LOGS TO GET ALL GAMEIDS#####
getGames<-function( Season, Type){
  url<-paste0(c("http://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=T&Season=", 
                Season,"&SeasonType=",Type ,"&Sorter=DATE"), collapse="")
  Data<-fromJSON(url)
  
  Data<-unname(unlist(Data$resultSet))
  Data<-Data[-1]
  storeNames<-Data[1:29]
  Data<-Data[-seq(1, 29)]
  
  #cancelled game
  if(Season=="2012-13"& Type=="Regular+Season"){
    Data<-Data[-c((which(Data=="1610612738")[2]-1):(which(Data=="1610612738")[2]+48))]
  }
  Data<-data.frame(t(matrix(Data, nrow=29)))
  colnames(Data)<-storeNames
  if(nrow(Data)>1){
    Data$Type<-gsub("[+]", " ", Type)
  }
  Data
}

getSeason<-function( Season) {
  rbindlist(lapply(c("Playoffs", "Regular+Season"), function(x) getGames( Season, x)), fill=T)
}
games<- data.frame(getSeason( season))
head(games)

###LOOP THROUGH ALL GAMEIDS AND SCRAPE/CLEAN PBP#####

nbaList<-list();length(nbaList)<-length(unique(games$GAME_ID))
boxList<-list();length(boxList)<-length(unique(games$GAME_ID))
stintsList<-list();length(stintsList)<-length(unique(games$GAME_ID))

for(k in 1:length(nbaList)){
  
  # k<-1
  id<-unique(games$GAME_ID)[k]
  source("PBP scrape function.R")   ##ignore readLines errors!

  #if plot does not =0 all the way through, then print
  if(sum(abs(rowSums(nba[, unique(box$PLAYER_ID)])))!=0){
    print(k)
  }
  
  nbaList[[k]]<-nba
  boxList[[k]]<-box
  stintsList[[k]]<-stints
}


####CALCULATE STINTS AND SAVE#########

hasPlays<-which(sapply(boxList, function(x) !is.null(x)) )
setdiff(1:length(boxList), hasPlays)
stintsList<-lapply(hasPlays, function(x) getStint(box=boxList[[x]],nba=nbaList[[x]] ))


save(list=ls()[ls()%in%c("games", "nbaList", "boxList", "stintsList")], file=fileName)

