library(caret);library(doSNOW);library(data.table);
library(reshape2);library(plyr);library(zoo);library(dplyr)
library(ggplot2);library(glmnet);library(MASS) 
options(stringsAsFactors=FALSE)

Metrics<-function(y, x) {
  
  Rsq<-round(1-sum((y-x)^2)/sum((y-mean(y))^2), 4) #Rsq
  RMSE<-round(sqrt(mean((y-x)^2)), 4) #RMSE
  MAE<-round(mean(abs(y-x)), 4) #RMSE
  # MAPE<-round(mean(abs((y[y!=0]-x[y!=0])/y[y!=0])*100), 4)
  bias<-round(mean(y-x), 4) #RMSE
  
  paste(c(paste("RSq:", Rsq), paste("RMSE:", RMSE),paste("bias:", bias), paste("MAE:", MAE) )) #,paste("MAPE:", MAPE)
}

###LOAD EACH PBP FILE SEPERATELY then combine####

loadStint<-function(year){
  load(paste0(c("PBP", substring(year, 3, 4), ".RData"), collapse=""))
  stints<-data.frame(rbindlist(stintsList,fill=T))
  stints$Year<-year
  
  box<-data.frame(rbindlist(boxList,fill=T))
  box$Year<-year
  
  list(stints=stints, box=box, games=games)
}
loadList<-lapply(years, loadStint)
stints<-rbindlist(lapply(loadList, function(x) x$stints), fill=T)
box<-rbindlist(lapply(loadList, function(x) x$box), fill=T)
games<-rbindlist(lapply(loadList, function(x) x$games), fill=T)
rm(loadList)

table(stints$Year)

###CLEAN DATA/define variables#####

games$Home<-ifelse(grepl("@",games$MATCHUP), 0, 1)

# head(stints)
stints$MARGIN<-100*((stints$HomeEnd-stints$HomeStart)-(stints$AwayEnd-stints$AwayStart))/stints$POSSESSIONS
stints$MARGIN[stints$POSSESSIONS==0]<-0
margins<-stints[ ,list(
  MARGIN=sum(MARGIN*POSSESSIONS, na.rm=T)/100
), by=c("GAME_ID")]
margins<-data.frame(margins)

games$MARGIN<-margins$MARGIN[match(games$GAME_ID, margins$GAME_ID)]
games$MARGIN[games$Home==0]<-games$MARGIN[games$Home==0]*-1
games$GAME_DATE<-as.Date(games$GAME_DATE)


stints<-data.frame(stints)
stints$MARGIN<-100*((stints$HomeEnd-stints$HomeStart)-(stints$AwayEnd-stints$AwayStart))/stints$POSSESSIONS
stints$MARGIN[stints$POSSESSIONS==0]<-0
stints[is.na(stints)]<-0

games<-data.frame(games)
stints<-data.frame(stints)
box<-data.frame(box)
