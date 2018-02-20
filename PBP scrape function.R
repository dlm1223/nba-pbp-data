

address<-paste0(c("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=", id,
                  "&RangeType=2&Season=2015-16&SeasonType=Regular+Season&StartPeriod=1&StartRange=0"), collapse="")
web_page <- readLines(address)

##regex to strip javascript bits and convert raw to csv format
x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
x4 <- gsub(";", ",",x3, perl=TRUE)

nba<-read.table(textConnection(x4), header=T, sep=",", fill=T, skip=2, stringsAsFactors=FALSE)
nba<-nba[!nba$GAME_ID%in% c("headers:","VIDEO_AVAILABLE_FLAG", "0" ), !colnames(nba)%in%"X" ]
nba[nba=="null"]<-NA
nba<-nba[, c("GAME_ID", "EVENTNUM", "EVENTMSGTYPE", "EVENTMSGACTIONTYPE",
             "PCTIMESTRING","HOMEDESCRIPTION", "NEUTRALDESCRIPTION" , "VISITORDESCRIPTION", "SCORE", 
             "PERSON1TYPE",       "PLAYER1_ID", "PLAYER1_NAME", "PLAYER1_TEAM_ID",  "PLAYER1_TEAM_ABBREVIATION", 
             "PERSON2TYPE",  "PLAYER2_ID", "PLAYER2_NAME", "PLAYER2_TEAM_ID",  "PLAYER2_TEAM_ABBREVIATION", 
             "PERSON3TYPE",  "PLAYER3_ID", "PLAYER3_NAME", "PLAYER3_TEAM_ID",  "PLAYER3_TEAM_ABBREVIATION"   )]


###TIME/QUARTER
nba<-nba[nba$GAME_ID==id, ]
nba$Time<-sapply(strsplit( nba$PCTIMESTRING, ":"), function(x) { as.numeric(head(x, 1))+as.numeric(tail(x, 1))/60 })

#add "quarter" variable
lastPlay<-which(diff(nba$Time)>4)
lastPlay<-c(0, lastPlay, length(nba$Time))
lastPlay<-diff(lastPlay)

#for some reason mapply causes error if lastPlay is all the same number
if(length(unique(lastPlay))==1) {
  nba$Quarter<-rep( c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)[1:length(lastPlay)], each=lastPlay[1])
} else {
  nba$Quarter<-unlist(mapply(rep, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)[1:length(lastPlay)],  lastPlay))
}


##GET GAME-BOX SCORE########
address<-paste0(c("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=55800&GameID=", id,
                  "&RangeType=2&Season=2015-16&SeasonType=Regular+Season&StartPeriod=1&StartRange=0"), collapse="")
web_page <- readLines(address)

##regex to strip javascript bits and convert raw to csv format
x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
x4 <- gsub(";", ",",x3, perl=TRUE)

box<-read.table(textConnection(x4), header=T, sep=",", fill=T, skip=2, stringsAsFactors=FALSE)
box<-box[1:(which(box$GAME_ID=="headers:")-1),!colnames(box)%in% "X"]


#in years <=2007, box-score errors where players not in nba.com box score
if(id=="0020700880"){
  box<-rbind.fill(data.frame(PLAYER_ID=2056, TEAM_ID=1610612761,GAME_ID=id, PLAYER_NAME="Primoz Brezec", START_POSITION=""), box)
}
if(id=="0020600448"){
  box<-rbind.fill(data.frame(PLAYER_ID=1891, TEAM_ID=1610612742,GAME_ID=id, PLAYER_NAME="Jason Terry", START_POSITION=""), box)
}


###ORGANIZE SUBSTITUTIONS####
nba[, unique(box$PLAYER_ID)]<-0

nba[nba$Quarter==1,  unique(box$PLAYER_ID[box$START_POSITION!=""])]<-1
nba$TimeElapsed<-ifelse(nba$Quarter<=4,12-nba$Time, 5-nba$Time)
nba$TimeElapsed<-ifelse(nba$Quarter<=4, nba$TimeElapsed+12*(nba$Quarter-1), nba$TimeElapsed+12*4+5*(nba$Quarter-5))

is_sub<- which(nba$EVENTMSGTYPE==8)
for(i in 1:nrow(nba)){
  
  # put player in if they register a play and were never in for the quarter (exception is technical fouls or weird game-violations --can get these while not in play)
  if(!i %in% is_sub &
     sum(is.na(nba[i,grepl("DESCRIPT", colnames(nba))]))!=3 &
     sum(grepl("T.FOUL|Ejection|Timeout|TECH.FOUL|Unsportsmanlike", nba[i,grepl("DESCRIPT", colnames(nba))]))==0 ){
   
     if(nba$PLAYER1_ID[i]%in% colnames(nba)){
      if(sum(nba[nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER1_ID[i])])==0){
        nba[nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER1_ID[i])]<-1
      }
    }
    if(nba$PLAYER2_ID[i]%in% colnames(nba)){
      if(sum(nba[nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER2_ID[i])])==0){
        nba[nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER2_ID[i])]<-1
      }
    }
    if(nba$PLAYER3_ID[i]%in% colnames(nba)){
      if(sum(nba[nba$Quarter==nba$Quarter[i],as.character( nba$PLAYER3_ID[i])])==0){
        nba[nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER3_ID[i])]<-1
      }
    }
    
  }
  
  #handling substitution events
  if(i %in% is_sub ){
    #player enterring
    nba[nba$TimeElapsed>nba$TimeElapsed[i]& nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER2_ID[i])]<-1  
    #player leaving
    nba[nba$TimeElapsed>nba$TimeElapsed[i]& nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER1_ID[i])]<-0
    
    #if player was not previously in during the quarter,and he wasn't an immediate sub-in->sub-out, then he must have started the quarter, so put him in for t<T
    player_court_sum<-sum(nba[nba$TimeElapsed<=nba$TimeElapsed[i] & nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER1_ID[i])] )
    just_subbed_in<-nba[nba$TimeElapsed==nba$TimeElapsed[i]& nba$EVENTMSGTYPE==8, c("PLAYER2_ID")]
    
    if(player_court_sum==0 & !nba$PLAYER1_ID[i]%in% just_subbed_in  ){
      nba[nba$TimeElapsed<=nba$TimeElapsed[i] & nba$Quarter==nba$Quarter[i], as.character(nba$PLAYER1_ID[i])]<-1
    }
    
  }
}
tail(nba[,grepl("EVENTM|PCTIME|HOMED|VISITORD|PLAYER1_NAME|PLAYER1_TEAM_ID|POSS", colnames(nba)) ],20)

#manual OT errors or other errors
source("PBP scrape errors.R")

# EVENTMSGTYPE
# 1 - Make 2 - Miss 3 - Free Throw 4 - Rebound 5 - out of bounds / Turnover / Steal 6 - Personal Foul 
# 7 - Violation 8 - Substitution 9 - Timeout 10 - Jumpball 12 - Start Q1? 13 - Start Q2?

# EVENTMSGACTIONTYPE
# 1 - Jumpshot 2 - Lost ball Turnover 3 - ? 4 - Traveling Turnover / Off Foul 5 - Layup 
#7 - Dunk 10 - Free throw 1-1 11 - Free throw 1-2 12 - Free throw 2-2 40 - out of bounds 
#41 - Block/Steal 42 - Driving Layup 50 - Running Dunk 52 - Alley Oop Dunk 55 - Hook Shot 
#57 - Driving Hook Shot 58 - Turnaround hook shot 66 - Jump Bank Shot 71 - Finger Roll Layup 72 - Putback Layup 108 - Cutting Dunk Shot

###POSESSIONS AND SCORING##########

nba$POSSESSION<-nba$PLAYER1_TEAM_ID

#team rebound
nba$POSSESSION[is.na(nba$PLAYER1_TEAM_ID)& !is.na(nba$PLAYER1_ID)& nba$PLAYER1_ID%in% unique(box$TEAM_ID)]<-
  nba$PLAYER1_ID[is.na(nba$PLAYER1_TEAM_ID)& !is.na(nba$PLAYER1_ID)& nba$PLAYER1_ID%in% unique(box$TEAM_ID)]

#subs, timeouts, game stoppage don't affect possession
nba$POSSESSION[is_sub]<-NA
nba$POSSESSION[nba$EVENTMSGTYPE==9| nba$PLAYER1_ID==0]<-NA

#a foul means the non-fouling team is in posession
#an offensive foul means the fouling team is in posession
nba$POSSESSION[nba$EVENTMSGTYPE==6& nba$EVENTMSGACTIONTYPE!=4& !is.na(nba$POSSESSION)]<-
  sapply(nba$POSSESSION[nba$EVENTMSGTYPE==6& nba$EVENTMSGACTIONTYPE!=4& !is.na(nba$POSSESSION)], function(x) unique(box$TEAM_ID[box$TEAM_ID!=x]))


nba$nextPOSS<-lead(nba$POSSESSION)
nba[ c("POSSESSION","nextPOSS")]<-sapply(nba[,  c("POSSESSION","nextPOSS")], na.locf, fromLast=F, na.rm=F)
nba$possComplete<-ifelse(nba$POSSESSION!=nba$nextPOSS,1, 0)
nba$possComplete[nba$EVENTMSGTYPE==12]<-0
nba$possComplete[nba$EVENTMSGTYPE==13]<-1

home<-tail(box$TEAM_ID,1)
nba[, unique(box$PLAYER_ID[box$TEAM_ID!=home]) ][nba[, unique(box$PLAYER_ID[box$TEAM_ID!=home]) ]==1]<-(-1)

nba[, c("HomePTS","AwayPTS")]<-NA
nba$HomePTS[!is.na(nba$SCORE)]<-as.numeric(sapply(strsplit(nba$SCORE[!is.na(nba$SCORE)], " - "),`[[`, 2))
nba$AwayPTS[!is.na(nba$SCORE)]<-as.numeric(sapply(strsplit(nba$SCORE[!is.na(nba$SCORE)], " - "),`[[`, 1))
nba[1, c("HomePTS","AwayPTS")]<-0
nba[, c("HomePTS","AwayPTS")]<-sapply(nba[,  c("HomePTS","AwayPTS")], na.locf, fromLast=F)
nba[ ,c("HomePTS","AwayPTS")]<-sapply(nba[,  c("HomePTS","AwayPTS")], as.numeric)


###OVERTIME ERROR FIX#########
#automatically fix OT problems by checking to see if there is a player off by exactly 5 minutes--happens if played whole OT w.o. stat

errors<-unname(which(rowSums(nba[, unique(box$PLAYER_ID)])!=0))
if(length(errors)>0 ){ #if there is an error
  
  quarterError<-unique(nba$Quarter[errors])
  if(length(quarterError)==1 &quarterError>=5)  { #if it is an OT error
    stints<-getStint(box, nba)
    
    minSums<-sapply(paste("X",unique(box$PLAYER_ID), sep=""), function(x) sum(stints$TimeEnd[stints[, x]!=0]-stints$TimeStart[stints[, x]!=0]))
    minSums<-data.frame(MINS=unname(minSums), ID=gsub("X", "", names(minSums)))
    minSums$boxMIN<-box$MIN[match(minSums$ID, box$PLAYER_ID)]
    minSums$boxMIN<-as.numeric(sapply(strsplit(minSums$boxMIN,":"),  `[[`, 1))+as.numeric(sapply(strsplit(minSums$boxMIN,":"),  `[[`, 2))/60
    minSums$boxDiff<-minSums$MINS-minSums$boxMIN
    fix<- minSums$ID[abs(minSums$boxDiff)>4.9 &abs(minSums$boxDiff)<5.1 ]
    nba[nba$Quarter==quarterError,fix]<-1
  }
  
}
nba[, unique(box$PLAYER_ID[box$TEAM_ID!=home]) ][nba[, unique(box$PLAYER_ID[box$TEAM_ID!=home]) ]==1]<-(-1)

# 
#homeStart and timeStart should begin at the line before the lineChange, homeend and lineend should be the line before the next linechange, 
#possessions should not include the line before the linechange and should go to the line before the next linechange

getStint<-function(box, nba){
  lineChange<-colDiffs(as.matrix(nba[, unique(box$PLAYER_ID)]))
  lineChange<-c(1, which(sapply(1:nrow(lineChange), function(x) sum(lineChange[x, ]!=0)) !=0))
  
  stints<-data.frame(rbindlist(lapply( 1:length(lineChange), function(i){
    if(i!=1){
      start<-lineChange[i]+1
    } else{
      start<-1
    }
    if(i==length(lineChange)){
      end<-nrow(nba)
    }else{
      end<-lineChange[i+1]
    }
    data<-data.frame(nba[start, unique(box$PLAYER_ID)])
    if(start!=1){
      data$HomeStart<-nba$HomePTS[(start-1)]
      data$AwayStart<-nba$AwayPTS[(start-1)]
      data$TimeStart<-nba$TimeElapsed[start-1]
    } else{
      data$HomeStart<-nba$HomePTS[(start)]
      data$AwayStart<-nba$AwayPTS[(start)]
      data$TimeStart<-nba$TimeElapsed[start]
    }
    data$HomeEnd<-nba$HomePTS[(end)]
    data$AwayEnd<-nba$AwayPTS[(end)]
    data$TimeEnd<-nba$TimeElapsed[end]
    
    data$POSSESSIONS<-sum(nba$possComplete[(start):(end)])
    data$HomePOSS<-sum(nba$possComplete[start:end][which(nba$POSSESSION[start:end]==tail(box$TEAM_ID,1)) ])
    data$AwayPOSS<-data$POSSESSIONS-data$HomePOSS
    
    data
  })))
  stints$GAME_ID<-box$GAME_ID[1]
  stints
}
plot( rowSums(nba[, unique(box$PLAYER_ID)]), main=k)


##use below to inspect errors i.e. if above plot is not a straight horizontal line. handle errors in pbp error script

stints<-getStint(box, nba)
# box[, 1:10]
# sapply(paste("X",unique(box$PLAYER_ID), sep=""), function(x) sum(stints$TimeEnd[stints[, x]!=0]-stints$TimeStart[stints[, x]!=0]))
# errors<-unname(which(rowSums(nba[, unique(box$PLAYER_ID[box$TEAM_ID==home])])!=5| rowSums(nba[, unique(box$PLAYER_ID[box$TEAM_ID!=home])])!=-5  ))
# nba[errors,!grepl("TEAM_ID|TEAM_ABBREV|_TYPE|NEUTRAL|PCT|GAME_|EVENTNUM|POSS|poss", colnames(nba)) ]
