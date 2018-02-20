
years<-2010:2013
#be sure to have files saved in directoy as PBP09, PBP10, PBP11, PBP12, etc, whatever years you are using in years
source("RAPM load data.R")

head(stints[, 1:20])
#save some data for RMarkdown
# save(stints, file="~/UVA/NBA/stints.Rda")
# save(coefs, file="~/UVA/NBA/coefs.Rda")
# save(fitList, file="~/UVA/NBA/fitList.Rda")

###CLEAN DATA#####

stints<-merge(stints, games[games$Home==1, c("Type", "GAME_ID", "GAME_DATE"), ],by.x="GAME_ID", by.y="GAME_ID", all.x=T)

test<-stints[stints$Year==max(years), ]
stints<-stints[!stints$Year%in% max(years) ,]
stints$XOTHER<-NULL
stints<-data.frame(stints)
stints$MARGIN<-100*((stints$HomeEnd-stints$HomeStart)-(stints$AwayEnd-stints$AwayStart))/stints$POSSESSIONS
stints$MARGIN[stints$POSSESSIONS==0]<-0
stints$StartDIFF<-stints$HomeStart-stints$AwayStart
stints$TimeTotal<-stints$TimeEnd-stints$TimeStart

stints[is.na(stints)]<-0
totals<-data.frame(POSS=abs(t(as.matrix(stints[, grepl("X", colnames(stints))])))%*%as.matrix(stints$POSSESSIONS))
totals$PLAYER_ID<-gsub("X", "", row.names(totals))
totals$MIN<-as.vector(abs(t(as.matrix(stints[, grepl("X", colnames(stints))])))%*%as.matrix(stints$TimeTotal))

head(totals)
##CROSS VALIDATION FOR RAPM PARAMETER OPTIMIZATION#####
#this will take several minutes to run

cutoff_param<-c( 1000, 2000 )  ###  lumping together players with minutes [layed <cutoff_param
year_param<-c( .5, .25)        ###  weighting older years by year_param
playoff_param<-c(0)            ###  adding 1+playoff_param weight to playoff games
lambda_param<-c(100, 50, 25)   ###  lambda=coefficient regularization. supply in descending order or else it gets reversed and messes up plots***
testGrid<-expand.grid(cutoff_param=cutoff_param,year_param=year_param ,playoff_param=playoff_param)

#specify folds--important so that folds will remain the same for each model tested
nfolds<-10
folds<-replicate(nfolds, sample(which(stints$Year==max(years)-1 & stints$POSSESSIONS>0),size=20000, replace=F))
folds<-rbind(folds,replicate(nfolds, which(stints$Year<max(years)-1 & stints$POSSESSIONS>0)))
outfolds<-sapply(1:ncol(folds),function(x) setdiff(which(stints$POSSESSIONS>0), folds[,x]))


cl<-makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

#specify parameters and fit model on each CV fold
fitList<-  foreach(k=1:nrow(testGrid), .packages = c("glmnet")) %dopar% {  
  
  players<-paste("X", totals$PLAYER_ID[totals$MIN>=testGrid$cutoff_param[k]]  , sep="")
  stints$XOTHER<-rowSums(stints[, grepl("X", colnames(stints) ) & !grepl("OTHER", colnames(stints))& !colnames(stints)%in% players ])
  indVars<- c( "XOTHER",  players)
  
  
  errors<-lapply(1:nfolds, function(x){
    fit<-glmnet( x=as.matrix(stints[folds[,x],indVars ]),
                 y=stints$MARGIN[folds[,x]],
                 weights=testGrid$year_param[k]^(max(stints$Year)-stints$Year[folds[,x]])*stints$POSSESSIONS[folds[,x]]*
                   (1+(stints$Type[folds[,x]]=="Playoffs")*testGrid$playoff_param[k]),#
                 alpha=0,  
                 # standardize=F,
                 lambda = lambda_param)
    
    #(out of fold errors are RMSE of predicted margins to actual margin, weighted by possessions)
    
    errors<-(predict(fit, newx=data.matrix(stints[outfolds[, x], indVars] ))*stints$POSSESSIONS[outfolds[,x]] -
               stints$MARGIN[outfolds[,x]]*stints$POSSESSIONS[outfolds[,x]] )^2
    errors<-sqrt(colMeans(errors))
    errors
  })
  #return parameters and aggregated results of OOF error
  cvm<-sapply(1:length(lambda_param), function(x) mean(sapply(errors, `[[`, x)))
  cvsd<-sapply(1:length(lambda_param), function(x) sd(sapply(errors, `[[`, x)))
  
  fit<-list();fit$cvm<-cvm;fit$cvsd<- cvsd;fit$lambda<-lambda_param
  fit[colnames(testGrid)]<-testGrid[k,]
  fit
}

#look at CV errors of tested models
par(mfrow=c(2, 2))
sapply(fitList, function(x) plot(x$cvm~log(x$lambda), type="l", main=paste0(c("yearWeight: ", x$year_param, ", minCutoff: ", x$cutoff_param), collapse="")))

params<-fitList[[which.min(sapply(fitList, function(x) min(x$cvm)))]]
params

#redefine data and redo model using optimal parameters
players<-paste("X", totals$PLAYER_ID[totals$MIN>=params$cutoff_param ] , sep="")
stints$XOTHER<-rowSums(stints[, grepl("X", colnames(stints) ) & !grepl("OTHER", colnames(stints))& !colnames(stints)%in% players ])
indVars<- c( "XOTHER",  players)

fit<-glmnet( x=as.matrix(stints[stints$POSSESSIONS>0,indVars ]),
             y=stints$MARGIN[stints$POSSESSIONS>0],
             weights=params$year_param^(max(stints$Year)-stints$Year[stints$POSSESSIONS>0])*stints$POSSESSIONS[stints$POSSESSIONS>0],#
             alpha=0, 
             lambda=params$lambda[which.min(params$cvm)])



stints$predMARGIN<-predict(fit, newx=data.matrix(stints[,indVars]))[, 1]
final<-data.table(stints)
final<-final[ ,list(
  predMARGIN=sum(predMARGIN*POSSESSIONS)/100,
  MARGIN=sum(MARGIN*POSSESSIONS)/100
  
), by=c("GAME_ID", "Year")]
final<-data.frame(final)

#in-sample accuracy*****
Metrics(final$MARGIN[final$Year==max(years)-1],final$predMARGIN[final$Year==max(years)-1])


#look at results on test-set

test$MARGIN<-100*((test$HomeEnd-test$HomeStart)-(test$AwayEnd-test$AwayStart))/test$POSSESSIONS
test$MARGIN[test$POSSESSIONS==0]<-0
test$StartDIFF<-test$HomeStart-test$AwayStart
test$StartDIFFTimeStart<-test$StartDIFF*test$TimeStart
test[is.na(test)]<-0
test<-data.frame(test)
#players who did not play last year/not enough minutes last year
test$XOTHER<-rowSums(test[, grepl("X", colnames(test) ) & !grepl("OTHER", colnames(test))& !colnames(test)%in% players ])

#players who were in last year, but not in this year
test[,setdiff(players, colnames(test))  ]<-0

test$predMARGIN<-predict(fit, newx=data.matrix(test[,indVars]))[, 1]

final<-data.table(test)
final<-final[ ,list(
  predMARGIN=sum(predMARGIN*POSSESSIONS)/100,   
  MARGIN=sum(MARGIN*POSSESSIONS)/100
  
), by=c("GAME_ID")]
final<-data.frame(final)

#OOS accuracy*****
Metrics( final$MARGIN, final$predMARGIN)
cor( final$MARGIN,final$predMARGIN)


