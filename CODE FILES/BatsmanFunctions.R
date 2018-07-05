#setwd(dir="C:\\Users\\SIDDHESH\\Desktop\\Data Science - Aegis\\Capstone project\\Code\\Team Code\\PROJECT\\PROJECT\\DATA\\")
    
#Function for data cleaning
DataClean <- function(file) {
      
      playerData <- read.csv(file,stringsAsFactor=FALSE,na.strings=c(NA,"-"))
      playerData <- playerData[,-c(1,11)]
      df <- playerData
      
      # Remove rows where the batsman 'did not bat' - DNB
      a <- df$Runs != "DNB"
      batsman <- df[a,]
      
      # Remove rows with 'TDNB'
      c <- batsman$Runs != "TDNB"
      batsman <- batsman[c,]
      
      # Remove rows with absent
      d <- batsman$Runs != "absent"
      batsman <- batsman[d,]
      
      # Remove the "* indicating not out
      batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
      
      #c <- complete.cases(batsman)
      
      #batsman$Mins[batsman$Mins==NA] <- ""
      
      #Regression for predicting missing Mins
      # predicted_mins <- lm(Mins ~ Runs+BF,data = batsman[!is.na(batsman$Mins),])
      # batsman$Mins[is.na(batsman$Mins)] <- round(predict(predicted_mins, batsman[is.na(batsman$Mins),]),0)
      
      
      #Computing missing SRs
      NewSR=round((batsman$Runs/batsman$BF)*100,2)
      NewSR <- replace(NewSR, is.na(NewSR), 0)
      batsman$SR=NewSR 
      
      batsmanComplete <- batsman
      list(val=dim(batsmanComplete),names = names(batsmanComplete),h=head(batsmanComplete))
      
      str1 <- batsmanComplete$Opposition
      batsmanComplete$Opposition=str_replace_all(str1, "v ", "")
      
      #Return the data frame 
      batsmanComplete
      # write.csv(batsmanComplete,"Clean.csv")
    }
#Function for capturing the basic statistics
Stats <- function(file,name){
      
      batsman <- NULL
      batsman <- DataClean(file)
      
      noofMatches=nrow(batsman)
      #Runs
      #avgruns=round(mean(batsman$Runs),0)
      #medruns=median(batsman$Runs)
      #IQRruns=IQR(batsman$Runs)
      #minruns=min(batsman$Runs)
      #maxruns=max(batsman$Runs)
      
      avgruns=paste("Average runs for ",name," - ",round(mean(batsman$Runs),0))
      medruns=paste("Median of runs for ",name," - ",median(batsman$Runs))
      IQRruns=paste("IQR of runs for ",name," - ",IQR(batsman$Runs))
      minruns=paste("Lowest runs for ",name," - ",min(batsman$Runs))
      maxruns=paste("Highest runs for ",name," - ",max(batsman$Runs))
      
      print(avgruns)
      print(medruns)
      print(IQRruns)
      print(minruns)
      print(maxruns)
    }
#Function to remove outliers
RemoveOutlier <- function(file) {
      batsman <- NULL
      batsman <- DataClean(file)
      
      var <- batsman$Runs
      var_name <- eval(substitute(var),eval(batsman))
      outlier <- boxplot.stats(var_name)$out
      
      var_name <- !var_name %in% outlier
      batsman <-  batsman[var_name,]
      batsman[complete.cases(batsman),]
      
    }
#Function to get most played PositionBat
getmodeBat <- function(file) {
      
      batsman <- NULL
      batsman <- RemoveOutlier(file)
      
      var <- batsman$Pos
      
      uniqv <- unique(var)
      
      uniqv[which.max(tabulate(match(var, uniqv)))]
    }


#Function to create Batting Indexes

Batting_Metrics <- function(file,name,team,role){
      
      batsman <- RemoveOutlier(file)
      
      #Games Played
      countInningsBat <- batsman %>% summarise(len=length(Runs))
      
      #Total Runs
      TotalRunsBat <- batsman %>% summarise(runs=sum(Runs))
      
      #Total Balls Faced
      TotalBallsBat <- batsman %>% summarise(balls=sum(BF))
      
      #Total Dismissals
      countOut<- batsman$Dismissal[batsman$Dismissal!="not out"]
      TotalDismissalBat <- length(countOut)
      
      #Total Boundries
      fours <- batsman %>% summarise(f= sum(X4s))
      sixes <- batsman %>% summarise(s= sum(X6s))
      TotalBoundriesBat <- fours+sixes
      
      #Batting Average
      BatAvg <- as.double(round(TotalRunsBat/TotalDismissalBat,2))
      
      #Batting Strike Rate
      BatSR <- as.double(round(TotalRunsBat/TotalBallsBat,2))
      
      #Average Contribution
      
      
      #Percentage Boundaries Hit
      PerBoundariesHit <- as.double(round(TotalBoundriesBat/TotalBallsBat,2))
      
      #PositionBat
      PositionBat <- getmodeBat(file)
      
      
      #Number of Wins
      Wins <- batsman$result[batsman$result=="won"]
      TotalWinsBat <- length(Wins)
      
      #Percentage Wins (Y) 
      PerWinBat <- round(TotalWinsBat/countInningsBat,2) 
      PerWinBat <- PerWinBat * 100
      
      TeamBat <- team
      RoleBat <- role
      NameBat <- name
      
      
      newDFBat <- data.frame(NameBat,TeamBat,RoleBat,PositionBat,countInningsBat,TotalRunsBat,TotalBallsBat,BatAvg,BatSR,fours,sixes,TotalBoundriesBat,PerBoundariesHit,TotalDismissalBat,TotalWinsBat,PerWinBat)
      names(newDFBat) <- c("Player","Team","Role","PositionBat","Game Played","Total Runs Scored","Total Balls Faced","Batting Average","Batting Strike Rate","Fours","Sixes","Total Boundries","Percentage Boundaries Hit","Total Dismissal","Total Wins","Percentage Wins")
      newDFBat
      
      
    }

Batting_Metrics_GroundWise <- function(file,name,team,role,ground){
  
  batsmanFrame <- RemoveOutlier(file)
  
  batsman <- batsmanFrame[batsmanFrame$Ground==ground,]
  
  #Games Played
  countInnings <- batsman %>% summarise(len=length(Runs))
  
  #Total Runs
  TotalRuns <- batsman %>% summarise(runs=sum(Runs))
  
  #Total Balls Faced
  TotalBalls <- batsman %>% summarise(balls=sum(BF))
  
  #Total Dismissals
  countOut<- batsman$Dismissal[batsman$Dismissal!="not out"]
  TotalDismissal <- length(countOut)
  
  #Total Boundries
  fours <- batsman %>% summarise(f= sum(X4s))
  sixes <- batsman %>% summarise(s= sum(X6s))
  TotalBoundries <- fours+sixes
  
  #Batting Average
  BatAvg <- round(TotalRuns/TotalDismissal,2)
  
  #Batting Strike Rate
  BatSR <- round(TotalRuns/TotalBalls,2)
  
  #Average Contribution
  
  
  #Percentage Boundaries Hit
  PerBoundariesHit <- round(TotalBoundries/TotalBalls,2)
  
  #Position
  PositionBat <- getmodeBat(file)
  
  
  #Number of Wins
  Wins <- batsman$result[batsman$result=="1"]
  TotalWins <- length(Wins)
  
  #Percentage Wins (Y) 
  PerWin <- round(TotalWins/countInnings,2)
  
  Team <- team
  Role <- role
  Name <- name
  Ground <- ground
  
  
  newDF <- data.frame(Name,Team,Role,Ground,PositionBat,countInnings,TotalRuns,TotalBalls,BatAvg,BatSR,fours,sixes,TotalBoundries,PerBoundariesHit,TotalDismissal,TotalWins,PerWin)
  names(newDF) <- c("Player","Team","Role","Ground","PositionBat","Game Played","Total Runs Scored","Total Balls Faced","Batting Average","Batting Strike Rate","Fours","Sixes","Total Boundries","Percentage Boundaries Hit","Total Dismissal","Total Wins","Percentage Wins")
  newDF

}

Batting_Metrics_Opposition <- function(file,name,team,role,oppo){
  
  batsmanFrame <- RemoveOutlier(file)
  
  batsman <- batsmanFrame[batsmanFrame$Opposition==oppo,]
  
  #Games Played
  countInnings <- batsman %>% summarise(len=length(Runs))
  
  #Total Runs
  TotalRuns <- batsman %>% summarise(runs=sum(Runs))
  
  #Total Balls Faced
  TotalBalls <- batsman %>% summarise(balls=sum(BF))
  
  #Total Dismissals
  countOut<- batsman$Dismissal[batsman$Dismissal!="not out"]
  TotalDismissal <- length(countOut)
  
  #Total Boundries
  fours <- batsman %>% summarise(f= sum(X4s))
  sixes <- batsman %>% summarise(s= sum(X6s))
  TotalBoundries <- fours+sixes
  
  #Batting Average
  BatAvg <- round(TotalRuns/TotalDismissal,2)
  
  #Batting Strike Rate
  BatSR <- round(TotalRuns/TotalBalls,2)
  
  #Average Contribution
  
  
  #Percentage Boundaries Hit
  PerBoundariesHit <- round(TotalBoundries/TotalBalls,2)
  
  #Position
  PositionBat <- getmodeBat(file)
  
  
  #Number of Wins
  Wins <- batsman$result[batsman$result=="1"]
  TotalWins <- length(Wins)
  
  #Percentage Wins (Y) 
  PerWin <- round(TotalWins/countInnings,2)
  
  Team <- team
  Role <- role
  Name <- name
  Opposition <- oppo
  
  
  newDF <- data.frame(Name,Team,Role,Opposition,PositionBat,countInnings,TotalRuns,TotalBalls,BatAvg,BatSR,fours,sixes,TotalBoundries,PerBoundariesHit,TotalDismissal,TotalWins,PerWin)
  names(newDF) <- c("Player","Team","Role","Opposition","PositionBat","Game Played","Total Runs Scored","Total Balls Faced","Batting Average","Batting Strike Rate","Fours","Sixes","Total Boundries","Percentage Boundaries Hit","Total Dismissal","Total Wins","Percentage Wins")
  newDF
  
}
