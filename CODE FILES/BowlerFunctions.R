#setwd(dir="C:\\Users\\SIDDHESH\\Desktop\\Data Science - Aegis\\Capstone project\\Code\\Team Code\\PROJECT\\PROJECT\\DATA\\")


#Function for data cleaning
cleanBowlerData <- function(file) {
  
  BPO <- Overs <- NULL
  # Read the <bowler>.csv file
  
  playerData <- read.csv(file,stringsAsFactor=FALSE,na.strings=c(NA,"-"))
  playerData <- playerData[,-c(1,9)]
  df <- playerData
  
  # Remove rows with did not bowl
  a <- df$Overs != "DNB"
  bowler <- df[a,]
  
  # Remove rows with 'TDNB' - team did not bowl
  c <- bowler$Overs != "TDNB"
  bowler <- bowler[c,]
  
  # Get all complete cases
  c <- complete.cases(bowler)
  bowlerComplete <- bowler[c,]
  
  # Normalize overs which had 8 balls per over to the number of overs if there 8 balls per over
  if(names(bowlerComplete)[3] == "BPO") {
    bowlerComplete <- mutate(bowlerComplete, Overs = ifelse(BPO==8,as.numeric(Overs)*8/6,Overs))   
  }
  
  str1 <- bowlerComplete$Opposition
  bowlerComplete$Opposition=str_replace_all(str1, "v ", "")
  
  #Return the data frame 
  bowlerComplete
}

#Function to get most played position
getmodeBowl <- function(file) {
  
  bowler <- NULL
  bowler <- cleanBowlerData(file)
  
  var <- bowler$Pos
  
  uniqv <- unique(var)
  
  uniqv[which.max(tabulate(match(var, uniqv)))]
}


#Function to create Bowling Indexes

Bowling_Metrics <- function(file,name,team,role){
  
  bowler <- cleanBowlerData(file)
  
  #Total Runs
  TotalRuns <- bowler %>% summarise(runs=sum(Runs))
  
  #Total Overs
  TotalOvers <- bowler%>% summarise(over=sum(as.numeric(Overs)))
  
  #Total Wickets
  TotalWkts <- bowler %>% summarise(wkt=sum(Wkts))
  
  #Total Maidans 
  TotalMaidanOvers <- bowler%>% summarise(over=sum(as.numeric(Mdns)))
  
  #Total Wickets  
  TotalWkts <- bowler %>% summarise(wkt=sum(Wkts))
  
  #Games Played
  countInnings <- bowler %>% summarise(len=length(Runs))
  
  #Economy
  Economy <- round(TotalRuns/TotalOvers,2)
  
  #Bowling Average 	
  BowlAvg <- round(TotalRuns/TotalWkts,2)
  
  #Dot balls
  TotalDotBalls <- TotalMaidanOvers*6
  
  
  #Balls Bowled
  Overs <- do.call(rbind, strsplit(as.character(TotalOvers),"\\."))
  whole <- as.numeric(Overs[1])
  extra <- as.numeric(Overs[2])
  if(is.na(extra))
  {
    TotalBalls <- whole*6
  }else{
    TotalBalls <- whole*6+extra
  }
  
  #Percentage Dot
  PercDot <- round(TotalDotBalls/TotalBalls,2)
  
  #Position
  Position <- getmodeBowl(file)
  
  #Number of Wins
  Wins <- bowler$result[bowler$result=="won"]
  TotalWins <- length(Wins)
  
  #Percentage Wins (Y) 
  PerWin <- round(TotalWins/countInnings,2)
  PerWin <- PerWin * 100
  
  Team <- team
  Role <- role
  Name <- name
  
  newBowlDF <- data.frame(Name,Team,Role,Position,countInnings,TotalOvers,TotalRuns,TotalBalls,TotalWkts,Economy,BowlAvg,TotalMaidanOvers,TotalDotBalls,PercDot,TotalWins,PerWin)
  names(newBowlDF) <- c("Player","Team","Role","Position","Game Played","Total Overs","Total Runs","Balls Bowled","Total Wickets","Economy","Bowling Average","Total Maidan Overs","Total Dot Balls","Percentage Dot","Total Wins","Percentage Wins")
  newBowlDF
  
  
  
}

Bowling_Metrics_GroundWise <- function(file,name,team,role,ground){
  
  bowlerFrame <- cleanBowlerData(file)
  
  bowler <- bowlerFrame[bowlerFrame$Ground==ground,]
  
  #Total Runs
  TotalRuns <- bowler %>% summarise(runs=sum(Runs))
  
  #Total Overs
  TotalOvers <- bowler%>% summarise(over=sum(as.numeric(Overs)))
  
  #Total Wickets
  TotalWkts <- bowler %>% summarise(wkt=sum(Wkts))
  
  #Total Maidans 
  TotalMaidanOvers <- bowler%>% summarise(over=sum(as.numeric(Mdns)))
  
  #Total Wickets  
  TotalWkts <- bowler %>% summarise(wkt=sum(Wkts))
  
  #Games Played
  countInnings <- bowler %>% summarise(len=length(Runs))
  
  #Economy
  Economy <- round(TotalRuns/TotalOvers,2)
  
  #Bowling Average 	
  BowlAvg <- round(TotalRuns/TotalWkts,2)
  
  #Dot balls
  TotalDotBalls <- TotalMaidanOvers*6
  
  
  #Balls Bowled
  Overs <- do.call(rbind, strsplit(as.character(TotalOvers),"\\."))
  whole <- as.numeric(Overs[1])
  extra <- as.numeric(Overs[2])
  if(is.na(extra))
  {
    TotalBalls <- whole*6
  }else{
    TotalBalls <- whole*6+extra
  }
  
  #Percentage Dot
  PercDot <- round(TotalDotBalls/TotalBalls,2)
  
  #Position
  Position <- getmodeBowl(file)
  
  #Number of Wins
  Wins <- bowler$result[bowler$result=="won"]
  TotalWins <- length(Wins)
  
  #Percentage Wins (Y) 
  PerWin <- round(TotalWins/countInnings,2)
  PerWin <- PerWin * 100
  
  Team <- team
  Role <- role
  Name <- name
  Ground <- ground
  
  newBowlDF <- data.frame(Name,Team,Role,Ground,Position,countInnings,TotalOvers,TotalRuns,TotalBalls,TotalWkts,Economy,BowlAvg,TotalMaidanOvers,TotalDotBalls,PercDot,TotalWins,PerWin)
  names(newBowlDF) <- c("Player","Team","Role","Ground","Position","Game Played","Total Overs","Total Runs","Balls Bowled","Total Wickets","Economy","Bowling Average","Total Maidan Overs","Total Dot Balls","Percentage Dot","Total Wins","Percentage Wins")
  newBowlDF
  
  
  
}

Bowling_Metrics_Opposition <- function(file,name,team,role,oppo){
  
  bowlerFrame <- cleanBowlerData(file)
  
  bowler <- bowlerFrame[bowlerFrame$Opposition==oppo,]
  
  #Total Runs
  TotalRuns <- bowler %>% summarise(runs=sum(Runs))
  
  #Total Overs
  TotalOvers <- bowler%>% summarise(over=sum(as.numeric(Overs)))
  
  #Total Wickets
  TotalWkts <- bowler %>% summarise(wkt=sum(Wkts))
  
  #Total Maidans 
  TotalMaidanOvers <- bowler%>% summarise(over=sum(as.numeric(Mdns)))
  
  #Total Wickets  
  TotalWkts <- bowler %>% summarise(wkt=sum(Wkts))
  
  #Games Played
  countInnings <- bowler %>% summarise(len=length(Runs))
  
  #Economy
  Economy <- round(TotalRuns/TotalOvers,2)
  
  #Bowling Average 	
  BowlAvg <- round(TotalRuns/TotalWkts,2)
  
  #Dot balls
  TotalDotBalls <- TotalMaidanOvers*6
  
  
  #Balls Bowled
  Overs <- do.call(rbind, strsplit(as.character(TotalOvers),"\\."))
  whole <- as.numeric(Overs[1])
  extra <- as.numeric(Overs[2])
  if(is.na(extra))
  {
    TotalBalls <- whole*6
  }else{
    TotalBalls <- whole*6+extra
  }
  
  #Percentage Dot
  PercDot <- round(TotalDotBalls/TotalBalls,2)
  
  #Position
  Position <- getmodeBowl(file)
  
  #Number of Wins
  Wins <- bowler$result[bowler$result=="won"]
  TotalWins <- length(Wins)
  
  #Percentage Wins (Y) 
  PerWin <- round(TotalWins/countInnings,2)
  PerWin <- PerWin * 100
  
  Team <- team
  Role <- role
  Name <- name
  Opposition <- oppo
  
  newBowlDF <- data.frame(Name,Team,Role,Opposition,Position,countInnings,TotalOvers,TotalRuns,TotalBalls,TotalWkts,Economy,BowlAvg,TotalMaidanOvers,TotalDotBalls,PercDot,TotalWins,PerWin)
  names(newBowlDF) <- c("Player","Team","Role","Opposition","Position","Game Played","Total Overs","Total Runs","Balls Bowled","Total Wickets","Economy","Bowling Average","Total Maidan Overs","Total Dot Balls","Percentage Dot","Total Wins","Percentage Wins")
  newBowlDF
  
  
  
}


#save(BowlerFunctions, file="BowlerFunctions.rda")
