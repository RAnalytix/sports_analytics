remove(list= ls())

#Loding the required packaged and libraries

#install.packages("magrittr")
library(magrittr)

#install.packages("dplyr")
library(dplyr)

#install.packages("scatterplot3d")
library(scatterplot3d)

#install.packages("lubridate")
library(lubridate)

#install.packages("forecast")
library(forecast)

#install.packages("XML")
library(XML)

#install.packages("randomForest")
library(randomForest)

#install.packages("corrplot")
library(corrplot)

#install.packages("caret")
library(caret)

#install.packages("ggplot2")
library(ggplot2)

# install.packages("ggthemes")
library(ggthemes)

#install.packages("stringr")
library(stringr)


#Setting up the working directory
setwd(dir="C:\\Users\\SIDDHESH\\Desktop\\Data Science - Aegis\\Capstone project\\Code\\Team Code\\PROJECT\\PROJECT\\DATA\\")

ids <- read.csv("playerid.csv")
ids
rows <-  nrow(ids)



#Setting up the working directory
setwd(dir="C:\\Users\\SIDDHESH\\Desktop\\Data Science - Aegis\\Capstone project\\Code\\Team Code\\PROJECT\\PROJECT\\DATA\\Career Data\\")

#Career Data Scraper
for(i in 1:rows){
  career_scraper(ids[i,2],ids[i,1])
}


#Current Form Date Scraper
for(i in 1:rows){
  currentFormscrape(ids[i,2],ids[i,1])
}



# Creating Metrics for Career Data
for(i in 1:rows){
  batfileName <- paste0(ids[i,1],"_career_bat.csv")
  bowlfileName <- paste0(ids[i,1],"_career_bowl.csv")
  playerName <- ids[i,1]
  country <- ids[i,3]
  role <- ids[i,4]
  
  if(role != "Bowler"){
    bat <- Batting_Metrics(batfileName,playerName,country,role)
    if(exists("finalBattingMetrics"))
      finalBattingMetrics<- rbind(finalBattingMetrics,bat)
    else
      finalBattingMetrics <- bat
  }
  if(role != "Batsman" & role != "WK-Batsman"){
    bowl <-Bowling_Metrics(bowlfileName,playerName,country,role)
    if(exists("finalBowlingMetrics"))
      finalBowlingMetrics<- rbind(finalBowlingMetrics,bowl)
    else
      finalBowlingMetrics <- bowl
  }
}


#To find out batting possition
for(i in 1:rows){
  batfilePos <- paste0(ids[i,1],"_career_bat.csv")
  playerN <- ids[i,1]
  pos <- getmodeBat(batfilePos)
  pos2<- data.frame(playerN,pos)
  
  if(exists("finalBattingPos"))
    finalBattingPos <- rbind(finalBattingPos,pos2)
  else
    finalBattingPos <- pos2
}

finalBattingPos

#normalization
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}





#Random Forest For importance - Batting
BattingData <-  finalBattingMetrics
BattingData$PositionBat <- as.factor(BattingData$PositionBat)
head(BattingData)
str(BattingData)
BattingData_n <- as.data.frame(lapply(BattingData[,5:15], normalize))
BattingData_n
Percentage_Wins <-BattingData[,16]
Percentage_Wins
BattingDataFinal <- cbind(BattingData[,1:4],BattingData_n,Percentage_Wins)
BattingDataFinal
str(BattingDataFinal)
corrplot(cor(BattingDataFinal[,5:15]), order = "hclust")


# train model to find optimum mtry and ntree
customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


metric <- "RMSE"
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(50, 100, 150, 200))
set.seed(123)
custom <- train(Percentage_Wins ~ PositionBat+Game.Played+Total.Runs.Scored+Total.Balls.Faced+Batting.Average+Batting.Strike.Rate+Fours+Sixes+Total.Boundries+Percentage.Boundaries.Hit+Total.Dismissal+Total.Wins,data=BattingDataFinal, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
custom
#summary(custom)
plot(custom)

mtry=custom$bestTune$mtry
ntree=custom$bestTune$ntree
#mtry=2
#ntree=120
str(BattingDataFinal)


#Applying Random Forest
BattingData.rf <- randomForest(Percentage_Wins ~ PositionBat+Game.Played+Total.Runs.Scored+Total.Balls.Faced+Batting.Average+Batting.Strike.Rate+Fours+Sixes+Total.Boundries+Percentage.Boundaries.Hit+Total.Dismissal+Total.Wins,data=BattingDataFinal,importance=TRUE,keep.forest=FALSE,ntree=ntree,mtry=mtry)
summary(BattingData.rf)
BattingData.rf$importance
BattingData.rf$importanceSD
impBat=importance(BattingData.rf,scale=TRUE)
impBat
#a <- getTree(BattingData.rf,k=1,labelVar = TRUE)
#a

row.names(impBat)<-c("PositionBat","Game Played","Total Runs Scored","Total Balls Faced","Batting Average","Batting Strike Rate","Fours","Sixes","Total Boundries","Percentage Boundaries Hit","Total Dismissal","Total Wins")
importance(BattingData.rf, type=1,scale=TRUE)
varImpPlot(BattingData.rf, sort=TRUE, n.var=min(30, nrow(impBat)), type=1, class=NULL, scale=TRUE, main="Batting Metric Random Forest Importance Plot") 
varImpPlot(BattingData.rf, sort=TRUE, n.var=min(30, nrow(impBat)), type=2, class=NULL, scale=TRUE, main="Batting Metric Random Forest Importance Plot") 

varImportanceBat <- data.frame(Variables = row.names(impBat), Importance = round(impBat[ ,'%IncMSE'],2))
rankImportanceBat <- varImportanceBat %>%  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
rankImportanceBat


ggplot(rankImportanceBat, aes(x = reorder(Variables, Importance), 
                              y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()


#Obtaining Important variables - Batting
rankImportanceBat$Variables <- as.character(rankImportanceBat$Variables)
rankImportanceBat <- rankImportanceBat[order(-rankImportanceBat$Importance),]
rankImportanceBat<- rankImportanceBat[0:5,]
rank1Bat=rankImportanceBat[1,]$Variables
rank1Bat
Imp1Bat=rankImportanceBat[1,]$Importance
Imp1Bat

rank2Bat=rankImportanceBat[2,]$Variables
Imp2Bat=rankImportanceBat[2,]$Importance

rank3Bat=rankImportanceBat[3,]$Variables
Imp3Bat=rankImportanceBat[3,]$Importance

rank4Bat=rankImportanceBat[4,]$Variables
Imp4Bat=rankImportanceBat[4,]$Importance

rank5Bat=rankImportanceBat[5,]$Variables
Imp5Bat=rankImportanceBat[5,]$Importance


#Random Forest For importance - BowlingData
BowlingData <-  finalBowlingMetrics
BowlingData

BowlingData$Position <- as.factor(BowlingData$Position)
head(BowlingData)
str(BowlingData)
BowlingData_n <- as.data.frame(lapply(BowlingData[,5:15], normalize))
BowlingData_n

Percentage_Wins_Bowl <-BowlingData[,16]
Percentage_Wins_Bowl
BowlingDataFinal <- cbind(BowlingData[,1:4],BowlingData_n,Percentage_Wins_Bowl)
BowlingDataFinal
str(BowlingDataFinal)

corrplot(cor(BowlingDataFinal[,5:15]), order = "hclust")

# train model to find optimum mtry and ntree
# set.seed(222)
custom <- train(Percentage_Wins_Bowl ~ Position+Game.Played+Total.Overs+Total.Runs+Balls.Bowled+Total.Wickets+Economy+Bowling.Average+Total.Maidan.Overs+Total.Dot.Balls+Percentage.Dot+Total.Wins,data=BowlingDataFinal, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
custom
#summary(custom)
plot(custom)

#mtry=custom$bestTune$mtry
#ntree=custom$bestTune$ntree


mtry=5
ntree=90


#Applying Random Forest
BowlingData.rf <- randomForest(Percentage_Wins_Bowl ~ Position+Game.Played+Total.Overs+Total.Runs+Balls.Bowled+Total.Wickets+Economy+Bowling.Average+Total.Maidan.Overs+Total.Dot.Balls+Percentage.Dot+Total.Wins,data=BowlingDataFinal,importance=TRUE,keep.forest=FALSE,ntree=ntree,mtry=mtry)
summary(BowlingData.rf)
BowlingData.rf$importance
BowlingData.rf$importanceSD
impBowl=importance(BowlingData.rf,scale=TRUE)
impBowl
#a <- getTree(BattingData.rf,k=1,labelVar = TRUE)
#a

row.names(impBowl)<-c("Position","Game Played","Total Overs","Total Runs","Balls Bowled","Total Wickets","Economy","Bowling Average","Total Maidan Overs","Total Dot Balls","Percentage Dot","Total Wins")
importance(BowlingData.rf, type=1,scale=TRUE)
varImpPlot(BowlingData.rf, sort=TRUE, n.var=min(30, nrow(impBowl)), type=1, class=NULL, scale=TRUE, main="Bowling Metric Random Forest Importance Plot") 
varImpPlot(BowlingData.rf, sort=TRUE, n.var=min(30, nrow(impBowl)), type=2, class=NULL, scale=TRUE, main="Bowling Metric Random Forest Importance Plot") 

varImportanceBowl <- data.frame(Variables = row.names(impBowl), Importance = round(impBowl[ ,'%IncMSE'],2))
rankImportanceBowl <- varImportanceBowl %>%  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
rankImportanceBowl


ggplot(rankImportanceBowl, aes(x = reorder(Variables, Importance), 
                               y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()



#Obtaining Important variables - Bowling
rankImportanceBowl$Variables <- as.character(rankImportanceBowl$Variables)
rankImportanceBowl <- rankImportanceBowl[order(-rankImportanceBowl$Importance),]
rankImportanceBowl<- rankImportanceBowl[0:5,]

rank1Bowl=rankImportanceBowl[1,]$Variables
rank1Bowl
Imp1Bowl=rankImportanceBowl[1,]$Importance
Imp1Bowl

rank2Bowl=rankImportanceBowl[2,]$Variables
Imp2Bowl=rankImportanceBowl[2,]$Importance

rank3Bowl=rankImportanceBowl[3,]$Variables
Imp3Bowl=rankImportanceBowl[3,]$Importance

rank4Bowl=rankImportanceBowl[4,]$Variables
Imp4Bowl=rankImportanceBowl[4,]$Importance

rank5Bowl=rankImportanceBowl[5,]$Variables
Imp5Bowl=rankImportanceBowl[5,]$Importance


# Creating new data frames for calculating indexes against Ground
  for(i in 1:rows){
    batfileNameGRD <- paste0(ids[i,1],"_career_bat.csv")
    bowlfileNameGRD <- paste0(ids[i,1],"_career_bowl.csv")
    
    playerNameGRD <- ids[i,1]
    countryGRD <- ids[i,3]
    roleGRD <- ids[i,4]
    GRD <- "Kolkata"
    
    if(roleGRD != "Bowler"){
      batGRD <- Batting_Metrics_GroundWise(batfileNameGRD,playerNameGRD,countryGRD,roleGRD,GRD)
      if(exists("finalBattingMetricsGRD"))
        finalBattingMetricsGRD<- rbind(finalBattingMetricsGRD,batGRD)
      else
        finalBattingMetricsGRD <- batGRD
      
      #Batting Index
      GRDBat <- finalBattingMetricsGRD
      attr1Bat<-GRDBat[,rank1Bat]
      attr2Bat<-GRDBat[,rank2Bat]
      attr3Bat<-GRDBat[,rank3Bat]
      attr4Bat<-GRDBat[,rank4Bat]
      attr5Bat<-GRDBat[,rank5Bat]
      
      BattingIndexGRD <- GRDBat[,rank1Bat]*Imp1Bat+GRDBat[,rank2Bat]*Imp2Bat+GRDBat[,rank3Bat]*Imp3Bat+GRDBat[,rank4Bat]*Imp4Bat+GRDBat[,rank5Bat]*Imp5Bat  
      GRDBatMetric=data.frame(GRDBat[,1:5],attr1Bat,Imp1Bat,attr2Bat,Imp2Bat,attr3Bat,Imp3Bat,attr4Bat,Imp4Bat,attr5Bat,Imp5Bat,BattingIndexGRD)
      names(GRDBatMetric) <- c("Player","Team","roleGRD","Ground","Position",rank1Bat,"Importance",rank2Bat,"Importance",rank3Bat,"Importance",rank4Bat,"Importance",rank5Bat,"Importance","Batting Index")
      
    }
    if(roleGRD != "Batsman" & roleGRD != "WK-Batsman"){
      bowlGRD <- Bowling_Metrics_GroundWise(bowlfileNameGRD,playerNameGRD,countryGRD,roleGRD,GRD)
      if(exists("finalBowlingMetricsGRD"))
        finalBowlingMetricsGRD<- rbind(finalBowlingMetricsGRD,bowlGRD)
      else
        finalBowlingMetricsGRD <- bowlGRD
      
      #Bowling Index
      GRDBowl <- finalBowlingMetricsGRD
      attr1Bowl<-GRDBowl[,rank1Bowl]
      attr2Bowl<-GRDBowl[,rank2Bowl]
      attr3Bowl<-GRDBowl[,rank3Bowl]
      attr4Bowl<-GRDBowl[,rank4Bowl]
      attr5Bowl<-GRDBowl[,rank5Bowl]
      
      BowlingIndexGRD <- GRDBowl[,rank1Bowl]*Imp1Bowl+GRDBowl[,rank2Bowl]*Imp2Bowl+GRDBowl[,rank3Bowl]*Imp3Bowl+GRDBowl[,rank4Bowl]*Imp4Bowl+GRDBowl[,rank5Bowl]*Imp5Bowl  
      GRDBowlMetric=data.frame(GRDBowl[,1:5],attr1Bowl,Imp1Bowl,attr2Bowl,Imp2Bowl,attr3Bowl,Imp3Bowl,attr4Bowl,Imp4Bowl,attr5Bowl,Imp5Bowl,BowlingIndexGRD)
      names(GRDBowlMetric) <- c("Player","Team","roleGRD","Ground","Position",rank1Bowl,"Importance",rank2Bowl,"Importance",rank3Bowl,"Importance",rank4Bowl,"Importance",rank5Bowl,"Importance","Bowling Index")
      
    }
    
  }
 
GRDBatMetric
GRDBowlMetric



# Creating new data frames for calculating indexes against Opposition
for(i in 1:rows){
  batfileNameoppo <- paste0(ids[i,1],"_career_bat.csv")
  bowlfileNameoppo <- paste0(ids[i,1],"_career_bowl.csv")
  
  playerNameoppo <- ids[i,1]
  countryoppo <- ids[i,3]
  roleoppo <- ids[i,4]
  oppo <- "Pakistan"
  
  if(roleoppo != "Bowler"){
    batoppo <- Batting_Metrics_Opposition(batfileNameoppo,playerNameoppo,countryoppo,roleoppo,oppo)
    if(exists("finalBattingMetricsoppo"))
      finalBattingMetricsoppo<- rbind(finalBattingMetricsoppo,batoppo)
    else
      finalBattingMetricsoppo <- batoppo
    
    #Batting Index
    oppoBat <- finalBattingMetricsoppo
    attr1Bat<-oppoBat[,rank1Bat]
    attr2Bat<-oppoBat[,rank2Bat]
    attr3Bat<-oppoBat[,rank3Bat]
    attr4Bat<-oppoBat[,rank4Bat]
    attr5Bat<-oppoBat[,rank5Bat]
    
    BattingIndexoppo <- oppoBat[,rank1Bat]*Imp1Bat+oppoBat[,rank2Bat]*Imp2Bat+oppoBat[,rank3Bat]*Imp3Bat+oppoBat[,rank4Bat]*Imp4Bat+oppoBat[,rank5Bat]*Imp5Bat  
    oppoBatMetric=data.frame(oppoBat[,1:5],attr1Bat,Imp1Bat,attr2Bat,Imp2Bat,attr3Bat,Imp3Bat,attr4Bat,Imp4Bat,attr5Bat,Imp5Bat,BattingIndexoppo)
    names(oppoBatMetric) <- c("Player","Team","roleoppo","Opposition","Position",rank1Bat,"Importance",rank2Bat,"Importance",rank3Bat,"Importance",rank4Bat,"Importance",rank5Bat,"Importance","Batting Index")
    
  }
  if(roleoppo != "Batsman" & roleoppo != "WK-Batsman"){
    bowloppo <- Bowling_Metrics_Opposition(bowlfileNameoppo,playerNameoppo,countryoppo,roleoppo,oppo)
    if(exists("finalBowlingMetricsoppo"))
      finalBowlingMetricsoppo<- rbind(finalBowlingMetricsoppo,bowloppo)
    else
      finalBowlingMetricsoppo <- bowloppo
    
    #Bowling Index
    oppoBowl <- finalBowlingMetricsoppo
    attr1Bowl<-oppoBowl[,rank1Bowl]
    attr2Bowl<-oppoBowl[,rank2Bowl]
    attr3Bowl<-oppoBowl[,rank3Bowl]
    attr4Bowl<-oppoBowl[,rank4Bowl]
    attr5Bowl<-oppoBowl[,rank5Bowl]
    
    BowlingIndexoppo <- oppoBowl[,rank1Bowl]*Imp1Bowl+oppoBowl[,rank2Bowl]*Imp2Bowl+oppoBowl[,rank3Bowl]*Imp3Bowl+oppoBowl[,rank4Bowl]*Imp4Bowl+oppoBowl[,rank5Bowl]*Imp5Bowl  
    oppoBowlMetric=data.frame(oppoBowl[,1:5],attr1Bowl,Imp1Bowl,attr2Bowl,Imp2Bowl,attr3Bowl,Imp3Bowl,attr4Bowl,Imp4Bowl,attr5Bowl,Imp5Bowl,BowlingIndexoppo)
    names(oppoBowlMetric) <- c("Player","Team","roleoppo","Opposition","Position",rank1Bowl,"Importance",rank2Bowl,"Importance",rank3Bowl,"Importance",rank4Bowl,"Importance",rank5Bowl,"Importance","Bowling Index")
    
  }
  
}


oppoBatMetric
oppoBowlMetric


# Creating data frames for calculating indexes based on Current Form

setwd(dir="C:\\Users\\SIDDHESH\\Desktop\\Data Science - Aegis\\Capstone project\\Code\\Team Code\\PROJECT\\PROJECT\\DATA\\CurrentFormData\\")
for(i in 1:rows){
  batfileNameCurrent <- paste0(ids[i,1],"_currentForm_bat.csv")
  bowlfileNameCurrent <- paste0(ids[i,1],"_currentForm_bowl.csv")
  
  playerNameCurrent <- ids[i,1]
  countryCurrent <- ids[i,3]
  roleCurrent <- ids[i,4]
  
  if(roleCurrent != "Bowler"){
    batCurrent <- Batting_Metrics(batfileNameCurrent,playerNameCurrent,countryCurrent,roleCurrent)
    if(exists("finalBattingMetricsCurrent"))
      finalBattingMetricsCurrent<- rbind(finalBattingMetricsCurrent,batCurrent)
    else
      finalBattingMetricsCurrent <- batCurrent
    
    #Batting Index
    CurrentFormBat <- finalBattingMetricsCurrent
    attr1Bat<-CurrentFormBat[,rank1Bat]
    attr2Bat<-CurrentFormBat[,rank2Bat]
    attr3Bat<-CurrentFormBat[,rank3Bat]
    attr4Bat<-CurrentFormBat[,rank4Bat]
    attr5Bat<-CurrentFormBat[,rank5Bat]
    
    BattingIndex <- CurrentFormBat[,rank1Bat]*Imp1Bat+CurrentFormBat[,rank2Bat]*Imp2Bat+CurrentFormBat[,rank3Bat]*Imp3Bat+CurrentFormBat[,rank4Bat]*Imp4Bat+CurrentFormBat[,rank5Bat]*Imp5Bat  
    CurrentFormBatMetric=data.frame(CurrentFormBat[,1:4],attr1Bat,Imp1Bat,attr2Bat,Imp2Bat,attr3Bat,Imp3Bat,attr4Bat,Imp4Bat,attr5Bat,Imp5Bat,BattingIndex)
    names(CurrentFormBatMetric) <- c("Player","Team","roleCurrent","PositionBat",rank1Bat,"Importance",rank2Bat,"Importance",rank3Bat,"Importance",rank4Bat,"Importance",rank5Bat,"Importance","Batting Index")
    
  }
  if(roleCurrent != "Batsman" & roleCurrent != "WK-Batsman"){
    bowlCurrent <- Bowling_Metrics(bowlfileNameCurrent,playerNameCurrent,countryCurrent,roleCurrent)
    if(exists("finalBowlingMetricsCurrent"))
      finalBowlingMetricsCurrent<- rbind(finalBowlingMetricsCurrent,bowlCurrent)
    else
      finalBowlingMetricsCurrent <- bowlCurrent
 
    #Bowling Index
    CurrentFormBowl <- finalBowlingMetricsCurrent
    attr1Bowl<-CurrentFormBowl[,rank1Bowl]
    attr2Bowl<-CurrentFormBowl[,rank2Bowl]
    attr3Bowl<-CurrentFormBowl[,rank3Bowl]
    attr4Bowl<-CurrentFormBowl[,rank4Bowl]
    attr5Bowl<-CurrentFormBowl[,rank5Bowl]
    
    BowlingIndex <- CurrentFormBowl[,rank1Bowl]*Imp1Bowl+CurrentFormBowl[,rank2Bowl]*Imp2Bowl+CurrentFormBowl[,rank3Bowl]*Imp3Bowl+CurrentFormBowl[,rank4Bowl]*Imp4Bowl+CurrentFormBowl[,rank5Bowl]*Imp5Bowl  
    CurrentFormBowlMetric=data.frame(CurrentFormBowl[,1:4],attr1Bowl,Imp1Bowl,attr2Bowl,Imp2Bowl,attr3Bowl,Imp3Bowl,attr4Bowl,Imp4Bowl,attr5Bowl,Imp5Bowl,BowlingIndex)
    names(CurrentFormBowlMetric) <- c("Player","Team","roleCurrent","Position",rank1Bowl,"Importance",rank2Bowl,"Importance",rank3Bowl,"Importance",rank4Bowl,"Importance",rank5Bowl,"Importance","Bowling Index")
    
   
    }
  
}

CurrentFormBatMetric
CurrentFormBowlMetric



#Combining three Indexes

COMBINEDBATMAT <- cbind(GRDBatMetric[,1:3],GRDBatMetric[,5],GRDBatMetric[,16],oppoBatMetric[,16],CurrentFormBatMetric[,15])
names(COMBINEDBATMAT) <- c("Player","Team","Role", "Position","Ground Index", "Opposition Index","Form Index")
COMBINEDBATMAT

#COMBINEDBATMAT <- cbind(GRDBatMetric[,1:3],GRDBatMetric[,5],GRDBatMetric[,16],oppoBatMetric[,16],CurrentFormBatMetric[,15])
#names(COMBINEDBATMAT) <- c("Player","Team","Role", "Position","Ground Index", "Opposition Index","Form Index")
#COMBINEDBATMAT

COMBINEDBOWLMAT <- cbind(GRDBowlMetric[,1:3],GRDBowlMetric[,5],GRDBowlMetric[,16],oppoBowlMetric[,16],CurrentFormBowlMetric[,15])
names(COMBINEDBOWLMAT) <- c("Player","Team","Role", "Position","Ground Index", "Opposition Index","Form Index")
COMBINEDBOWLMAT



#Pure Batsman
Purebatsman <- COMBINEDBATMAT[COMBINEDBATMAT$Role=="Batsman",]
Purebatsman

#WK Batsman
WKbatsman <- COMBINEDBATMAT[COMBINEDBATMAT$Role=="WK-Batsman",]
WKbatsman

#Pure Bowler
Purebowler <- COMBINEDBOWLMAT[COMBINEDBOWLMAT$Role=="Bowler",]
Purebowler

#Bowling Allrounder 
AllroundBAT <- COMBINEDBATMAT[COMBINEDBATMAT$Role=="Bowling Allrounder",]
AllroundBAT
AllroundBOWL <- COMBINEDBOWLMAT[COMBINEDBOWLMAT$Role=="Bowling Allrounder",]
AllroundBOWL

Allround <- rbind(AllroundBAT,AllroundBOWL)
Allround

#Final
Finaldf<-rbind(Purebatsman,WKbatsman,Allround,Purebowler)





#filter on data
#Data$Start.Date <- as.Date(Data$Start.Date, "%d %b %Y")
#Data <- Data[(Data$Start.Date> "2016-01-01" & Data$Start.Date < "2017-09-30"),]


#Function To Pick Playing 11
#Considering the final df as
# c("Player","Team","Role", "Position","Ground Index", "Opposition Index","Form Index")
PickTeam<- function(PlayerIndexData,NumberOfbowler){
  
  
  rows <-  nrow(PlayerIndexData)
  for(i in 1:rows){
    
    currentRow <- PlayerIndexData[i,]
    
    if((currentRow$`Ground Index` == "NaN" & currentRow$`Opposition Index` == "NaN") || (currentRow$`Ground Index` == "Inf" & currentRow$`Opposition Index` == "Inf")){
      currentRow$`Ground Index` <- currentRow$`Form Index`
      currentRow$`Opposition Index` <- currentRow$`Form Index`
      
    }else{
      
      if(currentRow$`Ground Index` == "NaN" || currentRow$`Ground Index` == "Inf"){
        currentRow$`Ground Index` <- (currentRow$`Form Index` + currentRow$`Opposition Index`)/2
        
      }
      if(currentRow$`Opposition Index` == "NaN" || currentRow$`Opposition Index` == "Inf"){
        
        currentRow$`Opposition Index` <- (currentRow$`Form Index` + currentRow$`Ground Index`)/2
      }
    }
    PlayerIndexData[i,] <- currentRow
  }
  
  
  
  
  #Adding a column for final Index
  PlayerIndexData <- within(PlayerIndexData, FinalIndex <- (`Ground Index` + `Form Index` +`Opposition Index`))
  PlayerIndexData <- within(PlayerIndexData,FinalIndex <- FinalIndex/3)
  #Select Captain
  Captain <- PlayerIndexData[grep("(c)",PlayerIndexData$Player),]
  
  #Select WK
  #WK Batsman
  WKbatsman <- PlayerIndexData[PlayerIndexData$Role=="WK-Batsman",]
  WKbatsmanORD <- WKbatsman[order(-WKbatsman$FinalIndex),]
  wicketK <- WKbatsmanORD[1,]
  PlayerIndexData <- PlayerIndexData[PlayerIndexData$Player != Captain$Player,]
  PlayerIndexData <- PlayerIndexData[PlayerIndexData$Player != wicketK$Player,]
  if(Captain$Player == wicketK$Player){
    team <- Captain
    remainingteamcount = 10
    
  }else{
    team <- rbind(Captain,wicketK)
    remainingteamcount = 9
    
  }
  
  batsmenCount <- remainingteamcount - NumberOfbowler
  
  batsmenOptions <- PlayerIndexData[PlayerIndexData$Role != "Bowler",]
  bowlingOptions <- PlayerIndexData[PlayerIndexData$Role != "Batsman" & PlayerIndexData$Role != "WK-Batsman" ,]
  
  batsmenOptions <- batsmenOptions[order(-batsmenOptions$FinalIndex),]
  bowlingOptions <- bowlingOptions[order(-bowlingOptions$FinalIndex),]
  
  bowlers <- bowlingOptions[1:NumberOfbowler,]
  batsmen <- batsmenOptions[1:batsmenCount,]
  
  team <- rbind(team,bowlers,batsmen)
  
  ## Code To remove if any Duplicate and Add another Player
  #team[duplicated(team$Name)]
  
  team <- team[!duplicated(team$Player),]
  teamCount <- nrow(team)
  if(teamCount < 11){
    remainingPlayers <- 11 - teamCount
    RemainingOptions <- batsmenOptions[!(batsmenOptions$Player %in% team$Player), ]
    RemainingOptions <- RemainingOptions[order(-RemainingOptions$FinalIndex),]
    team <- rbind(team,RemainingOptions[1:remainingPlayers,])
  }
  
  teamCount <- nrow(team)
  if(teamCount < 11){
    remainingPlayers <- 11 - teamCount
    RemainingOptions <- bowlingOptions[!(bowlingOptions$Player %in% team$Player), ]
    RemainingOptions <- RemainingOptions[order(-bowlingOptions$FinalIndex),]
    team <- rbind(team,RemainingOptions[1:remainingPlayers,])
  }
  
  for(i in 1:nrow(team))
  {
    poss <- finalBattingPos[finalBattingPos$playerN==team[[1]][i],2]
    
    if(exists("finalPoss"))
      finalPoss <- rbind(finalPoss,poss)
    else
      finalPoss <- poss
    
  }
  
  
  arr1=team[,1:3]
  Possition=finalPoss
  arr3=team[,5:8]
  
  FinalTeam <- data.frame(arr1,Possition,arr3)
  FinalTeam[order(FinalTeam$Possition),]
  
  
  }


PickTeam(Finaldf,3)



