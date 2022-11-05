options(prompt='R> ')
options(continue = '\t')
options(scipen=999)

#load the following libraries
library('randomForest')
library('plyr')
library('stringr')
library('tidyverse')
library("magrittr")
library("scatterplot3d")
library("stringr")
library("ggplot2")


#load the data and take a peek to make sure everything is in order
train = read.csv('D:/MSc/DATA 5000/train.csv')
head(train)
test_x = read.csv('D:/MSc/DATA 5000/test.csv')
head(test_x)
test_y = read.csv('D:/MSc/DATA 5000/test_salaries.csv')
head(test_y)

###############################################################
#                        DATA CLEANING                        #
###############################################################

#add train/test column
test_x$TrainTest = "test"
train$TrainTest =  "train"

test = cbind(test_y, test_x)
all_data = rbind(train,test)

#where are there missing values?
all_missing_list=  colnames(all_data)[colSums(is.na(all_data)) > 0]
all_missing_list

#fill the PrSt column with 'INT' for international players
all_data$PrSt = mapvalues(all_data$PrSt, from = "", to="INT")


#################################################################
#        Fix the columns with multiple values per player        #
#################################################################
#(Team, Position, Injuries)

# Make boolean Team columns
# Get the unique list of team acronyms from the all_data$Team column

teams = list()

for(i in 1:nrow(all_data)){
  x = strsplit(all_data$Team[i], "/")
  for(y in x){
    teams = c(teams, y)
  }
}

# Only keep unique teams
teams = unique(teams)

# Add columns with the team names as the header and 0 as values
for(x in teams){
  all_data[x] = 0
}

# Iterate through and record the teams for each player 
# Change each boolean team column to 1 if they play(ed) for it

for(i in 1:nrow(all_data)){
  playerteams = strsplit(all_data$Team[i], "/")
  for(team in playerteams){
    all_data[i,team] = 1
  }
}


#Make Position boolean columns
pos = list()
for( i in 1:nrow(all_data)){
  x = strsplit(all_data$Position[i], "/")
  for(y in x){
    pos = c(pos, y)
  }
}

pos = unique(pos)

# add columns with the pos names as the header and 0 as values
for(x in pos){
  all_data[x] = 0
}

#iterate through and record the position(s) for each player
for(i in 1:nrow(all_data)){
  playerpos = strsplit(all_data$Position[i], "/")
  for(pos in playerpos){
    all_data[i,pos] = 1
  }
}


#Make Injuries boolean columns
inj = list()
for( i in 1:nrow(all_data)){
  all_data$Injuries[i] = str_replace_all(all_data$Injuries[i], fixed(" "), "")
  x = strsplit(all_data$Injuries[i], ",")
  for(y in x){
    inj = c(inj, y)
  }
}

inj = unique(inj)

# add columns with the pos names as the header and 0 as values
for(x in inj){
  all_data[x] = 0
}

#iterate through and record the position(s) for each player
for(i in 1:nrow(all_data)){
  playerinj = strsplit(all_data$Injuries[i], ",")
  for(inj in playerinj){
    all_data[i,inj] = 1
  }
}

# Create a new column to indicate if a player is undrafted
all_data$undrafted = is.na(all_data$DftRd)


#################################################################
#                 Repair the birthday info                     #
#################################################################

# turn the born column into 3 integer columns year:month:date

bday_parts = str_split_fixed(all_data$Born, "-", 3)

all_data$birth_year = (bday_parts[,1])
all_data$birth_month = (bday_parts[,2])
all_data$birth_day = (bday_parts[,3])

##############################################################
#       FILL EMPTY NUMERICAL VALUES WITH MEDIAN VALUE        #
##############################################################

#loop through the dataframe, filling each column with the median of 
#the existing values for the entire dataset
#where are there still missing values?

missinglist =  colnames(all_data)[colSums(is.na(all_data)) > 0]
length(missinglist) == 0

#if above true all values are imputed!

for(i in missinglist){
  median = median(as.numeric(unlist(all_data[i])), na.rm=TRUE)
  all_data[i][is.na(all_data[i])] = median
}

missinglist =  colnames(all_data)[colSums(is.na(all_data)) > 0]
length(missinglist) == 0


###############################################################
#                    GRAPHICAL EXPLORATION                    #
###############################################################

# NHL Players per State/Prov (or Int)
barplot(sort(table(all_data$PrSt),
             decreasing=TRUE), 
        horiz=TRUE, 
        las=1,
        col=c("blue4","blue","skyblue"),
        main="NHL Players by Province / State", ylab="Province / State",xlab="Number of Players")

#Maybe remove Int?


# NHL Players per Country
barplot(sort(table(all_data$Nat),
             decreasing=TRUE), 
        horiz=TRUE, 
        las=1,
        col=c("blue4","blue","skyblue"),
        main="NHL Players by Country", ylab="Country",xlab="Number of Players")


# Distribution of Ages
hist(as.numeric(all_data$birth_year), breaks=28, 
     col="plum2", xlab='Birth Year', 
     main='Distribution of NHL Players by Birth Year (2016/2017 Season)')


# Distribution of Salaries
format(all_data$Salary, scientific =FALSE)
summary(all_data$Salary)
hist(all_data$Salary, breaks=50, 
     col="lightcyan2", xlab='Salary (USD)', 
     ylab = "Number of Players", main='NHL Salary Distribution (2016/2017 Season)',cex.axis=0.75)

# print the median salary
median(all_data$Salary)


# Salary vs. Goals Scored
plot(all_data$G, all_data$Salary, pch=20, xlab='Goals Scored (2016/2017 Season)', ylab='Salary (USD)', main="Salary vs. Goals Scored (2016/2017 Season)")
abline(lm(all_data$Salary ~ all_data$G), col="red1")
text(all_data$G, all_data$Salary, labels=all_data$LastName, cex=0.7, pos = 3)

# Salary vs. Goals Scored
plot(all_data$DftYr, all_data$Salary, pch=20, xlab='Draft Year', ylab='Salary (USD)', main="Salary vs. Draft Year")
#abline(lm(all_data$Salary ~ all_data$DftYr), col="red1")
#text(all_data$DftYr, all_data$Salary, labels=all_data$LastName, cex=0.7, pos = 3)

# Age, Salary & Goals
sd3 = scatterplot3d(all_data$G, 
                    all_data$birth_year, 
                    all_data$Salary,
                    pch=19,type="h",
                    cex.axis=0.5,
                    las=1,
                    lty.hplot=2,
                    color= "indianred2",
                    main="Player Age vs. Goals vs. Salary",
                    zlab="Salary (USD)",
                    xlab="Goals Scored (2016/2017 Season)",
                    ylab="Birth Year",
                    grid=TRUE)
sd3.coords = sd3$xyz.convert(all_data$G,all_data$birth_year,all_data$Salary)
text(sd3.coords$x, sd3.coords$y,labels=all_data$LastName,cex=.5, pos=4)

# Time on Ice vs. Salary
# Salary vs. Goals Scored
plot(all_data$Salary, all_data$TOI, pch=20, xlab='Salary (USD)', ylab='Time on Ice (2016/2017 Season) (mins)', main="Salary vs. Time on Ice (2016/2017 Season)")
abline(lm(all_data$TOI ~ all_data$Salary), col="red1")
#text(all_data$Salary, all_data$TOI, labels=all_data$LastName, cex=0.5, pos = 3)

################################################
#              Prepare Dataframe               #
################################################

names_col=all_data$LastName
all_data = all_data[, !(colnames(all_data) %in% c("LastName","FirstName","Cntry","Nat","Born","Team","City","Position","NHLid"))]
head(all_data)

train_dat = all_data[all_data$TrainTest == "train",]

test_dat = all_data[all_data$TrainTest == "test",]

#we lose anyone?
length(test_dat$TrainTest) + length(train_dat$TrainTest) == length(all_data$TrainTest)

#drop the train/test split columns
train_dat = train_dat[, !(colnames(train_dat) %in% c("TrainTest"))]
test_dat = test_dat[, !(colnames(test_dat) %in% c("TrainTest"))]

y_column = c("Salary")
all_columns = names(train_dat)
predictor_columns = all_columns[all_columns != y_column]

library("randomForestSRC")

train_dat[sapply(train_dat, is.character)] <- lapply(train_dat[sapply(train_dat, is.character)], 
                                       as.factor)

rf_result = rfsrc(Salary~., data = train_dat, ntree = 1000, importance = TRUE)
rf_result

plot(rf_result)


# TOP 3 VARIABLES IN MY RUN WERE:
# DRAFT YEAR
# GOALS AS SLAP SHOTS
# X1G (which should be just 1G) = TIMES THEY SCORED FIRST GOAL OF THE GAME



################################################################################
#             PLOTTING AND FURTHER EXPLORATION (NOT DONE)                      #
################################################################################

#CHANGE TO BE VISUALIZED WITH A COLOUR RAMP
score_3d = scatterplot3d(all_data$DftYr, all_data$GSlap, all_data$Salary,
                         pch=19,
                         type="h",
                         cex.axis=0.5,
                         las=1,
                         lty.hplot=2,
                         color= "black",
                         main="Top Random Forest Predictors for Player Salary",
                         zlab="Salary (USD)",
                         xlab="Draft Year",
                         ylab="Goals Scored with Slap Shots",
                         grid=TRUE)

sd3.coords = sd3$xyz.convert(all_data$DftYr, all_data$GSlap, all_data$Salary)
text(sd3.coords$x, sd3.coords$y, labels=names_col, cex=.5, pos=4)

# rf_train_test_feature_selection = function(train_df, test_df, y_column , predictor_columns){
#   outdf = data.frame(size=numeric(),t_mse=numeric(),lowest_predictor=character())
#   cols_to_use = c(y_column, predictor_columns)
#   rf_input=select(train_df,one_of(cols_to_use))	
#   while(length(names(rf_input)) > 2){
#     #train the model
#     #I can't find the cause of the bug but I have to hard code in salary :/
#     rf_result = randomForest(Salary~., data = rf_input, ntree = 10000, importance = TRUE)
#     test_prediction	= predict(rf_result, test_df[,predictor_columns])	
#     test_mse = mean((test_prediction - test_df[, y_column])^2)
#     
#     #find the worst predictor, add its name to the dataframe and drop column
#     importance_dat = rf_result$importance
#     sorted_predictors = sort(importance_dat[,1], decreasing=TRUE)
#     worst_pred = names(sorted_predictors[length(sorted_predictors)])
#     out_line = data.frame(size=length(sorted_predictors), t_mse=test_mse, lowest_predictor=worst_pred)
#     print(out_line)
#     outdf = rbind(outdf,out_line)
#     rf_input = rf_input[, !colnames(rf_input) %in% worst_pred]
#   }
#   return(outdf)
# }
# 
# #run the iterative dropfunction, performing a rf regression for each feature set
# model_selection = rf_train_test_feature_selection(train_dat, test_dat, y_column , predictor_columns)
# model_selection
# 
# #look at the plot, which number of features gives the best mse on the test data?
# 
# plot(model_selection$size, model_selection$t_mse)
# #improve this plot
# 
# #be sure to not lose the top predictor
# 
# '%!in%' = function(x,y)!('%in%'(x,y))
# 
# last_predictor = all_columns[all_columns %!in% model_selection$lowest_predictor]
# 
# #last predictor standing was XXXXXXXXX
# 
# #this list was determined using the iterative random forest drop above
# all_top_preds= c("nation_DEU","born_DEU","nation_HRV","nation_GBR","born_SVN",
#                  "born_NOR","born_LVA","born_ITA","born_HRV","born_GBR","born_EST",
#                  "nation_DNK","Maj","MIN","TOR","nation_AUT","born_DNK","S.J",
#                  "born_SWE","ARI","BUF","nzFOW","dzFOL","D","FOW.Close","iFOW",
#                  "nation_CAN","iFOW.1","PDO","FOL.Up","FOW.Down","SV.","Post",
#                  "G.Tip","A.60","OTG","Ht","iFOL.1","nzFOL","iFOL","iPenDf",
#                  "X.FOT","S.Wrap","DAP","undrafted","FO.","FOL.Down","SH.",
#                  "S.Bkhd","T.B","Diff.60","iHF","S.Dflct","ozFOL","iHF.1","CBar",
#                  "Pct.","A1","Misc","iPEND","G.Wrst","Pace","E...","DftRd","IPP.",
#                  "Diff","G","F.60","BLK.","OPS","iRB","S.Tip","Wt","G.Snap",
#                  "sDist.1","iPENT","PIM","iPenD","A","GWG","ozFOW","iGVA.1","xGA",
#                  "SA","PEND","S.Slap","Pass","iTKA.1","HA","FA","Pr.St","S.Snap",
#                  "iHA","sDist","iPenT","PENT","S.Wrst","DPS","CA","iTKA","Min",
#                  "RBF","iGVA","PS","Over","GS.G","TKA","Grit","A2","iRS","PTS",
#                  "SCA","iBLK","TOIX","Wide","Ovrl","RSA","iDS","ixG","iBLK.1",
#                  "TOI","iCF.1","DSA","GA","GS","GVA","iSF.1","iCF","Shifts","RBA",
#                  "iSCF","TOI.","iMiss","iFF","OTOI","iSF","iSF.2","TOI.GP.1","GP",
#                  "SCF","TOI.GP","FOW","birth_year","GF","DSF","FOL","HF","DftYr",
#                  "xGF","RSF","CF","FF","SF")
# 
# test_final_input=select(test_dat,one_of("Salary",all_top_preds))
# trian_final_input=select(train_dat,one_of("Salary",all_top_preds))
# 
# #final rf run
# final_rf_result = randomForest(Salary~., data = trian_final_input, ntree = 10000, importance = TRUE)
# final_importance_list  = sort(importance(final_rf_result)[,1], decreasing=TRUE)
# 
# print("Final rf Model:")
# final_rf_result
# 
# print("final feature importance list:")
# final_importance_list
# 
# print("Final test mean squared error:")
# final_test_prediction = predict(rf_result, test_dat[,predictor_columns])	
# final_test_mse = mean((final_test_prediction - test_dat[, y_column])^2)
# final_test_mse