
rm(list = ls())

require(data.table)
require(TunePareto)
require(caret)
require(glmnet)

setwd('/Users/aslierdal/Desktop/etm58D/Project')

testStart=as.Date('2018-08-16')
testStartend=as.Date('2019-01-05')
trainStart=as.Date('2012-07-15')
trainLateStart=as.Date('2015-07-15')

rem_miss_threshold=0.05 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')

# read data
matches_raw = readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
odd_details_raw = readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")
# preprocess matches
matches=matches_data_preprocessing(matches_raw)
# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)
# extract open and close odd type features from multiple bookmakers
features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)

# divide data based on the provided dates

features = features[,lapply(.SD, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  } else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
})]
features = features[complete.cases(features)]

train_features=features[Match_Date>=trainStart & Match_Date<testStart]
test_features=features[Match_Date>=testStart & Match_Date<testStartend]

not_included_feature_indices=c(1:5)

train_x=train_features[,-not_included_feature_indices,with=F]
train_y=train_features$Match_Result

test_x=test_features[,-not_included_feature_indices,with=F]
test_y=test_features$Match_Result

Match_Result=as.matrix(test_y)
outcomes=matrix(0,nrow(Match_Result),3)
for(i in 1:nrow(Match_Result))
{
  if(Match_Result[i]=='Home')
  {
    outcomes[i,1]=1
  }
  if(Match_Result[i]=='Tie')
  {
    outcomes[i,2]=1
  }
  if(Match_Result[i]=='Away')
  {
    outcomes[i,3]=1
  }
}

# glmnet --------------------------------------------------------

glm_train_ctrl <- trainControl(method="cv",
                               number=5,
                               returnResamp="all",
                               classProbs=TRUE,
                               summaryFunction=multiClassSummary)
set.seed(100)
glm_model <- train(train_x,
                   train_y,
                   method="glmnet",
                   trControl=glm_train_ctrl,
                   tuneGrid=expand.grid(alpha=1,
                                        lambda=seq(0.001,0.1,by = 0.001)))

glm_predicted_prob=predict(glm_model, newdata = test_x, type='prob')

RPS=RPS_matrix(glm_predicted_prob, outcomes)
results=cbind(glm_predicted_prob, Match_Result, RPS)
results
mean(RPS)

# Random Forest --------------------------------------------------

rf_train_ctrl <- trainControl(method="cv",
                              number=5,
                              returnResamp="all",
                              classProbs=TRUE,
                              summaryFunction=multiClassSummary)
set.seed(101)
rf_model <- train(train_x,
                  train_y,
                  method="rf",
                  trControl=rf_train_ctrl)

rf_predicted_prob=predict(rf_model, newdata = test_x, type='prob')

RPS=RPS_matrix(rf_predicted_prob, outcomes)
results=cbind(rf_predicted_prob, Match_Result, RPS)
results
mean(RPS)


# Ordered Logistic or Probit Regression --------------------------

pr_train_ctrl <- trainControl(method="cv",
                              number=5,
                              returnResamp="all",
                              classProbs=TRUE,
                              summaryFunction=multiClassSummary)
set.seed(102)
pr_model <- train(train_x,
                  train_y,
                  method="polr",
                  trControl=rf_train_ctrl)

pr_predicted_prob=predict(pr_model, newdata = test_x, type='prob')

RPS=RPS_matrix(pr_predicted_prob, outcomes)
results=cbind(pr_predicted_prob, Match_Result, RPS)
results
mean(RPS)


