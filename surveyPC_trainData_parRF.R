# Select readr library to import .csv file
library("readr")
# Set working directory to find .csv file; change directory to YOUR data location prior to running program.
setwd("/home/scott/Documents/Data")
# Import .csv file with data and name it "surveyPC_train"
surveyPC_train <- read.csv(file="Survey_Key_CR.csv", header = TRUE, sep = ",")
# Analyze survey data with "attributes" function to see the overall structure
attributes(surveyPC_train)
# Utilize the "summary" function to view the statistical min., 1st & 3rd Quartile, median, mean, and Max.
summary(surveyPC_train)
# Utilize the "str" function to view the data types within the .csv file
str(surveyPC_train)
# Change elevel column to data type "ordered"
surveyPC_train$elevel <- as.ordered(surveyPC_train$elevel)
# Change car column to data type "factor"
surveyPC_train$car <- as.factor(surveyPC_train$car)
# Change zipcode column to data type "factor"
surveyPC_train$zipcode <- as.factor(surveyPC_train$zipcode)
# Change brand column to data type factor
surveyPC_train$brand <- as.factor(surveyPC_train$brand)
# Review structure of data to insure data types were properly changed
str(surveyPC_train)
# Import caret library for model creation
library(caret)
# Model testing pipeline
set.seed(998)
# Partition .csv data into a "training set" (75%) and a "testing set" (25%)
inTraining <- createDataPartition(surveyPC_train$brand, p = .75, list = FALSE)
training <- surveyPC_train[inTraining,]
testing <- surveyPC_train[-inTraining,]
# Utilize 10 fold cross validation to ensure highest quality from model performance
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
# Train model with Parellel Random Forest from the caret package
LMFit1 <- train(brand~., data = training, method = "parRF", trControl=fitControl)
#check model training
#enter name of model
LMFit1
#predictor variables
predictors(LMFit1)
#make predictions
testPredLM1 <- predict(LMFit1, testing)
#check model - testing
postResample(testPredLM1, testing$brand)
#plot predicted vs actual
plot(testPredLM1,testing$brand)
