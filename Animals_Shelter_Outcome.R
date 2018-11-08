
rm(list = ls())
options( java.parameters = "-Xmx4g" )

library(randomForest) # classification algorithm
#library(MLmetrics)#For calculating Log Loss
library(lubridate)
#library(caret)

#loading data from CSV files.
source("Functions/ReadFromCsv.R")
training_data <- load_data("/Dataset/train.csv")
test_data <- load_data("/Dataset/test.csv")


#View(training_data)

source("Functions/SplitDatetimeAttribute.R")
source("Functions/SplitAgeOutcomeConvertDays.R")

#Process date attribute
training_data <- split_datetime_attribute(training_data)
test_data <- split_datetime_attribute(test_data)

#Process age attribute
training_data <- ConvertIntoDaysAndReplace(training_data)
training_data <- processAgeInDaysAttribute(training_data)

test_data <- ConvertIntoDaysAndReplace(test_data)
test_data <- processAgeInDaysAttribute(test_data)

#Process color attribute
source("Functions/ProcessColorAttribute.R")
training_data <- reduceColorLevels(training_data)
training_data <- convertColorToFeatures(training_data)

test_data <- reduceColorLevels(test_data)
test_data <- convertColorToFeatures(test_data)

#training_data <- popularColor(training_data)
#training_data <- popularColorByYear(training_data)

#test_data <- popularColor(test_data)
#test_data <- popularColorByYear(test_data)

#Process breed attribute
source("Functions/ProcessBreedAttribute.R")
source("Functions/ReduceBreedLevel.R")
training_data <- reduceMixedBreedLevels(training_data)
training_data <- reduceHairBreedLevels(training_data)
training_data <- reduceDomesticBreedLevels(training_data)
training_data <- convertBreedCatToFeatures(training_data)

test_data <- reduceMixedBreedLevels(test_data)
test_data <- reduceHairBreedLevels(test_data)
test_data <- reduceDomesticBreedLevels(test_data)
test_data <- convertBreedCatToFeatures(test_data)

#training_data <- popularBreed(training_data)
#training_data <- popularBreedByYear(training_data)

#test_data <- popularBreed(test_data)
#test_data <- popularBreedByYear(test_data)

#Process animal type attribute
source("Functions/ProcessAnimalTypeAttribute.R")
training_data <- convertAnimalType(training_data)
test_data <- convertAnimalType(test_data)

#Process name attribute
source("Functions/ProcessNameAttribute.R")
#training_data <- processNameAtrribute(training_data)
training_data <- processNameAtrribute2(training_data)
#training_data <- convertAnimalType(training_data)

test_data <- processNameAtrribute2(test_data)
#test_data <- convertAnimalType(test_data)

#Process sex outcome attribute
source("Functions/ProcessSexOutcomeAttribute.R")
training_data <- convertSexOutcomeType(training_data)
test_data <- convertSexOutcomeType(test_data)

View(training_data)
View(test_data)


########################################################################
gbm_training_data <- training_data
gbm_training_data$AnimalID <- NULL
gbm_training_data$Name <- NULL
gbm_training_data$DateTime <- NULL
gbm_training_data$Breed <- NULL
gbm_training_data$is_pattern <- as.factor(gbm_training_data$is_pattern)
gbm_training_data$is_hybrid <- as.factor(gbm_training_data$is_hybrid)
gbm_training_data$OutcomeSubtype <- NULL


gbm_test_data <- test_data
gbm_test_ID <- gbm_test_data$ID
gbm_test_data$ID <- NULL
gbm_test_data$Breed <- NULL
gbm_test_data$Name <- NULL
gbm_test_data$DateTime <- NULL

gbm1 <- gbm(
  OutcomeType ~ .,
  data=gbm_training_data,
  distribution="multinomial",
  shrinkage=0.05,
  n.trees=500,
  interaction.depth=6L,
  train.fraction=0.8,
  keep.data=FALSE,
  verbose=TRUE
)

testPreds2 <- predict(gbm1,gbm_test_data,type="response")
dim(testPreds2) <- c(nrow(gbm_test_data),5)
colnames(testPreds2) <- levels(gbm_training_data$OutcomeType)

options(scipen=100)

#ss <- data.frame(ID=test$ID)
ss <- cbind(gbm_test_data,testPreds2)

write.csv(ss, "Results/shelter_animals_gbm_mine_preproc.csv", row.names = F)

#######################################################################
#Random Forest
rf_mod <- randomForest(OutcomeType ~ AnimalType+SexuponOutcome+year+month+day+AgeInDaysLevels+is_pattern_numeric+is_hybrid_numeric, 
                       data = rf_data, 
                       ntree = 500, 
                       importance = TRUE)

#rf_data$SexuponOutcome[is.na(rf_data$SexuponOutcome)]   <- "Unknown"

rf_mod
prediction <- predict(rf_mod, rf_data, type = 'vote')
prediction1 <- predict(rf_mod, test_data, type = 'vote')

solution <- data.frame('ID' = test_data$ID, prediction1)
write.csv(solution, "Results/shelter_animals_rf_mine_preproc.csv", row.names = F)
#######################################################################

targets <- training_data$OutcomeType

training_data$Color <- NULL
training_data$Breed <- NULL
training_data$AnimalID <- NULL
training_data$Name <- NULL
training_data$DateTime <- NULL
training_data$OutcomeSubtype  <- NULL
training_data$SexuponOutcome  <- NULL
training_data$AnimalType <- NULL
training_data$OutcomeType <- NULL
training_data$is_pattern <- NULL
training_data$is_hybrid <- NULL
training_data$AgeInDays <- NULL
training_data$is_pattern_numeric <- NULL
training_data$AgeuponOutcome <- NULL

test_ID <- test_data$ID
test_data$ID <- NULL
test_data$Color <- NULL
test_data$Breed <- NULL
test_data$Name <- NULL
test_data$DateTime <- NULL
test_data$SexuponOutcome <- NULL
test_data$AnimalType <- NULL
test_data$is_pattern <- NULL
test_data$is_hybrid <- NULL
test_data$AgeInDays <- NULL
test_data$is_pattern_numeric <- NULL
test_data$AgeuponOutcome <- NULL

View(training_data)
View(test_data)

library(xgboost)
set.seed(121)

#training_data[is.na(training_data)]   <- 0
#test_data[is.na(test_data)]     <- 0

full_train_matrix <- matrix(as.numeric(data.matrix(training_data)),ncol=ncol(training_data))
test_matrix <- matrix(as.numeric(data.matrix(test_data)),ncol=ncol(test_data))

full_targets_train <- as.numeric(targets)-1

# Run xgb on full train set
model_one = xgboost(data=full_train_matrix, 
                         label=full_targets_train, 
                         nrounds=125, 
                         verbose=1, 
                         eta=0.1, 
                         max_depth=6, 
                         subsample=0.75, 
                         colsample_bytree=0.85,
                         objective="multi:softprob", 
                         eval_metric="mlogloss",
                         num_class=5)

model_two = xgboost(data=full_train_matrix, 
                    label=full_targets_train, 
                    nrounds=125, 
                    verbose=1, 
                    eta=0.1, 
                    max_depth=7, 
                    subsample=0.75, 
                    colsample_bytree=0.85,
                    objective="multi:softprob", 
                    eval_metric="mlogloss",
                    num_class=5)

model_three = xgboost(data=full_train_matrix, 
                    label=full_targets_train, 
                    nrounds=125, 
                    verbose=1, 
                    eta=0.1, 
                    max_depth=8, 
                    subsample=0.85, 
                    colsample_bytree=0.75,
                    objective="multi:softprob", 
                    eval_metric="mlogloss",
                    num_class=5)

model_four = xgboost(data=full_train_matrix, 
                      label=full_targets_train, 
                      nrounds=125, 
                      verbose=1, 
                      eta=0.1, 
                      max_depth=9, 
                      subsample=0.55, 
                      colsample_bytree=0.65,
                      objective="multi:softprob", 
                      eval_metric="mlogloss",
                      num_class=5)

model_five = xgboost(data=full_train_matrix, 
                     label=full_targets_train, 
                     nrounds=125, 
                     verbose=1, 
                     eta=0.1, 
                     max_depth=10, 
                     subsample=0.55, 
                     colsample_bytree=0.55,
                     objective="multi:softprob", 
                     eval_metric="mlogloss",
                     num_class=5)

prediction1 <- predict(model_one, test_matrix)
prediction2 <- predict(model_two, test_matrix)
prediction3 <- predict(model_three, test_matrix)
prediction4 <- predict(model_four, test_matrix)
prediction5 <- predict(model_five, test_matrix)

prediction = (prediction1 + prediction2 + prediction3 + prediction4 + prediction5) / 5

test_preds <- predict(prediction, test_matrix)
test_preds_frame <- data.frame(matrix(test_preds, ncol = 5, byrow=TRUE))
colnames(test_preds_frame) <- levels(targets)
submission <- cbind(data.frame(ID=test_ID), test_preds_frame)

write.csv(submission , "Results/shelter_animals_xgboost_three_eta03.csv", row.names=FALSE)


#plots
#source("Graphs/UnderstandingData.R")


#counts <- table(train$OutcomeType, train$year)
#barplot(counts, main="Distribution by Outcome and Year", xlab="Year", ylab = "Outcome Type", legend = rownames(counts))

bst <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
