
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
#training_data <- convertColorToFeatures(training_data)

test_data <- reduceColorLevels(test_data)
#test_data <- convertColorToFeatures(test_data)

#training_data <- popularColor(training_data)
#training_data <- popularColorByYear(training_data)

#test_data <- popularColor(test_data)
#test_data <- popularColorByYear(test_data)

#Process breed attribute
source("Functions/ProcessBreedAttribute.R")
source("Functions/ReduceBreedLevel.R")
training_data <- reduceMixedBreedLevels2(training_data)
training_data <- reduceHairBreedLevels(training_data)
training_data <- reduceDomesticBreedLevels(training_data)
#training_data <- convertBreedCatToFeatures(training_data)

test_data <- reduceMixedBreedLevels2(test_data)
test_data <- reduceHairBreedLevels(test_data)
test_data <- reduceDomesticBreedLevels(test_data)
#test_data <- convertBreedCatToFeatures(test_data)

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

#View(training_data)
#View(test_data)

#summary(training_data)
#training_data$SexuponOutcome[is.na(training_data$SexuponOutcome)] <- "Unknown"
#training_data$is_hybrid[is.na(training_data$is_hybrid)] <- "Unknown"

########################################################################
#Gradient Boosting Model
gbm_model <- gbm(
  OutcomeType ~ AnimalType+year+month+day+hour+AgeInDaysLevels
  +is_pattern_numeric+is_hybrid_numeric+type_hair+is_breed_domestic+isName+
    length_name_cat+Sex_Outcome,
  data=training_data,
  distribution="multinomial",
  shrinkage=0.05,
  n.trees=500,
  interaction.depth=6L,
  train.fraction=0.8,
  keep.data=FALSE,
  verbose=TRUE
)

gbm_prediction <- predict(gbm_model,test_data,type="response")
solution <- data.frame('ID' = test_data$ID, gbm_prediction)
write.csv(solution, "Results/shelter_animals_gbm_mine_preproc.csv", row.names = F)

#######################################################################
#Random Forest

#rf_mod <- randomForest(OutcomeType ~ AnimalType+year+month+day+hour+AgeInDaysLevels
#                       +is_pattern_numeric+is_hybrid_numeric+type_hair+is_breed_domestic+isName+length_name+length_name_cat+Sex_Outcome, 
#                       data = training_data, 
#                       ntree = 500, 
#                       importance = TRUE)

rf_model <- randomForest(OutcomeType ~ AnimalType+year+month+day+hour+AgeInDaysLevels
                       +is_pattern_numeric+is_hybrid_numeric+type_hair+is_breed_domestic+isName+
                         length_name_cat+Sex_Outcome, 
                       data = training_data, 
                       ntree = 500, 
                       importance = TRUE)
#rf_data$SexuponOutcome[is.na(rf_data$SexuponOutcome)]   <- "Unknown"
#rf_mod
#prediction <- predict(rf_model, training_data, type = 'vote')
rf_prediction <- predict(rf_model, test_data, type = 'vote')

solution <- data.frame('ID' = test_data$ID, rf_prediction)
write.csv(solution, "Results/shelter_animals_rf_mine_preproc2.csv", row.names = F)
#######################################################################
library(caret)
set.seed(849)
cctrl1 <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 10, 
                       classProbs = TRUE)
test_class_cv_form <- train(OutcomeType ~ AnimalType+year+month+day+hour+AgeInDaysLevels
                            +is_pattern_numeric+is_hybrid_numeric+type_hair+is_breed_domestic+isName+length_name+length_name_cat+Sex_Outcome, 
                            data = training_data, 
                            method = "svmRadial",
                            tuneGrid = data.frame(.C = c(.25, .5, 1),.sigma = .05), 
                            trControl = cctrl1,
                            preProc = c("center", "scale"))

prediction2 <- predict(test_class_cv_form, test_data, type = 'response')

solution <- data.frame('ID' = test_data$ID, prediction2)
write.csv(solution, "Results/shelter_animals_svm_mine_preproc2.csv", row.names = F)

library(e1071)
svm_model <- svm(OutcomeType ~ AnimalType+year+month+day+hour+AgeInDaysLevels
                 +is_pattern_numeric+is_hybrid_numeric+type_hair+is_breed_domestic+isName+
                   length_name_cat+Sex_Outcome,
                 training_data,type="C-classification", probability = TRUE)

svm_prediction <- predict(svm_model, test_data, decision.values = TRUE, probability = TRUE)
solution <- data.frame('ID' = test_data$ID, attr(svm_prediction, "probabilities"))
write.csv(solution, "Results/shelter_animals_svm_mine_preproc.csv", row.names = F)
#######################################################################


#Brown, chocolate, liver,Red, Gold, yellow, Cream, Fawn, Black, Blue, Grey, White



