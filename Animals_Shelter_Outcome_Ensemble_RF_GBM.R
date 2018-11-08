
rm(list = ls())
options( java.parameters = "-Xmx4g" )

library(randomForest) #classification algorithm
library(MLmetrics) #For calculating Log Loss
library(lubridate) # For datetime
library(caret) #For machine learning algorithms
library(gbm)
library(e1071)

source("Functions/ReadFromCsv.R")
source("Functions/PreProcessData.R")
source("Functions/SplitDatetimeAttribute.R")
source("Functions/SplitAgeOutcomeConvertDays.R")
source("Functions/ProcessColorAttribute.R")
source("Functions/ProcessBreedAttribute.R")
source("Functions/ReduceBreedLevel.R")
source("Functions/ProcessAnimalTypeAttribute.R")
source("Functions/ProcessNameAttribute.R")
source("Functions/ProcessSexOutcomeAttribute.R")

#loading data from CSV files.
training_data <- load_data("/Dataset/train.csv")
test_data <- load_data("/Dataset/test.csv")

training_data <- preProcessData(training_data)
test_data <- preProcessData(test_data)

#Reducing Breed and Color levels with top 49+1 other
df_breeds <- as.data.frame(table(training_data$Breed))
df_breeds <- df_breeds[order(df_breeds$Freq, decreasing = TRUE),]
top_50_breed <- head(df_breeds$Var1, 49)

df_colors <- as.data.frame(table(training_data$is_pattern))
df_colors <- df_colors[order(df_colors$Freq, decreasing = TRUE),]
top_50_color <- head(df_colors$Var1, 49)

training_data <- reduceToPopularColors(training_data, top_50_color, df_colors)
training_data <- reduceToPopularBreed(training_data,top_50_breed, df_breeds)

test_data <- reduceToPopularColors(test_data, top_50_color, df_colors)
test_data <- reduceToPopularBreed(test_data,top_50_breed, df_breeds)

#View(training_data)
#View(test_data)
#summary(training_data)

#Evaluating onn test dataset
#inTrain <- createDataPartition(training_data$OutcomeType, p = 0.8, list = FALSE)
#training_data <- training_data[inTrain,]
#test_data <- training_data[-inTrain,]
#training_data$SexuponOutcome[is.na(training_data$SexuponOutcome)] <- "Unknown"
#training_data$is_hybrid[is.na(training_data$is_hybrid)] <- "Unknown"

########################################################################
set.seed(1000)
gbm_model <- gbm(OutcomeType ~ AnimalType+year+month+day+hour+AgeInDaysLevels
                 +is_pattern_numeric+is_hybrid_numeric+type_hair+is_breed_domestic+isName
                 +length_name_cat+is_surgical_sterilization+sex_numeric+reduced_colors_numeric+reduced_breed_numeric,
                 data=training_data,
                 distribution="multinomial",
                 shrinkage=0.1,
                 n.trees=500,
                 interaction.depth=10L,
                 train.fraction=0.9,
                 keep.data=FALSE,
                 verbose=TRUE
)



gbm_prediction <- predict(gbm_model, test_data, type="response")

#check error rate on splitted data
#MultiLogLoss(as.matrix(as.data.frame(gbm_prediction)), test_data$OutcomeType)

solution <- data.frame('ID' = test_data$ID, gbm_prediction)
write.csv(solution, "Results/shelter_animals_gbm_iter_1.csv", row.names = F)

#######################################################################
#Random Forest
set.seed(1000)
rf_model <- randomForest(OutcomeType ~ AnimalType+year+month+day+hour+AgeInDaysLevels
                         +is_pattern_numeric+is_hybrid_numeric+type_hair+is_breed_domestic+isName
                         +length_name_cat+is_surgical_sterilization+sex_numeric+reduced_colors_numeric+reduced_breed_numeric, 
                         data = training_data, 
                         ntree = 2000, 
                         importance = TRUE)

rf_prediction <- predict(rf_model, test_data, type = 'vote')

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

prediction2 <- predict(test_class_cv_form, test_data, type = 'vote')

solution <- data.frame('ID' = test_data$ID, prediction2)
write.csv(solution, "Results/shelter_animals_svm_mine_preproc2.csv", row.names = F)
#######################################################################

#check error rate on splitted data
#MultiLogLoss(rf_prediction, test_data$OutcomeType)

solution <- data.frame('ID' = test_data$ID, rf_prediction)
write.csv(solution, "Results/shelter_animals_rf_iter_1.csv", row.names = F)
#######################################################################
#Ensembeling

pred = (as.data.frame(gbm_prediction)+as.data.frame(rf_prediction))/2
solution <- data.frame('ID' = test_data$ID, pred)
colnames(solution) <- c("ID", "Adoption", "Died", "Euthanasia", "Return_to_owner", "Transfer")
write.csv(solution, "Results/shelter_animals_ensble_iter_08_01_RF_GBM.csv", row.names = F)

save.image(file = "SavedModel/Animals_Shelter_Outcome_Ensemble_RF_GBM.RData")

saveRDS(gbm_model, file = "SavedModel/gbm_model.rds")
gbm_model <- readRDS("SavedModel/gbm_model.rds")

saveRDS(rf_model, file = "SavedModel/rf_model.rds")
rf_model <- readRDS("SavedModel/rf_model.rds")


dat <- data.frame(
  Algorithm = factor(c("Random Forest","Gradient Boosting Machine", "Support Vector Machine", "Ensemble"), 
                levels=c("Random Forest","Gradient Boosting Machine", "Support Vector Machine", "Ensemble")),
  MultiLogLoss = c(0.80026, 0.77396, 0.87825, 0.75603)
)

m <- ggplot(data=dat, aes(x=Algorithm, y=MultiLogLoss, fill=Algorithm)) + geom_bar(stat="identity") 
m+ ylim(0,1) +geom_text(aes(label=MultiLogLoss), vjust=1.6, color="white", size=3.5)

#Score on kaggle = 0.75603
#MultiLogLoss(as.matrix(pred1), test_data$OutcomeType)

