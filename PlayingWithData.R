#playing with data
#Popular color
df <- as.data.frame(table(train$Color))
df <- df[which(df$Freq >= floor(mean(df$Freq))),]
abc <- train
abc$pop_color <- ifelse(abc$Color %in% df$Var1,1,0)

#popular color by year
lev <- levels(as.factor(abc$Color))
for(i in 1:length(lev))
{
  s <- abc[which(abc$year == lev[i]),]
  myDf <- as.data.frame(table(s$Color))
  myDf <- myDf[which(myDf$Freq >= ceiling(mean(myDf$Freq))),]
  ss <- paste("col_pop_by_year_", as.character(lev[i]))
  c <- c(colnames(abc),ss)
  abc$pc <- ifelse(abc$Color %in% myDf$Var1,1,0)
  names(abc) <- c
}

#popular breed by year
lev <- levels(as.factor(abc$Breed))
for(i in 1:length(lev))
{
  s <- abc[which(abc$year == lev[i]),]
  myDf <- as.data.frame(table(s$Breed))
  myDf <- myDf[which(myDf$Freq >= ceiling(mean(myDf$Freq))),]
  ss <- paste("breed_pop_by_year_", as.character(lev[i]))
  c <- c(colnames(abc),ss)
  abc$pb <- ifelse(abc$Breed %in% myDf$Var1,1,0)
  names(abc) <- c
}

#c <- c(colnames(abc),ss)


training_data <- as.data.frame(training_data)
rf_mod <- randomForest(OutcomeType ~ ., 
                       data = training_data, 
                       ntree = 500, 
                       importance = TRUE)

rf_mod
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)

importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

varImportance
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

plot(rf_mod)
varImpPlot(rf_mod)

#testing the model on remaining training set and calculating log loss
prediction <- predict(rf_mod, training_data, type = 'vote')
MultiLogLoss(prediction, training_data$OutcomeType)

prediction1 <- predict(rf_mod, test_data, type = 'vote')

test_preds_frame <- data.frame(matrix(prediction1, ncol = 5, byrow=TRUE))
colnames(test_preds_frame) <- levels(training_data$OutcomeType)
submission <- cbind(data.frame(ID=test_ID), test_preds_frame)
write.csv(submission , "Results/shelter_animals_rf_one_t500.csv", row.names=FALSE)


model=train(training_data, training_data$OutcomeType, method = "knn", 
            preProcess = c("center", "scale", "knnImpute"), 
            metric="logLoss", trControl=c(classProbs = TRUE, summaryFunction = multiClassSummary))

prediction2 <- predict(rf_mod, test_data, type = 'vote')

test_preds_frame <- data.frame(matrix(prediction2, ncol = 5, byrow=TRUE))
colnames(test_preds_frame) <- levels(training_data$OutcomeType)
submission <- cbind(data.frame(ID=test_ID), test_preds_frame)
write.csv(submission , "Results/shelter_animals_rf_one_t500.csv", row.names=FALSE)

