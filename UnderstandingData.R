
#Understanding the data

library(rCharts)
n1 <- nPlot(Freq ~ Var2, group = "Var1", data = data.frame(table(training_data$OutcomeType, 
                  training_data$year)), type = "multiBarChart") 
n1$chart(stacked = TRUE)
n1$set(title = "Distribution of outcome by year")
n1

n2 <- nPlot(Freq ~ Var2, group = "Var1", data = data.frame(table(df_breeds$Freq, 
                                                                 df_breeds$Var1)), 
            type = "bar") 
n2

library(caret)
near_zero_variance <- nearZeroVar(training_data[,-4], saveMetrics = T)
near_zero_variance

library(Amelia)
missmap(training_data, col = c("wheat","darkred"))

ageMapping <- function(value)
{
  if (value > 0 && value <= 28) #4 weeks
  {
    return ("Baby") 
  } 
  else if (value > 28 && value <= 70) #10 weeks
  {
    return ("Toddlerhood") 
  }
  else if (value > 70 && value <= 182) #6 months
  {
    return ("Pre-Teens") 
  }
  else if (value > 182 && value <= 1460) #4 Years
  {
    return ("Teenage")
  }
  else if(value > 1460 && value < 3650) #10 Years
  {
    return ("Adulthood") 
  }
  else if(value > 3650)
  {
    return ("Old") #old
  }
  else
  {
    return ("Baby")
  }
}

training_data$ageParam <- sapply(as.numeric(training_data$AgeInDays), FUN=ageMapping)
n2 <- nPlot(Freq ~ Var2, group = "Var1", data = data.frame(table(training_data$OutcomeType, 
                                                                 training_data$ageParam)), 
            type = "multiBarChart") 
n2$chart(stacked = TRUE)
n2

