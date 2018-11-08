
SplitAgeOutcomeAndConverToDays <- function(age_outcome)
{
  split_unit <- strsplit(as.character(age_outcome), split=" ")
  if (grepl("year", split_unit[[1]][2]))
  {
    return (as.numeric(split_unit[[1]][1]) * 365)
  } 
  else if (grepl("month", split_unit[[1]][2]))
  { 
    return (as.numeric(split_unit[[1]][1]) * 30)
  } 
  else if (grepl("week", split_unit[[1]][2]))
  {
    return (as.numeric(split_unit[[1]][1]) * 7)
  } 
  else if (grepl("day", split_unit[[1]][2]))
  {
    return (as.numeric(split_unit[[1]][1]) * 1)
  }
  else
  {
    return (as.numeric(split_unit[[1]][1]) * 0)
  }
}

ConvertIntoDaysAndReplace <- function(dataset)
{
  dataset$AgeInDays <- sapply(dataset$AgeuponOutcome, FUN=SplitAgeOutcomeAndConverToDays)
  dataset
}


#http://pets.thenest.com/age-maturity-growth-cats-7699.html
reduceAgeLevels <- function(value)
{
  if (value > 0 && value <= 28) #4 weeks
  {
    return (1) #"Baby"
  } 
  else if (value > 28 && value <= 70) #10 weeks
  {
    return (2) #"toddlerhood"
  }
  else if (value > 70 && value <= 182) #6 months
  {
    return (3) #"Pre-Teens"
  }
  else if (value > 182 && value <= 1460) #4 Years
  {
    return (4) #Teenage
  }
  else if(value > 1460 && value < 3650) #10 Years
  {
    return (5) #Adulthood
  }
  else if(value > 3650)
  {
    return (6) #old
  }
  else
  {
    return (0)
  }
}

processAgeInDaysAttribute <- function(dataset)
{
  dataset$AgeInDays[is.na(dataset$AgeInDays)] <- 0 #change with impute later
  dataset$AgeInDaysLevels <- sapply(as.numeric(dataset$AgeInDays), FUN=reduceAgeLevels)
  dataset
}
