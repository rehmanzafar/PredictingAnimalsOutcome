#Process sex upon outcome

sexOutcomeMapping <- function(sex_outcome)
{
  if (grepl("Intact Female", sex_outcome, ignore.case = TRUE))
  {
    return (1)
  } 
  else if (grepl("Intact Male", sex_outcome, ignore.case = TRUE))
  {
    return (2)
  }
  else if (grepl("Spayed Female", sex_outcome, ignore.case = TRUE))
  {
    return (3)
  } 
  else if (grepl("Neutered Male", sex_outcome, ignore.case = TRUE))
  {
    return (4)
  }
  else
  {
    return (0)
  }
}

convertSexOutcomeType <- function(dataset)
{
  dataset$Sex_Outcome <- sapply(as.character(dataset$SexuponOutcome), FUN=sexOutcomeMapping)
  dataset
}