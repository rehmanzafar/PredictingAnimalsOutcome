
#Process animal type

processAnimalType <- function(dataset)
{
  lev <- levels(as.factor(dataset$AnimalType))
  for(i in 1:length(lev))
  {
    dataset$Animal_Type[which(dataset$AnimalType == lev[i])] <- i
  }
  dataset
}

animalTypeMapping <- function(anim_type)
{
  if (grepl("cat", anim_type, ignore.case = TRUE))
  {
    return (1)
  } 
  else if (grepl("dog", anim_type, ignore.case = TRUE))
  {
    return (2)
  }
  else
  {
    return (0)
  }
}

convertAnimalType <- function(dataset)
{
  dataset$anim_type <- sapply(as.character(dataset$AnimalType), FUN=animalTypeMapping)
  #dataset$AnimalType <- lapply(dataset$AnimalType, FUN=animalTypeMapping)
  dataset
}


