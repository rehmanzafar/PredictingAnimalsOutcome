breedLevels <- function(breed_value)
{
  if (grepl("mix", breed_value, ignore.case = TRUE))
  {
    return ("mix")
  } 
  else if (grepl("/", breed_value, ignore.case = TRUE))
  {
    return ("mix")
  }
  else
  {
    return (breed_value)
  }
}
breedLevels2 <- function(breed_value)
{
  if (grepl("mix", breed_value, ignore.case = TRUE))
  {
    return ("mix")
  } 
  else if (grepl("/", breed_value, ignore.case = TRUE))
  {
    return ("hybrid")
  }
  else
  {
    return ("pure")
  }
}
breedLevelsNumeric <- function(breed_value)
{
  if (grepl("mix", breed_value, ignore.case = TRUE))
  {
    return (1)
  }
  else if (grepl("/", breed_value, ignore.case = TRUE))
  {
    return (2)
  }
  else
  {
    return (0)
  }
}

breedLevelsNumeric2 <- function(breed_value)
{
  if (grepl("mix", breed_value, ignore.case = TRUE))
  {
    return (1)
  }
  else if (grepl("hybrid", breed_value, ignore.case = TRUE))
  {
    return (2)
  }
  else
  {
    return (0)
  }
}

reduceMixedBreedLevels <- function(dataset)
{
  dataset$is_hybrid <- sapply(as.character(dataset$Breed), FUN=breedLevels)
  dataset$is_hybrid_numeric <- sapply(as.character(dataset$is_hybrid), FUN=breedLevelsNumeric)
  dataset
}
reduceMixedBreedLevels2 <- function(dataset)
{
  dataset$is_hybrid <- sapply(as.character(dataset$Breed), FUN=breedLevels2)
  dataset$is_hybrid_numeric <- sapply(as.character(dataset$is_hybrid), FUN=breedLevelsNumeric2)
  dataset
}

breedHairLevels <- function(breed_value)
{
  if (grepl("shorthair", breed_value, ignore.case = TRUE))
  {
    return (1)
  } 
  else if (grepl("longhair", breed_value, ignore.case = TRUE))
  {
    return (2)
  }
  else
  {
    return (0)
  }
}

reduceHairBreedLevels <- function(dataset)
{
  dataset$type_hair <- sapply(as.character(dataset$Breed), FUN=breedHairLevels)
  dataset
}

breedDomesticLevels <- function(breed_value)
{
  if (grepl("domestic", breed_value, ignore.case = TRUE))
  {
    return (1)
  }
  else
  {
    return (0)
  }
}

reduceDomesticBreedLevels <- function(dataset)
{
  dataset$is_breed_domestic <- sapply(as.character(dataset$Breed), FUN=breedDomesticLevels)
  dataset
}

convertBreedCatToFeatures <- function(dataset)
{
  unique_breeds_train <- c(unique(dataset$is_hybrid))
  for (i in unique_breeds_train)
  {
    dataset[i] <- as.numeric(grepl(i, as.character(dataset$is_hybrid)))
  }
  colnames(dataset) <- gsub(" ", "_", colnames(dataset))
  dataset
}

