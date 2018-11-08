
#Process Breed

popularBreed <- function(dataset)
{
  df <- as.data.frame(table(dataset$Breed))
  df <- df[which(df$Freq >= floor(mean(df$Freq))),]
  dataset$pop_breed <- ifelse(dataset$Breed %in% df$Var1,1,0)
  dataset
}

popularBreedByYear <- function(dataset)
{
  lev <- levels(as.factor(dataset$year))
  for(i in 1:length(lev))
  {
    filtered_ds <- dataset[which(dataset$year == lev[i]),]
    df <- as.data.frame(table(filtered_ds$Breed))
    df <- df[which(df$Freq >= ceiling(mean(df$Freq))),]
    att_name <- paste("pop_breed_by_year_", as.character(lev[i]),sep="")
    col_names <- c(colnames(dataset),att_name)
    dataset$pb <- ifelse(dataset$Breed %in% df$Var1,1,0)
    names(dataset) <- col_names
  }
  dataset
}

