#Color

popularColor <- function(dataset)
{
  df <- as.data.frame(table(dataset$Color))
  df <- df[which(df$Freq >= floor(mean(df$Freq))),]
  dataset$pop_color <- ifelse(dataset$Color %in% df$Var1,1,0)
  dataset
}

popularColorByYear <- function(dataset)
{
  lev <- levels(as.factor(dataset$year))
  for(i in 1:length(lev))
  {
    filtered_ds <- dataset[which(dataset$year == lev[i]),]
    df <- as.data.frame(table(filtered_ds$Color))
    df <- df[which(df$Freq >= ceiling(mean(df$Freq))),]
    att_name <- paste("pop_col_by_year_", as.character(lev[i]),sep="")
    col_names <- c(colnames(dataset),att_name)
    dataset$pc <- ifelse(dataset$Color %in% df$Var1,1,0)
    names(dataset) <- col_names
  }
  dataset
}

colorLevels <- function(color_value)
{
  if (grepl("/", color_value, ignore.case = TRUE))
  {
    return ("pattern")
  }
  else
  {
    return (color_value)
  }
}
colorLevelsNumeric <- function(breed_value)
{
  if (grepl("pattern", breed_value, ignore.case = TRUE))
  {
    return (1)
  }
  else
  {
    return (0)
  }
}

reduceColorLevels <- function(dataset)
{
  dataset$is_pattern <- sapply(as.character(dataset$Color), FUN=colorLevels)
  dataset$is_pattern_numeric <- sapply(as.character(dataset$is_pattern), FUN=colorLevelsNumeric)
  dataset
}


convertColorToFeatures <- function(dataset)
{
  unique_color_train <- c(unique(dataset$is_pattern))
  for (i in unique_color_train)
  {
    dataset[i] <- as.numeric(grepl(i, as.character(dataset$is_pattern)))
  }
  colnames(dataset) <- gsub(" ", "_", colnames(dataset))
  dataset
}