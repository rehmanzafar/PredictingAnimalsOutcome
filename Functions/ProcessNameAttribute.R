#Process name attribute

processNameAtrribute <- function(dataset)
{
  dataset$length_of_anim_name <- sapply(as.character(dataset$Name),nchar)
  dataset$length_of_anim_name[is.na(dataset$length_of_anim_name)] <- 0
  dataset
}

processNameAtrribute2 <- function(dataset)
{
  #df <- as.data.frame(table(dataset$Name, useNA = "always"))
  #df <- df[which(df$Freq > mean(df$Freq)),]
  #df <- df[!rowSums(is.na(df[1])), ]
  #popName <- as.character(df$Var1)
  #dataset$isPopularName <- ifelse(dataset$Name%in%popName,1,0)
  dataset$isName <- ifelse(is.na(dataset$Name),0,1)
  
  dataset$length_name <- sapply(as.character(dataset$Name),nchar)
  dataset$length_name[is.na(dataset$length_name)] <- 0
  
  dataset$length_name_cat <- sapply(as.numeric(dataset$length_name), FUN=reduceNameLengthLevel)
  dataset
}

reduceNameLengthLevel <- function(value)
{
  if (value > 0 && value <= 4)
  {
    return (1) #"small"
  } 
  else if (value > 4 && value <= 8)
  {
    return (2) #"medium"
  }
  else if (value > 8)
  {
    return (3) #"Large"
  }
  else
  {
    return (0) #no name/zero length
  }
}