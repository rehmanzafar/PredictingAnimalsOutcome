

load_data <- function(filename) 
{
  return (read.csv(paste(getwd(), filename, sep = ''), header = TRUE, sep = ',', na.strings=c("","NA")))
}