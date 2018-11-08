
split_datetime_attribute <- function(dataset)
{
  dataset$year <- year(as.POSIXlt(dataset$DateTime))
  dataset$month <- month(as.POSIXlt(dataset$DateTime))
  dataset$day <- day(as.POSIXlt(dataset$DateTime))
  dataset$hour <- hour(as.POSIXlt(dataset$DateTime))
  dataset$DayofWeek=weekdays(as.Date(dataset$DateTime))
  dataset$IsWeekend <- ifelse(dataset$DayofWeek == 'Saturday', 1, ifelse(dataset$DayofWeek == 'Sunday', 1,0))
  dataset
}