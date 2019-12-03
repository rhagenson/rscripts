# Below are two similar functions that 
# Requires: 
#    library(lubridate) for working with dates
#    library(forcats) for working with factors
#
# Usage: 
#    MonthYear <- pull_month_year(ObservationDateTime)
#    WeekNum <- pull_week_number(ObservationDateTime)
#
# Caution: 
#    pull_week_number does not count from January 1st of a given year, rather it is counts based on what you would
#    see if looking at a calendar. 
#    I.e., if January 1st was a Saturday then January 2nd would be Sunday in the next week
pull_month_year <- function(datetime) fct_inorder(as_factor(paste(month(datetime, label = TRUE), year(datetime))))
pull_week_number <- function(datetime) as.factor(as.numeric(fct_inorder(as_factor(paste(year(datetime), isoweek(datetime))))))
