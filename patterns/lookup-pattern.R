# Below is a pattern I often use if you want to convert between two levels of understanding
# 1. Create a lookup table
# 2. Build a function that uses that lookup table for conversion
#
# Usage:
#    Type <- response_type(Response)
.lookup <- list(Avoid="Non-habituated", 
                Curious="Non-habituated",
                Ignore="Habituated",
                Feeding="Habituated",
                Other="Habituated")
response_type <- function(responses) {
  as.factor(sapply(responses, function(response) .lookup[response][[1]]))
}
