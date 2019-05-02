# Create a data.frame that will be returned to the user
# Data.frame format is <Date> <EloRating> <IndividualName>
# This can then be used in ggplot2 via aes(x=Date, y=EloRating, groupby=IndividualName)

extractEloByIndv <- function(eloobject) {
  # Extract eloratings which differ in order relative to individual due to being ordered by value
  # That is to say, "Max" might be in ordinal position "1" on Day 1 then in position "4" on Day 20
  e2dates <- unique(eloobject$truedates[eloobject$logtable$Date])

eloratings <- lapply(e2dates, function(e2date) {
  EloRating::extract.elo(eloobject, extractdate = e2date)
})

dplyr::bind_rows(lapply(seq_along(eloratings), function(posdate) {
  data.frame(stringsAsFactors = F,
             Date=e2dates[[posdate]],
             Indv=names(eloratings[[posdate]]),
             Rating=unlist(lapply(names(eloratings[[posdate]]), function(name) {
               eloratings[[posdate]][name]
             })))
}))
}

# Plotting example
# library(ggplot2)
# theme_set(theme_bw())
# ggplot(elo_frame) + 
# aes(x=Date, y=Rating, group=Indv, color=Indv, shape=Indv, linetype=Indv) + 
# geom_line() + 
# geom_point() +
# scale_colour_grey() +
# scale_x_date(breaks=pretty(elo_frame$Date, n=5)) +
# ylab(label = "Elo Rating") +
# xlab(label = "Date (YYYY-MM-DD)")