# extractEloByIndv creates a data.frame of Elo ratings
# Data.frame format is <Date> <Rating> <Indv>
# This can then be used in ggplot2 via aes(x=Date, y=Rating, groupby=Indv)
extractEloByIndv <- function(eloobject) {
  # Extract eloratings which differ in order relative to individual due to being ordered by value
  # That is to say, "Max" might be in ordinal position "1" on Day 1 then in position "4" on Day 20
  e2dates <- unique(eloobject$truedates[eloobject$logtable$Date])

  eloratings <- lapply(e2dates, function(e2date) {
    EloRating::extract.elo(eloobject, extractdate = e2date)
  })

  dplyr::bind_rows(lapply(seq_along(eloratings), function(posdate) {
    data.frame(
      stringsAsFactors = F,
      Date = e2dates[[posdate]],
      Indv = names(eloratings[[posdate]]),
      Rating = unlist(lapply(names(eloratings[[posdate]]), function(name) {
        eloratings[[posdate]][name]
      }))
    )
  }))
}

# transformToMatrix transforms the data.frame from extractEloByIndv into a matrix
# Dates become columns and Individuals become rows in the matrix
transformToMatrix <- function(df) {
  m <-
    matrix(
      NA,
      nrow = length(unique(df$Indv)),
      ncol = length(unique(df$Date)),
      dimnames = list(sort(unique(df$Indv)), sort(unique(df$Date)))
    )
  for (indv in rownames(m)) {
    for (date in colnames(m)) {
      val <- df$Rating[which(df$Indv == indv & df$Date == date)]
      m[indv, date] <- if (length(val) > 0)
        val
      else
        NA
    }
  }
  m
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
