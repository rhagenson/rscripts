# Built from: https://stackoverflow.com/questions/21511801/text-clustering-with-levenshtein-distances
# Groups strings into numerical factors based on their textual similarity using maxDist
# as the number of character differences allowed between elements in a given factor
auto_group <- function(str_vec, maxDist) {
  require("dplyr")
  uniqElms <- length(unique(str_vec))
  
  # Build distance matrix
  d  <- adist(unique(sort(str_vec)))
  rownames(d) <- unique(sort(str_vec))
  
  # Hierarchical cluster
  hc <- hclust(as.dist(d))
  
  # Build data.frame of minK to one minus unique groups results for pruning
  df <- cutree(hc, k = (uniqElms %/% 4):uniqElms) %>%
    cbind(Label = rownames(.)) %>%
    as.data.frame() %>%
    mutate_at(colnames(.)[grepl("\\d+", colnames(.))], as.numeric) %>%
    rename_at(colnames(.)[grepl("\\d+", colnames(.))], ~paste0("K", .)) %>%
    distinct() %>%
    group_by(Label)
  
  # Keep columns where matching labels are not in different groups
  # I.e., prune for overfitting
  col_keeps <- sapply(colnames(df)[grepl("K", colnames(df))], function(clm) {
    intra_group_distance <- df[, c("Label", clm)] %>%
      group_by(.data[[clm]]) %>%
      summarize(MaxN=max(adist(.data[["Label"]])))
    ifelse(any(intra_group_distance$MaxN > maxDist), NA, clm)
  })
  df[, c("Label", na.omit(col_keeps))] %>% distinct()
}
