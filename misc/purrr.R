# Me taking a look into purrr to add it to my toolbelt

# The below pattern was not exactly what I wanted.
# Intent: Take a df of Type, Species, Class, Count group and nest
#   to produce a matrix for each Type of shape Species (x)
#   and Class (y) with Count value, then produce a heatmap
#   for each matrix (labeling with Type name) and not reorganizing
#   the ordinal Class values.
# Actually: Does most of this, but does not label the heatmaps by Type
inoculum %>%
  group_by(Type) %>%
  nest() %>%
  mutate(
    Matrix = purrr::map(data, ~ reshape2::acast(., Species~Class, value.var = 'Count')),
    purrr::walk(Matrix, ~ pheatmap(., cluster_cols = FALSE))
  )
