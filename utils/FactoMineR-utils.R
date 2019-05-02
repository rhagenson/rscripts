# extract_dims extracts the dimensions from correspondence analysis via FactoMineR and factoextra
# To emulate the plotting of fviz_ca_biplot(...) use:
# ggplot(extract_dims(cs.object)) + aes(x=Dim1, y=Dim2, color=Type) + geom_point()
extract_dims <- function(X, row.type="Behavior", col.type="Strata", col.sup.type="Support") {
  # Get data
  rows <- cbind(as.data.frame(X$row$coord), Label = rownames(X$row$coord), Type=row.type)
  cols <- cbind(as.data.frame(X$col$coord), Label = rownames(X$col$coord), Type=col.type)
  col.sups <- cbind(as.data.frame(X$col.sup$coord), Label = rownames(X$col.sup$coord), Type=col.sup.type)
  
  # Make data.frame
  data <- rbind(rbind(rows, cols), col.sups)
  
  # Rename column and row names removing spaces and making numeric, respectively
  colnames(data) <- sub(" ", "", colnames(data))
  rownames(data) <- NULL
  
  # Return data
  data
}
