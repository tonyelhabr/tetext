


coerce_col_to_factor <- function(data, colname) {
  classes <- sapply(names(data), class)
  class_i <- classes[names(classes) == colname]
  nm_i <- names(class_i)
  if(class_i != "factor") {
    data <- data %>% dplyr::mutate_at(dplyr::vars(dplyr::contains(nm_i)), dplyr::funs(factor))
    message(sprintf("Coercing %s to a factor.", nm_i))
  }
  data
}