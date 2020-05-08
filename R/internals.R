#' @importFrom methods is
data_long <- function(data) {
  if(is(data, "mids")){
    data <- complete(data, action="long") %>% clean_data()
  }
  if(!".imp" %in% names(data)) {
    stop("Expected data frame of complete data exported from `mice`. Use `mice::complete(data, action='long')` to obtain data in the right format after imputation.")
  }
  data
}