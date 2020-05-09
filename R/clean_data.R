#' Prepare data for use with glmnet
#'
#' @param data A *data.frame*.
#'
#' @return A *data.frame* of the same structure as `data` but with all
#' factors converted to integer vectors.
#' @note Currently only supports binary factors. Expansion into dummy variables is not performed.
#' @export
#' @importFrom dplyr mutate_if
#' @importFrom dplyr %>%
#' @examples
#' library(mice)
#' clean_data(nhanes2)
clean_data <- function(data){
  data %>% mutate_if(is.factor, ~as.integer(.)-1)
}