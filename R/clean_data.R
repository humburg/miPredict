#' Prepare data for use with glmnet
#'
#' @param data A *data.frame*.
#'
#' @return A *data.frame* of the same structure as `data` but with all
#' factors converted to integer vectors.
#' @note Currently only supports binary factors. Expansion into dummy variables is not performed.
#' @export
#' @examples
#' library(mice)
#' clean_data(nhanes2)
clean_data <- function(data){
  factors <- which(sapply(data, is.factor))
  if(length(factors)){
    expanded <- vector(mode="list", length=length(factors))
    names(expanded) <- names(data)[factors]
    for(i in names(expanded)){
      expanded[[i]] <- model.matrix(as.formula(paste("~ 1", i, sep="+")), data=data)[, -1]
    }
    expanded <- do.call(cbind, expanded)
    data <- data[,-factors]
    data <- cbind(data, expanded)
  }
  
  data
}