#' Prepare data for use with glmnet
#'
#' @param data A *data.frame*.
#'
#' @return A *data.frame* of the same structure as `data` but with all
#' factors converted to integer (dummy coded) vectors.
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom stats model.matrix
#' @export
#' @examples
#' library(mice)
#' clean_data(nhanes2)
clean_data <- function(data){
  factors <- which(sapply(data, is.factor))
  if(length(factors)){
    expanded <- vector(mode="list", length=length(factors))
    names(expanded) <- names(data)[factors]
    data <- mutate(data, .clean_id=as.character(1:n()))
    for(i in names(expanded)){
      expanded[[i]] <- model.matrix(as.formula(paste("~ 1", i, sep="+")), data=data)[, -1, drop=FALSE]
      expanded[[i]] <- cbind(id=rownames(expanded[[i]]), as.data.frame(expanded[[i]]))
      if(ncol(expanded[[i]]) == 2) names(expanded[[i]])[2] <- names(expanded[i])
    }
    if(length(expanded) > 1) {
      expanded <- Reduce(function(x, y) full_join(x,y, by="id"), expanded)
    } else{
      expanded <- expanded[[1]]
    }
    data <- data[, -factors]
    data <- left_join(data, expanded, by=c(".clean_id"="id")) %>% select(-.clean_id)
  }
  names(data) <- make.names(names(data))
  
  data
}