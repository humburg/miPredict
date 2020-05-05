#' @importFrom stringr str_starts
#' @importFrom stringr fixed
#' @export
scale_data <-
function(data) {
  to_scale <- sapply(data, function(x) if(length(unique(x)) > 2 && !is.factor(x)) TRUE else FALSE)
  to_scale <- to_scale & !str_starts(names(data), fixed("."))
  for(i in which(to_scale)) {
    data[[i]] <- scale(data[[i]])[,1]
  }
  data
}
