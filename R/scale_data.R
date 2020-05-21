#' Scale numeric variables
#' 
#' @param data A data frame
#'
#' @return A data frame of the same structure as `data` with all numeric variables centred and scaled.
#' @author Peter Humburg
#' @importFrom stringr str_starts
#' @importFrom stringr fixed
#' @export
scale_data <-
function(data) {
  to_scale <- which_scale(data)
  for(i in to_scale) {
    data[[i]] <- scale(data[[i]])[,1]
  }
  data
}
