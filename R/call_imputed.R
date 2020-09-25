
#' Call binary outcomes from imputed data
#'
#' @param x A multiply imputed dataset
#' @param outcome Name of the outcome variable in `x`
#' @param threshold The threshold to be used for calling outcomes. Imputed outcomes with an average below this value will be called *0*, all others *1*.
#'
#' @return A numeric vector of *0*s and *1*s, indicating outcomes.
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
call_imputed <- function(x, outcome, threshold=0.5) {
  imp_out <- x %>%  data_long() %>% clean_data() %>% group_by(.data$.id) %>% summarise(!!outcome:=mean(.data[[outcome]]), .groups="drop")
  ifelse(imp_out[[outcome]] < threshold, 0, 1)
}
