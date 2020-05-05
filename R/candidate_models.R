#' @export
candidate_models <-
function(fit_list, s="lambda.min", min.rate=0.5, response="Poor_Outcome") {
  var_counts <- apply(sapply(fit_list, function(x)as.matrix(coef(x, s=s))),1, function(x) sum(x!=0))
  var_names <- rownames(coef(fit_list[[1]]))
  names(var_counts) <- var_names
  var_sets <- sort(unique(var_counts), decreasing = TRUE)
  var_sets <- var_sets[var_sets >= min.rate*max(var_sets)]
  candidates <- vector(mode="list", length(var_sets))
  for(i in 1:length(var_sets)){
    vars <- names(var_counts[var_counts >= var_sets[i]])
    vars <- vars[vars != "(Intercept)"]
    if(length(vars) == 0) {
      vars <- "1"
    }
    candidates[[i]] <- as.formula(paste(response, "~", paste(vars, collapse="+")))
  }
  candidates
}
