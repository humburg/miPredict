## Based on code by MrFlick via GitHub
## https://gist.github.com/MrFlick/ae299d8f3760f02de6bf

#' @importFrom stats terms
#' @importFrom stats model.frame
#' @importFrom stats .MFclass
makeglm <- function(formula, ..., family, data=NULL) {
  dots <- list(...)
  out<-list()
  tt <- terms(formula, data=data)
  if(!is.null(data)) {
    mf <- model.frame(tt, data)
    vn <- sapply(attr(tt, "variables")[-1], deparse)
    
    if((yvar <- attr(tt, "response"))>0)
      vn <- vn[-yvar]
    xlvl <- lapply(data[vn], function(x) if (is.factor(x))
      levels(x)
      else if (is.character(x))
        levels(as.factor(x))
      else
        NULL)
    attr(out, "xlevels") <- xlvl[!vapply(xlvl,is.null,NA)]
    attr(tt, "dataClasses") <- sapply(data[vn], .MFclass)
  }
  out$terms <- tt
  coef <- numeric(0)
  stopifnot(length(dots)>0 & !is.null(names(dots)))
  for(i in seq_along(dots)) {
    if((n<-names(dots)[i]) != "") {
      v <- dots[[i]]
      if(!is.null(names(v))) {
        coef[paste0(n, names(v))] <- v
      } else {
        stopifnot(length(v)==1)
        coef[n] <- v
      }
    } else {
      coef["(Intercept)"] <- dots[[i]]
    }   
  }
  out$coefficients <- coef
  out$rank <- length(coef)
  if (!missing(family)) {
    out$family <- if (class(family) == "family") {
      family
    } else if (class(family) == "function") {
      family()
    } else if (class(family) == "character") {
      get(family)()
    } else {
      stop(paste("invalid family class:", class(family)))
    }
    out$qr <- list(pivot=seq_len(out$rank))
    out$deviance <- 1
    out$null.deviance <- 1
    out$aic <- 1
    class(out) <- c("glm","lm")
  } else {
    class(out) <- "lm"
    out$fitted.values <- predict(out, newdata=data)
    out$residuals <- out$mf[attr(tt, "response")] - out$fitted.values
    out$df.residual <- nrow(data) - out$rank
    out$model <- data
    #QR doesn't work
  }
  out
}