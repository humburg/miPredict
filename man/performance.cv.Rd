% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance.R
\name{performance.cv}
\alias{performance.cv}
\title{Performance metrics based on cross-validation results}
\usage{
\method{performance}{cv}(
  x,
  outcome,
  metrics = c("roc", "auc", "specificity", "sensitivity", "accuracy", "precision",
    "brier"),
  ...
)
}
\arguments{
\item{x}{An object of class \emph{cv} as produced by \code{\link[=crossvalidate]{crossvalidate()}}.}

\item{outcome}{A vector (numeric or factor) of outcome labels. Alternatively, a character vector of length 1 giving the name of the outcome variable.}

\item{metrics}{Character vector indicating the performance metrics to be computed.}

\item{...}{Further arguments passed on to \code{\link[=class_perf]{class_perf()}}.}
}
\value{
A list with the requested performance metrics
}
\description{
Performance metrics based on cross-validation results
}
\seealso{
\code{\link[=crossvalidate]{crossvalidate()}}
}
