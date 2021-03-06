% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossvalidate.R
\name{crossvalidate}
\alias{crossvalidate}
\title{Cross-validation}
\usage{
crossvalidate(imputed, outcome, k = 10, force = FALSE, ...)
}
\arguments{
\item{imputed}{An object of class \link[mice:mids-class]{mice::mids} containing the multiply imputed dataset.}

\item{outcome}{Name of the outcome variable in \code{imputed}.}

\item{k}{Number of folds to use for cross-validation.}

\item{force}{Logical indicating whether cross-validation should be carried out even if the number of observations becomes too small in some of the folds.}

\item{...}{Further arguments passed to \code{\link[=fit_model]{fit_model()}}.}
}
\value{
A list of class \emph{cv} with components \strong{pooled}: a data frame with the pooled predictions and \strong{imputed}: a list of data frames with the predictions for each imputed dataset.
}
\description{
Cross-validation
}
\details{
The original dataset is partitioned into \emph{k} segments for cross-validation in such a way that
the proportion of each outcome, including missing values, are preserved as far as possible.
}
