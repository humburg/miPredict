% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_model.R
\name{fit_model}
\alias{fit_model}
\title{Variable selection and model fitting}
\usage{
fit_model(
  data,
  outcome,
  family = "binomial",
  s = c("lambda.min", "lambda.1se"),
  scale = FALSE,
  include = NULL,
  predictors
)
}
\arguments{
\item{data}{An object of class \link{mids}}

\item{outcome}{The name of the outcome measure. This should be the name of a variable in \code{data}.}

\item{family}{The \link{family} object to be used. May a family object or the name of the family to be used.}

\item{s}{A character string indicating which value of \eqn{\lambda} should be used. Either \code{lambda.min}
for the optimal \eqn{\lambda} determined by cross-validation or \code{lambda.1se} for the optimal \eqn{\lambda + 1} standard error.}

\item{scale}{Logical indicating whether input data should be scaled.}

\item{include}{Character vector of predictor names. If provided the named predictors will not be penalized (and included in all models)}

\item{predictors}{A character vector with the names of variables to include as predictors. If this is provided the model is fixed and no variable selection will take place.}
}
\value{
A list with components
\itemize{
\item \emph{selected_model}: A list with components \emph{formula} and \emph{fit} containing the \link{formula} for the selected
model and the model fits on the imputed datasets respectively.
\item \emph{pooled_model}: An object of class \link{glm} containing the pooled parameter estimates.
}
}
\description{
Selects variables for predictive models from multiply imputed
data.
}
\details{
The variable selection and model fitting process proceeds in three stages.
\enumerate{
\item \emph{LASSO} regression is used to select a subset of predictors for each
imputed dataset.
\item A consensus set of predictors is determined as follows
a. The frequency with which each predictor is selected across the
imputed datasets is determined.
b. Starting with the most frequently selected predictors, create a series
of models that include increasing numbers of predictors.
c. Select the model with the lowest AIC.
\item The consensus model parameters are estimated on all imputed datasets without penalty.
\item Model parameters are pooled to obtain the final model.
}

The pooled model returned by this function is useful to generate predictions from new data.
If you are interested in inspecting the parameter estimates and their variances instead use
\code{pool(ans$selected_model$fit)}, where \code{ans} is the object returned by this function.
}
\author{
Peter Humburg
}
