% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_input_kmres.R
\name{mod_input_kmres}
\alias{mod_input_kmres}
\title{Read of the kamila results of the training set}
\usage{
mod_input_kmres(PARAM_INPUTS, list = NULL)
}
\arguments{
\item{PARAM_INPUTS}{a data frame containing the data paths.}

\item{list}{List of input data frames.}
}
\value{
a \code{tidylist} containing the following tidy data frames:
\itemize{
\item \code{KM_RES} : contains the kamila results of the training set.
}
}
\description{
Read the kamila results of the training set and save the optimal number of
clusters kstar.
}
\author{
\href{mailto:layal.lettry@gmail.com}{Layal Christine Lettry}
}
