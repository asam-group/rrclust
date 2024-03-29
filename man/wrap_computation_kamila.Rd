% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_computation_kamila.R
\name{wrap_computation_kamila}
\alias{wrap_computation_kamila}
\title{Memoise a copy of \code{\link{wrap_computation_kamila}}}
\usage{
wrap_computation_kamila(tl_inp, tl_prepadata)
}
\arguments{
\item{tl_inp}{List of input data frames of which we use:
\itemize{
\item \code{PARAM_KAMILA$calc_kstar}: If TRUE, estimates the clusters. Else, takes the
parameter PARAM_KAMILA$param_kstar.
\item \code{PARAM_KAMILA$cont_var_expl}: List of continuous variables chosen as
explicative variables.
\item \code{PARAM_KAMILA$categ_var_expl}: List of categorical variables chosen as
explicative variables.
}}

\item{tl_prepadata}{List of data frames prepared in a first step.}
}
\value{
\code{tl_mod_calc_kamila} with the following data frames:
\itemize{
\item \code{PLOTDATKAM} Data frame containing the clusters factor and the other
variables.
\item \code{KM_RES_FINAL} Data frame containing the resulting parameters of the
clustering.
\item \code{CONTVARS} Data frame containing the continuous standardised variables.
\item \code{FULL_CONT_DF} Data frame containing the continuous variables used for
the estimation.
\item \code{FULL_CATEG_DF} Data frame containing the categorical variables used for
the estimation.
}
}
\description{
Create a memoised copy of \code{\link{wrap_computation_kamila}}.
}
\author{
\href{mailto:layal.lettry@gmail.com}{Layal Christine Lettry}
}
