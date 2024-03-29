% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{dist_col_type}
\alias{dist_col_type}
\title{dist_col_type
Inner column type record distance function}
\usage{
dist_col_type(a, b, col_type, weights, orders)
}
\arguments{
\item{a}{record a}

\item{b}{record b}

\item{col_type}{A vector encoding the column type for each column in the dataset. Can take values
in "categorical", "ordinal", "string", or "numeric"}

\item{weights}{A vector of weights for each column for making some column distances more important. Must sum to 1.
Defaults to equal weights.}

\item{orders}{A named list containing the order of the levels in each ordinal column. Defaults to NULL,
which corresponds to no ordinal variables.}
}
\description{
dist_col_type
Inner column type record distance function
}
