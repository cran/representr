% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/record_distance.R
\name{dist_binary}
\alias{dist_binary}
\alias{dist_col_type_slow}
\title{The distance between two records}
\usage{
dist_binary(a, b)

dist_col_type_slow(
  a,
  b,
  col_type,
  string_dist = utils::adist,
  weights = rep(1/length(a), length(a)),
  orders = NULL,
  ...
)
}
\arguments{
\item{a}{Record a}

\item{b}{Record b}

\item{col_type}{A vector encoding the column type for each column in the dataset. Can take values
in "categorical", "ordinal", "string", or "numeric"}

\item{string_dist}{String distance function. Default is edit distance. Function must take at least
two arguments (strings)}

\item{weights}{A vector of weights for each column for making some column distances more important. Must sum to 1.
Defaults to equal weights.}

\item{orders}{A named list containing the order of the levels in each ordinal column. Defaults to NULL,
which corresponds to no ordinal variables.}

\item{...}{Additional parameters passed to string distance function.}
}
\value{
\code{binary_dist} returns a numeric value indicating how many discrepancies there are between two records.

\code{col_type_dist} return a numeric value of the weighted column type specific distance between two records.
}
\description{
The distance between two records
}
\examples{
data("rl_reg1")
dist_binary(rl_reg1[1,], rl_reg1[2,])

type <- c("string", "string", "numeric", "numeric",
    "numeric", "categorical", "ordinal", "numeric", "numeric")
order <- list(education = c("Less than a high school diploma",
    "High school graduates, no college", "Some college or associate degree",
    "Bachelor's degree only", "Advanced degree"))

dist_col_type_slow(rl_reg1[1,], rl_reg1[2,], col_type = type, order = order)

}
