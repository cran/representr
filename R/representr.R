#' representr: A package for creating representative records post-record linkage.
#'
#' The representr package provides two types of representative record generation:
#' point prototyping and posterior prototyping.
#'
#' @section Point Prototyping:
#'
#' To bridge the gap between record linkage and a downstream task, there are three methods to choose
#' or create the representative records from linked data: random prototyping, minimax prototyping, and
#' composite. These are all based on a point estimate of the linkage structure post-record linkage (
#' rather than a posterior distribution).
#'
#' \bold{Random prototyping} chooses a record from each cluster at random, either uniformly or according
#' to a supplied distribution. \bold{Minimax prototyping} selects the record whose farthest neighbors
#' within the cluster is closest, based on some notion of closeness that is measured by a record distance
#' function. There are two distance functions included in this package (\link[=dist_binary]{binary} and
#' \link[=dist_col_type]{column-based}), or the user can specify their own. \bold{Composite} record
#' creation constructs the representative record by aggregating the records (in each cluster) to form
#' a composite record that includes information from each linked record.
#'
#' Each of these three types of prototyping can be used from the function \link[=represent]{represent}.
#'
#' @section Posterior prototyping:
#'
#' The posterior distribution of the linkage can be used in two ways in this package. The first, is as
#' weights or in a distance function for the above point prototyping methods. The second, is through
#' the posterior prototyping (PP) weights presented in Kaplan, Betancourt, and Steorts (2018+). The PP
#' weights are accessible through the \link[=pp_weights]{pp_weights} function.
#'
#' @references Kaplan, Andee, Brenda Betancourt, and Rebecca C. Steorts. "Posterior Prototyping: Bridging the Gap between Bayesian Record Linkage and Regression." arXiv preprint arXiv:1810.01538 (2018).
#'
#' @aliases representr-package
#' @name representr
NULL


