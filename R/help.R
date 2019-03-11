#' Tools for managing, measuring and visualizing policy portfolios
#'
#' PolicyPortfolios simplifies the creation of data structures suitable
#' for dealing with policy portfolios, that is, two-dimensional spaces of
#' of policy instruments and policy targets. It allows to generates measures of
#' their characteristics and facilitates its visualization.
#'
#' @references \url{http://xavier-fim.net/packages/PolicyPortfolios}.
#' @import ggplot2
#' @importFrom dplyr data_frame tbl_df filter mutate bind_rows case_when arrange %>%
#' @importFrom vegan diversity
#' @importFrom ineq Gini
#' @importFrom mgcv uniquecombs
#' @importFrom reshape2 acast
#' @docType package
#' @name PolicyPortfolios
#' @aliases PolicyPortfolios policyportfolios
NULL
