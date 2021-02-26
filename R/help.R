#' Tools for managing, measuring and visualizing policy portfolios
#'
#' Tools for simplifying the creation and management of data structures suitable
#' for dealing with policy portfolios, that is, two-dimensional spaces of
#' of policy instruments and policy targets. It allows to generate measures of
#' portfolio characteristics and facilitates their visualization.
#'
#' @references \url{http://xavier-fim.net/packages/PolicyPortfolios/}.
#' @references Fernández-i-Marín, X., Knill, C. & Steinebach, Y. (2021). Studying Policy Design Quality in Comparative Perspective. _American Political Science Review_, online first.
#' @references Knill, C., Schulze, K. & Tosun, J. (2012). Regulatory Policy Outputs and Impacts. Exploring a Complex Relationship. _Regulation & Governance_, 5(4), 427-444. doi:10.1111/j.1748-5991.2012.01150.x.
#' @import ggplot2
#' @importFrom dplyr tibble as_tibble filter mutate bind_rows case_when arrange n summarize group_by ungroup %>%
#' @importFrom vegan diversity
#' @importFrom ineq Gini
#' @importFrom mgcv uniquecombs
#' @importFrom reshape2 acast
#' @docType package
#' @name PolicyPortfolios
#' @aliases PolicyPortfolios policyportfolios
NULL
