#' Calculate a configuration of instruments
#'
#' Internal function used to assess the number of configurations of
#' instruments. Configurations are specific combinations of instruments thay
#' may appear in different targets.
#'
#' @param M Matrix with the policy portfolio, where the first dimension contains instruments and the second contains targets.
#' @return Vector with the distribution of configurations
configurations <- function(M) {
  dist.configurations <- as.numeric(table(attributes(mgcv::uniquecombs(M))$index))
  return(dist.configurations)
}

#' Subset a full dataset with only certain cases (Country / Year)
#'
#' Internal function used to select a subset of cases (Country / Year)
#' from a tidy dataset with portfolio data.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param id A list with up to two elements, namely "Country", and "Year" indicating the specific identification characteristics of the portfolio(s) that must be processed.
#' @return A Data frame with a subsect of D defined by id.
pass.id <- function(D, id = NULL) {
  if (!is.null(id)) {
    if (!is.null(id$Country)) {
      D <- filter(D, Country %in% id$Country)
    }
    if (!is.null(id$Year)) {
      D <- filter(D, Year %in% id$Year)
    }
  }
  return(D)
}

#' Calculate portfolio diversity
#'
#' Internal function used to calculate the diversity of a portfolio.
#' It is based on the idea of a Gini-Simpson diversity index.
#' The measure can be interpreted as the average probability that picking two
#' policy spaces from different Targets, they use a different Instrument.
#'
#' @param M Matrix with two dimensions (Instrument, Target) containing absence (0) or presence (1) of policy intervention.
#' @return A value of the portfolio diversity.
diversity <- function(M) {
  if (is.na(sum(M))) {
    diversity <- NA
  } else {
    if (sum(M) == 0) {
      diversity <- 0                           # Empty portfolio
    } else {                                   # Non-empty portfolio
      M <- M[,which(apply(M, 2, sum) > 0)]
      if (is.null(dim(M))) {                   # Only one target
        diversity <- 0
      } else {                                 # More than one target
        nI <- dim(M)[1]    # Number of instruments
        nrT <- dim(M)[2]   # Number of remaining targets, with at least one instrument
        divs <- NULL       # To store all diversities between each space and the rest (excluding own Target)
        for (t in 1:nrT) {       # Loop over targets that contain at least one intervention
          for (i in 1:nI) {      # Loop over instruments
            if (M[i,t] > 0) {    #   ... and check that instruments contain at least one intervention
              # Exclude the current target, and keep it a matrix
              M.exc <- M[,-t, drop = FALSE]
              # Now calculate the probability that the rest of the targets
              # contain a *different* instrument as the current one
              different.instruments <- sum(M.exc[-i,]) # Number of different instruments means excluding current reference instrument
              # Divide the total number of different instruments over all possible spaces outside the current target
              prob.matches <- different.instruments / sum(M.exc)
              divs <- c(divs, prob.matches)
            }
          }
        }
        diversity <- mean(divs)
      }
    }
  }
  return(diversity)
}

