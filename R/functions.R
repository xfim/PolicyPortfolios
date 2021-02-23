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
#' Internal function used to select a subset of cases (Country / Sector / Year)
#' from a tidy dataset with portfolio data.
#' When subsetting a sector, the remaining object limits the results to the available
#' Instruments and Sectors found in the original object.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param id A list with up to three elements, namely "Country", "Sector" and "Year" indicating the specific identification characteristics of the portfolio(s) that must be processed; and optionally "clean" (logical, FALSE by default) to remove non observed factor levels in Instruments, Targets, Country, Sector and Year.
#' @return A Data frame (tibble) with a part of the original input, defined by id.
pass.id <- function(D, id = NULL) {
  if (!is.null(id)) {
    if (!is.null(id$Sector)) {
      D <- dplyr::filter(D, Sector %in% id$Sector)
    }
    if (!is.null(id$Country)) {
      D <- dplyr::filter(D, Country %in% id$Country)
    }
    if (!is.null(id$Year)) {
      D <- dplyr::filter(D, Year %in% id$Year)
    }
    if (!is.null(id$clean)) {
      if (is.logical(id$clean)) {
        if (id$clean == TRUE) {
          D <- droplevels(D)
          message("\nOnly observed Instruments and Targets have been kept in the restricted portfolio.\nDon't forget to ensure that the portfolio is complete with pp_complete().")
        }
      } else {
        stop("'clean' needs a logical argument (TRUE / FALSE).")
      }
    }
  }
  if (dim(D)[1] > 0) {
    return(D)
  } else {
    return(NULL)
  }
}

#' Calculate portfolio diversity (Average Instrument Diversity)
#'
#' Internal function used to calculate the diversity of a portfolio (Average Instrument Diversity, AID).
#' It is adapted from the idea of a Gini-Simpson diversity index.
#' The measure can be interpreted as the average probability that picking two
#' policy spaces from different Targets, they use a different Instrument.
#'
#' @param M Matrix with two dimensions (Instrument, Target) containing absence (0) or presence (1) of policy intervention.
#' @return A value of the portfolio diversity.
#' @references Fernández-i-Marín, X., Knill, C. & Steinebach, Y. (2021). Studying Policy Design Quality in Comparative Perspective. _American Political Science Review_, online first.
#' @examples
#' data(consensus)
#' consensus %>%
#'   filter(Sector == "Environmental") %>%
#'   filter(Country %in% c("France", "United States")) %>%
#'   filter(Year %in% c(1976, 2005)) %>%
#'   pp_array() %>%
#'   apply(., c(1, 2, 3), diversity) %>%
#'   as.vector() %>%
#'   round(digits = 3)
#' # 1976 (FR, US), 2005 (FR, US)
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

