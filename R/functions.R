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
      D <- dplyr::filter(D, Sector %in% id$Sector) %>%
        droplevels()
    }
    if (!is.null(id$Country)) {
      D <- dplyr::filter(D, Country %in% id$Country) %>%
        droplevels()
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

#' Calculate burden based on how different learning assumptions  affect portfolio size
#'
#' Internal function to calculate a transformation of portfolio size based on a
#' non-regular assumption of how policy learning works at the instrument level.
#' This generates a weighted portfolio size that can be understood as the
#' "burden" of its size.
#'
#' - Arithmetical: assumes a continuous learning.
#' - Geometrical: assumes steep learning.
#' - Radical: assumes capped learning.
#'
#' This contrasts with the regular portfolio size that assumes no learning is
#' produced between policy instruments.
#'
#' @param M Matrix with the policy portfolio, where the first dimension contains instruments and the second contains targets.
#' @param nI Integer with the number of Instruments.
#' @param nT Integer with the number of Targets.
#' @return A value of burden (portfolio size using a different learning assumption).
burden <- function(A, nI, nT, learning) {
  if (is.null(learning)) {
    stop("A learning assumption must be passed Please use 'continuous', 'steep' or 'capped'.")
  }
  sum.i <- apply(A, 1, sum)
  if (!is.na(sum.i)) {
    parts <- NULL
    if (learning == "continuous") {
      # arithmetical decay
      l <- seq(0, 1, length.out = nT + 1)
      for (i in 1:nI) {
        if (sum.i[i] > 0) {
          parts <- c(parts, sum(1 - l[1:(sum.i[i])]))
        } else {
          parts <- c(parts, 0)
        }
      }
    } else if (learning == "steep") {
      # geometrical decay
      for (i in 1:nI) {
        if (sum.i[i] > 0) {
          parts <- c(parts, sum(1 / 1:(sum.i[i])))
        } else {
          parts <- c(parts, 0)
        }
      }
    } else if (learning == "capped") {
      # radical decay
      for (i in 1:nI) {
        if (sum.i[i] > 1) {
          parts <- c(parts, 1, (length(2:(sum.i[i])) * 0.5))
        } else if (sum.i[i] == 1) {
          parts <- c(parts, 1)
        } else {
          parts <- c(parts, 0)
        }
      }
    } else {
      stop("Learning assumption not known. Please use 'continuous', 'steep' or 'capped'.")
    }
    mx <- nI * nT
    ps <- sum(parts)
    return(ps / mx)
  } else {
    return(NA)
  }
}
