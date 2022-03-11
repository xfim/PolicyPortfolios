#' Complete a policy portfolio
#'
#' Complete an already tidy dataset with the full set of instruments, targets and years.
#' Used also to specify the order of instruments and targets.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param year.range Numerical vector of length two with the initial and final value of the years considered
#' @param Instrument.set Ordered factor with the full set of values of Instruments, to be combined with the already existing Instruments.
#' @param Target.set Ordered factor with the full set of values of Targets, to be combined with the already existing Targets. 
#' @param date. By default a portfolio by every year is employed. Otherwise, use a full date (DD-MM-YYYY).
#' @return D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors. It differs from the input in that this one includes the full set of pre-specified Instruments, Targets and temporal range.
#' @export
#' @examples
#' data(P.education)
#' range(P.education$Year)
#'
#' P.education.full <- pp_complete(P.education, year.range = c(2000, 2035))
#' range(P.education.full$Year)
pp_complete <- function(D, year.range = NULL, 
  Instrument.set = NULL, Target.set = NULL, date = FALSE) {

  # Ensure that the passed sets include all the values already observed.
  # Otherwise, pass the original values in their form
  if (!date) {
    if (!is.null(year.range)) {
      if (sum(which(!(unique(D$Year) %in% seq(year.range[1], year.range[2])))) > 0) {
        stop("The original object contains years outside the range provided.")
      }
    } else {
      year.range <- range(D$Year)
    }
  } else {
    date.range <- unique(D$Date)
  }
  if (!is.null(Instrument.set)) {
    if (sum(which(!(which(unique(D$Instrument) %in% Instrument.set)))) > 0) {
      stop("The original object contains instruments that are not present in the full set of instruments provided.")
    }
  } else {
    Instrument.set <- levels(D$Instrument)
  }
  if (!is.null(Target.set)) {
    if (sum(which(!(which(unique(D$Target) %in% Target.set)))) > 0) {
      stop("The original object contains targets that are not present in the full set of targets provided.")
    }
  } else {
    Target.set <- levels(D$Target)
  }

  # Proceed creating an expanded object to include the new possible combinations
  Original <- D
  if (!date) {
    Full <- tidyr::expand_grid(Country = levels(Original$Country),
                        Sector = levels(Original$Sector),
                        Year = as.integer(seq(year.range[1], year.range[2])),
                        Instrument = Instrument.set,
                        Target = Target.set)
  } else {
    Full <- tidyr::expand_grid(Country = levels(Original$Country),
                        Sector = levels(Original$Sector),
                        Date = unique(Original$Date),
                        Instrument = Instrument.set,
                        Target = Target.set)
  }

  # For the new sets of spaces assume that they are empty
  if (!date) {
    D <- suppressWarnings(dplyr::left_join(Full, Original, 
                          by = c("Country", "Sector", "Year", "Instrument", "Target")))
  } else {
    D <- suppressWarnings(dplyr::left_join(Full, Original, 
                          by = c("Country", "Sector", "Date", "Instrument", "Target")))
  }
  D <- D %>%
    dplyr::mutate(covered = ifelse(is.na(covered), 0, covered)) %>%
    # Ensure factors in the correct order
    dplyr::mutate(Country = factor(Country)) %>%
    dplyr::mutate(Sector = factor(Sector)) %>%
    dplyr::mutate(Instrument = factor(Instrument, levels = Instrument.set)) %>%
    dplyr::mutate(Target= factor(Target, levels = Target.set))

  message(paste("The original size of the data frame was ", dim(Original)[1], " observations.", sep = ""))
  message(paste("The size of the transformed data frame is now ", dim(D)[1], " observations.", sep = ""))
  message(paste(" Country: ", length(levels(Original$Country)), " -> ", length(levels(D$Country)), sep = ""))
  message(paste(" Year: ", length(unique(Original$Year)), " -> ", length(unique(D$Year)), sep = ""))
  message(paste(" Instrument: ", length(levels(Original$Instrument)), " -> ", length(levels(D$Instrument)), sep = ""))
  message(paste(" Target: ", length(levels(Original$Target)), " -> ", length(levels(D$Target)), sep = ""))
  return(D)
}
