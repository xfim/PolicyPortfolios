#' Measures of interest of a policy portfolio
#'
#' Calculate measures of interest of a policy portfolio.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param id A list with up to two elements, namely "Country", and "Year" indicating the specific identification characteristics of the portfolio(s) that must be processed. Defaults to NULL to process all portfolios.
#' @return A tidy dataset containing the portfolio identificators (Country, Sector and Year) plus the Measure identificators (Measure and Measure.label) and the value of the portfolio characteristic.
#' @export
#' @examples
#' data(PolicyPortfolio)
#' m.education <- pp_measures(P.education)
#' m.education
#'
#' # Calculate portfolio measures for a restricted set of portfolios defined by a list.
#' m.energy <- pp_measures(P.energy, id = list(Country = "Borduria", Year = 2022))
#' m.energy
pp_measures <- function(D, id = NULL) {
  # Manage passing only certain portfolio identificators
  if (!is.null(id)) {
    D <- pass.id(D, id)
  }

  # In case of more than one sector
  if (length(levels(D$Sector)) > 1) {
    warning("You have given a data frame with more than one sector.\nTargets and instruments will be assumed to be equal for every sector.")
  }

  # In case of portfolios not covering the same years
  clean.years <- FALSE
  D.years <- D %>%
    select(Country, Year) %>%
    unique()
  D.years.n <- D.years %>%
    group_by(Country) %>%
    summarize(N = n())
  if (length(unique(D.years.n$N)) > 1) {
    message("At least one portfolio contains a different number of years.")
    clean.years <- TRUE
    # This is used later in the cleaning of the extra years
    D.years.yn <- D.years %>%
      group_by(Country, Year) %>%
      summarize(N = n()) %>%
      ungroup() %>%
      spread(Year, N, fill = 0) %>%
      gather(Year, N, -Country)
  }

  # Agree in the portfolio(s) to pass and convert into matrix form
  # to make the calculations
  P.full <- pp_array(D, return_matrix = FALSE)

  # Prepare the loop
  nC <- dim(P.full)[1]
  Countries <- dimnames(P.full)[[1]]
  nS <- dim(P.full)[2]
  Sectors <- dimnames(P.full)[[2]]
  nY <- dim(P.full)[3]
  Years <- dimnames(P.full)[[3]]
  nIn <- dim(P.full)[4]
  Instruments <- dimnames(P.full)[[4]]
  nIt <- dim(P.full)[5]
  Targets <- dimnames(P.full)[[5]]

  O <- NULL
  for (s in 1:nS) {
    for (c in 1:nC) {
      for (y in 1:nY) {
        P <- P.full[c,s,y,,]
        # Calculate portfolio characteristics
        Space <- nIn * nIt
        Size <- sum(P) / Space
        n.Instruments <- length(which(apply(P, 1, function(x) length(which(x > 0))) > 0))
        p.Instruments <- n.Instruments / dim(P)[1]
        n.Targets <- length(which(apply(P, 2, function(x) length(which(x > 0))) > 0))
        p.Targets <- n.Targets / dim(P)[2]
        C <- configurations(P)
        Configurations <- length(C)
        Unique <- length(which(C == 1))
        C.eq <- 1 - ineq::Gini(C)
        Div.aid <- diversity(P)
        Div.gs <- vegan::diversity(apply(P, 1, sum), index = "simpson")
        Div.sh <- vegan::diversity(apply(P, 1, sum), index = "shannon", base = 2)
        # On average, how many instruments per target?
        In.Prep <- sum(apply(P, 2, function(x) length(which(x > 0)))) / n.Targets
        # Arrange it in a tidy dataframe
        O.measures.value <- c(Space, Size,
                              n.Instruments, p.Instruments,
                              n.Targets, p.Targets,
                              Unique, C.eq,
                              Div.aid, Div.gs, Div.sh,
                              In.Prep)
        O.measures.name <- c("Space", "Size",
                             "n.Instruments", "p.Instruments",
                             "n.Targets", "p.Targets",
                             "Unique", "C.eq",
                             "Div.aid", "Div.gs", "Div.sh",
                             "In.Prep")
        O.measures.label <- c("Portfolio space",
                              "Portfolio size",
                              "Number of instruments covered",
                              "Proportion of instruments covered",
                              "Number of targets covered",
                              "Proportion of targets covered",
                              "Number of unique instrument configurations",
                              "Equality of Instrument configurations",
                              "Diversity (Average Instrument Diversity)",
                              "Diversity (Gini-Simpson)", "Diversity (Shannon)",
                              "Instrument preponderance")
        nrep <- length(O.measures.value)
        O.full <- dplyr::data_frame(Country = rep(Countries[c], nrep),
                                    Sector = rep(Sectors[s], nrep),
                                    Year = rep(Years[y], nrep),
                                    Measure = O.measures.name,
                                    value = O.measures.value,
                                    Measure.label = O.measures.label)
        O <- dplyr::bind_rows(O, O.full)
      }
    }
  }
  O <- O %>%
    mutate(Country = factor(as.character(Country))) %>%
    mutate(Sector = factor(as.character(Sector))) %>%
    mutate(Year = as.integer(as.numeric(Year))) %>%
    mutate(Measure = factor(as.character(Measure))) %>%
    mutate(Measure.label = factor(as.character(Measure.label)))

  # Clean years for which some countries do not have data
  if (clean.years) {
    clean.countries <- as.character(D.years.n$Country[D.years.n$N < max(D.years.n$N)])
    for (c in 1:length(clean.countries)) {
      message(paste0("Cleaning years from ", clean.countries[c]))
      extra.years <- as.vector(filter(D.years.yn, Country == clean.countries[c] & N == 0)$Year)
      O <- O %>%
        filter(Country != clean.countries[c] | (Country == clean.countries[] & !(Year %in% extra.years)))
    }
  }
  return(O)
}
