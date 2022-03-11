#' Measures of interest of a policy portfolio
#'
#' Calculate measures of interest of a policy portfolio.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param id A list with up to two elements, namely "Country", and "Year" indicating the specific identification characteristics of the portfolio(s) that must be processed. Defaults to NULL to process all portfolios.
#' @return A tidy dataset containing the portfolio identificators (Country, Sector and Year) plus the Measure identificators (Measure and Measure.label) and the value of the portfolio characteristic.
#' @seealso \code{\link{diversity}} for Average Instrument Diversity, \code{\link[ineq]{Gini}}, \code{\link[vegan]{diversity}} \code{\link{configurations}}.
#' @references Fernández-i-Marín, X., Knill, C. & Steinebach, Y. (2021). Studying Policy Design Quality in Comparative Perspective. _American Political Science Review_, online first. For Average Instrument Diversity.
#' @references Adam, C., Knill, C. & Fernández-i-Marín, X. (2016). Rule growth and government effectiveness: why it takes the capacity to learn and coordinate to constrain rule growth. _Policy Sciences_, 50, 241–268. doi:10.1007/s11077-016-9265-x. For portfolio size.
#' @export
#' @examples
#' data(P.education)
#' m.education <- pp_measures(P.education)
#' m.education
#'
#' # Calculate portfolio measures for a restricted set of portfolios defined by a list.
#' data(P.energy)
#' m.energy <- pp_measures(P.energy, id = list(Country = "Borduria", Year = 2022))
#' m.energy
pp_measures <- function(D, id = NULL) {
  # Manage passing only certain portfolio identificators
  if (!is.null(id)) {
    D <- pass.id(D, id)
    if (is.null(D)) {
      #message("There are no spaces to process. Broaden the country/year.")
      return(NULL)
    }
  }

  # In case of more than one sector
  if (length(levels(D$Sector)) > 1) {
    warning("You have given a data frame with more than one sector.\nTargets and instruments will be assumed to be equal for every sector.")
  }

  # In case of portfolios not covering the same years
  clean.years <- FALSE
  D.years <- D %>%
    dplyr::select(Country, Year) %>%
    unique()
  D.years.n <- D.years %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(N = dplyr::n())
  if (length(unique(D.years.n$N)) > 1) {
    message("At least one portfolio contains a different number of years.")
    clean.years <- TRUE
    # This is used later in the cleaning of the extra years
    D.years.yn <- D.years %>%
      dplyr::group_by(Country, Year) %>%
      dplyr::summarize(N = dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(Year, N, fill = 0) %>%
      tidyr::gather(Year, N, -Country)
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
        Div.aid <- diversity_aid(P)
        Div.gs <- vegan::diversity(apply(P, 1, sum), index = "simpson")
        Div.sh <- vegan::diversity(apply(P, 1, sum), index = "shannon", base = 2)
        Eq.sh <- Div.sh / log(dim(P)[1], base = 2)
        # On average, how many instruments per target?
        In.Prep <- sum(apply(P, 2, function(x) length(which(x > 0)))) / n.Targets
        # Different learning assumptions make portfolio size convert into burden
        # Raw burden refers to instruments
        Burden.continuous <- burden(M = P, nI = nIn, nT = nIt, learning = "continuous")
        Burden.steep <- burden(M = P, nI = nIn, nT = nIt, learning = "steep")
        Burden.capped <- burden(M = P, nI = nIn, nT = nIt, learning = "capped")
        # Burden.targets refers to weights by instruments
        Burden.targets.continuous <- burden(M = P, nI = nIn, nT = nIt, learning = "continuous", weight_by = "target")
        Burden.targets.steep <- burden(M = P, nI = nIn, nT = nIt, learning = "steep", weight_by = "target")
        Burden.targets.capped <- burden(M = P, nI = nIn, nT = nIt, learning = "capped", weight_by = "target")
        # Arrange it in a tidy dataframe
        O.measures.value <- c(Space, Size,
                              n.Instruments, p.Instruments,
                              n.Targets, p.Targets,
                              Unique, C.eq,
                              Div.aid, Div.gs, Div.sh, Eq.sh,
                              In.Prep,
                              Burden.continuous, Burden.steep, Burden.capped,
                              Burden.targets.continuous, Burden.targets.steep, Burden.targets.capped)
        O.measures.name <- c("Space", "Size",
                             "n.Instruments", "p.Instruments",
                             "n.Targets", "p.Targets",
                             "Unique", "C.eq",
                             "Div.aid", "Div.gs", "Div.sh", "Eq.sh",
                             "In.Prep",
                             "Burden.continuous", "Burden.steep", "Burden.capped",
                             "Burden.targets.continuous", "Burden.targets.steep", "Burden.targets.capped")
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
                              "Equitability (Shannon)",
                              "Instrument preponderance",
                              "Burden (continuous learning)",
                              "Burden (steep learning)",
                              "Burden (capped learning)",
                              "Burden (weight by targets, continuous learning)",
                              "Burden (weight by targets, steep learning)",
                              "Burden (weight by targets, capped learning)")
        nrep <- length(O.measures.value)
        O.full <- dplyr::tibble(Country = rep(Countries[c], nrep),
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
    dplyr::mutate(Country = factor(as.character(Country))) %>%
    dplyr::mutate(Sector = factor(as.character(Sector))) %>%
    dplyr::mutate(Year = as.integer(as.numeric(Year))) %>%
    dplyr::mutate(Measure = factor(as.character(Measure))) %>%
    dplyr::mutate(Measure.label = factor(as.character(Measure.label)))

  # Clean years for which some countries do not have data
  if (clean.years) {
    clean.countries <- as.character(D.years.n$Country[D.years.n$N < max(D.years.n$N)])
    for (c in 1:length(clean.countries)) {
      message(paste0("Cleaning years from ", clean.countries[c]))
      extra.years <- as.vector(dplyr::filter(D.years.yn, Country == clean.countries[c] & N == 0)$Year)
      O <- O %>%
        dplyr::filter(!(Country == clean.countries[c] & Year %in% extra.years))
    }
  }
  return(O)
}
