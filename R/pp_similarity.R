#' Calculate the similarity between different portfolios
#'
#' Calculate similarity between portfolios.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param id A list with up to two elements, namely "Country", and "Year" indicating the specific identification characteristics of the portfolio(s) that must be processed. Defaults to NULL to process all portfolios.
#' @param method A character vector containing the indices of similarity requested. Defaults to "all". The implemented indices of binary similarity are "jaccard" (), "
#' @param return_all Logical indicating whether all possible combinations (countries and years of origin and destination) must be returned or only when they are different. Defaults to TRUE.
#' @return A tidy dataset containing the portfolio identificators (Country, Sector and Year) plus the similarity measures and their values.
#' @export
#' @examples
#'\dontrun{
#' data(P.education)
#' pp_similarity(P.education, id = list(Year = 2025))
#'}
pp_similarity <- function(D, id = NULL, method = "all", return_all = TRUE) {
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
      ungroup() %>%
      tidyr::spread(Year, N, fill = 0) %>%
      tidyr::gather(Year, N, -Country)
  }


  # Agree in the portfolio(s) to pass and convert into matrix form
  # to make the calculations
  P.full <- pp_array(D, return_matrix = FALSE)

  # Deal with methods
  all.methods <- c("Jaccard", "Hamming", "Dice", "Overlap", "Rand")
  all.methods.labels <- c("Similarity (Jaccard)", 
                          "Distance (Hamming)",
                          "Similarity (S\u00f8rensen\u002DDice)",
                          "Similarity (Szymkiewicz\u002DSimpson)",
                          "Similarity (Rand or Simple Matching Coefficient)")
  if (length(method) == 1) {
    if (method == "all") {
      methods <- all.methods
      methods.labels <- all.methods.labels
    } else {
      stop("No valid method provided.")
    }
  }
  if (length(method) != 1) {
    which.methods <- which(all.methods %in% method)
    if (length(which.methods) == 0) {
      stop("No valid method provided.")
    }
    methods <- all.methods[which.methods]
    methods.labels <- all.methods.labels[which.methods]
    message(paste0("Using methods: ", paste(methods, sep = "", collapse = ", ")))
  }
  nMethods <- length(methods)

  # Simple matching coefficient

  # Loop over countries and years
  # Prepare the loop
  nC <- dim(P.full)[1]
  Countries <- dimnames(P.full)[[1]]
  nS <- dim(P.full)[2]
  Sectors <- dimnames(P.full)[[2]]
  nY <- dim(P.full)[3]
  Years <- dimnames(P.full)[[3]]

  O <- NULL
  O <- list()
  i <- 1
  for (s in seq_len(nS)) {
    for (c in seq_len(nC)) {
      message("Processing country: ", Countries[c], sep = "")
      for (y in seq_len(nY)) {
        message("Processing year: ", Years[y], sep = "")
        P.origin <- P.full[c,s,y,,]
        for (c.dest in seq_len(nC)) {
          for (y.dest in seq_len(nY)) {
            P.destination <- P.full[c.dest,s,y.dest,,]
            similarity <- NULL
            for (m in seq_len(nMethods)) {
              # Most formulas need the following:
              # ioid: cases where 1's are IN ORIGIN (io) and IN DESTINATION (id)
              # compared to cases NOT IN ORIGIN (nio) and/or NOT IN DESTINATION (nid)
              ioid <- length(which(P.origin == 1 & P.destination == 1 & P.origin == P.destination))
              nionid <- length(which(P.origin == 0 & P.destination == 0 & P.origin == P.destination))
              ionid <- length(which(P.origin == 1 & P.destination == 0))
              nioid <- length(which(P.origin != 1 & P.destination == 1))
              if (methods[m] == "Jaccard") {
                jaccard <- ioid / (ionid + nioid + ioid)
                similarity <- c(similarity, jaccard)
              } else if (methods[m] == "Hamming") {
                hamming <- ioid / length(P.destination)
                similarity <- c(similarity, hamming)
              } else if (methods[m] == "Dice") {
                dice <- (2 * ioid) / (ionid + nioid + (2 * ioid))
                similarity <- c(similarity, dice)
              } else if (methods[m] == "Overlap") {
                overlap <- ioid / min(length(which(P.origin == 1)), length(which(P.destination == 1)))
                similarity <- c(similarity, overlap)
              } else if (methods[m] == "Rand") {
                rand <- (ioid + nionid) / (ioid + nionid + ionid + nioid)
                similarity <- c(similarity, rand)
              }
            }
            O[[i]] <- data.frame(Sector = rep(Sectors[s], nMethods),
                                 Country = rep(Countries[c], nMethods),
                                 Year = rep(Years[y], nMethods),
                                 Country.destination = rep(Countries[c.dest], nMethods),
                                 Year.destination = rep(Years[y.dest], nMethods),
                                 Method = methods,
                                 value = similarity,
                                 Method.label = methods.labels)
            i <- i + 1
          }
        }
      }
    }
  }
  O <- dplyr::as_tibble(dplyr::bind_rows(O))

  O <- O %>%
    dplyr::mutate(Country = factor(as.character(Country))) %>%
    dplyr::mutate(Country.destination = factor(as.character(Country.destination))) %>%
    dplyr::mutate(Sector = factor(as.character(Sector))) %>%
    dplyr::mutate(Year = as.integer(as.numeric(Year))) %>%
    dplyr::mutate(Year.destination = as.integer(as.numeric(Year.destination))) %>%
    dplyr::mutate(Method = factor(as.character(Method))) %>%
    dplyr::mutate(Method.label = factor(as.character(Method.label)))

  # Post-process values to arrange infinite and NaN
  O <- O %>%
    dplyr::mutate(value = ifelse(is.infinite(value), 1, value)) %>%
    dplyr::mutate(value = ifelse(is.nan(value), 0, value)) %>%
    dplyr::rename(Similarity = value)

  # Clean years for which some countries do not have data
  if (clean.years) {
    clean.countries <- as.character(D.years.n$Country[D.years.n$N < max(D.years.n$N)])
    for (c in seq_len(length(clean.countries))) {
      message(paste0("Cleaning years from ", clean.countries[c]))
      extra.years <- as.vector(dplyr::filter(D.years.yn, Country == clean.countries[c] & N == 0)$Year)
      O <- O %>%
        dplyr::filter(!(Country == clean.countries[c] & Year %in% extra.years)) %>%
        dplyr::filter(!(Country.destination == clean.countries[c] & Year %in% extra.years))
    }
  }

  # Filter origin == destination or not
  if (!return_all) {
    O <- O %>%
      dplyr::filter(!(Country == Country.destination & Year == Year.destination))
  }

  return(O)
}
