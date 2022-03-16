#' Tidy a dataset with a "long" policy portfolio structure
#'
#' Clean a policy portfolio dataset into a tidy object.
#'
#' @param d Data frame in an uncleaned and untidy structure containing data from a policy portfolio.
#' @param Sector Character vector with the Sector of the dataset.
#' @param Country.name Character vector of length one with the name of the variable that contains the country name.
#' @param Year.name Character vector of length one with the name of the variable that contains the year.
#' @param Instrument.name Character vector of length one with the name of the variable that contains the instruments.
#' @param Target.name Character vector of length one with the name of the variable that contains the targets.
#' @param coding.category.name Character vector of length one with the name of the variable that contains the coding category.
#' @param coding.category Numerical value with the level of the category that captures the combination of instrument and target.
#' @param Direction.name Character vector of length one with the name of the variable that contains the direction of the policy change.
#' @param directions Numerical vector with the numeric values of the direction of the policy changes, namely "Status quo", "Expansion" and "Dismantling". Defaults to, 0, 1 and -1, respectively.
#' @param associated.vars Character vector indicating variables that contain characteristics of the policy space.
#' @param date By default, return Year as the only time indicator. If TRUE, return the full date with dd-mm-YYYY.
#' @param debug Logical value. When TRUE, print more verbose information about the cleaning process.
#' @return D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @export
#' @examples
#'\dontrun{
#' X <- read.table("raw_data.csv", header = TRUE)
#' D <- pp_clean(X, Sector = "Education")
#'
#' # Now 'D' is a tidy dataset suitable for being used in the context of the 'PolicyPortfolio' package.
#'}
pp_clean <- function(d, Sector = NULL,
                     Country.name = "Country", Year.name = "Year",
                     Instrument.name = "Instrument", Target.name = "Target",
                     coding.category.name = "Coding category", coding.category = 2,
                     Direction.name = "Direction",
                     directions = c(0, 1, -1),
                     associated.vars = NULL,
                     date = FALSE,
                     debug = FALSE) {

  # Require a file and a sector
  if (is.null(d)) stop("A dataset is required.")
  if (is.null(Sector)) stop("A name for a 'Sector' is required.")

  # Ensure that the variable names are correct
  n <- names(d)
  if (!("Country" %in% n | Country.name %in% n)) {
    stop("It has not been possible to identify the variable containing the Country name.")
  }
  if (!("Instrument" %in% n | Instrument.name %in% n)) {
    stop("It has not been possible to identify the variable containing the Instrument names.")
  }
  if (!("Target" %in% n | Target.name %in% n)) {
    stop("It has not been possible to identify the variable containing the Target names.")
  }

  # Ensure that associated variables is passed correctly
  if (!is.null(associated.vars)) {
    if (class(associated.vars) != "character") {
      stop("Associated variables must be possed using a character vector")
    }
  }

  # Clarify the dataset containing the policy changes
  if (is.null(associated.vars)) {
    if (!date) {
      D.changes <- d[,c(Country.name, Year.name, Instrument.name, Target.name,
                        coding.category.name, Direction.name)]
      names(D.changes) <- c("Country", "Year", "Instrument", "Target", "cc", "direction")
    } else {
      d$Date <- as.Date(paste(d$Day, d$Month, d$Year, sep = "-"), "%d-%m-%Y")
      d$Date[is.na(d$Date) & !is.na(d$Year)] <- 
        as.Date(paste(31, 12, d$Year[is.na(d$Date) & !is.na(d$Year)], sep = "-"), "%d-%m-%Y")
      D.changes <- d[,c(Country.name, "Date", Year.name, Instrument.name, Target.name,
                        coding.category.name, Direction.name)]
      names(D.changes) <- c("Country", "Date", "Year", "Instrument", "Target", "cc", "direction")
    }
  } else {
    if (!date) {
      D.changes <- d[,c(Country.name, Year.name, Instrument.name, Target.name,
                        coding.category.name, Direction.name, associated.vars)]
      names(D.changes) <- c("Country", "Year", "Instrument", "Target", "cc", "direction", associated.vars)
    } else {
      d$Date <- as.Date(paste(d$Day, d$Month, d$Year, sep = "-"), "%d-%m-%Y")
      D.changes <- d[,c(Country.name, Date, Year.name, Instrument.name, Target.name,
                        coding.category.name, Direction.name, associated.vars)]
      names(D.changes) <- c("Country", "Date", "Year", "Instrument", "Target", "cc", "direction", associated.vars)
    }
  }

  # Convert the possible directions into the convenient 0, 1 and -1
  original.direction <- D.changes$direction
  if (length(unique(na.omit(original.direction))) > 3) {
    stop("There are more possible directions that options for status quo, expansion or dismantling.")
  }
  D.changes$direction[original.direction == directions[1]] <- 0
  D.changes$direction[original.direction == directions[2]] <- 1
  D.changes$direction[original.direction == directions[3]] <- -1

  # Directions can not be empty
  if (length(which(is.na(D.changes$direction) & D.changes$cc != 4)) > 0) {
    message("There are observations with missing values in 'Directions' with\ncoding category different from 4.\nThis should not happen, as it is hard to guess what to do\n(and dangerous to make such assumptions).")
    message(paste("Row: ", which(is.na(D.changes$direction) & D.changes$cc != 4), "\n", sep = ""))
    message("I am going to assume they are 'Status quo' to be able to proceed.")
    print(table(D.changes$direction, exclude = NULL))
    D.changes$direction[is.na(D.changes$direction) & D.changes$cc != 4] <- 0
    print(table(D.changes$direction, exclude = NULL))
  }

  # Instruments and targets can not be empty when coding category is 2
  if (length(which(is.na(D.changes$Instrument[D.changes$cc == coding.category]))) > 0) {
    message("There are observations with the relevant coding category and missing Instrument.")
    message(paste("Row: ", which(is.na(D.changes$Instrument & D.changes$cc == coding.category)), "\n", sep = ""))
  }
  if (length(which(is.na(D.changes$Target[D.changes$cc == coding.category]))) > 0) {
    message("There are observations with the relevant coding category and missing Target.")
    message(paste("Row: ", which(is.na(D.changes$Target& D.changes$cc == coding.category)), "\n", sep = ""))
  }

  if (!date) {
    # Manage the range of the temporal scope
    year.range <- range(D.changes$Year, na.rm = TRUE)

    # Create the tidy object to be filled later
    # Make the assumption that the policy portfolio is empty by default
    D <- expand.grid(Country = unique(na.omit(D.changes$Country)),
                     Instrument = unique(na.omit(D.changes$Instrument)),
                     Target = unique(na.omit(D.changes$Target)),
                     Year = as.integer(seq(year.range[1], year.range[2], by = 1)),
#                     covered = 0) %>% # Assumption
                     covered = NA) %>% # Assumption
      dplyr::as_tibble()
  } else {
    # Create the tidy object to be filled later
    # Make the assumption that the policy portfolio is empty by default
    # And create as much dates as policy changes
    potential.dates <- unique(as.Date(paste(d$Day, d$Month, d$Year, sep = "-"), "%d-%m-%Y"))
    D <- expand.grid(Country = unique(na.omit(D.changes$Country)),
                     Instrument = unique(na.omit(D.changes$Instrument)),
                     Target = unique(na.omit(D.changes$Target)),
                     Date = potential.dates,
#                     covered = 0) %>% # Assumption
                     covered = NA) %>% # Assumption
      dplyr::mutate(Year = format(Date, "%Y")) %>%
      dplyr::as_tibble()
  }
  if (!is.null(associated.vars)) {
    for (av in seq_len(length(associated.vars))) {
      D <- mutate(D, avname = NA)
      class(D$avname) <- class(eval(parse(text = paste0("d$`", associated.vars[av], "`"))))
      names(D)[dim(D)[2]] <- associated.vars[av]
    }
  }

  if (debug) {
    message(paste("The size of the final data frame is ", dim(D)[1], " observations.", sep = ""))
    message(paste(" Country: ", length(levels(D$Country)), sep = ""))
    message(paste(" Year: ", length(unique(D$Year)), sep = ""))
    message(paste(" Instrument: ", length(levels(D$Instrument)), sep = ""))
    message(paste(" Target: ", length(levels(D$Target)), sep = ""))
  }

  # Sort the policy changes to translate them into the tidy portfolio
  if (!date) {
    D.changes <- D.changes %>%
      dplyr::as_tibble() %>%
      dplyr::filter(cc == coding.category) %>%
      dplyr::group_by(Country, Instrument, Target) %>%
      dplyr::arrange(Country, Instrument, Target, Year)
  } else {
    D.changes <- D.changes %>%
      dplyr::as_tibble() %>%
      dplyr::filter(cc == coding.category) %>%
      dplyr::group_by(Country, Instrument, Target) %>%
      dplyr::arrange(Country, Instrument, Target, Date, Year)
  }
  for (o in seq_len(dim(D.changes)[1])) {
    direction.now <- D.changes$direction[o]
    if (!date) {
      position.now <- which(D$Country == D.changes$Country[o] &
                D$Instrument == D.changes$Instrument[o] &
                D$Target == D.changes$Target[o] &
                D$Year == D.changes$Year[o])
      value.now <- D$covered[position.now]
      positions.fromnow <- which(D$Country == D.changes$Country[o] &
                D$Instrument == D.changes$Instrument[o] &
                D$Target == D.changes$Target[o] &
                D$Year >= D.changes$Year[o])
      positions.beforenow <- which(D$Country == D.changes$Country[o] &
                D$Instrument == D.changes$Instrument[o] &
                D$Target == D.changes$Target[o] &
                D$Year < D.changes$Year[o])
      associated.vars.now <- D.changes[o, associated.vars]
    } else {
      position.now <- which(D$Country == D.changes$Country[o] &
                D$Instrument == D.changes$Instrument[o] &
                D$Target == D.changes$Target[o] &
                D$Date == D.changes$Date[o])
      value.now <- D$covered[position.now]
      positions.fromnow <- which(D$Country == D.changes$Country[o] &
                D$Instrument == D.changes$Instrument[o] &
                D$Target == D.changes$Target[o] &
                D$Date >= D.changes$Date[o])
      positions.beforenow <- which(D$Country == D.changes$Country[o] &
                D$Instrument == D.changes$Instrument[o] &
                D$Target == D.changes$Target[o] &
                D$Date < D.changes$Date[o])
      associated.vars.now <- D.changes[o, associated.vars]
    }



    # Inspect the scenarios and judge what to do
    if (length(value.now) > 0) {
      if (is.na(value.now) & direction.now == 1) {

        # Regular case of policy expansion
        # when policy intervention starts for the first time
        D$covered[positions.fromnow] <- 1
        D$covered[positions.beforenow] <- 0

      } else if (is.na(value.now) & direction.now == 0) {

        # First change in the space and it is for status quo
        # Assume that there was policy in place before
        # And it continues to do so
        if (debug) {
          message("\n\n=== Debug case ===")
          message(paste("* There is a status quo entry in a space that apparently was not being covered by policy intervention before at ", D.changes$Country[o], D.changes$Instrument[o], D.changes$Target[o], D.changes$Year[o], sep = "\n "))
          message("Proceeding with the assumption that there was policy intervention before.")
        }
        D$covered[positions.fromnow] <- 1
        D$covered[positions.beforenow] <- 1

      } else if (is.na(value.now) & direction.now == -1) {

        # First change in the space, and it is dismantling
        # Assume that there was policy intervention before
        # But it stops now

        if (debug ) {
          message("\n\n=== Debug case ===")
          message(paste("* There is a dismantling entry in a space that apparently was not being covered by policy intervention before at ", D.changes$Country[o], D.changes$Instrument[o], D.changes$Target[o], D.changes$Year[o], sep = "\n "))
          message("This can happen BUT only assuming that the policy was already in place\n at the beginning of the time period and that this is the first change\nin this space.")
          message("Proceeding with this assumption.")
        }
        D$covered[positions.fromnow] <- 0
        D$covered[positions.beforenow] <- 1

      } else if (value.now == 0 & direction.now == 1) {

        # Regular case of policy expansion
        # when policy intervention was not in place, but had been in the past
        D$covered[positions.fromnow] <- 1

      } else if (value.now == 0 & direction.now == 0) {

        # Strange case where there is no policy and the direction is status quo
        if (debug) {
          message("\n\n=== Debug case ===")
          message(paste("* There is a policy change from no intervention to status quo at at ", D.changes$Country[o], D.changes$Instrument[o], D.changes$Target[o], D.changes$Year[o], sep = "\n "))
          message("It is not problematic per se, but it would be worth checking it.")
        }

      } else if (value.now == 0 & direction.now == -1) {

        # There can be dismantling if there is no policy in place ...
        # ... but only a )after checking that all previous years have "no policy"
        # and b) assuming that the policy was already in place at the beginning of the period
        if (debug) {
          message("\n\n=== Debug case ===")
          message(paste("* There is a policy change from no policy to dismantling at ", D.changes$Country[o], D.changes$Instrument[o], D.changes$Target[o], D.changes$Year[o], sep = "\n "))
          message("This can happen BUT only assuming that the policy was already in place\n at the beginning of the time period and that this is the first change\nin this space.")
          message("Proceeding with this assumption.")
        }
        if (length(which(!D$covered[positions.beforenow] == 0)) > 0) {
          if (debug) {
            message("But this is not the first change in this policy space.")
            print("Past: ")
            print(D[positions.beforenow,])
            print("Present: ")
            print(D[position.now,])
            print("Direction in the present:")
            print(direction.now)
            message("This MUST be clarified before stating that the data has been cleaned.")
          }
        }

      } else if (value.now == 1 & direction.now == 0) {

        # Nothing should happen, although the entry is redundant

      } else if (value.now == 1 & direction.now == -1) {

        # Regular case of policy dismantling
        D$covered[positions.fromnow] <- 0

      } else if (value.now == 1 & direction.now == 1) {

        # There can not be expansion if there is policy already in place
        # Strange case where there is polici and direction is expansion
        if (debug) {
          message("\n\n=== Debug case ===")
          message(paste("** There is a policy change from existing policy to expansion at ", D.changes$Country[o], D.changes$Instrument[o], D.changes$Target[o], D.changes$Year[o], sep = "\n "))
          message("This should not happen.")
          message("I am going to assume that the portfolio still contains the policy,\nbut the input data should be inspected carefully.")
        }
        D$covered[positions.fromnow] <- 1

      } else {
        if (debug) {
          message("\n\n=== Debug case ===")
          message(paste("* There is a really strange situation at ", D.changes$Country[o], D.changes$Instrument[o], D.changes$Target[o], D.changes$Year[o], sep = "\n "))
          message(paste("Value: ", value.now))
          message(paste("Direction: ", direction.now))
        }
      }

      if (!is.null(associated.vars)) {
        D[position.now, associated.vars] <- associated.vars.now
      }
    }

  }
  D <- D %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Country = as.factor(Country)) %>%
    dplyr::mutate(Sector = as.factor(Sector)) %>%
    dplyr::mutate(Instrument = as.factor(as.character(Instrument))) %>%
    dplyr::mutate(Target = as.factor(as.character(Target))) %>%
    # If nothing has happened in a policy space, then assume it has not been covered
    dplyr::mutate(covered = ifelse(is.na(covered), 0, covered)) %>%
    dplyr::mutate(covered = as.integer(covered))
  if (!date) {
    D <- D %>%
      dplyr::arrange(Sector, Country, Instrument, Target, Year)
  } else {
    D <- D %>%
      dplyr::arrange(Sector, Country, Instrument, Target, Date)
  }
  if (is.null(associated.vars)) {
    if (!date) {
      D <- dplyr::select(D, Country, Sector, Year, Instrument, Target, covered)
    } else {
      D <- dplyr::select(D, Country, Sector, Date, Year, Instrument, Target, covered)
    }
  } else {
    if (!date) {
      D <- dplyr::select(D, Country, Sector, Year, Instrument, Target, covered, associated.vars)
    } else {
      D <- dplyr::select(D, Country, Sector, Date, Year, Instrument, Target, covered, associated.vars)
    }
  }

  return(D)
}
