#' Calculate portfolio diversity (Average Instrument Diversity)
#'
#' Function used to calculate the diversity of a portfolio (Average Instrument Diversity, AID).
#' It is adapted from the idea of a Gini-Simpson diversity index.
#' The measure can be interpreted as the average probability that picking two
#' policy spaces from different Targets, they use a different Instrument.
#'
#'
#' @section Details:
#'\deqn{
#'\forall_{t=1..T},  \forall_{i=1..I} \sum_{c=1}^{C} \frac{c_{t,i} = c_{!t,!i}}{C}
#'}
#'
#' where:
#'
#' T are the targets covered by at least one policy instrument
#' I are the instruments addressing at least one policy target
#' C are the entirety of target-instrument-constellations
#'
#' It is applied only to matrices, not to tidy objects in a proper policy portfolio.
#' For a proper treatment using tidy data, use it through pp_measures().
#'
#' @param M Matrix with two dimensions (Instrument, Target) containing absence (0) or presence (1) of policy intervention.
#' @return A value of the portfolio diversity.
#' @references Fernández-i-Marín, X., Knill, C. & Steinebach, Y. (2021). Studying Policy Design Quality in Comparative Perspective. _American Political Science Review_, online first.
#' @export
#' @examples
#' data(consensus)
#' consensus %>%
#'   filter(Sector == "Environmental") %>%
#'   filter(Country %in% c("France", "United States")) %>%
#'   filter(Year %in% c(1976, 2005)) %>%
#'   pp_array() %>%
#'   apply(., c(1, 2, 3), diversity_aid) %>%
#'   as.vector() %>%
#'   round(digits = 3)
#' # 1976 (FR, US), 2005 (FR, US)
diversity_aid <- function(M) {
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

