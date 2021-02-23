#' Convert a tidy dataset into a matrix of policy portfolios
#'
#' Take a tidy dataset containing one or several policy portfolios and convert it into a matrix or an array.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param return_matrix Logical value indicating whether the output must be in the form of an array with Country, Sector and Year dimensions present (although equal to one), or a simplified matrix of Instruments and Targets. Defaults to FALSE.
#' @return An array when return_single is FALSE (the default), or a two-dimensional matrix with Instruments and Targets as first and second dimensions, respectively.
#' @export
#' @examples
#' data(P.education)
#' # Returns an array
#' A <- pp_array(P.education)
#' dim(A)
#'
#' # Returns a matrix
#' M <- pp_array(subset(P.education, Country == "Syldavia" & Year == 2022),
#'   return_matrix = TRUE)
#' dim(M)
pp_array <- function(D, return_matrix = FALSE) {
  A <- reshape2::acast(D, Country ~ Sector ~ Year ~ Instrument ~ Target, value.var = "covered")
  if (return_matrix) {
    # Check that only one portfolio is passed (repeated in pp_plot()).
    if (length(which(dim(A)[1:3] != 1)) > 0) {
      stop("It is not possible to return a matrix\n, as there are more than one Country/Sector/Year as input.")
    }
    A <- A[1,1,1,,]
  }
  return(A)
}
