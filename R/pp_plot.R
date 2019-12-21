#' Plot a policy portfolio
#'
#' Plot a policy portfolio.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param id A list with up to two elements, namely "Country", and "Year" indicating the specific identification characteristics of the portfolio(s) that must be processed.
#' @param spacing Logical value. When TRUE, some space is added between tiles to help distinguish adjacent spaces. Defaults to FALSE.
#' @param subtitle Logical value. When TRUE (the default), a subtitle with the measures of the portfolio is included.
#' @param caption A character vector to overimpose the Source of the data. By default, publicpolicy-knill.org. When NULL, the caption is not printed.
#' @return A tidy dataset containing the portfolio identificators (Country, Sector and Year) plus the Measure identificators (Measure and Measure.label) and the value of the portfolio characteristic.
#' @export
#' @examples
#' data(PolicyPortfolio)
#' pp_plot(P.education, id = list(Country = "Borduria", Year = 2025))
pp_plot <- function(D, id = NULL,
                    spacing = FALSE,
                    subtitle = TRUE, caption = "Source: publicpolicy-knill.org") {
  # Manage passing only certain portfolio identificators
  if (!is.null(id)) {
    D <- pass.id(D, id)
  }

  # In case of more than one Country / Sector / Year
  # Check that only one portfolio is passed (repeated in pp_array()).
  A <- reshape2::acast(D, Country ~ Sector ~ Year ~ Instrument ~ Target, value.var = "covered")
  if (length(which(dim(A)[1:3] != 1)) > 0) {
    stop("It is not possible to plot the data frame,\n, as it includes more than one one Country/Sector/Year as input.")
  }

  # Label for the portfolio
  id.label <- list(Country = unique(D$Country),
                   Sector = unique(D$Sector),
                   Year = unique(D$Year))

  # Measures for the portfolio
  DM <- pp_measures(D, id = id.label)

  # Generate a ggplot object with its visual representation
  f <- ggplot(D, aes(x = Target, y = Instrument, fill = as.factor(covered)))
    # Raster should be prefered, as it is more efficient (less space),
    # but when combined with svg it is indeed interpolated
    #geom_raster(interpolate = FALSE) +
    # Therefore tile is used
  if (spacing) {
    f <- f + geom_tile(size = 2, color = "white")
  } else {
    f <- f + geom_tile()
  }
  f <- f +
    scale_fill_manual(values = c("white", "black")) +
    theme_minimal() +
    ggtitle(do.call("paste", c(id.label, sep = " : "))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    guides(fill = FALSE)
  # Add a caption
  if (!is.null(caption)) {
    f <- f + labs(caption = caption)
  }
  # Add a subtitle
  if (subtitle) {
    f <- f +
    labs(subtitle = paste("Space: ", DM$value[DM$Measure == "Space"],
                          "; Size: ", round(DM$value[DM$Measure == "Size"], 3),
                          "; Instruments: ", DM$value[DM$Measure == "n.Instruments"],
                          " (", round(DM$value[DM$Measure == "p.Instruments"] * 100, 2), "%)",
                          "; Targets: ", DM$value[DM$Measure == "n.Targets"],
                          " (", round(DM$value[DM$Measure == "p.Targets"] * 100, 2), "%)",
                          "; InPr: ", round(DM$value[DM$Measure == "In.Prep"], 2),
                          "; UnCfg: ", DM$value[DM$Measure == "Unique"],
                          "; CfgEq: ", round(DM$value[DM$Measure == "C.eq"], 3),
                          "; DivAID: ", round(DM$value[DM$Measure == "Div.aid"], 3),
                          "; DivGS: ", round(DM$value[DM$Measure == "Div.gs"], 3),
                          "; DivSh: ", round(DM$value[DM$Measure == "Div.sh"], 3),
                          "; EqSh: ", round(DM$value[DM$Measure == "Eq.sh"], 3),
                          sep = ""))
  }
  return(f)
}
