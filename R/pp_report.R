#' Produce a report of policy portfolios
#'
#' Produce a report of policy portfolios.
#'
#' @param D Data frame in a tidy format with the following columns: "Country", "Sector", "Year", "Instrument", "Target" and "covered". "covered" is a binary identificator of whether the portfolio space is covered by policy intervention (1) or not (0). The remaining columns identify the case. Notice that "Year" is a numeric value, while the remaining 4 case identifiers are factors.
#' @param id A list with up to two elements, namely "Country", and "Year" indicating the specific identification characteristics of the portfolio(s) that must be processed. Defaults to NULL to process all portfolios.
#' @param file Character vector with the name of the file to create. Defaults to a combination of the date and time of production and policy portfolio report.
#' @param title A character vector with the title of the report. If no title is passed, the default (NULL) is to use the id, or a generic title with the date and time of its production.
#' @param plot A character vector with the types of plot to perform. If no plot is desired, set it to NULL. By default it plots individual portfolios ("single") as well as comparative figures with portfolio measures ("comparative").
#' @param text A logical vector of whether in the case of single portfolios a textual report should be included.
#' @param width A number with the width of the device for individual portfolio figures. Defaults to 12 for the svg device.
#' @param height A number with the width of the device for individual portfolio figures. Defaults to 7 for the svg device.
#' @param comparative A character vector with the types of comparative plots to perform. If no comparative plot is desired, set it to NULL. Valid types of comparative plots are "temporal" (for showing time series of measures of portfolios) or "static" (for dotplots with the values of a single time period. If multiple years are passed and "static" is set, then the last year is used.
#' @param between A character vector indicating the variable to which the between-comparison must be performed. Defaults to "Sector". Not yet implemented.
#' @param within A character vector indicating the variable to which the between-comparison must be performed. Defaults to "Country". Not yet implemented.
#' @param dev_type_html Character. Character vector indicating the type of graphical device for the html output. By default, svg. See RMarkdown.
#' @param ... Other options passed to intermediate functions.
#' @return An html report.
#' @export
#' @examples
#'\dontrun{
#' data(PolicyPortfolio)
#' pp_report(P.education)
#'
#' pp_report(P.education, id = list(Country = "Borduria", Year = 2025))
#'}
pp_report <- function(D, id = NULL, file = NULL, title = NULL,
  plot = c("single", "comparative"), text = TRUE,
  width = 12, height = 7,
  comparative = c("temporal", "static"),
  between = "", within = "",
  dev_type_html = "svg", ...) {

  # Manage passing only certain portfolio identificators
  if (!is.null(id)) {
    D <- pass.id(D, id)
  }

  # Manage file names and extensions
  if (!is.null(file)) {
    file.extension.position <- regexpr("\\.([[:alnum:]]+)$", file)
    file.extension <- tolower(substr(file, file.extension.position + 1, nchar(file)))
    file.name <- substr(file, 1, file.extension.position - 1)
    output.html <- ifelse(file.extension == "html", TRUE, FALSE)
    file.html <- paste(file.name, ".html", sep = "")

    if (!output.html) {
      stop("File extension not known")
    }
    if (!(dev_type_html == "png" | dev_type_html == "svg")) {
      stop("Device type is not known")
    }
  } else {
    output.html <- TRUE
    file.name <- paste(format(Sys.time(), "%y%m%d-%H%M%S"), "-PolicyPortfolio-report", sep = "")
    file.html <- paste(file.name, ".html", sep = "")
  }
  file.rendered <- paste(file.name, "Rmd", sep = ".")

  t0 <- proc.time()
  object.name <- attributes(D)$description
  ratio <- 1.5

  # Write the header of the file
  sink(file.rendered)
  if (is.null(title)) {
    if (!is.null(id)) {
      title <- paste("Policy portfolio report for ", do.call("paste", c(id, sep = " : ")), sep = "")
    } else{
      title <- "Policy Portfolio Report"
    }
  }
  cat(paste(
    "---\ntitle: '",
    title, "'\n",
    "date: '`r Sys.time()`'\n",
    "author: '", Sys.getenv("USER"), "'\n",
    "output:\n  html_document:\n    toc: yes\n    self_contained: TRUE\n    dev: ",
    dev_type_html,
    "\n---\n\n",
    "```{r echo=FALSE}\nlibrary(PolicyPortfolios)\n",
    "load('tmp-PolicyPortfolios.RData')\n",
    sep = ""))
  if (dev_type_html == "png") {
    cat("library(knitr)\nopts_chunk$set(dev='png', dev.args=list(type='cairo'), dpi=72)\n")
  }
  if (dev_type_html == "svg") {
    cat("library(knitr)\nopts_chunk$set(dev='svg')\n")
  }
  cat("```\n")

  ## Pass the ggs() object in a temporal file
  tmp.data <- "tmp-PolicyPortfolios.RData"
  save(D, file = tmp.data)

  ##### Write the contents

  # Simply print each plot separately
  if ("single" %in% plot) {
    cat("\n# Single portfolio description\n")

    for (s in 1:length(levels(D$Sector))) {
      Sector <- levels(D$Sector)[s]
      for (c in 1:length(levels(D$Country))) {
        Country <- levels(D$Country)[c]
        for (y in 1:length(unique(D$Year))) {
          Year <- unique(D$Year)[y]
          DT <- pp_measures(D, id = list(Sector = Sector,  Country = Country, Year = Year))
          if (!is.null(DT)) {
            cat(paste("## ", paste(Country, Sector, Year, sep = " : ")), "\n", sep = "")

            # Report quantities of interest
            if (text) {
              DT <- pp_measures(D, id = list(Sector = Sector,  Country = Country, Year = Year))
              cat("\n\n")
              cat(paste("- *Space*: ", DT$value[DT$Measure == "Space"], "\n", sep = ""))
              cat(paste("- *Size*: ", round(DT$value[DT$Measure == "Size"], 3), "\n", sep = ""))
              cat(paste("- *Instruments*: ", DT$value[DT$Measure == "n.Instruments"], " (", round(DT$value[DT$Measure == "p.Instruments"] * 100, 2), "%)", "\n", sep = ""))
              cat(paste("- *Targets*: ", DT$value[DT$Measure == "n.Targets"], " (", round(DT$value[DT$Measure == "p.Targets"] * 100, 2), "%)", "\n", sep = ""))
              cat(paste("- *Instrument preponderance*: ", round(DT$value[DT$Measure == "In.Prep"], 3), "\n", sep = ""))
              cat(paste("- *Unique configurations (Instrument)*: ", DT$value[DT$Measure == "Unique"], "\n", sep = ""))
              cat(paste("- *Configuration equality (Instrument)*: ", round(DT$value[DT$Measure == "C.eq"], 3), "\n", sep = ""))
              cat(paste("- *Diversity (Gini-Simpsons)*: ", round(DT$value[DT$Measure == "Div.gs"], 3), "\n", sep = ""))
              cat(paste("- *Diversity (Shannon)*: ", round(DT$value[DT$Measure == "Div.sh"], 3), "\n", sep = ""))
              cat("\n\n")
            }

            # Produce figures
            cat(paste("```{r, echo = FALSE, fig.width = ", width, ", fig.height = ", height, "}\npp_plot(D, id = list(Country = \"", Country, "\", Sector = \"", Sector, "\", Year = \"", Year, "\"))\n```\n\n", sep = ""))
          }
        }
      }
    }
  }

  # Comparative plots
  if ("comparative" %in% plot) {
    if (is.null(between) | is.null(within)) {
      stop("Either 'between' or 'within' are NULL, and therefore I can't produce the relevant figures.\nPlease indicate which comparisons are relevant")
    }
    cat("\n# Comparative analysis\n")
    DM <- pp_measures(D)

    cat("```{r echo = FALSE}\nkable(unique(dplyr::select(DM, Measure, Label = Measure.label)))\n```")

    # When there is temporal dynamics
    if ("temporal" %in% comparative & length(unique(DM$Year)) > 1) {
      w <- 14; h <- 8
      cat("```{r echo = FALSE}
      f <- ggplot(DM, aes(x = Year, y = value, group = Country, color = Country)) +
        geom_line() +
        facet_wrap(~ Measure, scales = \"free_y\")
      ```\n\n")
      #cat(paste("```{r, echo = FALSE, fig.width = ", w, ", fig.height = ", h, "}\nprint(f)\n```\n\n", sep = ""))
      cat(paste("```{r, echo = FALSE, fig.width = ", w, ", fig.height = ", h, "}\nf\n```\n\n", sep = ""))
    }

    # For a dotplot with the status at the last period of time (if more than one Year is passed)
    if ("static" %in% comparative) {
      w <- 10; h <- 8
      cat("```{r echo = FALSE}
      DMY <- dplyr::filter(DM, Year == max(Year))
      f <- ggplot(DMY, aes(x = value, y = Country, color = Sector)) +
        geom_point() +
        facet_wrap(~ Measure, scales = \"free_x\")
      ```\n\n")
      #cat(paste("```{r, echo = FALSE, fig.width = ", w, ", fig.height = ", h, "}\nprint(f)\n```\n\n", sep = ""))
      cat(paste("```{r, echo = FALSE, fig.width = ", w, ", fig.height = ", h, "}\nf\n```\n\n", sep = ""))
    }
  }

  # Call rmarkdown to generate the report
  sink()
  rmarkdown::render(file.rendered)
  utils::browseURL(file.html)

  ## Close the files and clean the intermediate files
#  file.remove(file.rendered)
  file.remove(tmp.data)

  # Display messagges
  message(paste("Time taken to generate the report: ", signif((proc.time() - t0)[1], 2), " seconds.\n", sep = ""))
}
