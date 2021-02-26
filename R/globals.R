# Hack to avoid NOTES in R CMD check
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Country", "Sector", "Year", "Instrument", "Target", "covered",  # from the tidy dataset
    "direction", "cc", # from pp_clean()
    "Measure", "Measure.label", # from pp_measures()
    "comparative", "dev_type_html", # from pp_report()
    "pdf", "dev.off", # from grDevices, for pp_plot()
    "na.omit", # from stats() in pp_clean()
    "N", # from pp_similarity()
    "Country.destination", "Year.destination", # from pp_similarity()
    "Method", "Method.label", # from pp_similarity()
    "value", # from pp_similarity()
    "error", # from pp_similarity()
    "packageVersion", # from utils, for startup message
    "browseURL" # from utils, for pp_plot()
  ))
}
