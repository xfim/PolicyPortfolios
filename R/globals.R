# Hack to avoid NOTES in R CMD check
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Country", "Sector", "Year", "Instrument", "Target", "covered",  # from the tidy dataset
    "direction", "cc", # from pp_clean()
    "Measure", "Measure.label", # from pp_measures()
    "comparative", "dev_type_html", # from pp_report()
    "pdf", "dev.off", # from grDevices, for pp_plot()
    "na.omit", # from stats() in pp_clean()
    "browseURL" # from utils, for pp_plot()
  ))
}
