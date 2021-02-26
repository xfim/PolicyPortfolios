# .onLoad <- function(libname, pkgname) 
# {
#   library.dynam("PolicyPortfolios", pkgname, libname)
# }

PolicyPortfoliosStartupMessage <- function()
{
  msg <- c(paste0("PolicyPortfolios version ", 
           utils::packageVersion("PolicyPortfolios")),
           "\nType 'citation(\"PolicyPortfolios\")' for citing this R package\nor its datasets in publications.")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- PolicyPortfoliosStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'PolicyPortfolios' version", utils::packageVersion("PolicyPortfolios"))
  packageStartupMessage(msg)      
  invisible()
}
