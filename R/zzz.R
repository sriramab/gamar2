#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

defpath <- function(path) {
  gamapath <- paste0(path,"/plugins")
  plugins <- grep("*.jar",dir(gamapath),value=T)
  options(gamar.plugins=paste(paste0(gamapath,"/",plugins),collapse=":"))
}

.onAttach <- function(...) {
  packageStartupMessage("Welcome to gamar v0.1!")
  packageStartupMessage("WARNING: GAMA 1.6.2 needs to be installed on your machine.")
  packageStartupMessage("Check www.gama-platform.org for installation instructions.")
  defpath("/Users/choisy/Applications/Gama")
}

.onDetach <- function(...) {
  options(gamar.plugins=NULL)
}
