getsimulationparam <- function(attrnam,dictionary)
  dictionary$Simulation$.attrs[attrnam]

################################################################################

setsimulationparam <- function(attrnam,dictionary,value) {
  dictionary$Simulation$.attrs[attrnam] <- value
  dictionary
}

################################################################################

getsimulationid <- function(dictionary)
  getsimulationparam("id",dictionary)

################################################################################

setexperimentid <- function(dictionary,value)
  setsimulationparam("id",dictionary,value)

################################################################################

getexperimentname <- function(dictionary)
  getsimulationparam("experiment",dictionary)

################################################################################

setexperimentname <- function(dictionary,value)
  setsimulationparam("experiment",dictionary,value)

################################################################################

getseed <- function(dictionary)
  getsimulationparam("seed",dictionary)

################################################################################

setseed <- function(dictionary,value)
  setsimulationparam("seed",dictionary,value)

################################################################################

getmodelpath <- function(dictionary)
  getsimulationparam("sourcePath",dictionary)

################################################################################

setmodelpath <- function(dictionary,value)
  setsimulationparam("sourcePath",dictionary,value)

################################################################################

getfinalstep <- function(dictionary)
  getsimulationparam("finalStep",dictionary)

################################################################################

setfinalstep <- function(dictionary,value)
  setsimulationparam("finalStep",dictionary,value)

################################################################################

getoutputnames <- function(dictionary) {
  simoutput <- dictionary$Simulation$Outputs
  i <- 0
  outlist <- list()
  while(i<length(simoutput)) {
    out <- simoutput[i]$Output["name"]
    outlist <- append(outlist,out)
    i <- i + 1
  }
  as.character(outlist)
}

################################################################################

getparameternames<- function(dictionary) {
  siminput <- dictionary$Simulation$Parameters
  i <- 0
  outlist <- list()
  while(i < length(siminput)) {
    out <- siminput[i]$Parameter["name"]
    outlist <- append(outlist,out)
    i <- i + 1
  }
  as.character(outlist)
}

################################################################################

setparametervalue <- function(dictionary,name,value) {
  siminput <- dictionary$Simulation$Parameters
  i <- 0
  while(i<length(siminput)) {
    if(name == dictionary$Simulation$Parameters[i]$Parameter["name"])
      dictionary$Simulation$Parameters[i]$Parameter["value"] <- value
    i <- i + 1
  }
  dictionary
}

################################################################################

setoutputframerate <- function(dictionary,name,value) {
  siminput <- dictionary$Simulation$Outputs
  outlist <- list()
  i <- 0
  while(i<length(siminput)) {
    if(name != dictionary$Simulation$Outputs[i]$Output["name"] )
    { dictionary$Simulation$Outputs[i]$Output["framerate"] <- value }
    i <- i + 1
  }
  dictionary
}

################################################################################

getdefaultexperimentplanname <- function(experimentplan)
  experimentplan$Simulation$.attrs["experiment"]

################################################################################

#' Get parameters of a gama model
#'
#' Load experiment meta-data from a model file and an experimentname
#'
#' @inheritParams run
#' @param modelfile relative or absolute path pointing on your model file
#' @param experimentname name of the loaded experiment
#' @keywords internal
getmodelparameter <- function(modelfile,experimentname) {
  outfile <- createmodelparameterfilename(experimentname)
  trycommand <- system(paste0("java -jar ",getOption("gamar.startjar")," -Xms",
                              getOption("gamar.Xms")," -Xmx",
                              getOption("gamar.Xmx"),
                              " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                              "-application msi.gama.headless.id4 -xml ",
                              experimentname," ",modelfile," ",outfile),
                       ignore.stdout=T,ignore.stderr=T)
  if(trycommand>0) return(-1)
  XML::xmlToList(XML::xmlParse(outfile))
}
