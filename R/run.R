#' Check a directory
#'
#' Checks the existance of the directory "dir". If it exists, it renames it.
#'
#' @param dir Name of the directory to check and optionally rename.
#' @keywords internal
checkdir <- function(dir) {
  if(dir.exists(dir)) {
# Looking for the available index:
    index <- 2
    while(dir.exists(paste0(dir,index))) index <- index+1
# Renaming the 'output' directory:
    file.rename(dir,paste0(dir,index))
# Warning message:
    warning(paste0("Directory 'output' has been renamed in 'output",index,"'."))
  }
}

################################################################################

#' Call GAMA
#'
#' Call the simualation platform GAMA to run an experiment described in an XML
#' file.
#'
#' @inheritParams run
#' @param output Name of the output directory.
#' @keywords internal
callgama <- function(experiment,output="output",Xms=512,Xmx=2048) {
  system(paste0("java -cp ",getOption("gamar.plugins")," -Xms",Xms,"m -Xmx",Xmx,
                "m ","-Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                "-application msi.gama.headless.id4 ",normalizePath(experiment),
                " ",getwd(),"/",output),ignore.stdout=T,ignore.stderr=T)
}

################################################################################

#' Reads a GAMA output file
#'
#' Reads a GAMA output in an XML file and return in the form of a datarame with
#' one column per "monitor".
#'
#'  @param file Name of the XML file to read.
#'  @keywords internal
readxml <- function(file) {
  out <- XML::xmlToList(XML::xmlParse(file))
  out[[length(out)]] <- NULL # remove the last item that is NA
  n <- length(out[[1]]) # number of variable slots per time step
# (we remove this last empty slot in the following two lines)
  the_names <- sapply(out[[1]],function(x)x[1])[-n]
  out <- sapply(out,function(y)as.numeric(sapply(y,function(x)x[2])))[-n,]
  out <- as.data.frame(t(out))
  names(out) <- the_names
  rownames(out) <- NULL
# Returning the output:
  return(out)
}

################################################################################

#' Run a GAMA experiment
#'
#' Run a GAMA experiment from an XML file.
#'
#' If _experiment_ is an XML, it should contains the path to a GAML
#' file that describes the model. A GAML file starts with a model name and
#' should contain three sections defining (1) the world (global), (2) the
#' species (species), and (3) the experiments (experiment). The XML file should
#' contain a Simulation header and two slots: (1) Parameters et (2) Outputs. See
#' the _demo_ folder of the _gamar_ library for examples of such files. See
#' \url{https://github.com/gama-platform/gama/wiki/G__Headless} for more
#' detailed information on how these XML and GAML files should be structured.
#'
#' @param experiment Either an R experiment object or the path to an XML file
#' describing the experiment.
#' @param Xms Initial memory allocation pool for the Java Virtual Machine.
#' @param Xmx Maximal memory allocation pool for the Java Virtual Machine.
#' @return A data frame with, for each column, the values of the model state
#' variables for each time step.
#' @author Marc Choisy, Jean-Daniel Zucker
#' @export
#' @examples
#' ## Running an SIR epidemic model.
#' # Copying the example files to the working directory:
#' file.copy(system.file("examples/sir.xml",package="gamar"),".")
#' file.copy(system.file("examples/sir.gaml",package="gamar"),".")
#' # Running the experiment:
#' simulation <- run("sir.xml")
#' # Removing the example files from the working directory:
#' file.remove("sir.xml","sir.gaml")
#' # Plotting the simulations output:
#' with(simulation, {
#'   plot(Susceptible,col="blue",type="l",xlab="time",ylab="number of individuals")
#'   lines(Infectious,col="red")
#'   lines(Recovered,col="green")
#' })
#' # Adding the legend:
#' legend("right",c("susceptible","infected","recovered"),
#'   col=c("blue","red","green"),lty=1,bty="n")
run <- function(experiment,Xms=512,Xmx=2048) {
# TO DO: with the version 1.7.0 of GAMA, the index of the simulation-outputs XML
# file should be read from the "experiment" XML input file.
  output <- "output" # name of the temporary folder.
  outxml <- "simulation-outputs0.xml" # name of the XML output file.
# Checking that there is no "output" folder:
  checkdir(output)
# Running GAMA:
  callgama(experiment,output,Xms,Xmx)
# Reading the output:
  out <- readxml(paste0(output,"/",outxml))
## Cleaning:
  unlink(output,T,T)
## Returning the output:
  return(out)
}
