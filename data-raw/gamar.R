defpath <- function(path) {
  os <- paste0(Sys.info()["sysname"])
# if we use OSX, the plugin is located in the Contents/eclipse sub directory of
# gama otherwise it is at its root:
  subpath <- ifelse(os=="Darwin","/Contents/eclipse","")
  gamapath <- paste0(path,subpath,"/plugins")
  plugins <- grep("org.eclipse.equinox.launcher_.*",dir(gamapath),value=T)
  options(gamar.plugins = paste(paste0(gamapath,"/",plugins),collapse=":"))
  defaultjar <- paste0(gamapath,"/",plugins)
  options(gamar.startjar=defaultjar)
  options(gamar.Xmx="2048m")
  options(gamar.Xms="512m")
}


# working directory:
createworkingdirectory <- function() {
  outdirectory <- paste0(getwd(),"/workgamar")
  if(!file.exists(outdirectory)) dir.create(outdirectory)
  outdirectory
}


# returns the name of an available subfolder:
createmodelparameterfilename <- function(experimentname) {
  outdirectory <- createworkingdirectory()
  i <- 0
  repeat {
    i <- i + 1
    outfile <- paste0(outdirectory,"/",experimentname,"_",i,".xml")
    if(!file.exists(outfile)) break
  }
  outfile
}


getdefaultexperimentplanname <- function(experimentplan)
  experimentplan$Simulation$.attrs["experiment"]


# output directory name:
createoutputdirectoryname <- function(experimentplan) {
  outdirectory <- createworkingdirectory()
  defaultname <- getdefaultexperimentplanname(experimentplan)
  i <- 0
  repeat {
    i <- i + 1
    outfile <- paste0(outdirectory,"/out_",defaultname,"_",i)
    if(!file.exists(outfile)) break
  }
  outfile
}


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

#' store experimentplan
#'
#' Save an experiment plan to an xml file. it returns the path of the xml file
#'
#' @inheritParams run
#' @param experimentplan  experiment plan to store
writemodelparameterfile <- function(experimentplan) {
  outfile <- createmodelparameterfilename(getdefaultexperimentplanname(experimentplan))
  xml <- buildxmlfromexperimentplan(experimentplan)
  write(xml,outfile,sep="")
  outfile
}


startexperimentplan <- function(experimentplan,hpc=1,outputdirectory="") {
  parameterxmlfile <- writemodelparameterfile(experimentplan)
  if(outputdirectory=="")
    outputdirectory <- createoutputdirectoryname(experimentplan)
  trycommand <- system(paste0("java -jar ",getOption("gamar.startjar")," -Xms",
                              getOption("gamar.Xms")," -Xmx",getOption("gamar.Xmx"),
                              " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                              "-application msi.gama.headless.id4 -hpc ",hpc," ",
                              parameterxmlfile," ",outputdirectory),
                       ignore.stdout=F,ignore.stderr=T)
  if(trycommand>0) return(-1)
}


getsimulationparam <- function(attrnam,dictionary)
  dictionary$Simulation$.attrs[attrnam]


setsimulationparam <- function(attrnam,dictionary,value) {
  dictionary$Simulation$.attrs[attrnam] <- value
  dictionary
}


getsimulationid <- function(dictionary)
  getsimulationparam("id",dictionary)


setexperimentid <- function(dictionary,value)
  setsimulationparam("id",dictionary,value)


getexperimentname <- function(dictionary)
  getsimulationparam("experiment",dictionary)


setexperimentname <- function(dictionary,value)
  setsimulationparam("experiment",dictionary,value)


getseed <- function(dictionary)
  getsimulationparam("seed",dictionary)


setseed <- function(dictionary,value)
  setsimulationparam("seed",dictionary,value)


getmodelpath <- function(dictionary)
  getsimulationparam("sourcePath",dictionary)


setmodelpath <- function(dictionary,value)
  setsimulationparam("sourcePath",dictionary,value)


getfinalstep <- function(dictionary)
  getsimulationparam("finalStep",dictionary)


setfinalstep <- function(dictionary,value)
  setsimulationparam("finalStep",dictionary,value)


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


addoutput <- function(dictionary,name,framerate) {
  id <- length(dictionary$Simulation$Outputs)
  Output <- c("framerate"=framerate,"id"=id,"name"=name)
  dictionary$Simulation$Outputs <- append(dictionary$Simulation$Outputs,
                                          list("Output"=Output))
  dictionary
}


removeoutput <- function(dictionary,name)
{
  siminput <- dictionary$Simulation$Outputs
  i <- 0
  while(i<length(siminput)) {
    if(name==dictionary$Simulation$Outputs[i]$Output["name"]) {
      dictionary$Simulation$Outputs <- dictionary$Simulation$Outputs[-i]
      dictionary
    }
    i <- i + 1
  }
}


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


addtoexperimentplan <- function(simulation,experimentplan=list()) {
  experimentplan <- append(experimentplan,simulation)
  experimentplan
}


buildxmlfromexperimentplan <- function(experimentplan) {
  out <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><Experiment_plan>"
  i <- 0
  while(i<length(experimentplan)) {
    out <-paste0(out,buildxmlfromsimulation(experimentplan[i]$Simulation))
    i <- i + 1
  }
  paste0(out,"</Experiment_plan>")
}


buildxmlfromsimulation <- function(sim) {
  siminput <- sim$Parameters
  simoutput <- sim$Outputs
  experimentname <- sim$.attrs["experiment"]
  finalstep <- sim$.attrs["finalStep"]
  id <- sim$.attrs["id"]
  seed <- sim$.attrs["seed"]
  sourcepath <- sim$.attrs["sourcePath"]
  result <- paste0("<Simulation id=\"",id,"\" sourcePath=\"",sourcepath,
                   "\" experiment=\"",experimentname,"\" finalStep=\"",
                   finalstep,"\" seed=\"",seed,"\">")
  i <- 0
  result <- paste0(result,"<Parameters>")
  while(i<length(siminput)) {
    name <- siminput[i]$Parameter["name"]
    type <- siminput[i]$Parameter["type"]
    value <- siminput[i]$Parameter["value"]
    result <- paste0(result,buildxmlfromparameter(name,type,value))
    i <- i + 1
  }
  result <- paste0(result,"</Parameters><Outputs>")
  i <- 0
  while(i<length(simoutput)) {
    name <- simoutput[i]$Output["name"]
    id <- simoutput[i]$Output["id"]
    framerate <- simoutput[i]$Output["framerate"]
    result <- paste0(result,buildxmlfromoutput(name,id,framerate))
    i <- i + 1
  }
  result <- paste0(result,"</Outputs></Simulation>")
  return(result)
}


buildxmlfromparameter <- function(name,type,value)
  paste0("<Parameter name=\"",name,"\" type=\"",type,"\" value=\"",value,"\"/>")


buildxmlfromoutput <- function(name,id,framerate)
  paste0("<Output name=\"",name,"\" id=\"",id,"\" framerate=\"",framerate,"\"/>")
