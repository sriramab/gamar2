buildxmlfromexperimentplan <- function(experimentplan) {
  out <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><Experiment_plan>"
  i <- 0
  while(i<length(experimentplan)) {
    out <-paste0(out,buildxmlfromsimulation(experimentplan[i]$Simulation))
    i <- i + 1
  }
  paste0(out,"</Experiment_plan>")
}

################################################################################

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

################################################################################

buildxmlfromparameter <- function(name,type,value)
  paste0("<Parameter name=\"",name,"\" type=\"",type,"\" value=\"",value,"\"/>")

################################################################################

buildxmlfromoutput <- function(name,id,framerate)
  paste0("<Output name=\"",name,"\" id=\"",id,"\" framerate=\"",framerate,"\"/>")
