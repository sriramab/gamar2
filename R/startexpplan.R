# working directory:
createworkingdirectory <- function() {
  outdirectory <- paste0(getwd(),"/workgamar")
  if(!file.exists(outdirectory)) dir.create(outdirectory)
  outdirectory
}

################################################################################

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

################################################################################

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

################################################################################

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

################################################################################

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
  return(dir(path = parameterxmlfile, pattern = "*.xml"), full.names = TRUE)
}
