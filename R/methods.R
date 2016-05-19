print.experiment <- function(object) {
  object <- object[[1]]
  tmp <- do.call(rbind,object$Parameters)
  parameters <- as.numeric(tmp[,"value"])
  names(parameters) <- tmp[,"name"]
  tmp <- do.call(rbind,object$Outputs)
  framerate <- as.integer(tmp[,"framerate"])
  names(framerate) <- tmp[,"name"]
  cat("\nModel parameters:\n")
  print(parameters)
  cat("\nOutputs frame rates:\n")
  print(framerate)
  cat("\n")
  invisible(object)
}
