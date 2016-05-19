makemovie <- function(experiment,output="animation.gif",...) {
  mydir <- paste0(dirname(attr(experiment,"path")),"/snapshot/")
  info_display <- experiment$info_display
  my_files <- unlist(lapply(info_display,function(x)paste0(mydir,x)))
  animation::ani.options(interval=.01,loop=T,autobrowse=F,autoplay=T,
                       nmax=length(info_display))
  animation::ani.options(...)
  animation::im.convert(files=my_files,output=paste0(getwd(),"/",output))
}
