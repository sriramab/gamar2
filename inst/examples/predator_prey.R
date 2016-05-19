library(gamar)
defpath("/Applications/Gama.app")
experiment1 <- getmodelparameter("inst/examples/predator_prey/models/predator_prey.gaml","prey_predator")
getparameternames(experiment1)
getoutputnames(experiment1)
experiment1 <- setparametervalue(experiment1,"Initial number of preys: ",990)
experiment1 <- setparametervalue(experiment1,"Initial number of predators: ",100)
experiment1 <- setparametervalue(experiment1,"Predator probability reproduce: ",0.1)
experiment1 <- setfinalstep(experiment1,100)
experimentplan <- addtoexperimentplan(experiment1)
# alternatively, running it automatically:
output <- runexpplan(experimentplan,1)
with(output[[1]],plot(step,`Number of preys`,type="l",lwd=2,col="red"))

library(animation)
mydir <-attr(output[[1]],"path")
# ERROR DO NOT RETURN A PATH BUT A FILE
mydir <- "/Users/jdz/git/gamar/workgamar/out_prey_predator_1/snapshot/"
# reading my png files
my.files <- output[[1]]$info_display
my.files2 <- unlist(lapply(my.files, function(x) paste0(mydir,x)))


#interval = 0.1
#a positive number to set the time interval of the animation (unit in seconds); default to be 1.
#nmax = length(output[[1]]$info_display)
#maximum number of steps in a loop (e.g. iterations) to create animation frames. Note: the actual number of frames can be less than this number, depending on specific animations. Default to be 50.
#ani.width = 480
#ani.height= 480
#width and height of image frames (unit in px); see graphics devices like png, jpeg, ...; default to be 480. NB: for different graphics devices, the units of these values might be different, e.g. PDF devices usually use inches, whereas bitmap devices often use pixels.
#imgdir = 'images'
#character: the name of the directory (a relative path) for images when creating HTML animation pages; default to be 'images'.
#htmlfile = 'index.html'
#character: name of the target HTML main file (without path name; basename only; default to be 'index.html')
#ani.dev = 'png'
#a function or a function name: the graphics device; e.g. (png, pdf, ...); default to be 'png'
#ani.type = 'gif'
#character: image format for animation frames, e.g. png, jpeg, ...; default to be 'png'; this will be used as the file extension of images, so don't forget to change this option as well when you changed the option ani.dev
#title  = ''
#description = ''
#character: the title and description of the animation in the HTML page created by saveHTML
#verbose = FALSE
#logical or character: if TRUE, write a footer part in the HTML page containing detailed technical information; if given a character string, it will be used as the footer message; in other cases, the footer of the page will be blank.
#loop = TRUE
#whether to iterate or not (default TRUE to iterate for infinite times)
#autobrowse = TRUE
#logical: whether auto-browse the animation page immediately after it is created? (default to be interactive())
#autoplay = TRUE
#logical: whether to autoplay the animation when the HTML page is loaded (default to be TRUE); only applicable to saveHTML
oopt = ani.options(interval = 0.01, loop = TRUE, autobrowse = FALSE, autoplay = TRUE, nmax = length(output[[1]]$info_display))
im.convert(files = my.files2,output=paste0(mydir,"animation.gif"))
