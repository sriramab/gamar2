#install.packages("devtools")
#source('gamar.R')
#defpath("/Applications/gama_1_7_b2.app")
defpath("/Users/choisy/Applications/Gama")

#load experiment meta-data
rootexperiment <- getmodelparameter("/Users/nicolas/Desktop/gama_releaseB70/gama_1_7_b2.app/Contents/eclipse/headless/samples/models/predatorPrey.gaml","prey_predator")

#get output names
outputs <- getoutputnames(rootexperiment)

#get parameter names
parameters <- getparameternames(rootexperiment)

# 1 item <- prey initial number
# 5 item <- predator initial number
experimentclone1 <- setparametervalue(rootexperiment, parameters[1],30)
experimentclone1 <- setparametervalue(experimentclone1, parameters[5],40)
experimentclone1 <- setoutputframerate(experimentclone1,outputs[3],50)

experimentclone2 <- setparametervalue(rootexperiment, parameters[1],50)
experimentclone2 <- setparametervalue(experimentclone2, parameters[5],40)
experimentclone2 <- setoutputframerate(experimentclone2,outputs[3],50)
experimentclone2 <- setexperimentid(experimentclone2,2)

experimentclone3 <- setparametervalue(rootexperiment, parameters[1],70)
experimentclone3 <- setparametervalue(experimentclone3, parameters[5],40)
experimentclone3 <- setoutputframerate(experimentclone3,outputs[3],50)
experimentclone3 <- setexperimentid(experimentclone3,3)


experimentclone4 <- setparametervalue(rootexperiment, parameters[1],90)
experimentclone4 <- setparametervalue(experimentclone4, parameters[5],40)
experimentclone4 <- setoutputframerate(experimentclone4,outputs[3],50)
experimentclone4 <- setexperimentid(experimentclone4,4)

experimentclone5 <- setparametervalue(rootexperiment, parameters[1],120)
experimentclone5 <- setparametervalue(experimentclone5, parameters[5],40)
experimentclone5 <- setoutputframerate(experimentclone5,outputs[3],50)
experimentclone5 <- setexperimentid(experimentclone5,2)

experimentclone6 <- setparametervalue(rootexperiment, parameters[1],130)
experimentclone6 <- setparametervalue(experimentclone6, parameters[5],40)
experimentclone6 <- setoutputframerate(experimentclone6,outputs[3],50)
experimentclone6 <- setexperimentid(experimentclone6,6)

experimentclone7 <- setparametervalue(rootexperiment, parameters[1],170)
experimentclone7 <- setparametervalue(experimentclone7, parameters[5],40)
experimentclone7 <- setoutputframerate(experimentclone7,outputs[3],5)
experimentclone7 <- setexperimentid(experimentclone7,7)

experimentplan <- addtoexperimentplan(experimentclone1)
experimentplan <- addtoexperimentplan(experimentclone2,experimentplan)
experimentplan <- addtoexperimentplan(experimentclone3,experimentplan)
experimentplan <- addtoexperimentplan(experimentclone4,experimentplan)
experimentplan <- addtoexperimentplan(experimentclone5,experimentplan)
experimentplan <- addtoexperimentplan(experimentclone6,experimentplan)
experimentplan <- addtoexperimentplan(experimentclone7,experimentplan)

startexperimentplan(experimentplan,hpc=8)


