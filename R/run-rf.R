##############################################################################
# LOCAL PARTITIONS                                                           #
# Copyright (C) 2023                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - PhD Elaine Cecilia Gatto | Prof PhD Ricardo Cerri                      #
# 2 - Prof PhD Mauri Ferrandin                                               #
# 3 - Prof PhD Celine Vens | PhD Felipe Nakano Kenji                         #
# 4 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            #
# 2 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 3 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium           #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 4 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################


###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/Local-Partitions"
FolderScripts = "~/Local-Partitions/R"


###########################################################################
#
###########################################################################
run.rf <- function(parameters){
  
  FolderRoot = "~/Local-Partitions"
  FolderScripts = "~/Local-Partitions/R"
  
  setwd(FolderScripts)
  source("local-rf.R")
  
  if(parameters$Config.File$Number.Cores == 0){
    
    cat("\n\n##########################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please      #")
    cat("\n# choose a value greater than or equal to 1.               #")
    cat("\n############################################################\n\n")
    
  } else {
    
    cl <- parallel::makeCluster(parameters$Config.File$Number.Cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(parameters$Config.File$Number.Cores==1){
      cat("\n\n########################################################")
      cat("\n# Running Sequentially!                                #")
      cat("\n########################################################\n\n")
    } else {
      cat("\n\n############################################################")
      cat("\n# Running in parallel with ", parameters$Config.File$Number.Cores, " cores! #")
      cat("\n############################################################\n\n")
    }
  }
  
  
  cl = cl
  
  retorno = list()
  
  
  cat("\n\n##################################################")
    cat("\n# RUN: Names Labels                              #")
    cat("\n##################################################\n\n")
  name.file = paste(parameters$Directories$FolderNamesLabels, "/",
                    parameters$Config.File$Dataset.Name,
                    "-NamesLabels.csv", sep="")
  labels.names = data.frame(read.csv(name.file))
  names(labels.names) = c("Index", "Labels")
  parameters$Names.Labels = labels.names
  
  
  cat("\n\n#################################################")
  cat("\n# RUN: label space                                #")
  cat("\n###################################################\n\n")
  time.ls = system.time(resLS <- labelSpace(parameters))
  parameters$LabelSpace = resLS
  
  
  cat("\n\n############################################")
  cat("\n# RUN: label dependency                      #")
  cat("\n##############################################\n\n")
  time.dependency = system.time(compute.label.dependecy(parameters))
  
  
  cat("\n\n####################################################")
    cat("\n# RUN: Execute Random Forests                      #")
    cat("\n####################################################\n\n")
  time.execute = system.time(execute.local.python(parameters))
  
  
  cat("\n\n##########################################################")
  cat("\n# RUN: Evaluate                                          #")
  cat("\n##########################################################\n\n")
  time.evaluate = system.time(evaluate.local.python(parameters))
  
  
  cat("\n\n##########################################################")
    cat("\n# RUN: Gather Evaluated Measures                         #")
    cat("\n##########################################################\n\n")
  time.gather.evaluate = system.time(gather.eval.local.python(parameters))
  
  
  cat("\n\n###########################################################")
    cat("\n# RUN: Save Runtime                                       #")
    cat("\n###########################################################\n\n")
  RunTimeGlobal = rbind(time.dependency, time.execute, time.evaluate, 
                        time.gather.evaluate)
  setwd(parameters$Directories$FolderLocal)
  write.csv(RunTimeGlobal, "Run-RunTime.csv")
  
  
  cat("\n\n###########################################################")
    cat("\n# RUN: Stop Parallel                                      #")
    cat("\n###########################################################\n\n")
  parallel::stopCluster(cl) 	
  
}


###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #
###############################################################################
