##############################################################################
# Local PARTITIONS                                                          #
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
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri           #
# Ferrandin | Prof. Dr. Celine Vens | Dr. Felipe Nakano Kenji                #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium               #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
##############################################################################




###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/Local-Partitions"
FolderScripts = "~/Local-Partitions/R"


##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             # 
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
run.clus <- function(parameters){
  
  setwd(FolderScripts)
  source("local-clus.R")
  
  
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
  
  setwd(parameters$Directories$FolderNamesLabels)
  nome = paste(parameters$Config.File$Dataset.Name, "-NamesLabels.csv", sep="")
  nameslabels = data.frame(read.csv(nome))
  names(nameslabels) = c("indice", "labels")
  parameters$NamesLabel = nameslabels
  
  
  cat("\n\n#################################################################################")
  cat("\n# RUN: Joins the configuration, training and test files for running the clus      #")
  cat("\n###################################################################################\n\n")
  time.gather.files = system.time(gather.files.clus(parameters))
  
  
  cat("\n\n############################################################")
  cat("\n# RUN: Execute Clus Local                                   #")
  cat("\n##############################################################\n\n")
  time.execute = system.time(execute.clus(parameters))
  
  
  cat("\n\n############################################################")
  cat("\n# RUN: gather predicstion                                   #")
  cat("\n##############################################################\n\n")
  time.gather.preds = system.time(gather.predicts.clus(parameters))
  
  
  cat("\n\n############################################################")
  cat("\n# RUN: Evaluates Local Partitions                           #")
  cat("\n##############################################################\n\n")
  time.evaluate = system.time(evaluate.clus(parameters))
  
  
  cat("\n\n############################################################")
  cat("\n# RUN: Gather Evaluated Measures                             #")
  cat("\n##############################################################\n\n")
  time.gather.eval = system.time(gather.eval.clus(parameters))
  
  
  cat("\n\n############################################################")
  cat("\n# RUN: Save Runtime                                          #")
  cat("\n##############################################################\n\n")
  RunTimeLocal = rbind(time.gather.files, time.execute, 
                        time.gather.preds, time.evaluate,
                        time.gather.eval)
  
  setwd(parameters$Directories$FolderLocal)
  write.csv(RunTimeLocal, paste(dataset_name, 
                                 "-RunTime-Clus.csv", sep=""))
  
  
  cat("\n\n############################################################")
  cat("\n# RUN: Stop Parallel                                         #")
  cat("\n##############################################################\n\n")
  parallel::stopCluster(cl) 	
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################