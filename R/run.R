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
executeLP <- function(ds, 
                      dataset_name,
                      number_dataset, 
                      number_cores, 
                      number_folds, 
                      folderResults){
  
  diretorios = directories(dataset_name, folderResults)
  
  if(number_cores == 0){
    
    cat("\n\n##########################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please      #")
    cat("\n# choose a value greater than or equal to 1.               #")
    cat("\n############################################################\n\n")
    
  } else {
    
    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n##########################################################")
      cat("\n# Running Sequentially!                                    #")
      cat("\n############################################################\n\n")
    } else {
      cat("\n\n############################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!         #")
      cat("\n##############################################################\n\n")
    }
  }
  cl = cl
  
  setwd(diretorios$folderNamesLabels)
  namesLabels = data.frame(read.csv(paste(dataset_name,
                                          "-NamesLabels.csv", sep="")))
  namesLabels = c(namesLabels$x)
  
  if(package=="mulan"){
    
    setwd(FolderScripts)
    source("local-mulan.R")
    
    str0 = "~/Local-Partitions/Reports"
    if(dir.exists(str0)==FALSE){dir.create(str0)}
    
    str1 = paste(str0, "/mulan", sep="")
    if(dir.exists(str1)==FALSE){dir.create(str1)}
    
    cat("\n\n############################################################")
    cat("\n# RUN LOCAL: Execute BR MULAN                               #")
    cat("\n##############################################################\n\n")
    time.execute = system.time(execute.mulan(ds, 
                                             dataset_name,
                                             number_dataset, 
                                             number_cores, 
                                             number_folds, 
                                             folderResults))
    
    
    cat("\n\n####################################################")
    cat("\n# RUN LOCAL: Evaluates BR MULAN                      #")
    cat("\n######################################################\n\n")
    time.evaluate = system.time(evaluate.mulan(ds, dataset_name, 
                                               number_folds, folderResults))
    
    
    cat("\n\n##################################################")
    cat("\n# RUN LOCAL: Gather BR MULAN                       #")
    cat("\n###################################################\n\n")
    time.gather = system.time(gather.mulan(ds, dataset_name, number_folds, 
                                           folderResults))
    
    cat("\n\n##########################################################")
    cat("\n# RUN LOCAL: Save Runtime                                  #")
    cat("\n############################################################\n\n")
    RunTimeLOCAL = rbind(time.execute, time.evaluate, time.gather)
    setwd(diretorios$folderLocal)
    write.csv(RunTimeLOCAL, paste(dataset_name, "-run-runtime.csv", sep=""))
    
    
  } else if(package=="utiml") {
    
    setwd(FolderScripts)
    source("local-utiml.R")
    
    str0 = "~/Local-Partitions/Reports"
    if(dir.exists(str0)==FALSE){dir.create(str0)}
    
    str1 = paste(str0, "/utiml", sep="")
    if(dir.exists(str1)==FALSE){dir.create(str1)}
    
    
    cat("\n\n#########################################################")
      cat("\n# RUN LOCAL: Execute BR UTIML                           #")
      cat("\n#########################################################\n\n")
    time.execute = system.time(execute.utiml(ds, 
                                             dataset_name,
                                             number_dataset, 
                                             number_cores, 
                                             number_folds, 
                                             folderResults))
    
    
    cat("\n\n##################################################")
      cat("\n# RUN LOCAL: Evaluates BR UTIML                  #")
      cat("\n##################################################\n\n")
    time.evaluate = system.time(evaluate.utiml(ds, 
                                               dataset_name, 
                                               number_folds, 
                                               folderResults))
    
    
    
    cat("\n\n#################################################")
      cat("\n# RUN LOCAL: Gather BR UTIML                    #")
      cat("\n#################################################\n\n")
    time.gather = system.time(gather.utiml(ds, 
                                           dataset_name, 
                                           number_folds, 
                                           folderResults))
    
    
    
    cat("\n\n########################################################")
      cat("\n# RUN LOCAL: Save BR UTIML Runtime                     #")
      cat("\n########################################################\n\n")
    RunTimeLOCAL = rbind(time.execute, time.evaluate, time.gather)
    setwd(diretorios$folderLocal)
    write.csv(RunTimeLOCAL, paste(dataset_name, 
                                  "-Runtime-Utiml.csv", sep=""))
    
    
  } else if(package=="clus"){
    
    setwd(FolderScripts)
    source("local-clus.R")
    
    str0 = "~/Local-Partitions/Reports"
    if(dir.exists(str0)==FALSE){dir.create(str0)}
    
    str1 = paste(str0, "/clus", sep="")
    if(dir.exists(str1)==FALSE){dir.create(str1)}
    
    
    cat("\n\n############################################################")
      cat("\n# LOCAL: Joins training and test files in a single folder for running the clus #")
      cat("\n############################################################\n\n")
    time.gather.files = system.time(gather.files.clus(ds,
                                                      dataset_name,
                                                      number_dataset,
                                                      number_cores,
                                                      number_folds,
                                                      namesLabels,
                                                      folderResults))
    
    
    cat("\n\n##########################################################")
      cat("\n# LOCAL: Split the labels                                #")
      cat("\n##########################################################\n\n")
    time.execute = system.time(execute.clus(ds,
                                            dataset_name,
                                            number_dataset,
                                            number_cores,
                                            number_folds,
                                            namesLabels,
                                            folderResults))
    
    
    cat("\n\n##############################################################")
      cat("\n# LOCAL: Splits the real outputs and the predicted outputs   #")
      cat("\n##############################################################\n\n")
    time.gather.preds = system.time(gather.preds.clus(ds,
                                                      dataset_name,
                                                      number_dataset,
                                                      number_cores,
                                                      number_folds,
                                                      namesLabels,
                                                      folderResults))
    
    
    cat("\n\n###########################################################")
      cat("\n# LOCAL: Evaluates Local Partition                        #")
      cat("\n###########################################################\n\n")
    time.evaluate = system.time(evaluate.clus(ds,
                                              dataset_name,
                                              number_dataset,
                                              number_cores,
                                              number_folds,
                                              namesLabels,
                                              folderResults))
    
    
    cat("\n\n########################################################")
      cat("\n# LOCAL: Gather Evaluated Measures                     #")
      cat("\n########################################################\n\n")
    time.gather = system.time(gather.eval.clus(ds,
                                               dataset_name,
                                               number_dataset,
                                               number_cores,
                                               number_folds,
                                               folderResults))
    
    
    cat("\n\n#########################################################")
      cat("\n# LOCAL: Save Runtime                                   #")
      cat("\n#########################################################\n\n")
    RunTimeLocal = rbind(time.gather.files,
                         time.execute,
                         time.gather.preds,
                         time.evaluate,
                         time.gather)
    
    setwd(diretorios$folderLocal)
    write.csv(RunTimeLocal, paste(dataset_name, 
                                  "-Runtime-Clus.csv", sep=""))
    
    
  } else {
    
    
    setwd(FolderScripts)
    source("local-python.R")
    
    str0 = "~/Local-Partitions/Reports"
    if(dir.exists(str0)==FALSE){dir.create(str0)}
    
    str1 = paste(str0, "/python", sep="")
    if(dir.exists(str1)==FALSE){dir.create(str1)}
    
    cat("\n\n#########################################################")
    cat("\n# RUN LOCAL: Execute PYTHON                             #")
    cat("\n#########################################################\n\n")
    time.execute = system.time(execute.python(ds, 
                                              dataset_name,
                                              number_dataset, 
                                              number_cores, 
                                              number_folds, 
                                              folderResults))
    
    
    cat("\n\n##################################################")
    cat("\n# RUN LOCAL: Evaluates PYTHON                      #")
    cat("\n####################################################\n\n")
    time.evaluate = system.time(evaluate.python(ds, dataset_name, 
                                                number_folds, folderResults))
    
    
    cat("\n\n################################################")
    cat("\n# RUN LOCAL: Gather PYTHON                       #")
    cat("\n##################################################\n\n")
    time.gather = system.time(gather.python(ds, dataset_name, number_folds, 
                                            folderResults))
    
    
    cat("\n\n##########################################################")
    cat("\n# RUN LOCAL: Save Runtime                                  #")
    cat("\n############################################################\n\n")
    RunTimeLOCAL = rbind(time.execute, time.evaluate, time.gather)
    setwd(diretorios$folderLocal)
    write.csv(RunTimeLOCAL, paste(dataset_name, "-run-runtime.csv", sep=""))
    
  }
  
  
  cat("\n\n############################################################")
  cat("\n# RUN LOCAL: Stop Parallel                                 #")
  cat("\n############################################################\n\n")
  parallel::stopCluster(cl) 	
  
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF RUN LOCAL. Thanks God!                                                                 #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n") 
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
