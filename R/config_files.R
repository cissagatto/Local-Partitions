rm(list=ls())

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


###############################################################################
# LOAD LIBRARY/Implementation                                                        #
###############################################################################
library(stringr)


###############################################################################
# READING DATASET INFORMATION FROM DATASETS-ORIGINAL.CSV                      #
###############################################################################
setwd(FolderRoot)
datasets = data.frame(read.csv("datasets-original.csv"))
n = nrow(datasets)


###############################################################################
# CREATING FOLDER TO SAVE CONFIG FILES                                        #
###############################################################################
FolderCF = paste(FolderRoot, "/config-files", sep="")
if(dir.exists(FolderCF)==FALSE){dir.create(FolderCF)}


###############################################################################
# CREATING FOLDER TO SAVE CONFIG FILES                                        #
###############################################################################
Implementation.1 = c("clus", "mulan", "python", "utiml")
Implementation.2 = c("c", "m", "p", "u")


j = 1
while(j<=length(Implementation.1)){
  
  ###############################################################################
  # CREATING FOLDER TO SAVE CONFIG FILES                                        #
  ###############################################################################
  FolderClassifier = paste(FolderCF, "/", Implementation.1[j], sep="")
  if(dir.exists(FolderClassifier)==FALSE){dir.create(FolderClassifier)}

  
  cat("\n\n==================================")
  cat("\n Classificador:", Implementation.1[j])
  cat("\n==================================\n\n")
  
  i = 1
  while(i<=n){
    
    # specific dataset
    ds = datasets[i,]
    
    # print the dataset name
    cat("\n\tdataset = ", ds$Name)
    
    # Confi File Name
    file_name = paste(FolderClassifier, "/l", Implementation.2[j], "-", 
                      ds$Name, ".csv", sep="")
    
    # Starts building the configuration file
    output.file <- file(file_name, "wb")
    
    # Config file table header
    write("Config, Value",
          file = output.file, append = TRUE)
    
    # Absolute path to the folder where the dataset's "tar.gz" is stored
    
    write("Dataset_Path, /home/biomal/Datasets/", 
         file = output.file, append = TRUE)
    
    # job name
    job_name = paste("l", Implementation.2[j], "-", ds$Name,sep = "")
    
    # directory name
    folder_name = paste("/dev/shm/", job_name, sep = "")
    
    # Absolute path to the folder where temporary processing will be done. 
    # You should use "scratch", "tmp" or "/dev/shm", it will depend on the 
    # cluster model where your experiment will be run.
    str.0 = paste("Temporary_Path, ", folder_name, sep="")
    write(str.0,file = output.file, append = TRUE)
    
    
    str.1 = paste("Implementation, ", Implementation.1[j], sep="")
    write(str.1, file = output.file, append = TRUE)
    
    # dataset name
    str.2 = paste("Dataset_name, ", ds$Name, sep="")
    write(str.2, file = output.file, append = TRUE)
    
    # Dataset number according to "datasets-original.csv" file
    str.3 = paste("Number_dataset, ", ds$Id, sep="")
    write(str.3, file = output.file, append = TRUE)
    
    # Number used for X-Fold Cross-Validation
    write("Number_folds, 10", file = output.file, append = TRUE)
    
    # Number of cores to use for parallel processing
    write("Number_cores, 1", file = output.file, append = TRUE)
    
    # finish writing to the configuration file
    close(output.file)
    
    # increment
    i = i + 1
    
    # clean
    gc()
  }
  
  j = j + 1
  gc()
}


###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################
