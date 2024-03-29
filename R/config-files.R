# clean
rm(list=ls())


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


cat("\n################################")
cat("\n# Set Work Space               #")
cat("\n###############################\n\n")
FolderRoot = "~/Local-Partitions"
FolderScripts = "~/Local-Partitions/R"




###############################################################################
# LOAD LIBRARY/PACKAGE                                                        #
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
FolderCF = paste(FolderRoot, "/config-files-laptop", sep="")
if(dir.exists(FolderCF)==FALSE){dir.create(FolderCF)}


###############################################################################
# QUAL Implementation USAR
###############################################################################
Implementation.1 = c("rf", "clus", "mulan", "utiml")
Implementation.2 = c("r", "c", "m", "u")


###############################################################################
# CREATING CONFIG FILES FOR EACH DATASET                                      #
###############################################################################
w = 1
while(w<=length(Implementation.1)){
  
  FolderPa = paste(FolderCF, "/", Implementation.1[w], sep="")
  if(dir.exists(FolderPa)==FALSE){dir.create(FolderPa)}
  
  i = 1
  while(i<=n){
    
    # specific dataset
    ds = datasets[i,]
    
    # print the dataset name
    cat("\n================================================")
    cat("\n\tDataset:", ds$Name)
    cat("\n\tPackge:", Implementation.1[w])
    
    # Confi File Name
    # "~/Local-Partitions/config-files/utiml/eg-3s-bbc1000.csv"
    file_name = paste(FolderPa, "/l", Implementation.2[w], "-",
                      ds$Name, ".csv", sep="")
    
    # Starts building the configuration file
    output.file <- file(file_name, "wb")
    
    # Config file table header
    write("Config, Value", file = output.file, append = TRUE)
    
    write("Dataset_Path, ~/Local-Partitions/Datasets", 
          file = output.file, append = TRUE)
    
    name = paste("l", Implementation.2[w], "-", ds$Name, sep = "")
    
    temp.name = paste("/dev/shm/", name, sep = "")
    
    # Absolute path to the folder where temporary processing will be done. 
    # You should use "scratch", "tmp" or "/dev/shm", it will depend on the 
    # cluster model where your experiment will be run.
    str.0 = paste("Temporary_Path, ", temp.name, sep="")
    write(str.0,file = output.file, append = TRUE)
    
    # "implementation, utiml"
    str.1 = paste("Implementation, ", Implementation.1[w], sep="")
    write(str.1, file = output.file, append = TRUE)
    
    # "dataset_name, 3s-bbc1000"
    str.2 = paste("Dataset_Name, ", ds$Name, sep="")
    write(str.2, file = output.file, append = TRUE)
    
    # Dataset number according to "datasets-original.csv" file
    # "number_dataset, 1"
    str.3 = paste("Number_Dataset, ", ds$Id, sep="")
    write(str.3, file = output.file, append = TRUE)
    
    # Number used for X-Fold Cross-Validation
    write("Number_Folds, 10", file = output.file, append = TRUE)
    
    # Number of cores to use for parallel processing
    write("Number_Cores, 1", file = output.file, append = TRUE)
    
    # finish writing to the configuration file
    close(output.file)
    
    # increment
    i = i + 1
    
    # clean
    gc()
  }
  
  cat("\n================================================")
  w = w + 1
  gc()
}

###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################