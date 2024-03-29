cat("\n\n###############################################################")
  cat("\n# RSCRIPT: START EXECUTE Local PARTITIONS                    #")
  cat("\n###############################################################\n\n")


# clean
rm(list=ls())


##############################################################################
# Local PARTITIONS                                                           #
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


cat("\n########################################")
cat("\n# Loading R Sources                    #")
cat("\n########################################\n\n")

setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")


cat("\n########################################")
cat("\n# R Options Configuration              #")
cat("\n########################################\n\n")
options(java.parameters = "-Xmx64g")  # JAVA
options(show.error.messages = TRUE)   # ERROR MESSAGES
options(scipen=20)                    # number of places after the comma


cat("\n########################################")
cat("\n# Creating parameters list              #")
cat("\n########################################\n\n")
parameters = list()


cat("\n########################################")
cat("\n# Reading Datasets-Original.csv        #")
cat("\n########################################\n\n")
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets-original.csv"))
parameters$Datasets.List = datasets


cat("\n#####################################")
cat("\n# GET ARGUMENTS FROM COMMAND LINE   #")
cat("\n#####################################\n\n")
args <- commandArgs(TRUE)


# config_file = "~/Local-Partitions/config-files-laptop/clus/lc-GpositiveGO.csv"


config_file <- args[1]


parameters$Config.File.Name = config_file
if(file.exists(config_file)==FALSE){
  cat("\n################################################################")
  cat("#\n Missing Config File! Verify the following path:              #")
  cat("#\n ", config_file, "                                            #")
  cat("#################################################################\n\n")
  break
} else {
  cat("\n########################################")
  cat("\n# Properly loaded configuration file!  #")
  cat("\n########################################\n\n")
}


cat("\n########################################")
cat("\n# Config File                          #\n")
config = data.frame(read.csv(config_file))
print(config)
cat("\n########################################\n\n")

cat("\n########################################")
cat("\n# Getting Parameters                   #\n")
cat("\n########################################")
dataset_path = toString(config$Value[1])
dataset_path = str_remove(dataset_path, pattern = " ")
parameters$Config.File$Dataset.Path = dataset_path

folderResults = toString(config$Value[2]) 
folderResults = str_remove(folderResults, pattern = " ")
parameters$Config.File$Folder.Results = folderResults

implementation = toString(config$Value[3])
implementation = str_remove(implementation, pattern = " ")
parameters$Config.File$Implementation = implementation

dataset_name = toString(config$Value[4])
dataset_name = str_remove(dataset_name, pattern = " ")
parameters$Config.File$Dataset.Name = dataset_name

number_dataset = as.numeric(config$Value[5])
parameters$Config.File$Number.Dataset = number_dataset

number_folds = as.numeric(config$Value[6])
parameters$Config.File$Number.Folds = number_folds

number_cores = as.numeric(config$Value[7])
parameters$Config.File$Number.Cores = number_cores

# cat("\n################################################################\n")
# print(parameters$Config.File)
# cat("\n################################################################\n\n")


ds = datasets[number_dataset,]
parameters$Dataset.Info = ds

# cat("\n################################################################\n")
# print(ds)
# cat("\n################################################################\n\n")


cat("\n########################################")
cat("\n# Creating temporary processing folder #")
cat("\n########################################\n\n")
if (dir.exists(folderResults) == FALSE) {dir.create(folderResults)}


cat("\n###############################\n")
cat("\n# Get directories             #")
cat("\n###############################\n\n")
diretorios <- directories(parameters)
parameters$Directories = diretorios

# cat("\n################################################################\n")
# print(parameters$Directories)
# cat("\n################################################################\n\n")



cat("\n####################################################################")
cat("\n# Checking the dataset tar.gz file                                 #")
cat("\n####################################################################\n\n")
str00 = paste(dataset_path, "/", parameters$Config.File$Dataset.Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){
  
  cat("\n######################################################################")
  cat("\n# The tar.gz file for the dataset to be processed does not exist!    #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00, "                                  #")
  cat("\n######################################################################\n\n")
  break
  
} else {
  
  cat("\n####################################################################")
  cat("\n# tar.gz file of the DATASET loaded correctly!                     #")
  cat("\n####################################################################\n\n")
  
  # COPIANDO
  str01 = paste("cp ", str00, " ", parameters$Directories$FolderDataset, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }
  
  # DESCOMPACTANDO
  str02 = paste("tar xzf ",  parameters$Directories$FolderDataset, "/", ds$Name,
                ".tar.gz -C ",  parameters$Directories$FolderDataset, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }
  
  #APAGANDO
  str03 = paste("rm ",  parameters$Directories$FolderDataset, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }
  
}




##############################################################################
#
##############################################################################
if(implementation=="utiml"){
  # 
  # setwd(FolderScripts)
  # source("run-utiml.R")
  # 
  # cat("\n\n############################################################")
  # cat("\n# RSCRIPT Local START                                     #")
  # cat("\n############################################################\n\n")
  # timeFinal <- system.time(results <- run.ecc.utiml(ds, 
  #                                                   dataset_name,
  #                                                   number_dataset, 
  #                                                   number_cores, 
  #                                                   number_folds, 
  #                                                   folderResults))  
  # 
  # cat("\n\n#####################################################")
  # cat("\n# RSCRIPT SAVE RUNTIME                              #")
  # cat("\n#####################################################\n\n")
  # result_set <- t(data.matrix(timeFinal))
  # setwd(diretorios$folderLocal)
  # write.csv(result_set, "Runtime-Final.csv")
  # x.minutos =(1 * as.numeric(result_set[3]))/60
  # setwd(diretorios$folderLocal)
  # write(x.minutos, "minutos.txt")
  # 
  # 
  # cat("\n\n#####################################################")
  # cat("\n# RSCRIPT DELETE                                   #")
  # cat("\n####################################################\n\n")
  # str5 = paste("rm -r ", diretorios$folderDataset, sep="")
  # print(system(str5))
  # 
  # 
  # 
  # cat("\n\n######################################################")
  # cat("\n# RSCRIPT COPY TO GOOGLE DRIVE                       #")
  # cat("\n######################################################\n\n")
  # origem = diretorios$folderLocal
  # destino = paste("nuvem:Local/Utiml/", dataset_name, sep="")
  # comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  # cat("\n", comando, "\n") 
  # a = print(system(comando))
  # a = as.numeric(a)
  # if(a != 0) {
  #   stop("Erro RCLONE")
  #   quit("yes")
  # }
  # 
  
} else if(implementation=="rf"){
  
  setwd(FolderScripts)
  source("run-rf.R")
  
  cat("\n\n############################################################")
  cat("\n# RSCRIPT Local RANDOM FORESTS START                     #")
  cat("\n###########################################################\n\n")
  timeFinal <- system.time(results <- run.rf(parameters))  
  
  
  cat("\n\n#####################################################")
  cat("\n# RSCRIPT SAVE RUNTIME                              #")
  cat("\n#####################################################\n\n")
  result_set <- t(data.matrix(timeFinal))
  setwd(parameters$Directories$FolderLocal)
  write.csv(result_set, "Final-Runtime.csv", row.names = FALSE)
  
  
  cat("\n\n###################################################")
  cat("\n# RSCRIPT DELETE                                  #")
  cat("\n###################################################\n\n")
  str5 = paste("rm -r ", parameters$Directories$FolderDataset, sep="")
  print(system(str5))
  
  cat("\n\n###################################################################")
  cat("\n# LOCAL: COMPRESS RESULTS                                      #")
  cat("\n#####################################################################\n\n")
  str3 = paste("tar -zcvf ", parameters$Directories$FolderLocal, "/",
               parameters$Dataset.Info$Name, "-results-local.tar.gz ",
               parameters$Directories$FolderLocal, sep="")
  print(system(str3))
  
  
  cat("\n\n###################################################################")
  cat("\n# ====> GPC: COPY TO HOME                                     #")
  cat("\n#####################################################################\n\n")
  
  str0 = "~/Local-Partitions/Reports/"
  if(dir.exists(str0)==FALSE){dir.create(str0)}
  
  str3 = paste(parameters$Directories$FolderLocal, "/",
               dataset_name, "-results-local.tar.gz", sep="")
  
  str4 = paste("cp ", str3, " ", str0, sep="")
  print(system(str4))
  
  
  cat("\n\n######################################################")
  cat("\n# RSCRIPT COPY TO GOOGLE DRIVE                       #")
  cat("\n######################################################\n\n")
  origem = parameters$Directories$FolderLocal
  destino = paste("nuvem:Local/RandomForests/", dataset_name, sep="")
  comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  cat("\n", comando, "\n") 
  a = print(system(comando))
  a = as.numeric(a)
  if(a != 0) {
    stop("Erro RCLONE")
    quit("yes")
  }
  
  
  # cat("\n\n######################################################")
  # cat("\n# RSCRIPT COPY TO GOOGLE DRIVE                       #")
  # cat("\n######################################################\n\n")
  # origem = diretorios$folderDataset
  # destino = paste("nuvem:Datasets/", dataset_name, sep="")
  # comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  # cat("\n", comando, "\n") 
  # a = print(system(comando))
  # a = as.numeric(a)
  # if(a != 0) {
  #   stop("Erro RCLONE")
  #   quit("yes")
  # }
  
} else if(implementation=="mulan"){
  # 
  # 
  # setwd(FolderScripts)
  # source("run-mulan.R")
  # 
  # 
  # cat("\n\n############################################################")
  # cat("\n# RSCRIPT Local START                                     #")
  # cat("\n############################################################\n\n")
  # timeFinal <- system.time(results <- run.ecc.mulan(ds, 
  #                                                   dataset_name,
  #                                                   number_dataset, 
  #                                                   number_cores, 
  #                                                   number_folds, 
  #                                                   folderResults))  
  # 
  # 
  # 
  # cat("\n\n#####################################################")
  # cat("\n# RSCRIPT SAVE RUNTIME                              #")
  # cat("\n#####################################################\n\n")
  # result_set <- t(data.matrix(timeFinal))
  # setwd(diretorios$folderLocal)
  # write.csv(result_set, "Final-Runtime.csv")
  # x.minutos = (1 * as.numeric(result_set[3]))/60
  # setwd(diretorios$folderLocal)
  # write(x.minutos, "minutos.txt")
  # 
  # 
  # cat("\n\n#################################################")
  # cat("\n# RSCRIPT DELETE                                  #")
  # cat("\n###################################################\n\n")
  # str5 = paste("rm -r ", diretorios$folderDataset, sep="")
  # print(system(str5))
  # 
  # 
  # cat("\n\n######################################################")
  # cat("\n# RSCRIPT COPY TO GOOGLE DRIVE                       #")
  # cat("\n######################################################\n\n")
  # origem = diretorios$folderLocal
  # destino = paste("nuvem:Local/Mulan/", dataset_name, sep="")
  # comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  # cat("\n", comando, "\n") 
  # a = print(system(comando))
  # a = as.numeric(a)
  # if(a != 0) {
  #   stop("Erro RCLONE")
  #   quit("yes")
  # }
  
  
  
} else {
  
  setwd(FolderScripts)
  source("run-clus.R")
  
  cat("\n\n############################################################")
  cat("\n# RSCRIPT Local START                                     #")
  cat("\n############################################################\n\n")
  timeFinal <- system.time(results <- run.clus(parameters))  
  
  
  cat("\n\n#####################################################")
  cat("\n# RSCRIPT SAVE RUNTIME                              #")
  cat("\n#####################################################\n\n")
  result_set <- t(data.matrix(timeFinal))
  setwd(diretorios$folderLocal)
  write.csv(result_set, "Final-Runtime.csv")
  
  
  cat("\n\n####################################################")
  cat("\n# RSCRIPT DELETE                                   #")
  cat("\n####################################################\n\n")
  str5 = paste("rm -r ", parameters$Directories$FolderDataset, sep="")
  print(system(str5))
  
  
  # 
  # cat("\n\n######################################################")
  # cat("\n# RSCRIPT COPY TO GOOGLE DRIVE                       #")
  # cat("\n######################################################\n\n")
  # origem = diretorios$folderLocal
  # destino = paste("nuvem:Local/Clus/", dataset_name, sep="")
  # comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  # cat("\n", comando, "\n") 
  # a = print(system(comando))
  # a = as.numeric(a)
  # if(a != 0) {
  #   stop("Erro RCLONE")
  #   quit("yes")
  # }
  
  cat("\n\n###################################################################")
  cat("\n# LOCAL: COMPRESS RESULTS                                      #")
  cat("\n#####################################################################\n\n")
  str3 = paste("tar -zcvf ", parameters$Directories$FolderLocal, "/",
               parameters$Dataset.Info$Name, "-results-local.tar.gz ",
               parameters$Directories$FolderLocal, sep="")
  print(system(str3))
  
  
  cat("\n\n###################################################################")
  cat("\n# ====> GPC: COPY TO HOME                                     #")
  cat("\n#####################################################################\n\n")
  
  str0 = "~/Local-Partitions/Reports/"
  if(dir.exists(str0)==FALSE){dir.create(str0)}
  
  str3 = paste(parameters$Directories$FolderLocal, "/",
               dataset_name, "-results-local.tar.gz", sep="")
  
  str4 = paste("cp ", str3, " ", str0, sep="")
  print(system(str4))
  
  
}


# cat("\n\n###################################################################")
# cat("\n# ECC Local: COMPRESS RESULTS                                      #")
# cat("\n#####################################################################\n\n")
# str3 = paste("tar -zcvf ", diretorios$folderResults, "/", 
#              dataset_name, "-results-Local.tar.gz ", 
#              diretorios$folderResults, sep="")
# print(system(str3))


# cat("\n\n##############################################################")
# cat("\n# ECC Local: COPY TO FOLDER REPORTS                           #")
# cat("\n###############################################################\n\n")
# str0 = "/Local-Partitions/Reports/"
# if(dir.exists(str0)==FALSE){dir.create(str0)}
# str1 = paste(diretorios$folderResults, "/", dataset_name,
#              "-results-Local.tar.gz", sep="")
# str4 = paste("cp -r ", str1 , " ", str0, sep="")
# print(system(str4))



cat("\n\n#######################################################")
cat("\n# CLEAN                                               #")
cat("\n#######################################################\n\n")
cat("\nDelete folder \n")
str5 = paste("rm -r ", folderResults, sep="")
print(system(str5))


cat("\n\n################################################################")
cat("\n# RSCRIPT SUCCESSFULLY FINISHED                                #")
cat("\n################################################################\n\n")


rm(list = ls())
gc()

###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #
###############################################################################
