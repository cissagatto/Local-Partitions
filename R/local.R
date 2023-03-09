cat("\n\n################################################################")
cat("\n# RSCRIPT: START EXECUTE LOCAL PARTITION                       #")
cat("\n################################################################\n\n")

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
# PhD Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri       #
# Ferrandin | Prof. Dr. Celine Vens | PhD Felipe Nakano Kenji                #
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
# LOAD INTERNAL SCRIPTS                                                       #
###############################################################################

setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")

setwd(FolderScripts)
source("run.R")


###############################################################################
# R Options Configuration                                                     #
###############################################################################
options(java.parameters = "-Xmx64g")  # JAVA
options(show.error.messages = TRUE)   # ERROR MESSAGES
options(scipen=20)                    # number of places after the comma



###############################################################################
# Reading the "datasets-original.csv" file to get dataset information         #
# for code execution!                                                         #
###############################################################################
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets-original.csv"))



###############################################################################
# ARGS COMMAND LINE                                                          #
###############################################################################
cat("\n############################################")
cat("\n# RSCRIPT: GET ARGUMENTS FROM COMMAND LINE #")
cat("\n############################################\n\n")
args <- commandArgs(TRUE)



###############################################################################
# FIRST ARGUMENT: getting specific dataset information being processed        #
# from csv file                                                               #
###############################################################################


# config_file = "/home/biomal/Local-Partitions/config-files/rf/lrf-GpositiveGO.csv"


config_file <- args[1]


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

dataset_path = toString(config$Value[1])
dataset_path = str_remove(dataset_path, pattern = " ")

folderResults = toString(config$Value[2]) 
folderResults = str_remove(folderResults, pattern = " ")

package = toString(config$Value[3]) 
package = str_remove(package, pattern = " ")

dataset_name = toString(config$Value[4])
dataset_name = str_remove(dataset_name, pattern = " ")

number_dataset = as.numeric(config$Value[5])
number_folds = as.numeric(config$Value[6])
number_cores = as.numeric(config$Value[7])

ds = datasets[number_dataset,]


###############################################################################
# Creating temporary processing folder                                        #
###############################################################################
if (dir.exists(folderResults) == FALSE) {dir.create(folderResults)}


###############################################################################
# Creating all directories that will be needed for code processing            #
###############################################################################
cat("\n#############################")
cat("\n# RSCRIPIT: Get directories #")
cat("\n#############################\n")
diretorios <- directories(dataset_name, folderResults)



###############################################################################
# Copying datasets from ROOT folder on server                                 #
###############################################################################

cat("\n####################################################################")
cat("\n# Checking the dataset tar.gz file                                 #")
cat("\n####################################################################\n\n")
str00 = paste(dataset_path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){
  
  cat("\n######################################################################")
  cat("\n# The tar.gz file for the dataset to be processed does not exist!    #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00, "                                  #")
  cat("\n######################################################################\n\n")
  break
  
} else {
  
  cat("\n##################################################################")
  cat("\n# tar.gz file of the dataset loaded correctly!                   #")
  cat("\n##################################################################\n\n")
  
  # COPIANDO
  str01 = paste("cp ", str00, " ", diretorios$folderDataset, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }
  
  # DESCOMPACTANDO
  str02 = paste("tar xzf ", diretorios$folderDataset, "/", ds$Name,
                ".tar.gz -C ", diretorios$folderDataset, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }
  
  #APAGANDO
  str03 = paste("rm ", diretorios$folderDataset, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }
  
}



cat("\n\n##############################################################")
cat("\n# RSCRIPT: START                                             #")
cat("\n##############################################################\n\n")
time.final <- system.time(results <- executeLP(ds, 
                                               dataset_name,
                                               number_dataset, 
                                               number_cores, 
                                               number_folds, 
                                               folderResults))




cat("\n\n################################################################")
cat("\n# RSCRIPT: SAVE RUNTIME                                        #")
cat("\n################################################################\n\n")
result_set <- t(data.matrix(time.final))
setwd(diretorios$folderLocal)
write.csv(result_set, "Runtime-Final.csv")
x.minutos = (1 * as.numeric(result_set[3]))/60
setwd(diretorios$folderLocal)
write(x.minutos, "minutos.txt")



cat("\n\n################################################################")
cat("\n# RSCRIPT: DELETE DATASET                                      #")
cat("\n################################################################\n\n")
str5 = paste("rm -r ", diretorios$folderDataset, sep="")
print(system(str5))


# cat("\n\n###################################################################")
# cat("\n# ECC LOCAL: COMPRESS RESULTS                                      #")
# cat("\n#####################################################################\n\n")
# system(paste("tar -zcf ", ds$Name, ".tar.gz ", 
#              diretorios$folderLocal, "/*", sep=""))
# 
# 
# cat("\n\n##############################################################")
# cat("\n# ECC LOCAL: COPY TO FOLDER REPORTS                           #")
# cat("\n###############################################################\n\n")
# str0 = paste("~/Local-Partitions/Reports/", package, sep="")
# str1 = paste(diretorios$folderLocal, "/", dataset_name, ".tar.gz", sep="")
# str4 = paste("cp ", str1 , " ", str0, sep="")
# print(system(str4))

if(package=="clus"){
  
  cat("\n\n################################################################")
  cat("\n# RSCRIPT: COPY TO GOOGLE DRIVE                                #")
  cat("\n################################################################\n\n")
  origem = diretorios$folderLocal
  destino = paste("nuvem:/Local/Clus/", dataset_name, sep="")
  comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  cat("\n", comando, "\n")
  a = print(system(comando))
  a = as.numeric(a)
  if(a != 0) {
    stop("Erro RCLONE")
    quit("yes")
  }
  
} else if(package=="mulan"){
  
  
  cat("\n\n################################################################")
  cat("\n# RSCRIPT: COPY TO GOOGLE DRIVE                                #")
  cat("\n################################################################\n\n")
  origem = diretorios$folderLocal
  destino = paste("nuvem:/Local/Mulan/", dataset_name, sep="")
  comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  cat("\n", comando, "\n")
  a = print(system(comando))
  a = as.numeric(a)
  if(a != 0) {
    stop("Erro RCLONE")
    quit("yes")
  }
  
  
} else if(package=="python"){
  
  cat("\n\n################################################################")
  cat("\n# RSCRIPT: COPY TO GOOGLE DRIVE                                #")
  cat("\n################################################################\n\n")
  origem = diretorios$folderLocal
  destino = paste("nuvem:/Local/RandomForests/", dataset_name, sep="")
  comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  cat("\n", comando, "\n")
  a = print(system(comando))
  a = as.numeric(a)
  if(a != 0) {
    stop("Erro RCLONE")
    quit("yes")
  }
  
} else {
  
  cat("\n\n################################################################")
  cat("\n# RSCRIPT: COPY TO GOOGLE DRIVE                                #")
  cat("\n################################################################\n\n")
  origem = diretorios$folderLocal
  destino = paste("nuvem:/Local/Utiml/", dataset_name, sep="")
  comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  cat("\n", comando, "\n")
  a = print(system(comando))
  a = as.numeric(a)
  if(a != 0) {
    stop("Erro RCLONE")
    quit("yes")
  }
  
}





cat("\n\n################################################################")
cat("\n# RSCRIPT: CLEAN                                               #")
cat("\n################################################################\n\n")
cat("\nDelete folder \n")
str5 = paste("rm -r ", folderResults, sep="")
print(system(str5))


cat("\n\n#################################################################")
cat("\n# RSCRIPT: SUCCESSFULLY FINISHED                                #")
cat("\n#################################################################\n\n")


rm(list = ls())
gc()

###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################
