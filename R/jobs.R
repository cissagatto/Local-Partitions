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
# LOAD LIBRARY/PACKAGE                                                        #
###############################################################################
library(stringr)


###############################################################################
# READING DATASET INFORMATION FROM DATASETS-ORIGINAL.CSV                      #
###############################################################################
datasets = data.frame(read.csv("datasets-original.csv"))
n = nrow(datasets)


###############################################################################
# CREATING FOLDER TO SAVE CONFIG FILES                                        #
###############################################################################
FolderJob = paste(FolderRoot, "/jobs", sep = "")
if (dir.exists(FolderJob) == FALSE) {dir.create(FolderJob)}


###############################################################################
# CREATING .SH FILES FOR EACH DATASET                                         #
###############################################################################
i = 1
while (i <= n) {
  
  # select the specific dataset
  dataset = datasets[i, ]
  
  # print dataset name
  cat("\n\tDataset:", dataset$Name)
  
  # job name
  job_name = paste("el-", dataset$Name, sep = "")
  
  # directory name
  folder_name = paste("/scratch/", job_name, sep = "")
  
  # Confi File Name
  file_name = paste("el-", dataset$Name, ".csv", sep="")
  
  # sh file name
  sh_name = paste(FolderJob, "/", job_name, ".sh", sep = "")
  
  # start writing
  output.file <- file(sh_name, "wb")
  
  # bash parameters
  write("#!/bin/bash", file = output.file)
  
  str1 = paste("#SBATCH -J ", job_name, sep = "")
  write(str1, file = output.file, append = TRUE)
  
  write("#SBATCH -o %j.out", file = output.file, append = TRUE)
  
  # number of processors
  write("#SBATCH -n 1", file = output.file, append = TRUE)
  
  # number of cores
  write("#SBATCH -c 10", file = output.file, append = TRUE)
  
  # uncomment this line if you are using slow partition
  # write("#SBATCH --partition slow", file = output.file, append = TRUE)
  
  # uncomment this line if you are using slow partition
  # write("#SBATCH -t 720:00:00", file = output.file, append = TRUE)
  
  # comment this line if you are using slow partition
  write("#SBATCH -t 128:00:00", file = output.file, append = TRUE)
  
  # uncomment this line if you need to use all node memory
  # write("#SBATCH --mem=0", file = output.file, append = TRUE)
  
  # amount of node memory you want to use
  # comment this line if you are using -mem=0
  write("#SBATCH --mem-per-cpu=35GB", file = output.file, append = TRUE)
  
  # email to receive notification
  write("#SBATCH --mail-user=elainegatto@estudante.ufscar.br",
        file = output.file, append = TRUE)
  
  # type of notification
  write("#SBATCH --mail-type=ALL", file = output.file, append = TRUE)
  write("", file = output.file, append = TRUE)
  
  # FUNCTION TO CLEAN THE JOB
  str2 = paste("local_job=",  "\"/scratch/", job_name, "\"", sep = "")
  write(str2, file = output.file, append = TRUE)
  write("function clean_job(){", file = output.file, append = TRUE)
  str3 = paste(" echo", "\"CLEANING ENVIRONMENT...\"", sep = " ")
  write(str3, file = output.file, append = TRUE)
  str4 = paste(" rm -rf ", "\"${local_job}\"", sep = "")
  write(str4, file = output.file, append = TRUE)
  write("}", file = output.file, append = TRUE)
  write("trap clean_job EXIT HUP INT TERM ERR", file = output.file, append = TRUE)
  write("", file = output.file, append = TRUE)
  
  
  # MANDATORY PARAMETERS
  write("set -eE", file = output.file, append = TRUE)
  write("umask 077", file = output.file, append = TRUE)
  
  
  write("", file = output.file, append = TRUE)
  write("echo =====================================", file = output.file, append = TRUE)
  write("echo SBATCH: RUNNING Local PARTITIONS", file = output.file, append = TRUE)
  write("echo =====================================", file = output.file, append = TRUE)
  
  
  write("", file = output.file, append = TRUE)
  write("echo DELETING THE FOLDER", file = output.file, append = TRUE)
  str11 = paste("rm -rf ", folder_name, sep = "")
  write(str11, file = output.file, append = TRUE)
  
  
  write("", file = output.file, append = TRUE)
  write("echo CREATING THE FOLDER", file = output.file, append = TRUE)
  str11 = paste("mkdir ", folder_name, sep = "")
  write(str11, file = output.file, append = TRUE)
  
  
  write("", file = output.file, append = TRUE)
  write("echo COPYING CONDA ENVIRONMENT", file = output.file, append = TRUE)
  str20 = paste("cp /home/u704616/miniconda3.tar.gz ", folder_name, sep ="")
  write(str20 , file = output.file, append = TRUE)
  
  
  write(" ", file = output.file, append = TRUE)
  write("echo UNPACKING MINICONDA", file = output.file, append = TRUE)
  str22 = paste("tar xzf ", folder_name, "/miniconda3.tar.gz -C ", 
                folder_name, sep = "")
  write(str22 , file = output.file, append = TRUE)
  
  
  write(" ", file = output.file, append = TRUE)
  write("echo DELETING MINICONDA TAR.GZ", file = output.file, append = TRUE)
  str22 = paste("rm -rf ", folder_name, "/miniconda3.tar.gz", sep = "")
  write(str22 , file = output.file, append = TRUE)
  
  
  write(" ", file = output.file, append = TRUE)
  write("echo SOURCE", file = output.file, append = TRUE)
  str21 = paste("source ", folder_name,
                "/miniconda3/etc/profile.d/conda.sh ", sep = "")
  write(str21, file = output.file, append = TRUE)
  
  
  write(" ", file = output.file, append = TRUE)
  write("echo ACTIVATING MINICONDA ", file = output.file, append = TRUE)
  write("conda activate AmbienteTeste", file = output.file, append = TRUE)
  write(" ", file = output.file, append = TRUE)
  
  
  write("echo RUNNING", file = output.file, append = TRUE)
  str7 = paste("Rscript /home/u704616/Local-Partitions/R/Local.R \"/home/u704616/config-files/",
               file_name, "\"", sep = "")
  write(str7, file = output.file, append = TRUE)
  write(" ", file = output.file, append = TRUE)
  
  
  write("echo DELETING JOB FOLDER", file = output.file, append = TRUE)
  str11 = paste("rm -rf ", folder_name, sep = "")
  write(str11, file = output.file, append = TRUE)
  
  
  write("", file = output.file, append = TRUE)
  write("echo ==================================", file = output.file, append = TRUE)
  write("echo SBATCH: ENDED SUCCESSFULLY", file = output.file, append = TRUE)
  write("echo ==================================", file = output.file, append = TRUE)
  
  close(output.file)
  
  i = i + 1
  gc()
}

###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################
