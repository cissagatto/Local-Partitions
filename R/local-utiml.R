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
# FUNCTION EXECUTE ECC Local                                                                   #
#   Objective                                                                                    #
#       Tests Local partitions                                                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       folder: folder path                                                                      #
#   Return                                                                                       #
#       configurations files                                                                     #
##################################################################################################
execute.utiml <- function(ds, 
                         dataset_name,
                         number_dataset, 
                         number_cores, 
                         number_folds, 
                         folderResults){
  
  # from fold = 1 to number_folds
  f = 1
  ecc.local.parallel <- foreach(f = 1:number_folds) %dopar% {
  # while(f<=number_folds){
    
    ####################################################################################
    folderRoot = "~/Local-Partitions/"
    folderScripts = paste(folderRoot, "/R/", sep="")
    
    ####################################################################################
    setwd(folderScripts)
    source("libraries.R")
    
    setwd(folderScripts)
    source("utils.R")
    
    ####################################################################################
    diretorios = directories(dataset_name, folderResults)
    
    ####################################################################################
    cat("\nFold: ", f)
    
    ####################################################################################
    folderSplit = paste(diretorios$folderLocal, "/Split-", f, sep="")
    if(dir.exists(folderSplit)==FALSE){dir.create(folderSplit)}
    
    ########################################################################################
    cat("\nOpen Train file ")
    setwd(diretorios$folderCVTR)
    nome_arq_tr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))
    
    #######################################################################################
    cat("\nOpen Validation file ")
    setwd(diretorios$folderCVVL)
    nome_arq_vl = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")
    arquivo_vl = data.frame(read.csv(nome_arq_vl))
    
    ########################################################################################
    cat("\nOpen Test file ")
    setwd(diretorios$folderCVTS)
    nome_arq_ts = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
    arquivo_ts = data.frame(read.csv(nome_arq_ts))
    
    ########################################################################################
    cat("\nJoin validation and train")
    arquivo_tr2 = rbind(arquivo_tr, arquivo_vl)
    
    # separando os rótulos verdadeiros
    y_true = arquivo_ts[,ds$LabelStart:ds$LabelEnd]
    
    # gerando indices
    number = seq(ds$LabelStart, ds$LabelEnd, by=1)
    
    # transformando treino em mldr
    ds_train = mldr_from_dataframe(arquivo_tr2, labelIndices = number)
    
    # transformando test em mldr
    ds_test = mldr_from_dataframe(arquivo_ts, labelIndices = number)
    
    # aplicando modelo br
    brmodel = br(ds_train, "RF", seed=123)
    
    # testando modelo br
    predict <- predict(brmodel, ds_test)
    
    # Apply a threshold
    thresholds <- scut_threshold(predict, ds_test)
    new.test <- fixed_threshold(predict, thresholds)
    
    new.test2 = as.matrix(new.test)
    y_predict = data.frame(new.test2)
    
    setwd(folderSplit)
    write.csv(y_predict, "y_predict.csv", row.names = FALSE)
    write.csv(y_true, "y_true.csv", row.names = FALSE)
    
    
    # f = f + 1
    gc()
  }
  
  gc()
  cat("\n#########################################################")
  cat("\n# END FUNCTION EXECUTE                                  #") 
  cat("\n#########################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION EVALUATE GENERAL                                                                      #
#   Objective:                                                                                   #
#       Evaluate Multilabel                                                                      #  
#   Parameters:                                                                                  #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds to be created                                              #
#       folder: folder where the folds are                                                       #
#   Return:                                                                                      #
#       Confusion Matrix                                                                         #
##################################################################################################
evaluate.utiml <- function(ds, dataset_name, number_folds, folderResults){    
  
  
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
  f = 1
  avaliaParalel <- foreach (f = 1:number_folds) %dopar%{    
    #while(f<=number_folds){
    
    folderRoot = "~/Local-Partitions/"
    folderScripts = paste(folderRoot, "/R/", sep="")
    
    ####################################################################################
    setwd(folderScripts)
    source("utils.R")
    
    library("mldr")
    library("utiml")
    
    ####################################################################################
    diretorios = directories(dataset_name, folderResults)
    
    cat("\n\nSplit: ", f)    
    
    folderSplit = paste(diretorios$folderLocal, "/Split-", f, sep="")
    
    ####################################################################################
    cat("\nAbrindo pred and true")
    setwd(folderSplit)
    y_pred = data.frame(read.csv("y_predict.csv"))
    y_true = data.frame(read.csv("y_true.csv"))
    
    cat("\nConvertendo em numerico")
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    cat("\nsalvando")
    salva3 = paste("ConfMatFold-", f, ".txt", sep="")
    setwd(folderSplit)
    sink(file=salva3, type="output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    cat("\nmatriz de confusão")
    resConfMat = multilabel_evaluate(confmat)
    resConfMat = data.frame(resConfMat)
    names(resConfMat) = paste("Fold-", f, sep="")
    setwd(folderSplit)
    write.csv(resConfMat, "ResConfMat.csv")    
    
    str = paste()
    
    #f = f + 1
    gc()
  }
  
  gc()
  cat("\n#########################################################")
  cat("\n# END FUNCTION EVALUATE                                #") 
  cat("\n#########################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER PREDICTS Local PARTITIONS                                                     #
#   Objective                                                                                    #
#      Evaluates the Local partitions                                                           #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       folder: path of Local partition results                                                 #
#   Return                                                                                       #
#       Assessment measures for each Local partition                                            #
##################################################################################################
gather.utiml <- function(ds, dataset_name, number_folds, folderResults){
  
  diretorios = directories(dataset_name, folderResults) 
  
  # vector with names measures
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  
  # dta frame
  confMatFinal = data.frame(measures)
  folds = c("")
  
  # from fold = 1 to number_labels
  f = 1
  while(f<= number_folds){
    cat("\nFold: ", f)
    
    folderSplit = paste(diretorios$folderLocal, "/Split-", f, sep="")
    setwd(folderSplit)
    
    # cat("\n\tOpen ResConfMat ", f)
    confMat = data.frame(read.csv(paste(folderSplit, "/ResConfMat.csv", sep="")))
    names(confMat) = c("Measures", "Fold")
    confMatFinal = cbind(confMatFinal, confMat$Fold) 
    folds[f] = paste("Fold-", f, sep="")
    setwd(folderSplit)
    unlink("y_predict.csv", recursive = TRUE)
    unlink("y_true.csv", recursive = TRUE)
    
    f = f + 1
    gc()
  } 
  
  # save measures
  setwd(diretorios$folderLocal)
  names(confMatFinal) = c("Measures", folds)
  write.csv(confMatFinal, paste(dataset_name, "-Local-Test-Evaluated.csv", sep=""), row.names = FALSE)
  
  # calculando a média dos 10 folds para cada medida
  media = data.frame(apply(confMatFinal[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(diretorios$folderLocal)
  write.csv(media, "Mean10Folds.csv", row.names = FALSE)
  
  mediana = data.frame(apply(confMatFinal[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(diretorios$folderLocal)
  write.csv(mediana, "Median10Folds.csv", row.names = FALSE)
  
  dp = data.frame(apply(confMatFinal[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(diretorios$folderLocal)
  write.csv(dp, "desvio-padrão-10-folds.csv", row.names = FALSE)
  
  gc()
  cat("\n#########################################################")
  cat("\n# END FUNCTION GATHER                                   #") 
  cat("\n#########################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################

