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
execute.mulan <- function(ds, 
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
    cat("\n# ========================================= #")
    cat("\n# Fold: ", f)
    cat("\n# ========================================= #")
    
    ####################################################################################
    folderSplit = paste(diretorios$folderLocal, "/Split-", f, sep="")
    if(dir.exists(folderSplit)==FALSE){dir.create(folderSplit)}
    
    ########################################################################################
    cat("\n\tOpen Train file ")
    setwd(diretorios$folderCVTR)
    nome_arq_tr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))
    
    #######################################################################################
    cat("\n\tOpen Validation file ")
    setwd(diretorios$folderCVVL)
    nome_arq_vl = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")
    arquivo_vl = data.frame(read.csv(nome_arq_vl))
    
    ########################################################################################
    cat("\n\tOpen Test file ")
    setwd(diretorios$folderCVTS)
    nome_arq_ts = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
    arquivo_ts = data.frame(read.csv(nome_arq_ts))
    
    ########################################################################################
    cat("\n\tJoin validation and train")
    arquivo_tr2 = rbind(arquivo_tr, arquivo_vl)
    
    ########################################################################################
    cat("\n\tSaving Y True")
    y_true = arquivo_ts[,ds$LabelStart:ds$LabelEnd]
    
    ########################################################################################
    cat("\n\tGet names labels")
    names.labels = colnames(y_true)
    
    ########################################################################################
    cat("\n\tLabel Index")
    number = seq(ds$LabelStart, ds$LabelEnd, by=1)
    
    #####################################################################
    cat("\n\tTRAIN: Transform into MLDR")
    str.train = paste(ds$Name, "-split-tr-", f,
                      "-weka.filters.unsupervised.attribute.NumericToNominal-R", 
                      ds$LabelStart, "-", ds$LabelEnd, sep="")
    
    train.mldr <- mldr::mldr_from_dataframe(dataframe = arquivo_tr2, 
                                            labelIndices = number, 
                                            name = str.train)
    
    train.name = paste(folderSplit, "/", ds$Name, "-split-tr-", f, ".arff", sep="")
    train.xml.name = paste(folderSplit, "/", ds$Name, "-split-tr-", f, sep="")
    mldr::write_arff(train.mldr, train.xml.name, write.xml = T)
    
    
    #####################################################################
    cat("\n\tTEST: Transform into MLDR")
    str.test = paste(ds$Name, "-split-ts-", f,
                     "-weka.filters.unsupervised.attribute.NumericToNominal-R", 
                     ds$LabelStart, "-", ds$LabelEnd, sep="")
    
    test.mldr <- mldr::mldr_from_dataframe(dataframe = arquivo_ts, 
                                           labelIndices = number, 
                                           name = str.test)
    
    test.name = paste(folderSplit, "/", ds$Name, "-split-ts-", f, ".arff", sep="")
    test.xml.name = paste(folderSplit, "/", ds$Name, "-split-ts-", f, sep="")
    mldr::write_arff(test.mldr, test.xml.name, write.xml = T)
    
    #####################################################################
    cat("\n\n\t#=========================================#")
    cat("\n\t# Execute BR MULAN\n")
    
    mulan = paste("/usr/lib/jvm/java-1.8.0-openjdk-amd64/bin/java -Xmx8g -jar ", 
                  diretorios$folderUtils, "/mymulanexec.jar", sep="")
    
    mulan.str <- paste(mulan, " -t ", train.name, " -T ", test.name, " -x ",
                       train.xml.name, ".xml -o out.csv -c J48 -a BR", sep = "")
    
    setwd(folderSplit)
    tempo <- system.time(res <- system(mulan.str))
    
    if(res!=0){
      cat("\n\tThere's some problem with BR Mulan\n")
      break 
    } else {
      cat("\n\tBR Mulan executed with sucess!\n")
    }
    
    result_set <- t(data.matrix(tempo))
    setwd(folderSplit)
    write.csv(result_set, "mulan-runtime.csv")
    
    cat("\n\t#=========================================#\n\n")
    
    
    #####################################################################
    cat("\n\tOpen mulan preds")
    setwd(folderSplit)
    mulan.preds <- data.frame(as.matrix(read.csv("pred_out.csv", header = FALSE)))
    names(mulan.preds ) = names.labels
    
    #####################################################################
    cat("\n\tOpen test file")
    test = paste(folderSplit, "/", ds$Name, "-split-ts-", f, sep="")
    test.file <- mldr(test, force_read_from_file = T)
    
    #####################################################################
    cat("\n\tMULAN result")
    mulan.result <- multilabel_evaluate(test.file, mulan.preds, labels=TRUE)
    setwd(folderSplit)
    write.csv(data.frame(mulan.result$multilabel), "mulan-prediction-1.csv")
    write.csv(data.frame(mulan.result$labels), "mulan-prediction-2.csv")
    
    #####################################################################
    cat("\n\tUTIML Threshold")
    utiml.threshold <- scut_threshold(mulan.preds, test.file)
    final.predictions <- data.frame(as.matrix(fixed_threshold(mulan.preds, 
                                                              utiml.threshold)))
    
    #####################################################################
    cat("\n\tSaving preds")
    setwd(folderSplit)
    write.csv(final.predictions, "y_predict.csv", row.names = FALSE)
    write.csv(y_true, "y_true.csv", row.names = FALSE)
    
    names(y_true) = paste(names.labels, "-true", sep="")
    names(final.predictions) = paste(names.labels, "-pred", sep="")
    names(mulan.preds ) = paste(names.labels, "-original", sep="")
    as.matrix(mulan.preds)
    
    all.preds = cbind(y_true, final.predictions, mulan.preds)
    
    setwd(folderSplit)
    write.csv(all.preds, "fold-predictions.csv", row.names = FALSE)
    
    #####################################################################
    cat("\n\tDeleting files")
    setwd(folderSplit)
    unlink(train.name)
    unlink(test.name)
    unlink(paste(train.xml.name, ".xml", sep=""))
    unlink(paste(test.xml.name, ".xml", sep=""))
    unlink("pred_out.csv")
    
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
evaluate.mulan <- function(ds, dataset_name, number_folds, folderResults){    
  
  
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
gather.mulan <- function(ds, dataset_name, number_folds, folderResults){
  
  diretorios = directories(dataset_name, folderResults) 
  
  retorno = list()
  
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
  cat("\n########################################################")
  cat("\n# END EVALUATED                                        #") 
  cat("\n########################################################")
  cat("\n\n\n\n")
}




##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################

