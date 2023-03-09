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
execute.python <- function(ds, 
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
    FolderRoot = "~/Local-Partitions"
    FolderScripts = "~/Local-Partitions/R"
    
    ####################################################################################
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
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
    nome_arq_tr = paste(diretorios$folderCVTR, "/" ,
                        dataset_name, "-Split-Tr-", f, ".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))
    
    #######################################################################################
    cat("\n\tOpen Validation file ")
    nome_arq_vl = paste(diretorios$folderCVVL, "/" ,
                        dataset_name, "-Split-Vl-", f, ".csv", sep="")
    arquivo_vl = data.frame(read.csv(nome_arq_vl))
    
    ########################################################################################
    cat("\n\tOpen Test file ")
    nome_arq_ts = paste(diretorios$folderCVTS, "/", 
                        dataset_name, "-Split-Ts-", f, ".csv", sep="")
    arquivo_ts = data.frame(read.csv(nome_arq_ts))
    
    ##########################################################################
    labels.indices = seq(ds$LabelStart, ds$LabelEnd, by=1)
    
    ##########################################################################
    mldr.teste = mldr_from_dataframe(arquivo_ts, labelIndices = labels.indices)
    
    
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
    
    
    ##################################################################
    # EXECUTE ECC PYTHON
    # /home/biomal/Local-Partitions/Utils/br-python
    str.execute = paste("python3 ", 
                        diretorios$folderUtils,
                        "/main.py ", 
                        nome_arq_tr, " ",
                        nome_arq_vl,  " ",
                        nome_arq_ts, " ", 
                        ds$AttEnd, " ",
                        ds$LabelEnd, " ", 
                        folderSplit,
                        sep="")
    
    # EXECUTA
    res = print(system(str.execute))
    
    if(res!=0){
      break
    }
    
    
    ###################
    setwd(folderSplit)
    y_preds = data.frame(read.csv("y_pred.csv"))
    y_trues = data.frame(read.csv("y_true.csv"))
    y_probas = data.frame(read.csv("y_proba.csv"))     
    
    
    #####################################################################
    nomes.rotulos = colnames(y_trues)
    names(y_probas) = nomes.rotulos
    
    
    #####################################################################
    cat("\nPlot ROC curve")
    roc.curva(predictions = y_preds,
              probabilities = y_probas,
              test = mldr.teste,
              Folder = folderSplit)
    
    ##############################################
    cat("\nInformações das predições")
    predictions.information(nomes.rotulos=nomes.rotulos, 
                            proba = y_probas, 
                            preds = y_preds, 
                            trues = y_trues, 
                            folder = folderSplit)
    
    #####################################################################
    cat("\nSave original and pruned predictions")
    pred.o = paste(colnames(y_preds), "-pred", sep="")
    names(y_preds) = pred.o
    
    true.labels = paste(colnames(y_trues), "-true", sep="")
    names(y_trues) = true.labels
    
    proba = paste(colnames(y_probas), "-proba", sep="")
    names(y_probas) = proba
    
    all.predictions = cbind(y_preds, y_trues, y_probas)
    
    setwd(folderSplit)
    write.csv(all.predictions, "folder-predictions.csv", row.names = FALSE)
    
    
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
evaluate.python <- function(ds, dataset_name, number_folds, folderResults){    
  
  
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
  f = 1
  avaliaParalel <- foreach (f = 1:number_folds) %dopar%{    
    #while(f<=number_folds){
    
    FolderRoot = "~/Local-Partitions"
    FolderScripts = "~/Local-Partitions/R"
    
    ####################################################################################
    setwd(FolderScripts)
    source("utils.R")
    
    ####################################################################################
    setwd(FolderScripts)
    source("libraries.R")
    
    ####################################################################################
    diretorios = directories(dataset_name, folderResults)
    
    cat("\n\nSplit: ", f)    
    
    folderSplit = paste(diretorios$folderLocal, "/Split-", f, sep="")
    
    ####################################################################################
    cat("\nAbrindo pred and true")
    setwd(folderSplit)
    y_pred = data.frame(read.csv("y_pred.csv"))
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
    
    
    ###############################################################
    conf.mat = data.frame(confmat$TPl, confmat$FPl,
                          confmat$FNl, confmat$TNl)
    names(conf.mat) = c("TP", "FP", "FN", "TN")
    
    
    # porcentagem
    conf.mat.perc = data.frame(conf.mat/nrow(y_true))
    names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
    
    # calculando o total de rótulos classificados errados
    wrong = conf.mat$FP + conf.mat$FN
    
    # calculando a porcentagem de rótulos classificados errados
    wrong.perc = wrong/nrow(y_true)
    
    # calculando o total de rótulos classificados corretamente
    correct = conf.mat$TP + conf.mat$TN
    
    # calculando a porcentagem de rótulos classificados corretamente
    correct.perc = correct/nrow(y_true)
    
    conf.mat = data.frame(conf.mat, conf.mat.perc, wrong, correct, 
                          wrong.perc, correct.perc)
    
    setwd(folderSplit)
    write.csv(conf.mat, "matrix-confusion.csv")
    
    
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
gather.python <- function(ds, dataset_name, number_folds, folderResults){
  
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
  
  final.proba.micro.auc = c(0)
  final.proba.macro.auc = c(0)
  final.proba.auc = c(0)
  final.proba.ma.mi.auc = c(0)
  
  final.pred.micro.auc = c(0)
  final.pred.macro.auc = c(0)
  final.pred.auc = c(0)
  
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
    
    setwd(folderSplit)
    
    #################################
    proba.auc = data.frame(read.csv("proba-auc.csv"))
    names(proba.auc) = c("fold", "value")
    final.proba.auc = rbind(final.proba.auc, proba.auc)
    
    proba.micro.auc = data.frame(read.csv("proba-micro-auc.csv"))
    names(proba.micro.auc) = c("fold", "value")
    final.proba.micro.auc = rbind(final.proba.micro.auc, proba.micro.auc)
    
    proba.macro.auc = data.frame(read.csv("proba-macro-auc.csv"))
    names(proba.macro.auc) = c("fold", "value")
    final.proba.macro.auc = rbind(final.proba.macro.auc, proba.macro.auc)
    
    proba.ma.mi.auc = data.frame(read.csv("y_proba_mami.csv"))
    final.proba.ma.mi.auc = rbind(final.proba.ma.mi.auc, proba.ma.mi.auc)
    
    ##################
    pred.auc = data.frame(read.csv("pred-auc.csv"))
    names(pred.auc) = c("fold", "value")
    final.pred.auc = rbind(final.pred.auc, pred.auc)
    
    pred.micro.auc = data.frame(read.csv("pred-micro-auc.csv"))
    names(pred.micro.auc) = c("fold", "value")
    final.pred.micro.auc = rbind(final.pred.micro.auc, pred.micro.auc)
    
    pred.macro.auc = data.frame(read.csv("pred-macro-auc.csv"))
    names(pred.macro.auc) = c("fold", "value")
    final.pred.macro.auc = rbind(final.pred.macro.auc, pred.macro.auc)
    
    f = f + 1
    gc()
  } 
  
  
  # save measures
  setwd(diretorios$folderLocal)
  names(confMatFinal) = c("Measures", folds)
  write.csv(confMatFinal, paste(dataset_name, "-Local-Test-Evaluated.csv", sep=""), row.names = FALSE)
  
  fold = seq(1, number_folds, by =1)
  
  final.proba.auc = final.proba.auc[-1,]
  final.proba.auc = data.frame(fold, auc = final.proba.auc$value)
  
  final.proba.micro.auc = final.proba.micro.auc[-1,]
  final.proba.micro.auc = data.frame(fold, micro.auc = final.proba.micro.auc$value)
  
  final.proba.macro.auc = final.proba.macro.auc[-1,]
  final.proba.macro.auc = data.frame(fold, macro.auc = final.proba.macro.auc$value)
  
  final.proba.ma.mi.auc = final.proba.ma.mi.auc[-1,]
  final.proba.ma.mi.auc = data.frame(fold, final.proba.ma.mi.auc)
  
  setwd(diretorios$folderLocal)
  write.csv(final.proba.auc, "proba-auc.csv", row.names = FALSE)  
  write.csv(final.proba.macro.auc, "proba-macro-auc.csv", row.names = FALSE)  
  write.csv(final.proba.micro.auc, "proba-micro-auc.csv", row.names = FALSE)
  write.csv(final.proba.ma.mi.auc, "proba-ma-mi-auprc.csv", row.names = FALSE)  
  
  #################
  final.pred.auc = final.pred.auc[-1,]
  final.pred.auc = data.frame(fold, auc = final.pred.auc$value)
  
  final.pred.micro.auc = final.pred.micro.auc[-1,]
  final.pred.micro.auc = data.frame(fold, micro.auc = final.pred.micro.auc$value)
  
  final.pred.macro.auc = final.pred.macro.auc[-1,]
  final.pred.macro.auc = data.frame(fold, macro.auc = final.pred.macro.auc$value)
  
  setwd(diretorios$folderLocal)
  write.csv(final.pred.auc, "pred-auc.csv", row.names = FALSE)  
  write.csv(final.pred.macro.auc, "pred-macro-auc.csv", row.names = FALSE)  
  write.csv(final.pred.micro.auc, "pred-micro-auc.csv", row.names = FALSE)
  
  
  #######################
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

