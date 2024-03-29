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



############################################################################
#
############################################################################
gather.files.clus <- function(parameters){
  
  # from fold = 1 to number_folds
  s = 1
  # gfflParalel <- foreach(s = 1:parameters$Config.File$Number.Folds) %dopar% {
  while(s<=parameters$Config.File$Number.Folds){
    
    cat("\nFold: ", s)
    # creating folder
    FS = paste(parameters$Directories$FolderLocal, "/Split-", s, sep="")
    if(dir.exists(FS)==FALSE){dir.create(FS)}
    
    # names files
    nome_tr = paste(parameters$Config.File$Dataset.Name, "-Split-Tr-", s, ".arff", sep="")
    nome_ts = paste(parameters$Config.File$Dataset.Name, "-Split-Ts-", s, ".arff", sep="")
    nome_vl = paste(parameters$Config.File$Dataset.Name, "-Split-Vl-", s, ".arff", sep="")
    
    # copying train files
    setwd(parameters$Directories$FolderCVTR)
    if(file.exists(nome_tr) == TRUE){
      setwd(parameters$Directories$FolderCVTR)
      copia = paste(parameters$Directories$FolderCVTR, "/", nome_tr, sep="")
      cola = paste(FS, "/", nome_tr, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    } else {
      break
    }
    
    # copying test files
    setwd(parameters$Directories$FolderCVTS)
    if(file.exists(nome_ts) == TRUE){
      setwd(parameters$Directories$FolderCVTS)
      copia = paste(parameters$Directories$FolderCVTS, "/", nome_ts, sep="")
      cola = paste(FS, "/", nome_ts, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    } else {
      break
    }
    
    # copying test files
    setwd(parameters$Directories$FolderCVVL)
    if(file.exists(nome_vl) == TRUE){
      setwd(parameters$Directories$FolderCVVL)
      copia = paste(parameters$Directories$FolderCVVL, "/", nome_vl, sep="")
      cola = paste(FS, "/", nome_vl, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    } else {
      break
    }
    
    setwd(parameters$Directories$FolderCVVL)
    validation = data.frame(foreign::read.arff(nome_vl))
    
    setwd(parameters$Directories$FolderCVTR)
    train = data.frame(foreign::read.arff(nome_tr))
    
    treino = rbind(train, validation)
    
    # unlink(nome_ts)
    # unlink(nome_vl)
    unlink(nome_tr)
    
    nome_tr_2 = paste(FS, "/", parameters$Config.File$Dataset.Name, "-Split-Tr-", s, ".csv", sep="")
    write.csv(treino, nome_tr_2, row.names = FALSE)
    
    ########################################################################
    converteArff <- function(arg1, arg2, arg3, folderUtils){
      str = paste("java -jar ", parameters$Directories$FolderUtils,
                  "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      print(system(str))
      cat("\n\n")
    }
    
    #########################################################################
    # Targets
    inicio = parameters$Dataset.Info$LabelStart
    fim = ncol(treino)
    ifr = data.frame(inicio, fim)
    write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
    
    
    ########################################################################
    # TRAIN: Convert CSV to ARFF
    arg1Tr = nome_tr_2
    arg2Tr = paste(FS, "/", nome_tr, sep="")
    arg3Tr = paste(inicio, "-", fim, sep="")
    converteArff(arg1Tr, arg2Tr, arg3Tr, parameters$Directories$FolderUtils)
    
    
    #########################################################################
    str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arg2Tr, sep="")
    print(system(str0))
    
    # unlink(nome_tr_2)
    
    s = s + 1
    gc()
  }
  
  gc()
  cat("\n#########################################################")
  cat("\n# END FUNCTION COPY                                     #") 
  cat("\n#########################################################")
  cat("\n\n\n\n")
  
}


##########################################################################
#
##########################################################################
execute.clus <- function(parameters){
  # from fold = 1 to number_folds
  f = 1
  slParalel <- foreach (f = 1:parameters$Config.File$Number.Folds) %dopar%{
  # while(f<=parameters$Config.File$Number.Folds){
    
    FolderRoot = "~/Local-Partitions"
    FolderScripts = paste(FolderRoot, "/R", sep="")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    # specifying fold
    folderSplit = paste(parameters$Directories$FolderLocal, "/Split-", f, sep="")
    
    nome_tr = paste(parameters$Config.File$Dataset.Name, "-Split-Tr-", f, ".arff", sep="")
    nome_ts = paste(parameters$Config.File$Dataset.Name, "-Split-Ts-", f, ".arff", sep="")
    nome_vl = paste(parameters$Config.File$Dataset.Name, "-Split-Vl-", f, ".arff", sep="")
    
    # get train
    setwd(folderSplit)
    treino = data.frame(foreign::read.arff(nome_tr))
    atributosTr = treino[parameters$Dataset.Info$AttStart:parameters$Dataset.Info$AttEnd]
    classesTr = treino[parameters$Dataset.Info$LabelStart:parameters$Dataset.Info$LabelEnd]
    
    # get teste
    teste = data.frame(foreign::read.arff(nome_ts))
    atributosTs = teste[parameters$Dataset.Info$AttStart:parameters$Dataset.Info$AttEnd]
    classesTs = teste[parameters$Dataset.Info$LabelStart:parameters$Dataset.Info$LabelEnd]
    
    # from label 1 to total labels
    j = 1
    while(j<= parameters$Dataset.Info$Labels){
      
      library("foreign")
      
      cat("\n\tLabel [", parameters$NamesLabel$labels[j], "] \n")
      
      # get train
      classeTr = data.frame(classesTr[,parameters$NamesLabel$labels[j]])
      names(classeTr) = toString(parameters$NamesLabel$labels[j])
      thisGroupTr = cbind(atributosTr, classeTr)
      
      # get test
      classeTs = data.frame(classesTs[,parameters$NamesLabel$labels[j]])
      names(classeTs) = toString(parameters$NamesLabel$labels[j])
      thisGroupTs = cbind(atributosTs, classeTs)
      
      # specifying folder
      folderLabel = paste(folderSplit, "/", parameters$NamesLabel$labels[j], sep="")
      if(dir.exists(folderLabel)==TRUE){
        cat("\n")
      } else {
        dir.create(folderLabel)
      }
      
      cat("\n\tCreating Train File")
      #cat("\n\t[", parameters$NamesLabel$labels[j], "]: Save Train CSV\n")
      rotuloTr = paste(parameters$NamesLabel$labels[j], "-tr-", f, ".csv", sep="")
      setwd(folderLabel)
      write.csv(thisGroupTr, rotuloTr, row.names = FALSE)
      
      #cat("\n\t[", parameters$NamesLabel$labels[j], "]: Convert TRAIN CSV to ARFF\n")
      setwd(folderLabel)
      arg1Tr = rotuloTr
      arg2Tr = paste(parameters$NamesLabel$labels[j], "-tr-", f, ".arff", sep="")
      arg3Tr = paste(ncol(thisGroupTr), "-", ncol(thisGroupTr), sep="")
      str = paste("java -jar ", parameters$Directories$FolderUtils,
                  "/R_csv_2_arff.jar ", arg1Tr , " ", arg2Tr, " ", arg3Tr , sep="")
      cat("\n")
      print(system(str))
      cat("\n")
      
      #cat("\n\t[", parameters$NamesLabel$labels[j], "]: Verify and correct {0} and {1}\n")
      arquivo = paste(folderLabel, "/", arg2Tr, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
      cat("\n")
      print(system(str0))
      cat("\n")
      
      cat("\n\tCreating Test File")
      #cat("\n\t[", parameters$NamesLabel$labels[j], "]: Save Test CSV\n")
      rotuloTs = paste(parameters$NamesLabel$labels[j], "-ts-", f, ".csv", sep="")
      setwd(folderLabel)
      write.csv(thisGroupTs, rotuloTs, row.names = FALSE)
      
      #cat("\n\t[", parameters$NamesLabel$labels[j], "]: Convert TEST CSV to ARFF\n")
      setwd(folderLabel)
      arg1Ts = rotuloTs
      arg2Ts = paste(parameters$NamesLabel$labels[j], "-ts-", f, ".arff", sep="")
      arg3Ts = paste(ncol(thisGroupTs), "-", ncol(thisGroupTs), sep="")
      str = paste("java -jar ", parameters$Directories$FolderUtils,
                  "/R_csv_2_arff.jar ", arg1Ts, " ", arg2Ts, " ", arg3Ts, sep="")
      cat("\n")
      print(system(str))
      cat("\n")
      
      #cat("\n\t[", parameters$NamesLabel$labels[j], "]: Verify and correct {0} and {1}\n")
      arquivo = paste(folderLabel, "/", arg2Ts, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
      cat("\n")
      print(system(str0))
      cat("\n")
      
      setwd(folderLabel)
      cat("\n\t[", parameters$NamesLabel$labels[j], "]: Generated Config File\n")
      
      setwd(folderLabel)
      nome_config = paste(parameters$NamesLabel$labels[j], "-", f , ".s", sep="")
      sink(nome_config, type = "output")
      
      cat("[General]")
      cat("\nCompatibility = MLJ08")
      
      cat("\n\n[Data]")
      cat(paste("\nFile = ", arg2Tr, sep=""))
      
      cat(paste("\nTestSet = ", arg2Ts, sep=""))
      
      cat("\n\n[Attributes]")
      cat("\nReduceMemoryNominalAttrs = yes")
      
      cat("\n\n[Attributes]")
      cat(paste("\nTarget = ", ncol(thisGroupTr), sep=""))
      cat("\nWeights = 1")
      
      cat("\n")
      cat("\n[Tree]")
      cat("\nHeuristic = VarianceReduction")
      cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
      
      cat("\n\n[Model]")
      cat("\nMinimalWeight = 5.0")
      
      cat("\n\n[Output]")
      cat("\nWritePredictions = {Test}")
      cat("\n")
      sink()
      
      cat("\n\tExecute CLUS")
      str = paste("java -jar ", parameters$Directories$FolderUtils,
                  "/Clus.jar ", nome_config, sep="")
      cat("\n")
      print(system(str))
      cat("\n")
      
      # delete files
      cinco = paste(parameters$NamesLabel$labels[j], "-", f, ".s", sep="")
      seis = paste(parameters$NamesLabel$labels[j], "-", f, ".model", sep="")
      sete = paste(parameters$NamesLabel$labels[j], "-", f, ".out", sep="")
      oito = paste("Variance_RHE_1.csv")
      
      
      setwd(folderLabel)
      unlink(nome_config, recursive = TRUE)
      unlink(rotuloTr, recursive = TRUE)
      unlink(rotuloTs, recursive = TRUE)
      unlink(arg2Tr, recursive = TRUE)
      unlink(arg2Ts, recursive = TRUE)
      unlink(arg2Ts, recursive = TRUE)
      unlink(cinco, recursive = TRUE)
      unlink(seis, recursive = TRUE)
      unlink(sete, recursive = TRUE)
      unlink(oito, recursive = TRUE)
      
      
      j = j + 1
      gc()
    } # end labels
    
    # delete files
    setwd(folderSplit)
    unlink(nome_tr, recursive = TRUE)
    unlink(nome_ts, recursive = TRUE)
    unlink(nome_vl, recursive = TRUE)
    
    # f = f +1
    gc()
  } # end folds
  
  gc()
  cat("\n#########################################################")
  cat("\n# END FUNCTION EXECUTE                                  #") 
  cat("\n#########################################################")
  cat("\n\n\n\n")
}



#############################################################################
#
#############################################################################
gather.predicts.clus <- function(parameters){
  
  # from fold = 1 to number_folds
  f = 1
  glpParalel <- foreach (f = 1:parameters$Config.File$Number.Folds) %dopar%{
    #while(f<=parameters$Config.File$Number.Folds){
    
    FolderRoot = "~/Local-Partitions"
    FolderScripts = paste(FolderRoot, "/R", sep="")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    # specifying fold
    folderSplit = paste(parameters$Directories$FolderLocal, "/Split-", f, sep="")
    
    # data frame
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)
    
    # from label 1 to total labels
    j = 1
    while(j<=parameters$Dataset.Info$Labels){
      
      cat("\n\tLabel [", parameters$NamesLabel$labels[j], "] \n")
      
      # specifying folder
      folderLabel = paste(folderSplit, "/", parameters$NamesLabel$labels[j], sep="")
      
      # cat("\n\t[", parameters$NamesLabel$labels[j], "]: Open Test.Pred.Arff\n")
      setwd(folderLabel)
      testPred1 = paste(folderLabel, "/", parameters$NamesLabel$labels[j], "-" ,
                        f, ".test.pred.arff", sep="")
      testPred2 = data.frame(foreign::read.arff(testPred1))
      
      #cat("\n\t[", parameters$NamesLabel$labels[j], "]: Y TRUE\n")
      classes = data.frame(testPred2[,1])
      names(classes) = parameters$NamesLabel$labels[j]
      
      #cat("\n\t[", parameters$NamesLabel$labels[j], "]: Y PRED\n")
      coluna = paste("Pruned.p.", parameters$NamesLabel$labels[j], sep="")
      pred = data.frame(testPred2[,coluna])
      names(pred) = parameters$NamesLabel$labels[j]
      
      # put together
      y_true = cbind(y_true, classes)
      y_pred = cbind(y_pred, pred)
      
      j = j + 1
      gc()
    } # end labels
    
    #cat("\nSave Fold: ", f)
    setwd(folderSplit)
    y_true = y_true[,-1]
    y_pred = y_pred[,-1]
    write.csv(y_true, "y_true.csv", row.names = FALSE)
    write.csv(y_pred, "y_predict.csv", row.names = FALSE)
    
    #f = f + 1
    gc()
  } # end folds
  
  gc()
  cat("\n#########################################################")
  cat("\n# END FUNCTION GATHER                                   #") 
  cat("\n#########################################################")
  cat("\n\n\n\n")
}



############################################################################
# 
############################################################################
evaluate.clus <- function(parameters){
  # data frame
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
  # from fold = 1 to number_folds
  f = 1
  avaliaParalel <- foreach (f = 1:parameters$Config.File$Number.Folds) %dopar%{
    #while(f<=number_folds){
    
    library("utiml")
    library("mldr")
    
    cat("\nFold: ", f)
    
    # specifying folder
    folderSplit = paste(parameters$Directories$FolderLocal, "/Split-", f, sep="")
    
    # get true and predict labels
    setwd(folderSplit)
    y_pred = data.frame(read.csv("y_predict.csv"))
    y_true = data.frame(read.csv("y_true.csv"))
    
    # compute measures
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    # save conf matx
    salva3 = paste("ConfMatFold-", f, ".txt", sep="")
    setwd(folderSplit)
    sink(file=salva3, type="output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    resConfMat = multilabel_evaluate(confmat)
    resConfMat = data.frame(resConfMat)
    names(resConfMat) = paste("Fold-", f, sep="")
    setwd(folderSplit)
    write.csv(resConfMat, "ResConfMat.csv")
    
    # delete files
    setwd(folderSplit)
    unlink("y_predict.csv", recursive = TRUE)
    unlink("y_true.csv", recursive = TRUE)
    
    #f = f + 1
    gc()
  } # end folds
  
  gc()
  cat("\n#########################################################")
  cat("\n# END FUNCTION EVALUATE                                #") 
  cat("\n#########################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# FUNCTION GATHER EVALUATION LOCAL PARTITIONS                                                    #
#   Objective                                                                                    #
#      Evaluates the local partitions                                                            #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       parameters$Config.File$Dataset.Name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       folder: path of local partition results                                                  #
#       folderReports: path to store results                                                     #
#   Return                                                                                       #
#       Assessment measures for each local partition                                             #
##################################################################################################
gather.eval.clus <- function(parameters){
  
  # vector with names measures
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
              "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
              "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
              "recall","subset-accuracy","wlp")
  
  # data frame
  confMatFinal = data.frame(measures)
  
  folds = c("")
  
  # from fold = 1 to number_folds
  f = 1
  while(f<=parameters$Config.File$Number.Folds){
    
    cat("\nFold: ", f)
    
    # specifying folder
    folderSplit = paste(parameters$Directories$FolderLocal, "/Split-", f, sep="")
    setwd(folderSplit)
    
    #cat("\n\tOpen ResConfMat ", f)
    confMat = data.frame(read.csv("ResConfMat.csv"))
    names(confMat) = c("Measures", "Fold")
    confMatFinal = cbind(confMatFinal, confMat$Fold)
    
    folds[f] = paste("Fold-", f, sep="")
    
    f = f + 1
    gc()
  } # end folds
  
  # save measures
  setwd(parameters$Directories$FolderLocal)
  names(confMatFinal) = c("Measures", folds)
  write.csv(confMatFinal, paste(parameters$Config.File$Dataset.Name, "-Local-Test-Evaluated.csv", sep=""), row.names = FALSE)
  
  # calculando a média dos 10 folds para cada medida
  media = data.frame(apply(confMatFinal[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(parameters$Directories$FolderLocal)
  write.csv(media, "Mean10Folds.csv", row.names = FALSE)
  
  mediana = data.frame(apply(confMatFinal[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(parameters$Directories$FolderLocal)
  write.csv(mediana, "Median10Folds.csv", row.names = FALSE)
  
  dp = data.frame(apply(confMatFinal[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(parameters$Directories$FolderLocal)
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

