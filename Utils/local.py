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


import sys
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier  
from sklearn.metrics import average_precision_score
from skmultilearn.problem_transform import BinaryRelevance

if __name__ == '__main__':
    
    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    start = int(sys.argv[4])
    end = int(sys.argv[5])
    diretorio = sys.argv[6]
    
    # train = pd.read_csv("/dev/shm/grf-GpositiveGO/Dataset/GpositiveGO/CrossValidation/Tr/GpositiveGO-Split-Tr-1.csv")
    # valid = pd.read_csv("/dev/shm/grf-GpositiveGO/Dataset/GpositiveGO/CrossValidation/Vl/GpositiveGO-Split-Vl-1.csv")
    # test = pd.read_csv("/dev/shm/grf-GpositiveGO/Dataset/GpositiveGO/CrossValidation/Ts/GpositiveGO-Split-Ts-1.csv")
    # start = 912
    # end = 916
    # diretorio = "/dev/shm/grf-GpositiveGO/Local/Split-1/"
    
    
    # juntando treino com validação
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    # treino: separando os atributos e os rótulos
    X_train = train.iloc[:, :start]    # atributos 
    Y_train = train.iloc[:, start:]    # rótulos 
    
    # teste: separando os atributos e os rótulos
    X_test = test.iloc[:, :start]    # atributos 
    Y_test = test.iloc[:, start:]    # rótulos 
    
    # obtendo os nomes dos rótulos
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    
    # obtendo os nomes dos atributos
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)
    
    # parametros do classificador base
    random_state = 0    
    n_estimators = 200
    baseModel = RandomForestClassifier(n_estimators = n_estimators, random_state = random_state)
    classifier = BinaryRelevance(baseModel)
    classifier.fit(X_train, Y_train)
    binary_predictions = classifier.predict(X_test)
    probabilities = classifier.predict_proba(X_test)
    
    true = (diretorio + "/y_true.csv")
    pred = (diretorio + "/y_pred_bin.csv")
    proba = (diretorio + "/y_pred_proba.csv")
    
    test[labels_y_test].to_csv(true, index=False)
    
    binary_predictions_2 = binary_predictions.toarray()
    binary_predictions_2 = pd.DataFrame(binary_predictions_2)
    binary_predictions_2.columns = labels_y_test
    binary_predictions_2.to_csv(pred, index=False)
    
    probabilities_2 = probabilities.toarray()
    probabilities_2 = pd.DataFrame(probabilities_2)
    probabilities_2.columns = labels_y_test
    probabilities_2.to_csv(proba, index=False)
