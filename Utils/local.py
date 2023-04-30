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
