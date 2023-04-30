##############################################################################
# LOCAL PARTITIONS                                                           #
import sys
from sklearn.metrics import average_precision_score
import matplotlib.pyplot as plt
import pandas as pd

y_true = pd.read_csv(sys.argv[1])
y_pred = pd.read_csv(sys.argv[2])
name = sys.argv[3]

micro = average_precision_score(y_true, y_pred, average = "micro")
macro = average_precision_score(y_true, y_pred, average = "macro")

res = pd.DataFrame([micro, macro]).T
res.columns = ["Micro-AUPRC", "Macro-AUPRC"]
res.to_csv(name, index=False)
