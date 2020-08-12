# region Ejemplo con sklearn
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import warnings
warnings.filterwarnings("ignore")
from pylab import rcParams
# % matplotlib inline

# Loading the CSV with pandas
data = pd.read_csv('C:/Users/IOR_C/Downloads/Telco-Customer-Churn.csv')

# Data to plot
sizes = data['Churn'].value_counts(sort = True)
colors = ["grey","purple"]
rcParams['figure.figsize'] = 5,5

# Plot
plt.pie(sizes, colors=colors,
        autopct='%1.1f%%', shadow=True, startangle=270,)
plt.title('Percentage of Churn in Dataset')
plt.show()

# Cleaning dataset
data.drop(['customerID'], axis=1, inplace=True)
data['TotalCharges'] = pd.to_numeric(data['TotalCharges'].replace(' ', np.nan))
data.dropna(subset=['TotalCharges'], inplace=True)

# Prepare the dataset
data = pd.get_dummies(data, columns=['gender', 'Contract', 'PaymentMethod', 'InternetService'])
data.replace('No internet service|No phone service', 0, regex=True, inplace=True)
data.replace('No', 0, regex=True, inplace=True)
data.replace('Yes', 1, regex=True, inplace=True)

# Separate datasets
Y = data["Churn"].values
X = data.drop(labels = ["Churn"], axis = 1)

# Create Train & Test Data
from sklearn.model_selection import train_test_split
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=101)

# Create the model
from sklearn.linear_model import LogisticRegression
model = LogisticRegression()
result = model.fit(X_train, Y_train)

# Test the model
from sklearn import metrics
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import roc_auc_score
prediction_test = model.predict(X_test)

# Print the evaluation
print(metrics.accuracy_score(Y_test, prediction_test))
print(classification_report(Y_test, prediction_test))
print(confusion_matrix(Y_test, prediction_test))
print(roc_auc_score(Y_test, prediction_test))


# To get the weights of all the variables
weights = pd.Series(model.coef_[0], index=X.columns.values)
print(weights.sort_values(ascending = False))

# Pruebas
dfp = data.copy()
dfp['pred'] = model.predict(X)
dfp['pred_prob'] = model.predict_proba(X_test)[:, 1]
dfp['test'] = np.where(dfp.pred == dfp.Churn, 1, 0)

X_nuevo = np.array([1, 2, 3, 4, 5, 6]).reshape(-1,1)
# endregion

# region Ejemplo con statsmodels
import pandas as pd
import numpy as np
import statsmodels.api as sm

# Loading the CSV with pandas
data = pd.read_csv('C:/Users/IOR_C/Downloads/Telco-Customer-Churn.csv')

# Cleaning dataset
data.drop(['customerID'], axis=1, inplace=True)
data['TotalCharges'] = pd.to_numeric(data['TotalCharges'].replace(' ', np.nan))
data.dropna(subset=['TotalCharges'], inplace=True)

# Prepare the dataset
data = pd.get_dummies(data, columns=['gender', 'Contract', 'PaymentMethod', 'InternetService'])
data.replace('No internet service|No phone service', 0, regex=True, inplace=True)
data.replace('No', 0, regex=True, inplace=True)
data.replace('Yes', 1, regex=True, inplace=True)

Y = data["Churn"].values
X = data.drop(labels=["Churn"], axis=1)

# Modelo
model = sm.Logit(Y, X)
result = model.fit(method='newton')

logmodel = sm.Logit(Y, sm.add_constant(X))
logmodel.fit().summary()

result.summary()


# endregion
