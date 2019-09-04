"""
Created on Mon May 20 19:32:12 2019

@author: marrsantos
"""
# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os

#diretório de trabalho
data_folder = os.getcwd()+'/'

# Opening the dataset
dftrain = pd.read_csv('train.csv')
dfreal = pd.read_csv('test.csv')

# Data preparation
# Training/test dataset
dftrain.iloc[:, 1:4]
dftrain['Surname'] = dftrain.Name.str.split(',',1,expand=True)[0]
dftrain = dftrain.drop(columns='Name')
dftrain['Embarked'].fillna("NoEmbarked", inplace = True)
dftrain['Cabin'].fillna("NoCabin", inplace = True)
dftrain['Age'].fillna(dftrain.Age.median(), inplace = True)
# Real data for prediction
dfreal.iloc[:, 1:4]
dfreal['Surname'] = dfreal.Name.str.split(',',1,expand=True)[0]
dfreal = dfreal.drop(columns='Name')
dfreal['Embarked'].fillna("NoEmbarked", inplace = True)
dfreal['Cabin'].fillna("NoCabin", inplace = True)
dfreal['Age'].fillna(dfreal.Age.median(), inplace = True)
dfreal['Fare'].fillna(dfreal.Fare.median(), inplace = True)

# creating numeric vectors (Training dataset)
X = dftrain.iloc[:, 2:13].values
y = dftrain.iloc[:, 1].values
dftrain = dftrain.drop(columns='Survived')
# creating numeric vectors (Real dataset)
Xr = dfreal.iloc[:, 1:12].values

# Enconding categorical variables
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
labelencoder_X = LabelEncoder()
# Training dataset
X[:,1] = labelencoder_X.fit_transform(X[:,1])
X[:,5] = labelencoder_X.fit_transform(X[:,5])
X[:,7] = labelencoder_X.fit_transform(X[:,7])
X[:,8] = labelencoder_X.fit_transform(X[:,8])
X[:,9] = labelencoder_X.fit_transform(X[:,9])
# Real dataset
Xr[:,1] = labelencoder_X.fit_transform(Xr[:,1])
Xr[:,5] = labelencoder_X.fit_transform(Xr[:,5])
Xr[:,7] = labelencoder_X.fit_transform(Xr[:,7])
Xr[:,8] = labelencoder_X.fit_transform(Xr[:,8])
Xr[:,9] = labelencoder_X.fit_transform(Xr[:,9])

#Creating dummies variables
onehotencoder = OneHotEncoder(handle_unknown='ignore')
# Training dataset
X = onehotencoder.fit_transform(X).toarray()
# Real dataset
Xr = onehotencoder.transform(Xr).toarray()

# Splitting the dataset into the training and test
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X,y, test_size = 0.25, random_state = 0)

#Feature scaling
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)
# Real dataset
X_real = sc_X.fit_transform(Xr)

# Fitting classifier to the Training set
from sklearn.linear_model import LogisticRegression
classifier =  LogisticRegression(random_state=0)
classifier.fit(X_train,y_train)

#Predicting y for Training dataset
y_pred = classifier.predict(X_test) 

#Confusing Matrix (training)
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)
cm

#saving results (Training)
dfresults = pd.DataFrame(data=y_pred)
dfresults.to_csv(data_folder+'resultsPY.csv',index=False)

# Predicting y for Real dataset
yr_pred = classifier.predict(X_real) 

#saving results (Real dataset)
Results = {'PassengerId': dfreal.PassengerId, 'Survived': yr_pred}
dfresults = pd.DataFrame(data=Results)
dfresults.to_csv(data_folder+'resultsRealPY.csv',index=False)

#########################################
# Visualising the Training set results
from matplotlib.colors import ListedColormap
X_set, y_set = X_train, y_train
X1, X2 = np.meshgrid(np.arange(start = X_set[:, 0].min() - 1, stop = X_set[:, 0].max() + 1, step = 0.01),
                     np.arange(start = X_set[:, 1].min() - 1, stop = X_set[:, 1].max() + 1, step = 0.01))
plt.contourf(X1, X2, classifier.predict(np.array([X1.ravel(), X2.ravel()]).T).reshape(X1.shape),
             alpha = 0.75, cmap = ListedColormap(('red', 'green')))
plt.xlim(X1.min(), X1.max())
plt.ylim(X2.min(), X2.max())
for i, j in enumerate(np.unique(y_set)):
    plt.scatter(X_set[y_set == j, 0], X_set[y_set == j, 1],
                c = ListedColormap(('red', 'green'))(i), label = j)
plt.title('Classifier (Training set)')
plt.xlabel('Age')
plt.ylabel('Estimated Salary')
plt.legend()
plt.show()

