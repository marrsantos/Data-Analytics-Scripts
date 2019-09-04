"""
Created on Mon May 20 19:32:12 2019

@author: marrsantos
"""
# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import GridSearchCV
from time import gmtime, strftime
from sklearn.model_selection import cross_val_score
from keras.layers import Dropout

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

# Correlation matrix for dftrain
dftrain.corr()
dftrain.real()

# creating numeric vectors (Training dataset)
X = dftrain.iloc[:, 2:13].values
y = dftrain.iloc[:, 1].values
dftrain = dftrain.drop(columns='Survived')
# creating numeric vectors (Real dataset)
Xr = dfreal.iloc[:, 1:12].values

# Enconding categorical variables
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
# Training dataset
labelencoder_X1 = LabelEncoder()
X[:,1] = labelencoder_X1.fit_transform(X[:,1])
labelencoder_X2 = LabelEncoder()
X[:,5] = labelencoder_X2.fit_transform(X[:,5])
labelencoder_X3 = LabelEncoder()
X[:,7] = labelencoder_X3.fit_transform(X[:,7])
labelencoder_X4 = LabelEncoder()
X[:,8] = labelencoder_X4.fit_transform(X[:,8])
labelencoder_X5 = LabelEncoder()
X[:,9] = labelencoder_X5.fit_transform(X[:,9])
# Real dataset
labelencoder_Xr1 = LabelEncoder()
Xr[:,1] = labelencoder_X1.fit_transform(Xr[:,1])
labelencoder_Xr2 = LabelEncoder()
Xr[:,5] = labelencoder_Xr2.fit_transform(Xr[:,5])
labelencoder_Xr3 = LabelEncoder()
Xr[:,7] = labelencoder_Xr3.fit_transform(Xr[:,7])
labelencoder_Xr4 = LabelEncoder()
Xr[:,8] = labelencoder_Xr4.fit_transform(Xr[:,8])
labelencoder_Xr5 = LabelEncoder()
Xr[:,9] = labelencoder_Xr5.fit_transform(Xr[:,9])

#Creating dummies variables
onehotencoder = OneHotEncoder(handle_unknown='ignore')
# Training dataset
Xaux = X[:,[2,6]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,3,4,5,7,8,9]
for i in a: 
    Xaux1 = onehotencoder.fit_transform(X[:,i].reshape(-1,1)).toarray()  
    Xaux2 = np.append(Xaux2,Xaux1[:,1:],axis=1)
X1 = np.append(Xaux2,Xaux,axis=1)
X1 = X1.astype(float)

# Real dataset
Xaux = Xr[:,[2,6]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,3,4,5,7,8,9]
for i in a: 
    Xaux1 = onehotencoder.fit_transform(Xr[:,i].reshape(-1,1)).toarray()  
    Xaux2 = np.append(Xaux2,Xaux1[:,1:],axis=1)
Xr1 = np.append(Xaux2,Xaux,axis=1)
Xr1 = Xr1.astype(float)

# Splitting the dataset into the training and test
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X1,y, test_size = 0.25, random_state = 0)

#Feature scaling
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)
# Real dataset
X_real = sc_X.fit_transform(Xr1)

# Importing the Keras libraries and packages
from keras.models import Sequential
from keras.layers import Dense

# Initialising the ANN
classifier = Sequential()

# Adding the input layer and the first hidden layer (units=(input_layer+output_layer)/2)
classifier.add(Dense(units = 757, kernel_initializer = 'uniform', activation = 'relu', input_dim = 1512))

# Adding the second hidden layer
classifier.add(Dense(units = 80, kernel_initializer = 'uniform', activation = 'relu'))

# Adding the third hidden layer (number of hidden neurons should be less than twice the size of the input layer)
#classifier.add(Dense(units = 150, kernel_initializer = 'uniform', activation = 'relu'))

# Adding the output layer
classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))

# Compiling the ANN (if dependent variable is categorical then it should be categorical_crossentropy)
classifier.compile(optimizer = 'sgd', loss = 'binary_crossentropy', metrics = ['accuracy'])

# Fitting the ANN to the Training set
hist = classifier.fit(X_train, y_train, batch_size = 10, epochs = 20)

# Part 3 - Making predictions and evaluating the model

# Predicting the Test set results
y_pred = classifier.predict(X_test)
y_pred = (y_pred > 0.5)

# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)
(117+47)/(117+47+22+37)

# Evaluating the ANN 
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import cross_val_score

def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(units = 757, kernel_initializer = 'uniform', activation = 'relu', input_dim = 1512))
    classifier.add(Dense(units = 757, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dropout(p = 0.2)) # it can be removed
    classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier, batch_size = 10, nb_epoch = 100)
accuracies = cross_val_score(estimator = classifier, X = X_train, y = y_train, cv = 10, n_jobs = -1)
media=accuracies.mean()
mediavar = accuracies.var()
mediasd = accuracies.std()

# checking for overfitting (Dropout Regularization to reduce overfitting if needed)
#from keras.layers import Dropout
# include classifier.add(Dropout(p = 0.1))   ## it could be 0.1 until 0.4
# you can include it for the different layers (input and hidden)
# it will randomily exclude some neurons allowing the model learn some independent correlations.

# Tuning the ANN (GridSearch)
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import GridSearchCV
from time import gmtime, strftime

# medindo o tempo
time_ini = strftime("%Y-%m-%d %H:%M:%S", gmtime())
def build_classifier(Optimizer, Units):
    classifier = Sequential()
    classifier.add(Dense(units = Units, kernel_initializer = 'uniform', activation = 'relu', input_dim = 1512))
    classifier.add(Dense(units = Units, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = Optimizer, loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier)
parameters = {'batch_size': [25,64],
              'nb_epoch': [100,500],
              'Optimizer': ['adam','rmsprop'],
              'Units': [350,1000]}
grid_search = GridSearchCV(estimator = classifier,
                           param_grid = parameters,
                           scoring = 'accuracy',
                           cv = 10)
grid_search = grid_search.fit(X_train, y_train)
#
time_end = strftime("%Y-%m-%d %H:%M:%S", gmtime())
time_ini
time_end
#
best_parameters = grid_search.best_params_
best_accurary = grid_search.best_score_









#saving results (Training)
dfresults = pd.DataFrame(data=y_pred)
dfresults.to_csv(data_folder+'resultsPY.csv',index=False)

