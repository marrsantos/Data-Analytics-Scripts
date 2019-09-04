"""
Created on Mon May 20 19:32:12 2019

@author: marrsantos
"""
# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os
import random
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import GridSearchCV
from time import gmtime, strftime
from sklearn.model_selection import cross_val_score
from keras.layers import Dropout
from sklearn.metrics import confusion_matrix
from sklearn.decomposition import PCA
from sklearn.decomposition import IncrementalPCA

#diretório de trabalho
data_folder = os.getcwd()+'/'

# Opening the dataset
dftrain = pd.read_csv('train.csv')
dfreal = pd.read_csv('test.csv')

# Data preparation
# Analyzing variables
dftrain.groupby('Survived')['Survived'].agg(['count'])
dftrain.groupby('Embarked')['Survived'].agg(['mean','count'])
#
dftrain['Age'].fillna(dftrain.Age.mean(), inplace = True)
# Completing Embarked with "S" as majority of them
dftrain['Embarked'].fillna("S", inplace = True)
# Completing Fare nan with median
dftrain.groupby('Fare')['Survived'].agg(['mean','count'])
dftrain['Fare'].fillna(dftrain.Fare.mean(), inplace = True)
# Creating Famuly members (Parch + SibSp)
dftrain.groupby('SibSp')['Survived'].agg(['mean','count'])
dftrain.groupby('Parch')['Survived'].agg(['mean','count'])
dftrain['FamilyMembers'] = dftrain['SibSp']+dftrain['Parch']
# Creating SameTicket Column (individuals that bought ticket with other ones)
aaa=dftrain.groupby('Ticket').size().reset_index(name='SameTicket')
dftrain=pd.merge(dftrain, aaa, how='left', on=['Ticket'])
dftrain.groupby('SameTicket')['Survived'].agg(['mean','count'])
#
dftrain['TicketMultiple'] = dftrain.SameTicket>1
dftrain['WithFamily'] = dftrain.FamilyMembers>1
dftrain['hasCabin'] = dftrain.Cabin.notna()
dftrain['IsChild']=dftrain.Age<2
#
###########################################################################
# Process #3
# increasing number of records...
dftrain = pd.concat([dftrain,dftrain,dftrain])
###########################################################################
# removing non-relevant columns
dftrain = dftrain.drop(columns=['PassengerId','Name','Cabin','SibSp','Parch','Ticket','FamilyMembers','SameTicket'])
dftrain.info()
###########################################################################
# Completing Embarked with "S" as majority of them
dfreal['Age'].fillna(dfreal.Age.mean(), inplace = True)
dfreal['Embarked'].fillna("S", inplace = True)
# Completing Fare nan with median
dfreal['Fare'].fillna(dfreal.Fare.mean(), inplace = True)
# Creating Famuly members (Parch + SibSp)
dfreal['FamilyMembers'] = dfreal['SibSp']+dfreal['Parch']
# Creating SameTicket Column (individuals that bought ticket with other ones)
aaa=dfreal.groupby('Ticket').size().reset_index(name='SameTicket')
dfreal=pd.merge(dfreal, aaa, how='left', on=['Ticket'])
#
dfreal['TicketMultiple'] = dfreal.SameTicket>1
dfreal['WithFamily'] = dfreal.FamilyMembers>1
dfreal['hasCabin'] = dfreal.Cabin.notna()
dfreal['IsChild']=dfreal.Age<2

# removing non-relevant columns
dfreal = dfreal.drop(columns=['Name','Cabin','SibSp','Parch','Ticket','FamilyMembers','SameTicket'])
dfreal.info()
###########################################################################
###########################################################################
# Creating a dataset for male
dftrain = dftrain[dftrain.Sex=='male']
dfreal = dfreal[dfreal.Sex=='male']
dftrain = dftrain.drop(columns='Sex')
dfreal = dfreal.drop(columns='Sex')
#
vPassengerId = dfreal['PassengerId']
dfreal = dfreal.drop(columns=['PassengerId'])
###########################################################################

# creating numeric vectors (Training dataset)
X = dftrain.iloc[:, 1:10].values
y = dftrain.iloc[:, 0].values
dftrain = dftrain.drop(columns='Survived')
# creating numeric vectors (Training dataset)
Xr = dfreal.iloc[:, 0:9].values
#
# Enconding categorical variables
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
# Training dataset
labelencoder_X2 = LabelEncoder()
X[:,3] = labelencoder_X2.fit_transform(X[:,3])
Xr[:,3] = labelencoder_X2.transform(Xr[:,3])
labelencoder_X3 = LabelEncoder()
X[:,4] = labelencoder_X3.fit_transform(X[:,4])
Xr[:,4] = labelencoder_X3.transform(Xr[:,4])
labelencoder_X4 = LabelEncoder()
X[:,5] = labelencoder_X4.fit_transform(X[:,5])
Xr[:,5] = labelencoder_X4.transform(Xr[:,5])
labelencoder_X5 = LabelEncoder()
X[:,6] = labelencoder_X5.fit_transform(X[:,6])
Xr[:,6] = labelencoder_X5.transform(Xr[:,6])
labelencoder_X6 = LabelEncoder()
X[:,7] = labelencoder_X6.fit_transform(X[:,7])
Xr[:,7] = labelencoder_X6.transform(Xr[:,7])

#Creating dummies variables (training)
onehotencoder = OneHotEncoder(handle_unknown='ignore')
# Training dataset
Xaux = X[:,[1,2,4,5,6,7]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,3]
for i in a: 
    Xaux1 = onehotencoder.fit_transform(X[:,i].reshape(-1,1)).toarray()  
    Xaux2 = np.append(Xaux2,Xaux1[:,1:],axis=1)
X1 = np.append(Xaux2,Xaux,axis=1)
X1 = X1.astype(float)

#Creating dummies variables (real)
# Training dataset
Xaux = Xr[:,[1,2,4,5,6,7]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,3]
for i in a: 
    Xaux1 = onehotencoder.fit_transform(Xr[:,i].reshape(-1,1)).toarray()  
    Xaux2 = np.append(Xaux2,Xaux1[:,1:],axis=1)
Xr1 = np.append(Xaux2,Xaux,axis=1)
Xr1 = Xr1.astype(float)

# Splitting the dataset into the training and test
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X1,y, test_size = 0.25, random_state = 7)
Xr_real = Xr1

#Feature scaling
seed = 7
np.random.seed(seed)
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)
Xr_real = sc_X.transform(Xr_real)

# Performing PCA
seed = 7
np.random.seed(seed)
ipca = IncrementalPCA(n_components=5)
X_train = ipca.fit_transform(X_train)
X_test = ipca.transform(X_test)
vVar = ipca.explained_variance_ratio_
Xr_real = ipca.transform(Xr_real)

########################## manually running model
seed = 7
np.random.seed(seed)
classifier = Sequential()
classifier.add(Dense(units = 35, kernel_initializer = 'uniform', activation = 'relu', input_dim = 5))
classifier.add(Dense(units = 15, kernel_initializer = 'uniform', activation = 'relu'))
classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
hist = classifier.fit(X_train, y_train, batch_size = 32, epochs = 500)

y_pred = classifier.predict(X_test)
y_pred = (y_pred >= 0.5)

cm = confusion_matrix(y_test, y_pred)
cm

#saving results (Training)
###########################
y_predreal = classifier.predict(Xr_real)
y_predreal = np.where(y_predreal > 0.5,1,0)

dfresults = pd.DataFrame(data=(np.append(np.array(vPassengerId).reshape(len(y_predreal),1),y_predreal.T.reshape(len(y_predreal),1),axis=1)),columns=['PassengerId','Survived'])
dfresults.to_csv(data_folder+'resultsPYmale.csv',index=False)

dfreal['newsurv1'] = dfresults['Survived'].values
dfreal.groupby('newsurv1')['newsurv1'].agg(['mean','count'])
