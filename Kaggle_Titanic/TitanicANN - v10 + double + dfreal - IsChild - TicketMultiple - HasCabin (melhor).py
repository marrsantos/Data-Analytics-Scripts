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
dftrain.groupby('Sex')['Survived'].agg(['mean','count'])
dftrain.groupby('Embarked')['Survived'].agg(['mean','count'])
dftrain.groupby(['Embarked','Sex'])['Survived'].agg(['mean','count'])
# Completing Embarked with "S" as majority of them
dftrain['Embarked'].fillna("S", inplace = True)
dftrain.groupby('Pclass')['Survived'].agg(['mean','count'])
dftrain.groupby(['Pclass','Sex'])['Survived'].agg(['mean','count'])
dftrain.groupby(['Pclass'])['Survived'].agg(['mean','count'])
# Completing Fare nan with median
dftrain.groupby('Fare')['Survived'].agg(['mean','count'])
dftrain['Fare'].fillna(dftrain.Fare.mean(), inplace = True)
#dftrain['FareRange'] = pd.cut(dftrain.Fare, bins=[-1,8,17,26,41,800], precision=0)
dftrain.groupby(['Fare','Sex'])['Survived'].agg(['mean','count'])
# Creating Famuly members (Parch + SibSp)
dftrain.groupby('SibSp')['Survived'].agg(['mean','count'])
dftrain.groupby('Parch')['Survived'].agg(['mean','count'])
dftrain['FamilyMembers'] = dftrain['SibSp']+dftrain['Parch']
# Create AgeRange + Completing Age nan with randint from mean +/- std
#dftrain['Age'].fillna(pd.Series(np.random.randint(dftrain['Age'].mean() - dftrain['Age'].std(), dftrain['Age'].mean() + dftrain['Age'].std(), size=len(dftrain))), inplace = True)
dftrain['Age'].fillna(dftrain.Age.mean(), inplace = True)
#dftrain['AgeRange'] = pd.cut(dftrain.Age, bins=[0,17,32,47,100], precision=0)
# Creating SameTicket Column (individuals that bought ticket with other ones)
aaa=dftrain.groupby('Ticket').size().reset_index(name='SameTicket')
dftrain=pd.merge(dftrain, aaa, how='left', on=['Ticket'])
dftrain.groupby('SameTicket')['Survived'].agg(['mean','count'])
#dftrain['SameTicket'] = pd.cut(dftrain.SameTicket, bins=[0,1,2,3,4,8], precision=0)
dftrain['TicketMultiple'] = dftrain.SameTicket>1
dftrain['WithFamily'] = dftrain.FamilyMembers>1
dftrain['hasCabin'] = dftrain.Cabin.notna()
dftrain['IsChild']=dftrain.Age<2
#
###########################################################################
# Process #3
# increasing number of records...
dftrain = pd.concat([dftrain,dftrain])
###########################################################################

# removing non-relevant columns
dftrain = dftrain.drop(columns=['PassengerId','Name','Cabin','SibSp','Parch','Ticket','FamilyMembers','SameTicket'])
dftrain.info()

###########################################################################
# Completing Embarked with "S" as majority of them
dfreal['Embarked'].fillna("S", inplace = True)
# Completing Fare nan with median
dfreal['Fare'].fillna(dfreal.Fare.mean(), inplace = True)
#dfreal['FareRange'] = pd.cut(dfreal.Fare, bins=[-1,8,17,26,41,800], precision=0)
# Creating Famuly members (Parch + SibSp)
dfreal['FamilyMembers'] = dfreal['SibSp']+dfreal['Parch']
# Create AgeRange + Completing Age nan with randint from mean +/- std
#dfreal['Age'].fillna(pd.Series(np.random.randint(dfreal['Age'].mean() - dfreal['Age'].std(), dfreal['Age'].mean() + dfreal['Age'].std(), size=len(dfreal))), inplace = True)
dfreal['Age'].fillna(dfreal.Age.mean(), inplace = True)
#dfreal['AgeRange'] = pd.cut(dfreal.Age, bins=[0,17,32,47,100], precision=0)
# Creating SameTicket Column (individuals that bought ticket with other ones)
aaa=dfreal.groupby('Ticket').size().reset_index(name='SameTicket')
dfreal=pd.merge(dfreal, aaa, how='left', on=['Ticket'])
#dfreal['SameTicket'] = pd.cut(dfreal.SameTicket, bins=[0,1,2,3,4,8], precision=0)
dfreal['TicketMultiple'] = dfreal.SameTicket>1
dfreal['WithFamily'] = dfreal.FamilyMembers>1
dfreal['hasCabin'] = dfreal.Cabin.notna()
dfreal['IsChild']=dfreal.Age<2

# removing non-relevant columns
vPassengerId = dfreal['PassengerId']
dfreal = dfreal.drop(columns=['PassengerId','Name','Cabin','SibSp','Parch','Ticket','FamilyMembers','SameTicket'])
dfreal.info()
###########################################################################
###########################################################################

# creating numeric vectors (Training dataset)
X = dftrain.iloc[:, 1:11].values
y = dftrain.iloc[:, 0].values
dftrain = dftrain.drop(columns='Survived')
# creating numeric vectors (Training dataset)
Xr = dfreal.iloc[:, 0:10].values

# Enconding categorical variables
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
# Training dataset
labelencoder_X1 = LabelEncoder()
X[:,1] = labelencoder_X1.fit_transform(X[:,1])
Xr[:,1] = labelencoder_X1.transform(Xr[:,1])
labelencoder_X4 = LabelEncoder()
X[:,4] = labelencoder_X4.fit_transform(X[:,4])
Xr[:,4] = labelencoder_X4.transform(Xr[:,4])
labelencoder_X5 = LabelEncoder()
X[:,5] = labelencoder_X5.fit_transform(X[:,5])
Xr[:,5] = labelencoder_X5.transform(Xr[:,5])
labelencoder_X6 = LabelEncoder()
X[:,6] = labelencoder_X6.fit_transform(X[:,6])
Xr[:,6] = labelencoder_X6.transform(Xr[:,6])
labelencoder_X7 = LabelEncoder()
X[:,7] = labelencoder_X7.fit_transform(X[:,7])
Xr[:,7] = labelencoder_X7.transform(Xr[:,7])
labelencoder_X8 = LabelEncoder()
X[:,8] = labelencoder_X8.fit_transform(X[:,8])
Xr[:,8] = labelencoder_X8.transform(Xr[:,8])

#Creating dummies variables (training)
onehotencoder = OneHotEncoder(handle_unknown='ignore')
# Training dataset
Xaux = X[:,[1,2,3,5,6,7]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,4]
for i in a: 
    Xaux1 = onehotencoder.fit_transform(X[:,i].reshape(-1,1)).toarray()  
    Xaux2 = np.append(Xaux2,Xaux1[:,1:],axis=1)
X1 = np.append(Xaux2,Xaux,axis=1)
X1 = X1.astype(float)

#Creating dummies variables (real)
# Training dataset
Xaux = Xr[:,[1,2,3,5,6,7]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,4]
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

########################## manually running model
seed = 7
np.random.seed(seed)
classifier = Sequential()
classifier.add(Dense(units = 30, kernel_initializer = 'uniform', activation = 'relu', input_dim = 10))
classifier.add(Dense(units = 20, kernel_initializer = 'uniform', activation = 'relu'))
classifier.add(Dense(units = 5, kernel_initializer = 'uniform', activation = 'relu'))
classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
hist = classifier.fit(X_train, y_train, batch_size = 1, epochs = 20)

y_pred = classifier.predict(X_test)
y_pred = (y_pred >= 0.5)

cm = confusion_matrix(y_test, y_pred)
cm

#saving results (Training)
###########################
y_predreal = classifier.predict(Xr_real)
y_predreal = np.where(y_predreal > 0.5,1,0)

dfresults = pd.DataFrame(data=(np.append(np.array(vPassengerId).reshape(418,1),y_predreal.T.reshape(418,1),axis=1)),columns=['PassengerId','Survived'])
dfresults.to_csv(data_folder+'resultsPY.csv',index=False)

dfreal['newsurv1'] = dfresults['Survived']
dfreal.groupby('Sex')['newsurv1'].agg(['mean','count'])
dfreal.groupby('newsurv1')['newsurv1'].agg(['mean','count'])
