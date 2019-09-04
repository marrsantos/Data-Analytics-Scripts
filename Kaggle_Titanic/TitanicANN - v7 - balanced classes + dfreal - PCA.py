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
dftrain['Fare'].fillna(dftrain.Fare.median(), inplace = True)
dftrain['FareRange'] = pd.cut(dftrain.Fare, bins=[-1,50,800], precision=0)
dftrain.groupby(['FareRange','Sex'])['Survived'].agg(['mean','count'])
# Creating Famuly members (Parch + SibSp)
dftrain.groupby('SibSp')['Survived'].agg(['mean','count'])
dftrain.groupby('Parch')['Survived'].agg(['mean','count'])
dftrain['FamilyMembers'] = dftrain['SibSp']+dftrain['Parch']
# Create AgeRange + Completing Age nan with randint from mean +/- std
#dftrain['Age'].fillna(pd.Series(np.random.randint(dftrain['Age'].mean() - dftrain['Age'].std(), dftrain['Age'].mean() + dftrain['Age'].std(), size=len(dftrain))), inplace = True)
dftrain['Age'].fillna(dftrain.Age.mean(), inplace = True)
dftrain['AgeRange'] = pd.cut(dftrain.Age, bins=[0,5,15,40,90], precision=0)
dftrain.groupby('AgeRange')['Survived'].agg(['mean','count'])
# Creating SameTicket Column (individuals that bought ticket with other ones)
aaa=dftrain.groupby('Ticket').size().reset_index(name='SameTicket')
dftrain=pd.merge(dftrain, aaa, how='left', on=['Ticket'])
dftrain.groupby('SameTicket')['Survived'].agg(['mean','count'])
dftrain['SameTicket'] = pd.cut(dftrain.SameTicket, bins=[0,1,3,8], precision=0)
#
dftrain['FareMale'] = (dftrain.FareRange.cat.codes==0) & (dftrain.Sex=='male')
#
aaa=dftrain.groupby(['FamilyMembers','FareRange','AgeRange','SameTicket','Sex'])['Survived'].agg(['mean','count'])
dftrain.groupby(['FareRange','Sex'])['Survived'].agg(['mean','count'])
#
pd.crosstab(dftrain['Age'], dftrain['Survived'])
#
# removing non-relevant columns
dftrain = dftrain.drop(columns=['PassengerId','Name','Age','Cabin','Fare','SibSp','Parch','Ticket'])
dftrain.info()

########################################################
# Up-sample Minority Class
from sklearn.utils import resample

df_majority = dftrain[dftrain.Survived==0]
df_minority = dftrain[dftrain.Survived==1]
 
df_minority_upsampled = resample(df_minority, 
                                 replace=True,                     # sample with replacement
                                 n_samples=len(df_majority),       # to match majority class
                                 random_state=7)                   # reproducible results
 
# Combine majority class with upsampled minority class
dftrain = pd.concat([df_majority, df_minority_upsampled])
 
# Display new class counts
dftrain.Survived.value_counts()
########################################################

###########################################################################
###########################################################################
# Completing Embarked with "S" as majority of them
dfreal['Embarked'].fillna("S", inplace = True)
# Completing Fare nan with median
dfreal['Fare'].fillna(dfreal.Fare.median(), inplace = True)
dfreal['FareRange'] = pd.cut(dfreal.Fare, bins=[-1,50,800], precision=0)
# Creating Famuly members (Parch + SibSp)
dfreal['FamilyMembers'] = dfreal['SibSp']+dfreal['Parch']
# Create AgeRange + Completing Age nan with randint from mean +/- std
#dfreal['Age'].fillna(pd.Series(np.random.randint(dfreal['Age'].mean() - dfreal['Age'].std(), dfreal['Age'].mean() + dfreal['Age'].std(), size=len(dfreal))), inplace = True)
dfreal['Age'].fillna(dfreal.Age.mean(), inplace = True)
dfreal['AgeRange'] = pd.cut(dfreal.Age, bins=[0,5,15,40,90], precision=0)
# Creating SameTicket Column (individuals that bought ticket with other ones)
aaa=dfreal.groupby('Ticket').size().reset_index(name='SameTicket')
dfreal=pd.merge(dfreal, aaa, how='left', on=['Ticket'])
dfreal['SameTicket'] = pd.cut(dfreal.SameTicket, bins=[0,1,3,8], precision=0)
#
dfreal['FareMale'] = (dfreal.FareRange.cat.codes==0) & (dfreal.Sex=='male')
#
# removing non-relevant columns
vPassengerId = dfreal['PassengerId']
dfreal = dfreal.drop(columns=['PassengerId','Name','Age','Cabin','Fare','SibSp','Parch','Ticket'])
dfreal.info()
###########################################################################
###########################################################################

# creating numeric vectors (Training dataset)
X = dftrain.iloc[:, 1:9].values
y = dftrain.iloc[:, 0].values
dftrain = dftrain.drop(columns='Survived')
# creating numeric vectors (Training dataset)
Xr = dfreal.iloc[:, 0:8].values

# Enconding categorical variables
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
# Training dataset
labelencoder_X1 = LabelEncoder()
X[:,1] = labelencoder_X1.fit_transform(X[:,1])
Xr[:,1] = labelencoder_X1.transform(Xr[:,1])
labelencoder_X2 = LabelEncoder()
X[:,2] = labelencoder_X2.fit_transform(X[:,2])
Xr[:,2] = labelencoder_X2.transform(Xr[:,2])
labelencoder_X3 = LabelEncoder()
X[:,3] = labelencoder_X3.fit_transform(X[:,3])
Xr[:,3] = labelencoder_X3.transform(Xr[:,3])
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
Xaux = X[:,[1,7]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,2,3,4,5,6]
for i in a: 
    Xaux1 = onehotencoder.fit_transform(X[:,i].reshape(-1,1)).toarray()  
    Xaux2 = np.append(Xaux2,Xaux1[:,1:],axis=1)
X1 = np.append(Xaux2,Xaux,axis=1)
X1 = X1.astype(float)

#Creating dummies variables (real)
# Training dataset
Xaux = Xr[:,[1,7]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,2,3,4,5,6]
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
ipca = IncrementalPCA(n_components=10)
X_train = ipca.fit_transform(X_train)
X_test = ipca.transform(X_test)
vVar = ipca.explained_variance_ratio_
Xr_real = ipca.transform(Xr_real)

# fix random seed for reproducibility
seed = 7
np.random.seed(seed)
def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(76, kernel_initializer = 'uniform', activation = 'relu', input_dim = 19))
    classifier.add(Dense(19, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier, batch_size = 1, nb_epoch = 600)
accuracies = cross_val_score(estimator = classifier, X = X_train, y = y_train, cv = 10, n_jobs = -1)
print("Results: %.2f%% (%.2f%%)" % (accuracies.mean()*100, accuracies.std()*100))

# checking for overfitting (Dropout Regularization to reduce overfitting if needed)
# classifier.add(Dropout(p = 0.1))

########################## manually running model
seed = 7
np.random.seed(seed)
classifier = Sequential()
classifier.add(Dense(units = 30, kernel_initializer = 'uniform', activation = 'relu', input_dim = 10))
classifier.add(Dense(units = 20, kernel_initializer = 'uniform', activation = 'relu'))
classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
hist = classifier.fit(X_train, y_train, batch_size = 1, epochs = 100)

y_pred = classifier.predict(X_test)
y_pred = (y_pred >= 0.5)

cm = confusion_matrix(y_test, y_pred)
cm

###########################
# Tuning the ANN (GridSearch)
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import GridSearchCV
from time import gmtime, strftime

# medindo o tempo
time_ini = strftime("%Y-%m-%d %H:%M:%S", gmtime())
seed = 7
np.random.seed(seed)
def build_classifier(Optimizer, Units1, Units2):
    classifier = Sequential()
    classifier.add(Dense(Units1, kernel_initializer = 'uniform', activation = 'relu', input_dim = 10))
    if Units2 != 0:
        classifier.add(Dense(Units2, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = Optimizer, loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier)
parameters = {'batch_size': [1,5,10],
              'nb_epoch': [50,100,200],
              'Optimizer': ['adam'],
              'Units1': [5,10,15],
              'Units2': [0,5,10,15]}
grid_search = GridSearchCV(estimator = classifier,
                           param_grid = parameters,
                           scoring = 'accuracy',
                           cv = 5)
grid_search = grid_search.fit(X_train, y_train)
#
time_end = strftime("%Y-%m-%d %H:%M:%S", gmtime())
time_ini
time_end
#
best_parameters = grid_search.best_params_
best_accurary = grid_search.best_score_

# new small dataset (including Title)
# Parameters:
# 0.83.37 adam, units=22,22, batch_size=5, nb_epoch=50
# 0.83.67 adam, units=22,32, batch_size=5, nb_epoch=50

#saving results (Training)
###########################
y_predreal = classifier.predict(Xr_real)
y_predreal = np.where(y_predreal > 0.5,1,0)

dfresults = pd.DataFrame(data=(np.append(np.array(vPassengerId).reshape(418,1),y_predreal.T.reshape(418,1),axis=1)),columns=['PassengerId','Survived'])
dfresults.to_csv(data_folder+'resultsPY.csv',index=False)

dfreal['newsurv1'] = dfresults['Survived']
dfreal.groupby('Sex')['newsurv1'].agg(['mean','count'])
