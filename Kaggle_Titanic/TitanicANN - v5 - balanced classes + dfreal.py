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

#diretório de trabalho
data_folder = os.getcwd()+'/'

# Opening the dataset
dftrain = pd.read_csv('train.csv')
dfreal = pd.read_csv('test.csv')

# Data preparation
# Analyzing variables
dftrain.groupby('Survived')['Survived'].agg(['count'])
dftrain.groupby('Sex')['Sex'].agg(['count'])
dftrain.groupby('Sex')['Survived'].agg(['mean','count'])
dftrain.groupby('Embarked')['Survived'].agg(['mean','count'])
dftrain.groupby(['Embarked','Sex'])['Survived'].agg(['mean','count'])
# Completing Embarked with "S" as majority of them
dftrain['Embarked'].fillna("S", inplace = True)
dftrain.groupby('Pclass')['Survived'].agg(['mean','count'])
dftrain.groupby(['Pclass','Sex'])['Survived'].agg(['mean','count'])
# Completing Fare nan with median
dftrain.groupby('Fare')['Survived'].agg(['mean','count'])
dftrain['Fare'].fillna(dftrain.Fare.median(), inplace = True)
dftrain['FareRange'] = pd.cut(dftrain.Fare, bins=[-1,50,800], precision=0)
dftrain.groupby('FareRange')['Survived'].agg(['mean','count'])
# Creating Famuly members (Parch + SibSp)
dftrain.groupby('SibSp')['Survived'].agg(['mean','count'])
dftrain.groupby('Parch')['Survived'].agg(['mean','count'])
dftrain['FamilyMembers'] = dftrain['SibSp']+dftrain['Parch']
# Create AgeRange + Completing Age nan with randint from mean +/- std
dftrain['Age'].fillna(pd.Series(np.random.randint(dftrain['Age'].mean() - dftrain['Age'].std(), dftrain['Age'].mean() + dftrain['Age'].std(), size=len(dftrain))), inplace = True)
dftrain['AgeRange'] = pd.cut(dftrain.Age, bins=[0,5,15,40,90], precision=0)
dftrain.groupby('AgeRange')['Survived'].agg(['mean','count'])
# Creating SameTicket Column (individuals that bought ticket with other ones)
aaa=dftrain.groupby('Ticket').size().reset_index(name='SameTicket')
dftrain=pd.merge(dftrain, aaa, how='left', on=['Ticket'])
dftrain.groupby('SameTicket')['Survived'].agg(['mean','count'])
dftrain['SameTicket'] = pd.cut(dftrain.SameTicket, bins=[0,1,3,8], precision=0)
dftrain.groupby('SameTicket')['Survived'].agg(['mean','count'])
# Creating Title
dftrain['Title'] = dftrain.Name.str.split(',',1,expand=True)[1].str.split('.',1,expand=True)[0]
dftrain['Title'] = dftrain['Title'].str.strip()
dftrain['Title'] = dftrain['Title'].replace('Mlle', 'Miss')
dftrain['Title'] = dftrain['Title'].replace('Ms', 'Miss')
dftrain['Title'] = dftrain['Title'].replace('Lady', 'Miss')
dftrain['Title'] = dftrain['Title'].replace('Mr', 'Mrs')
dftrain['Title'] = dftrain['Title'].replace(['the Countess','Capt', 'Col', \
       'Don', 'Dr', 'Major', 'Mme','Rev', 'Sir', 'Jonkheer', 'Dona'], 'Other')
dftrain.groupby('Title')['Survived'].agg(['mean','count'])
#
dftrain['1stWm'] = (dftrain.Pclass!=3) & (dftrain.Sex=='female')
#
dftrain['EmbWm'] = (dftrain.Embarked!='S') & (dftrain.Sex=='female')
#
dftrain['FareFam'] = (dftrain.FareRange.cat.codes==1) & (dftrain.FamilyMembers<4)
#
dftrain['FareClass'] = (dftrain.FareRange.cat.codes==1) & (dftrain.Pclass==1)
#
dftrain['FareEmb'] = (dftrain.FareRange.cat.codes==1) & (dftrain.Embarked!='Q')
#
dftrain['IsChild'] = (dftrain.AgeRange.cat.codes==0)
#
dftrain.groupby(['AgeRange'])['Survived'].agg(['mean','count'])
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
                                 random_state=7)                 # reproducible results
 
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
dfreal['Age'].fillna(pd.Series(np.random.randint(dfreal['Age'].mean() - dfreal['Age'].std(), dfreal['Age'].mean() + dfreal['Age'].std(), size=len(dfreal))), inplace = True)
dfreal['AgeRange'] = pd.cut(dfreal.Age, bins=[0,5,15,40,90], precision=0)
# Creating SameTicket Column (individuals that bought ticket with other ones)
aaa=dfreal.groupby('Ticket').size().reset_index(name='SameTicket')
dfreal=pd.merge(dfreal, aaa, how='left', on=['Ticket'])
dfreal['SameTicket'] = pd.cut(dfreal.SameTicket, bins=[0,1,3,8], precision=0)
# Creating Title
dfreal['Title'] = dfreal.Name.str.split(',',1,expand=True)[1].str.split('.',1,expand=True)[0]
dfreal['Title'] = dfreal['Title'].str.strip()
dfreal['Title'] = dfreal['Title'].replace('Mlle', 'Miss')
dfreal['Title'] = dfreal['Title'].replace('Ms', 'Miss')
dfreal['Title'] = dfreal['Title'].replace('Lady', 'Miss')
dfreal['Title'] = dfreal['Title'].replace('Mr', 'Mrs')
dfreal['Title'] = dfreal['Title'].replace(['the Countess','Capt', 'Col', \
       'Don', 'Dr', 'Major', 'Mme','Rev', 'Sir', 'Jonkheer', 'Dona'], 'Other')
dfreal['1stWm'] = (dfreal.Pclass!=3) & (dfreal.Sex=='female')
dfreal['EmbWm'] = (dfreal.Embarked!='S') & (dfreal.Sex=='female')
dfreal['FareFam'] = (dfreal.FareRange.cat.codes==1) & (dfreal.FamilyMembers<4)
dfreal['FareClass'] = (dfreal.FareRange.cat.codes==1) & (dfreal.Pclass==1)
dfreal['FareEmb'] = (dfreal.FareRange.cat.codes==1) & (dfreal.Embarked!='Q')
dfreal['IsChild'] = (dfreal.AgeRange.cat.codes==0)
#
# removing non-relevant columns
vPassengerId = dfreal['PassengerId']
dfreal = dfreal.drop(columns=['PassengerId','Name','Age','Cabin','Fare','SibSp','Parch','Ticket'])
dfreal.info()
###########################################################################
###########################################################################

# creating numeric vectors (Training dataset)
X = dftrain.iloc[:, 1:16].values
y = dftrain.iloc[:, 0].values
dftrain = dftrain.drop(columns='Survived')
# creating numeric vectors (Training dataset)
Xr = dfreal.iloc[:, 0:15].values

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
labelencoder_X7 = LabelEncoder()
X[:,8] = labelencoder_X7.fit_transform(X[:,8])
Xr[:,8] = labelencoder_X7.transform(Xr[:,8])
labelencoder_X8 = LabelEncoder()
X[:,9] = labelencoder_X8.fit_transform(X[:,9])
Xr[:,9] = labelencoder_X8.transform(Xr[:,9])
labelencoder_X9 = LabelEncoder()
X[:,10] = labelencoder_X9.fit_transform(X[:,10])
Xr[:,10] = labelencoder_X9.transform(Xr[:,10])
labelencoder_X10 = LabelEncoder()
X[:,11] = labelencoder_X10.fit_transform(X[:,11])
Xr[:,11] = labelencoder_X10.transform(Xr[:,11])
labelencoder_X11 = LabelEncoder()
X[:,12] = labelencoder_X11.fit_transform(X[:,12])
Xr[:,12] = labelencoder_X11.transform(Xr[:,12])
labelencoder_X12 = LabelEncoder()
X[:,13] = labelencoder_X12.fit_transform(X[:,13])
Xr[:,13] = labelencoder_X12.transform(Xr[:,13])

# Correlation Matrix for X (with encoded categorical variables) 
a=y.reshape(len(y),1)
aa = np.append(X,a,axis=1)
aa = aa.astype(float)
bb = pd.DataFrame(data=aa).corr()

#Creating dummies variables (training)
onehotencoder = OneHotEncoder(handle_unknown='ignore')
# Training dataset
Xaux = X[:,[1,8,9,10,11,12,13]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,2,3,4,5,6,7]
for i in a: 
    Xaux1 = onehotencoder.fit_transform(X[:,i].reshape(-1,1)).toarray()  
    Xaux2 = np.append(Xaux2,Xaux1[:,1:],axis=1)
X1 = np.append(Xaux2,Xaux,axis=1)
X1 = X1.astype(float)

#Creating dummies variables (real)
# Training dataset
Xaux = Xr[:,[1,8,9,10,11,12,13]]
Xaux2 = np.empty((len(Xaux),0),dtype=float)
a=[0,2,3,4,5,6,7]
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

# fix random seed for reproducibility
seed = 7
np.random.seed(seed)
def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(29, kernel_initializer = 'uniform', activation = 'relu', input_dim = 28))
    classifier.add(Dense(15, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(15, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier, batch_size = 1, nb_epoch = 100)
accuracies = cross_val_score(estimator = classifier, X = X_train, y = y_train, cv = 10, n_jobs = -1)
print("Results: %.2f%% (%.2f%%)" % (accuracies.mean()*100, accuracies.std()*100))

# checking for overfitting (Dropout Regularization to reduce overfitting if needed)
# classifier.add(Dropout(p = 0.1))

########################## manually running model
seed = 7
np.random.seed(seed)
classifier = Sequential()
classifier.add(Dense(units = 15, kernel_initializer = 'uniform', activation = 'relu', input_dim = 28))
classifier.add(Dense(units = 29, kernel_initializer = 'uniform', activation = 'relu'))
classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
hist = classifier.fit(X_train, y_train, batch_size = 10, epochs = 600)

y_pred = classifier.predict(X_test)
y_pred = (y_pred > 0.5)

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
def build_classifier(Optimizer, Units1, Units2, Units3):
    classifier = Sequential()
    classifier.add(Dense(Units1, kernel_initializer = 'uniform', activation = 'relu', input_dim = 28))
    if Units2 != 0:
        classifier.add(Dense(Units2, kernel_initializer = 'uniform', activation = 'relu'))
    if Units3 != 0:
        classifier.add(Dense(Units3, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = Optimizer, loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier)
parameters = {'batch_size': [1],
              'nb_epoch': [300],
              'Optimizer': ['adam'],
              'Units1': [15,29,37,59],
              'Units2': [0,15,29,37],
              'Units3': [0,7,15,29]}
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

# new small dataset (including Title)
# Parameters:
# 0.83.37 adam, units=22,22, batch_size=5, nb_epoch=50
# 0.83.67 adam, units=22,32, batch_size=5, nb_epoch=50

#saving results (Training)
###########################
y_predreal = classifier.predict(Xr_real)
y_predreal = np.where(y_predreal >= 0.4,1,0)

dfresults = pd.DataFrame(data=(np.append(np.array(vPassengerId).reshape(418,1),y_predreal.T.reshape(418,1),axis=1)),columns=['PassengerId','Survived'])
dfresults.to_csv(data_folder+'resultsPY.csv',index=False)
