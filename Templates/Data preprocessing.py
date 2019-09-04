"""
Created on Mon May 20 19:32:12 2019

@author: marrsantos
"""
# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Opening the dataset
dftrain = pd.read_csv('train.csv')
# dataset info
dftrain.info()
dftrain.head()
dftrain.shape
dftrain.describe()
# Querying data
# http://pandas.pydata.org/pandas-docs/stable/getting_started/comparison/comparison_with_r.html
dftrain.query('Sex == "male"')
dftrain[(dftrain.Sex=='male') & (dftrain.Survived==0)]
# Data preparation
dftrain.iloc[:, 1:4]
dftrain['Surname'] = dftrain.Name.str.split(',',1,expand=True)[0]
dftrain = dftrain.drop(columns='Name')
dftrain['Embarked'].fillna("NoEmbarked", inplace = True)
dftrain['Cabin'].fillna("NoCabin", inplace = True)
dftrain['Age'].fillna(dftrain.Age.median(), inplace = True)
# creating numeric vectors
X = dftrain.iloc[:, 2:13].values
y = dftrain.iloc[:, 1].values
# grouping & summarize
gdf = dftrain.groupby('Surname')
gdf = dftrain.groupby('Surname').agg({'Age': 'mean'})
# Enconding categorical variables
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
labelencoder_X = LabelEncoder()
X[:,1] = labelencoder_X.fit_transform(X[:,1])
X[:,5] = labelencoder_X.fit_transform(X[:,5])
X[:,7] = labelencoder_X.fit_transform(X[:,7])
X[:,8] = labelencoder_X.fit_transform(X[:,8])
X[:,9] = labelencoder_X.fit_transform(X[:,9])
#Creating dummies variables
onehotencoder = OneHotEncoder(handle_unknown='ignore')
X1 = onehotencoder.fit_transform(X[:,[1,2,3,4,5,7,8,9]]).toarray()
np.append(X[:,6].reshape(len(X),1),X1,axis=1)
#
