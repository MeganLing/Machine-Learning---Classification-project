# -*- coding: utf-8 -*-
"""
Created on Fri Mar  1 13:44:01 2019

@author: megan
"""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
Import Libraries Section
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
from keras.models import Sequential
from keras.layers import Dense 
from sklearn import preprocessing
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
import time

import sys
if "C:\\My_Python_Lib" not in sys.path:
    sys.path.append("C:\\My_Python_Lib")
    
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
Load Data Section
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""   
start = time.time()

data = pd.read_csv("winequality-white.csv",
                    header=0,
                   names = ['fixed acidity', 'volatile acidity', 'citric acid', 'residual sugar', 
                            'chlorides', 'free sulfur dioxide',
                            'total sulfur dioxide', 'density', 'pH', 
                            'sulphates', 'alcohol', 'quality'])

x_vars = ['fixed acidity', 'volatile acidity', 'citric acid', 'residual sugar', 
                            'chlorides', 'free sulfur dioxide',
                            'total sulfur dioxide', 'density', 'pH', 
                            'sulphates', 'alcohol',]

y_vars = ['quality']
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
Pretreat Data Section
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

x_data = data[x_vars]
y_data = data[y_vars]

#print (len(x_data))
#print (len(y_data))

x_data_MinMax = preprocessing.MinMaxScaler()

x_data.values
x_data = np.array(x_data).reshape((len(x_data), 11))

y_data.values
y_data = np.array(y_data).reshape((len(y_data), 1))

x_data = x_data_MinMax.fit_transform(x_data)

x_train, x_test, y_train, y_test = train_test_split(x_data, y_data, test_size=0.33, random_state=7)
x_train, x_val, y_train, y_val = train_test_split(x_train, y_train, test_size=0.2, random_state=7)

x_train.mean(axis=0)
y_train.mean(axis=0)
x_test.mean(axis=0)
y_test.mean(axis=0)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
Define Model Section - RMSPROP
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

model = Sequential()
model.add(Dense(12, activation='relu', input_shape=(x_train.shape[1],)))
model.add(Dense(12, activation='relu'))
model.add(Dense(12, activation='relu'))
model.add(Dense(1,activation = 'sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='Adagrad', metrics=['accuracy'])


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
K-fold validation
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

history = model.fit(x_train, y_train,
                        validation_data=(x_val, y_val),
                        epochs=200, batch_size=50, verbose=0)

train_loss = history.history["loss"]
validate_loss = history.history["val_loss"]
train_acc = history.history["acc"]
validate_acc = history.history["val_acc"]

end = time.time()
print(end - start)

print(max(train_acc))
print(min(validate_acc))
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
Final Model Application
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
model = Sequential()
model.add(Dense(12, activation='relu', input_shape=(x_train.shape[1],)))
model.add(Dense(12, activation='relu'))
model.add(Dense(1,activation = 'sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='Nadam', metrics=['accuracy'])
new = model.fit(x_test, y_test, epochs=80, batch_size=16, verbose=0)
train_acc_new = new.history["acc"]
print(max(train_acc_new))

