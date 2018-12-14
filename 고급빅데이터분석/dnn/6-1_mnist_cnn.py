# -*- coding: utf-8 -*-
"""
Created on Mon Oct 16 17:20:53 2017

@author: User
"""

from __future__ import print_function
import numpy as np
np.random.seed(1337)  # for reproducibility
import json

from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation, Flatten
from keras.layers import Conv2D, MaxPooling2D
from keras.utils import to_categorical
from keras import backend as K

# batch_size 작을 때 장점 : overfitting을 피할 수 있다
# batch_size 클 때 장점 : 수렴 속도가 빠르다
batch_size = 128
nb_classes = 10 # MNIST는 0~9까지 판별하는 문제
nb_epoch = 20

# input image dimensions
img_rows, img_cols = 28, 28
# number of convolutional filters to use
nb_filters = 3
# convolution kernel size
nb_conv = 5
# size of pooling area for max pooling
nb_pool = 2

# the data, shuffled and split between tran and test sets
(X_train, y_train), (X_test, y_test) = mnist.load_data()

X_train = X_train.reshape(X_train.shape[0], img_rows, img_cols, 1)
X_test = X_test.reshape(X_test.shape[0], img_rows, img_cols, 1)
input_shape = (img_rows, img_cols, 1)

X_train = X_train.astype('float32') # 연산 과정에서는 float형이어야 되므로 정수에서 실수로 변경
X_test = X_test.astype('float32') # 연산 과정에서는 float형이어야 되므로 정수에서 실수로 변경
X_train /= 255 # 스케일링
X_test /= 255 # 스케일링
print('X_train shape:', X_train.shape)
print(X_train.shape[0], 'train samples')
print(X_test.shape[0], 'test samples')

# convert class vectors to binary class matrices
Y_train = to_categorical(y_train, nb_classes)
Y_test = to_categorical(y_test, nb_classes)

model = Sequential()

model.add(Conv2D(nb_filters, (nb_conv, nb_conv), input_shape=input_shape))
model.add(Activation('relu'))
model.add(Conv2D(nb_filters, (nb_conv, nb_conv)))
model.add(Activation('relu'))
model.add(MaxPooling2D(pool_size=(nb_pool, nb_pool)))
model.add(Dropout(0.25))

model.add(Flatten()) # 3차원 된 상황에서 1차원으로 펴주겠다
model.add(Dense(128))
model.add(Activation('relu'))
model.add(Dropout(0.5))
model.add(Dense(nb_classes))
model.add(Activation('softmax'))

model.compile(loss='categorical_crossentropy', optimizer='adadelta', metrics=['accuracy'])

model.summary()

hist = model.fit(X_train, Y_train, batch_size=batch_size, 
                 epochs=nb_epoch, verbose=1, validation_split=0.2)
score = model.evaluate(X_test, Y_test, verbose=0)
print('Test score:', score[0])
print('Test accuracy:', score[1])

# Save the model and weights
import os
os.chdir("C:/Users/renz/Desktop/SNU/고급빅데이터분석/dnn/MNIST") # 저장 경로
json_string = model.to_json()
open('mnist_model_architecture.json','w').write(json_string)
model.save_weights('mnist_model_weights.h5')

# Save History
with open('mnist_model_history.json','w') as fp:
    json.dump(hist.history, fp)
    
# Plot history
hist = json.loads(open('mnist_model_history.json').read())

import matplotlib.pyplot as plt
plt.figure('history')
plt.subplot(211)
plt.title('Loss over epochs')
plt.plot(hist['loss'],'r',label='loss')
plt.plot(hist['val_loss'], 'b',label='val_loss')
plt.xlabel('Epochs')
plt.ylabel('Loss')

plt.subplot(212)
plt.title('Accuracy over epochs')
plt.plot(hist['acc'],'r',label='acc')
plt.plot(hist['val_acc'], 'b',label='val_acc')
plt.xlabel('Epochs')
plt.ylabel('Accuracy')

plt.tight_layout()
plt.show()
