# -*- coding: utf-8 -*-
import numpy as np
np.random.seed(10) #for reproducibility

# LSTM for sequence classification in the IMDB dataset
from keras.datasets import imdb
from keras.models import Sequential
from keras.layers import LSTM, Dense, Dropout
from keras.layers.embeddings import Embedding
from keras.preprocessing import sequence

batch_size=64
nb_epoch=3
nb_classes=1

# load the dataset but only keep the top n words
n_words = 5000
# number of maximum words length in review
max_length = 500
# embedding vector length
embedding_length = 32

#the data, shuffled and split between train and test sets
(X_train, y_train), (X_test, y_test) = imdb.load_data(num_words=n_words)
print('X_train shape: ',X_train.shape)
print(len(X_train[0]))
print(X_train[0][:10])
print(X_train[0][-10:])

# truncate and pad input sequences
X_train = sequence.pad_sequences(X_train, maxlen=max_length)
X_test = sequence.pad_sequences(X_test, maxlen=max_length)
print('X_train shape: ',X_train.shape)
print(len(X_train[0]))
print(X_train[0][:10])
print(X_train[0][-10:])

# create the model
model = Sequential()

model.add(Embedding(n_words, embedding_length, input_length=max_length))
model.add(Dropout(0.1))
model.add(LSTM(100))
model.add(Dropout(0.2))

model.add(Dense(nb_classes, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])

model.summary()

model.fit(X_train, y_train, epochs=nb_epoch, batch_size=batch_size, validation_split=0.2)
scores = model.evaluate(X_test, y_test, verbose=0)
print("Test score: ", scores[0])
print("Test Accuracy: ", scores[1])
