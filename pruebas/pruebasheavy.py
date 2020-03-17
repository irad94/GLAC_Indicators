import time
import dask.dataframe as dd
import pandas as pd
import numpy as np


start_time = time.time()

df = dd.read_csv("C:/Users/IOR_C/Downloads/PERSONA_BDP.csv")

df.npartitions

df.columns

print('Nothing took %s seconds' % (time.time() - start_time))

# Método de pandas

chunks = pd.read_csv("C:/Users/IOR_C/Downloads/PERSONA_BDP.csv", chunksize=100000)
data = pd.concat(chunks)

ls = []
for i in range(0,100):
    v = str('var_' + str(i))
    ls.append(v)

N = 5000000
# creating a pandas dataframe (df) with 8 columns and N rows with random integers between 999 and 999999 and with column names from A to H
df = pd.DataFrame(np.random.randint(999, 999999, size=(N, 100)), columns=ls)
df.to_csv('C:/Users/IOR_C/Downloads/pruebaheavy.csv')

# Pruebas de lectura
# Pandas
start_time = time.time()
df = pd.read_csv("C:/Users/IOR_C/Downloads/pruebaheavy.csv")
final_time = time.time() - start_time
print('Load with pandas took %s seconds' % (time.time() - start_time))


start_time = time.time()
df = dd.read_csv("C:/Users/IOR_C/Downloads/pruebaheavy.csv")
df = df.persist()
final_time = time.time() - start_time
print('Load with dask took %s seconds' % (time.time() - start_time))