import pandas as pd
import os

path = os.getcwd()


df = pd.read_excel('C:/Users/IOR_C/Downloads/SCIAN2018_Categorias-productos.xlsx', usecols='A:B', sheet_name='SECTOR',
                   skiprows=1)
df.dropna(subset=['Código'], inplace=True)
df.to_excel('C:/Users/IOR_C/Downloads/SCIAN.xlsx', index=False)