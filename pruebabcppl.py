import gspread
from oauth2client.service_account import ServiceAccountCredentials
import pprint
import pandas as pd

scope = ['https://spreadsheets.google.com/feeds','https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name('C:/Users/IOR_C/Downloads/bcppl-8b0075836ec5.json', scope)
client = gspread.authorize(creds)

sheet = client.open('hola mundo').sheet1

df = sheet.get_all_records()
pprint.pprint(df)

df = pd.DataFrame(df)