#all these packages must be install before the script can run
from bs4 import BeautifulSoup
from urllib.request import urlopen
import csv
import pandas as pd
import re
import requests

def scrape():
    root='https://www.baseball-reference.com/leagues/MLB/'
    url_list=[]
    for i in range(2016,2020):
        url_list.append(root+str(i)+'-transactions.shtml')
    fdf=pd.DataFrame()
    for url in url_list:
        pagea = requests.get(url).text
        soupa = BeautifulSoup(pagea,'html')
        tablea = soupa.find_all('li')
        rows=[]
        for i in range(0,len(tablea)):
            row=tablea[i].text
            rows.append(row)   
        df=pd.DataFrame(rows)
        fdf=fdf.append(df,ignore_index=True)
    return fdf

if __name__ == '__main__': 
    data=scrape() 
    data.to_csv(r'C:\Users\peter\Documents\baseball-databases\mlb_trade_rumors\transaction.csv',index=False)
    
