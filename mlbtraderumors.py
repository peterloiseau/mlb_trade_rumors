#all these packages must be install before the script can run
from bs4 import BeautifulSoup
from urllib.request import urlopen
import csv
import pandas as pd
import re
import requests
from selenium import webdriver
from selenium.webdriver.firefox.options import Options

def get_links():     
    root_url='https://www.mlbtraderumors.com/'
    page = urlopen(root_url).read()
    soup1 = BeautifulSoup(page,'html.parser')
    teams=soup1.find_all("li",class_='menu-item menu-item-type-custom menu-item-object-custom menu-item-58255')
    url_list=[]
    for i in range(1,len(teams)):
        try:
            url_list.append(teams[i].find('a')['href']+'?show=all')
        except:
            pass
    return url_list

def scrape(url_list):
    fdf=pd.DataFrame()
    options = Options()
    options.headless = True
    browser = webdriver.Firefox(options=options)
    for url in url_list:
        browser.get(url)
        browser.implicitly_wait(20)
        try:
            browser.find_element_by_id('malinky-ajax-pagination-button').click()
        except TimeoutException:
            pass
        html = browser.page_source
        soup1 = BeautifulSoup(html,'html.parser')
        header = soup1.find_all("article",itemtype='https://schema.org/CreativeWork')
        time = soup1.find_all("time",class_='entry-time')
        content = soup1.find_all("div",class_='entry-content')
        team=re.search(r'com/(.*)\?show.*',url).group(1)
        rows=[]
        for i in range(0,len(header)):
            row=[team,header[i]['class'][0],time[2*i].text,header[i].text,content[i].text]
            rows.append(row)   
        df=pd.DataFrame(rows)
        fdf=fdf.append(df,ignore_index=True)
    return fdf

if __name__ == '__main__': 
    urls = get_links() 
    data=scrape(urls) 
    data.to_csv(r'C:\Users\peter\Documents\baseball-databases\mlb_trade_rumors\articles2.csv',index=False)
    
