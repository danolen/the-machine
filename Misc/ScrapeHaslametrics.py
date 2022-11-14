import mechanicalsoup
import requests, re
import pandas as pd
from bs4 import BeautifulSoup
import numpy as np
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException

driver = webdriver.Chrome() #path to webdriver

driver.get('https://haslametrics.com/ratings.php')
list_category_elements = driver.find_element("xpath", '/html/body/div/table/tbody/tr[5]/td/div[3]/div/div/table') #finds hasla table elements
links = list_category_elements.find_elements(By.CLASS_NAME,"scoreproj1")
links2= list_category_elements.find_elements(By.CLASS_NAME,"scoreproj2")
links_values2=[]
links2_values2=[]
for i in range(len(links)):
    
     
    links_values2.append(links[i].text)
    
for i in range(len(links2)):
    
   
    links2_values2.append(links2[i].text)


dfh1=pd.DataFrame(links_values2)
dfh2=pd.DataFrame(links2_values2)
links_values2=np.array(links_values2)
links2_values2=np.array(links2_values2)

dfh4=pd.DataFrame((links_values2[0::2]),columns=['Home']) #creates hasla df
dfh3=pd.DataFrame((links_values2[1::2]),columns=['HScore'])

Hasla=pd.concat([dfh4, dfh3.reindex(dfh4.index)], axis=1)#.iloc[0:50]

dfh5=pd.DataFrame((links2_values2[0::2]),columns=['Away'])
dfh6=pd.DataFrame((links2_values2[1::2]),columns=['Ascore'])

Hasla2=pd.concat([dfh5, dfh6.reindex(dfh5.index)], axis=1)#.iloc[0:50]

Halsa=Hasla
Hasla2=Hasla2

Hasla2=pd.concat([dfh5, dfh6], axis=1)#.iloc[0:50]
HaslaF=pd.concat([Hasla, Hasla2], axis=1)#.iloc[0:25]

HaslaF['HScore']=pd.to_numeric(HaslaF['HScore'])
HaslaF['Ascore']=pd.to_numeric(HaslaF['Ascore'])
HaslaF["Hspread"]=np.abs((HaslaF['HScore']-HaslaF['Ascore']))
HaslaF['Hspread']=pd.to_numeric(HaslaF['Hspread'])

HaslaF2 = HaslaF[HaslaF["Home"] != '']

HaslaF2.to_csv("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine/Haslametrics.csv", index=False)
