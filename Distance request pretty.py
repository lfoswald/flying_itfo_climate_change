
# coding: utf-8

# # Distance request 
# 
# Web scraping distance request to get travel distances for all trips

# In[11]:


import requests
import pandas as pd
import os
from bs4 import BeautifulSoup


# In[12]:


### Set variables ###
local_folder = '/Users/lisaoswald/python'
file_name = 'data_flyid_final.csv'


# In[13]:


# url construction + web logic in one separate function that can be re-used and applied across rows
def get_route_and_distance(start, dest):

    # if we don't have a start or a destination, no need to look anything up!
    if pd.isna(start) or pd.isna(dest):
        return None, None

    my_url = "https://www.distance.to/" + start + "/" + dest
    page = requests.get(my_url)
    soup = BeautifulSoup(page.content, 'html.parser')
    route = soup.find(class_="main-route trip").get_text()
    dist_calc = soup.find(class_="value km").get_text()
    return route, dist_calc


# In[14]:


# url construction + web logic in one separate function that can be re-used and applied across rows
def get_distance(row):
    start = row['start']
    dest = row['destination']
    
    print('Getting distance...')
    
    # if we don't have a start or a destination, no need to look anything up!
    if pd.isna(start) or pd.isna(dest):
        return None, None

    my_url = "https://www.distance.to/" + start + "/" + dest
    page = requests.get(my_url)
    soup = BeautifulSoup(page.content, 'html.parser')
    dist_calc = soup.find(class_="value km").get_text()
    print('Got distance.')
    return dist_calc


# In[15]:


get_ipython().run_line_magic('time', "get_route_and_distance('London', 'Paris')")
# how long does one of these requests take? 


# It takes around 300ms for 1 row, so for 2000 rows it should take around 600s; roughly 10 minutes. 
# Most of this time is probably spent waiting for the request to return, and we could parallelise this if speed became important. 

# In[16]:


# load input data set (csv)
data_full = pd.read_csv(os.path.join(local_folder, file_name), encoding = "ISO-8859-1", 
                       sep=',', error_bad_lines=False, na_values=".", index_col=0)


# creat data frame only with travel data
data = pd.DataFrame({"start1": data_full['FL02x01'],
                          "destination1": data_full['FL09x01'],
                          "start2": data_full['FL02x02'],
                          "destination2": data_full['FL09x02'],
                          "start3": data_full['FL02x03'],
                          "destination3": data_full['FL09x03'],
                          "start4": data_full['FL02x04'],
                          "destination4": data_full['FL09x04'],
                          "start5": data_full['FL02x05'],
                          "destination5": data_full['FL09x05'],
                          "start6": data_full['FL02x06'],
                          "destination6": data_full['FL09x06'],
                          "start7": data_full['FL02x07'],
                          "destination7": data_full['FL09x07'],
                          "start8": data_full['FL02x08'],
                          "destination8": data_full['FL09x08'],
                          "start9": data_full['FL02x09'],
                          "destination9": data_full['FL09x09'],
                          "start10": data_full['FL02x10'],
                          "destination10": data_full['FL09x10'],
                          "start11": data_full['FL02x11'],
                          "destination11": data_full['FL09x11'],
                          "start12": data_full['FL02x12'],
                          "destination12": data_full['FL09x12'],
                          "start13": data_full['FL02x13'],
                          "destination13": data_full['FL09x13'],
                          "start14": data_full['FL02x14'],
                          "destination14": data_full['FL09x14'],
                          "start15": data_full['FL02x15'],
                          "destination15": data_full['FL09x15'],
                          "start16": data_full['FL02x16'],
                          "destination16": data_full['FL09x16'],
                          "start17": data_full['FL02x17'],
                          "destination17": data_full['FL09x17'],
                          "start18": data_full['FL02x18'],
                          "destination18": data_full['FL09x18'],
                          "start19": data_full['FL02x19'],
                          "destination19": data_full['FL09x19'],
                          "start20": data_full['FL02x20'],
                          "destination20": data_full['FL09x20'],
                          "start21": data_full['FL02x21'],
                          "destination21": data_full['FL09x21'],

                          "start26": data_full['FL08x01'],
                          "destination26": data_full['FL10x01'],
                          "start27": data_full['FL08x02'],
                          "destination27": data_full['FL10x02'],
                          "start28": data_full['FL08x03'],
                          "destination28": data_full['FL10x03'],
                          "start29": data_full['FL08x04'],
                          "destination29": data_full['FL10x04'],
                          "start30": data_full['FL08x05'],
                          "destination30": data_full['FL10x05'],
                          "start31": data_full['FL08x06'],
                          "destination31": data_full['FL10x06'],
                          "start32": data_full['FL08x07'],
                          "destination32": data_full['FL10x07'],
                          "start33": data_full['FL08x08'],
                          "destination33": data_full['FL10x08'],
                          "start34": data_full['FL08x09'],
                          "destination34": data_full['FL10x09'],
                          "start35": data_full['FL08x10'],
                          "destination35": data_full['FL10x10'],
                          "start36": data_full['FL08x11'],
                          "destination36": data_full['FL10x11'],
                          "start37": data_full['FL08x12'],
                          "destination37": data_full['FL10x12'],
                          "start38": data_full['FL08x13'],
                          "destination38": data_full['FL10x13'],
                          "start39": data_full['FL08x14'],
                          "destination39": data_full['FL10x14'],
                          "start40": data_full['FL08x15'],
                          "destination40": data_full['FL10x15'],
                          "start41": data_full['FL08x16'],
                          "destination41": data_full['FL10x16'],
                          "start42": data_full['FL08x17'],
                          "destination42": data_full['FL10x17'],
                          "start43": data_full['FL08x18'],
                          "destination43": data_full['FL10x18'],
                          "start44": data_full['FL08x19'],
                          "destination44": data_full['FL10x19'],
                          "start45": data_full['FL08x20'],
                          "destination45": data_full['FL10x20'],
                          "start46": data_full['FL08x21'],
                          "destination46": data_full['FL10x21'],
                          "start47": data_full['FL08x22'],
                          "destination47": data_full['FL10x22'],
                          "start48": data_full['FL08x23'],
                          "destination48": data_full['FL10x23'],
                          "start49": data_full['FL08x24'],
                          "destination49": data_full['FL10x24'],
                          "start50": data_full['FL08x25'],
                          "destination50": data_full['FL10x25'],
                          })


# #### Multi-column indexing - Reducing repetitive code 

# In[17]:


# turn the column labels into a multi-column index, so we have multiple starts and multiple destinations

fields = ['destination', 'start']
indices = range(1, 47)
column_names = [(field, index) for field in fields for index in indices]
data_new = data.sort_index(axis=1).copy()
data_new.columns = pd.MultiIndex.from_tuples(column_names)
data_new.head()


# In[18]:


# stack the data, so each row has one start and one destination
stacked = data_new.stack()


# In[19]:


# calculate the distance for each row
stacked['distance'] = stacked.apply(get_distance, axis=1)


# In[20]:


# unstack to get the data back to the way it was
output = stacked.unstack()


# In[21]:


# have a look at the data
output


# In[22]:


# create pandas data frame
my_df = pd.DataFrame(output)

# convert pandas df into csv file
my_df.to_csv(os.path.join(local_folder, 'flight_data_final.csv'), sep=',', index=False, encoding='utf-8')

# check
data = pd.read_csv(os.path.join(local_folder, 'flight_data_final.csv'), sep=',', na_values=".")
print(data)    

