# -*- coding: utf-8 -*-
"""
Created on Wed Apr 26 10:08:15 2017

@author: d_floriello

Functions for Daily Measures Extractor
"""


import zipfile
import os
from os.path import isfile, join
import datetime
import time
from collections import OrderedDict
import pandas as pd
import numpy as np
import shutil
import re
#import unidecode


####################################################################################################
def Aggregator2(df):
    v = np.repeat(0.0, 24)    
    df2 = df[df.columns[2:98]]
    df2 = df2.values.ravel().astype(float)
    for k in range(1,25):
        v[k-1] += np.sum(np.array([x for x in df2[4*(k-1):4*k]], dtype = np.float64))
    return v
####################################################################################################
def Converter(s):
    points = [m.start() for m in re.finditer('\.', s)]
    if len(points) <= 1:
        return float(np.where(np.isnan(float(s)), 0, float(s)))
    else:
        s2 = s[:points[len(points)-1]].replace('.','') + s[points[len(points)-1]:]
        return float(np.where(np.isnan(float(s2)), 0, float(s2)))
####################################################################################################
def MeasureExtractor(s):
    mis = []
    E = [m.start() for m in re.finditer('=', s)]
    for e in E:
        se = ''
        for i in range(2, 50):
            if s[e+i] != '"':
                se += s[e+i]
            else:
                break
        mis.append(float(se.replace(',','.')))
    return mis
####################################################################################################
def FileFilter(ld, directory):
    mesi = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    to_be_extracted = []
    list_files = [of for of in os.listdir(directory) if isfile(join(directory, of))]
    M = datetime.datetime(2017,1,1,0,0,0)
    last_file = 0
    for f in list_files:
        #filedate = datetime.datetime(2017, int(f[2:4]), int(f[5:7]))
        fdt = time.ctime(os.path.getmtime(directory + "/" + f))
        filedate = datetime.datetime(int(fdt[20:]), mesi.index(fdt[4:7])+1, int(fdt[8:10]), hour = int(fdt[11:13]), minute = int(fdt[14:16]), second = int(fdt[17:19]))        
        if filedate > ld:
            to_be_extracted.append(f)
            if filedate > M:
                M = filedate
                last_file = f
    return to_be_extracted, time.ctime(os.path.getmtime(directory + "/" + last_file))
####################################################################################################    
def ZIPExtractor():
    mesi = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    last_date = open("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/last_date.txt")
    ld = last_date.read()
    LD = datetime.datetime(int(ld[20:]), mesi.index(ld[4:7])+1, int(ld[8:10]), hour = int(ld[11:13]), minute = int(ld[14:16]), second = int(ld[17:19]))
    strm = str(LD.month) if len(str(LD.month)) > 1 else "0" + str(LD.month)
    directory = "H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/" + str(LD.year) + "-" + strm
    tbe, M = FileFilter(LD, directory)
    for t in tbe:
        #print(t)
        path = directory + "/" + t
        zf = zipfile.ZipFile(path)
        lzf = [x for x in zf.namelist() if ".zip" in x]
        zf.extractall(path = "H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/TEMP")
        for z in lzf:
                #print(z)
            path2 = "H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/TEMP/" + str(z)
            with zipfile.ZipFile(path2) as zf2:
                right = [str(rf) for rf in zf2.namelist() if 'T1' in str(rf)]
                if len(right) > 0:
                    zf2.extract(right[0], path = "H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/TBP")
    shutil.rmtree("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/TEMP")
    os.makedirs("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/TEMP")
    new_last_date = open("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/last_date.txt", 'r+')
    new_last_date.write(M)
    return 1    
####################################################################################################
def D_Monthly_Aggregator(month):
    cl = ['E', 'F']
    for h in range(24):
        cl.append(str(h) + '.A')
        cl.append(str(h) + '.B')
        cl.append(str(h) + '.C')
        cl.append(str(h) + '.D')
    cl.append('G')
    Aggdf = pd.read_excel("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/Aggregatore_orari-2017.xlsx")
    Aggdf.columns = [['POD', 'Area', 'Giorno', '1', '2', '3', '4', '5', '6',
                    '7', '8', '9', '10', '11', '12',
                    '13', '14', '15', '16', '17', '18',
                    '19', '20', '21', '22', '23', '24']]
    m = month
    diz = OrderedDict()
    count = 0
    strm = str(m) if len(str(m)) > 1 else "0" + str(m)        
    if month >= 3:
        crppm = pd.read_excel("H:/Energy Management/02. EDM/01. MISURE/4. CRPP/2017/" + strm + "-2017/_All_CRPP_" + strm + "_2017.xlsx")
    else:
        crppm = pd.read_excel("H:/Energy Management/02. EDM/01. MISURE/4. CRPP/2017/03-2017/_All_CRPP_03_2017.xlsx")
    pathm = "H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/2017-" + strm + "/Giornalieri/CSV"
    csvfiles = os.listdir(pathm)
    csvfiles = [cf for cf in csvfiles if 'T1' in cf and '.txt' not in cf]
    for cf in csvfiles:
        zona = ""        
        pod = cf[10:24]
        date = datetime.datetime(2017, int(cf[2:4]), int(cf[5:7]))
        if crppm["ZONA"].ix[crppm["POD"] == pod].values.size > 0:
            zona = crppm["ZONA"].ix[crppm["POD"] == pod].values[0]
        df = pd.read_csv(pathm + "/" + cf, sep = ";", dtype = object)
        df.columns = cl
        vec = np.repeat(0.0, 24)
        td = []
        for h in range(24):
            ha = str(h) + '.A'
            hb = str(h) + '.B'
            hc = str(h) + '.C'
            hd = str(h) + '.D'
            va = Converter(str(df[ha].ix[0]))
            vb = Converter(str(df[hb].ix[0]))
            vc = Converter(str(df[hc].ix[0]))
            vd = Converter(str(df[hd].ix[0]))
            vec[h] = np.sum([va, vb, vc, vd], dtype = np.float64)
            
        td.append(pod)
        td.append(zona)
        td.append(date)
        td.extend(vec.tolist())
        diz[count] = td
        count += 1
            
    diz = pd.DataFrame.from_dict(diz, orient = 'index')    
    diz.columns = [['POD', 'Area', 'Giorno', '1', '2', '3', '4', '5', '6',
                    '7', '8', '9', '10', '11', '12',
                    '13', '14', '15', '16', '17', '18',
                    '19', '20', '21', '22', '23', '24']]
    Aggdf = Aggdf.append(diz, ignore_index = True)
    return Aggdf
####################################################################################################
def Aggregator(today):
    cl = ['E', 'F']
    for h in range(24):
        cl.append(str(h) + '.A')
        cl.append(str(h) + '.B')
        cl.append(str(h) + '.C')
        cl.append(str(h) + '.D')
    cl.append('G')
    Aggdf = pd.read_excel("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/Aggregatore_orari-2017.xlsx")
    Aggdf.columns = [['POD', 'Area', 'Giorno', '1', '2', '3', '4', '5', '6',
                    '7', '8', '9', '10', '11', '12',
                    '13', '14', '15', '16', '17', '18',
                    '19', '20', '21', '22', '23', '24']]
    m = today.month
    diz = OrderedDict()
    count = 0
    strm1 = str(m) if len(str(m)) > 1 else "0" + str(m)        
    strm2 = str(m-1) if len(str(m-1)) > 1 else "0" + str(m-1)        
    crppm1 = pd.read_excel("H:/Energy Management/02. EDM/01. MISURE/4. CRPP/2017/" + strm1 + "-2017/_All_CRPP_" + strm1 + "_2017.xlsx")
    crppm2 = pd.read_excel("H:/Energy Management/02. EDM/01. MISURE/4. CRPP/2017/" + strm2 + "-2017/_All_CRPP_" + strm2 + "_2017.xlsx")
    pathm = "H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/TBP"
    csvfiles = os.listdir(pathm)
    csvfiles = [cf for cf in csvfiles if 'T1' in cf and '.txt' not in cf]
    for cf in csvfiles:
        pod = cf[10:24]
        date = datetime.datetime(2017, int(cf[2:4]), int(cf[5:7]))
        zona = 0
        if date.month == m:
            zona = crppm1["ZONA"].ix[crppm1["POD"] == pod].values[0] if crppm1["ZONA"].ix[crppm1["POD"] == pod].values.size > 0 else 0
            shutil.copy2(pathm + "/" + cf,"H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/2017-" + strm1 + "/Giornalieri/CSV")
        else:
            zona = crppm2["ZONA"].ix[crppm2["POD"] == pod].values[0] if crppm2["ZONA"].ix[crppm2["POD"] == pod].values.size > 0 else 0
            shutil.copy2(pathm + "/" + cf,"H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/2017-" + strm2 + "/Giornalieri/CSV")
        df = pd.read_csv(pathm + "/" + cf, sep = ";", dtype = object)
        df.columns = cl
        vec = np.repeat(0.0, 24)
        td = []
        for h in range(24):
            ha = str(h) + '.A'
            hb = str(h) + '.B'
            hc = str(h) + '.C'
            hd = str(h) + '.D'
            va = Converter(str(df[ha].ix[0]))
            vb = Converter(str(df[hb].ix[0]))
            vc = Converter(str(df[hc].ix[0]))
            vd = Converter(str(df[hd].ix[0]))
            vec[h] = np.sum([va, vb, vc, vd], dtype = np.float64)
            
        td.append(pod)
        td.append(zona)
        td.append(date)
        td.extend(vec.tolist())
        diz[count] = td
        count += 1
            
    diz = pd.DataFrame.from_dict(diz, orient = 'index')    
    diz.columns = [['POD', 'Area', 'Giorno', '1', '2', '3', '4', '5', '6','7', '8', '9', '10', '11', '12','13', '14', '15', '16', '17', '18','19', '20', '21', '22', '23', '24']]
    Aggdf = Aggdf.append(diz, ignore_index = True)
    shutil.rmtree("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/TBP")
    os.makedirs("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/TBP")
    #Aggdf = Aggdf.drop_duplicates()
    return Aggdf    
##################################################################################################    
def T1_Mover(m):
    strm = str(m) if len(str(m)) > 1 else "0" + str(m)   
    os.makedirs("H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/2017-" + strm + "/T1")
    pathm = "H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/2017-" + strm + "/Giornalieri/CSV/"
    csvfiles = os.listdir(pathm)
    csvfiles = [cf for cf in csvfiles if 'T1' in cf and '.txt' not in cf]
    for cf in csvfiles:
        shutil.copy2(pathm + cf, "H:/Energy Management/02. EDM/01. MISURE/3. DISTRIBUTORI/ENEL Distribuzione S.p.A/2017/2017-" + strm + "/T1")
    return 1
##################################################################################################    
    
    
    