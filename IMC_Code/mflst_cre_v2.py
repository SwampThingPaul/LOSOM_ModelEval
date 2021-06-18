# -*- coding: utf-8 -*-


import pydss
import sys, csv
import datetime as dt 
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np 
from numpy import *



# convert Gregorian date to Julian date
def cal2jd(year, month, day):
    a = (14 - month)//12
    y = year + 4800 - a
    m = month + 12*a - 3
    julday=day + ((153*m + 2)//5) + 365*y + y//4 - y//100 + y//400 - 32045
    return julday

# Julian period day.
def jd2cal(julday): 
    a = julday + 32044
    b = (4*a + 3)//146097
    c = a - (146097*b)//4
    d = (4*c + 3)//1461
    e = c - (1461*d)//4
    m = (5*e + 2)//153
    day = e + 1 - (153*m + 2)//5
    month = m + 3 - 12*(m//10)
    year = 100*b + d - 4800 + m/10
    return year, month, day


#### program start here

ninputs=len(sys.argv)-1

if ninputs !=3:
   print (" program have inputs ", ninputs)
   print (" usage:'/opt/hesm/apps/anaconda3/bin/python mflstr_cre_v2.py dss_file model_run output_dir")
   print ( "program aborting")
   sys.exit(1)

dss_file=sys.argv[1]
model_run=sys.argv[2]
output_dir=sys.argv[3]



#command line input sample
#python  mflstr_cre_v2.py /nw/hesm_nas/projects/LOSOM/Models/RSMBN/Iteration1/ABNE/output_022621_xml17134_losum_dev_5775/RSMBN_output.dss ABNE /nw/hesm_nas/applications/rtopa/mflst/cre/debug


### sample inputs
#dss_file='/nw/hesm_nas/projects/LOSOM/Models/RSMBN/Iteration1/ABNE/output_022621_xml17134_losum_dev_5775/RSMBN_output.dss'
#model_run='ABNE'
#output_dir='/nw/hesm_nas/applications/rtopa/mflst/cre/debug'


#### read data from the dss file 
start_year=1965
start_month=1
start_day=1

end_year=2016
end_month=12
end_day=31


start = dt.datetime(int(start_year),int(start_month),int(start_day))
end = dt.datetime(int(end_year),int(end_month),int(end_day))

f1 = pydss.openfile(dss_file)
d_list=[ ['S79',f1,'/RSMBN/S79/FLOW/01JAN1965/1DAY/SIMULATED/']
       ]
df = pydss.retrieve_df(start, end, d_list)    
gauge_data=df.iloc[:,0].to_numpy()
pydss.closefile(f1)



#### add excel or csv file here if data is not from DSS


##### check the exceedance
data_days=df.shape[0]

dates=df.index
year_data=dates.year
month_data=dates.month
day_data=dates.day

julian_data=np.empty(shape=(data_days))

for i in range(data_days):
    julian_data[i]=cal2jd(year_data[i],month_data[i],day_data[i])


gauge_30ma_data=empty(shape=(data_days))    

exc_data=empty(shape=(data_days))
exc_ct=empty(shape=(data_days))

exc_data2=empty(shape=(data_days))    
exc_end_data=empty(shape=(data_days))   #exceedance end data 
exc_reset_data=empty(shape=(data_days))


gauge_30ma_data[:]=-9999.0
exc_ct[:]=int(0)
exc_data[:]=0

exc_data2[:] = int(0)  
exc_end_data[:]  =  int(0)  
exc_reset_data[:] =int(0)  

### CRE MFL criteria
mfl_criteria1=457


exc_n=0  
ma_value=30   #30 day moving average

for bg_ct in range(0,data_days+1, 1):     

   if bg_ct-ma_value>=0: 
     tmp_gauge_data=gauge_data[bg_ct-ma_value:bg_ct]
     valid_idx=where(tmp_gauge_data >-9999)
     gauge_30ma_data[bg_ct-1]=   sum(tmp_gauge_data[valid_idx[0]])/len(valid_idx[0])
     
     if gauge_30ma_data[bg_ct-1] < mfl_criteria1:
        exc_data[bg_ct-1]=1
       
     else:
         exc_data[bg_ct-1]=0
                 
     if exc_data[bg_ct-2]==1 and exc_data[bg_ct-1]==0:
         exc_end_data[bg_ct-2]=1     #found the last exceedance dates 
   
     else:
         exc_end_data[bg_ct-2]=0
    


counts=0
 
for bg_ct in range(ma_value,data_days, 1):      #skip the first 30 days 
    
     #reset counts
    
     if  exc_data[bg_ct-1]==0 and exc_data[bg_ct]==1:
         counts=1
         
    
     if  exc_end_data[bg_ct]==1:
            if exc_reset_data[bg_ct-1]<1:
                exc_reset_data[bg_ct]=365
                
            else:    
                exc_reset_data[bg_ct]=exc_reset_data[bg_ct-1]-1
                if exc_reset_data[bg_ct]==0 and exc_data[bg_ct]==1:
                    exc_reset_data[bg_ct]=365
       
     else:
            
            exc_reset_data[bg_ct]=exc_reset_data[bg_ct-1]-1 
            counts=counts+1
            
            if counts>366 and exc_data[bg_ct]==1:
                exc_reset_data[bg_ct]=365
                counts=0
                
            if exc_reset_data[bg_ct]==0 and exc_data[bg_ct]==1:
                    exc_reset_data[bg_ct]=365
      
     
     #identify yearly violation          
     if exc_reset_data[bg_ct]<0:
         if exc_data[bg_ct]==1:
             
            if exc_data[bg_ct-1]!=1:
                exc_data2[bg_ct]=1
                exc_n=exc_n+1    
            else:
                 exc_data2[bg_ct]=0   
         else:
             exc_data2[bg_ct]=0
         
     else:
         #
        if exc_reset_data[bg_ct]==365 and exc_end_data[bg_ct]==0:
            exc_data2[bg_ct]=1
            exc_n=exc_n+1  
        else:
            exc_data2[bg_ct]=0    
     

print (' found total exceedance  ',exc_n)

#save the data out to csv file            
csvfile=open(output_dir+r'/S79_check_results_'+model_run+'.csv', 'w',newline='')       
csvfo = csv.writer(csvfile, dialect='excel')
csvfo.writerow(["julian","year","month","day","daily_flow","30 MA flows","daily_exceedance","end_exceedance"," count down data", "yearly_exceeedance"])
for i in range(0, data_days,1):

    csvfo.writerow([str(julian_data[i])] + [str(year_data[i])] + [str(month_data[i])] + [str(day_data[i])] + ["%.4f" % gauge_data[i]]+["%.4f" % gauge_30ma_data[i]]+["%.4f" % exc_data[i]]+["%.4f" % exc_end_data[i]]+["%.4f" % exc_reset_data[i]]+["%.4f" % exc_data2[i]])
csvfile.close()
print ('save check data into file', output_dir+r'/S79_check_results_'+model_run+'.csv')



#plot the results         

mpl_date=julian_data-1721425
mpl_ge_x=[min(mpl_date)-20, max(mpl_date)+30]


gauge_data88=gauge_data

temp=where(gauge_data88 > -999)
miny=min(gauge_data88[temp[0]])


maxy=max(gauge_30ma_data)+2000



fig = plt.figure(figsize=(10,7.5))
ax1 = fig.add_subplot(111)

idx=where(gauge_30ma_data==-9999.)  

gauge_30ma_data[idx[0]]=np.nan

ax1.plot_date(mpl_date, gauge_30ma_data, fmt="b-", linewidth = 2, label='Fresh Water Inflow')


plt.title('MFL Recovery Water Body - Caloosahachee River 30 day average flow at S79 ' +'\n('+model_run+':'+ str(int(exc_n))+' exceedance events for 52 years of similation)')

plt.figtext(0.1, 0.025, 'Exceedance Criteria: 30 day average flow below MFL Criteria: <'+str(mfl_criteria1)+'(CFS)', size='x-small')
plt.figtext(0.1, 0.0075, 'The recovery designation indicates that exceedances and violations are expected to occur until all of the projects listed in the recovery strategy are completed.', size='x-small')

ax1.set_ylabel('30 Day Average Flow at S79 (CFS)')
ax1.set_xlabel('Date')

ax1.yaxis.grid(color='gray', linestyle=':', linewidth = 2)
#ax1.set_axisbelow()
plt.ylim([miny,maxy]) 


mfl_ge_y=empty(shape=(len(mpl_ge_x)))
mfl_ge_y[:]=mfl_criteria1
ax1.plot_date(mpl_ge_x, mfl_ge_y, color='brown', linewidth = 2, linestyle='dashed', marker='', label='MFL Criteria')


temp=where(exc_data < -99)
temp=temp[0]
exc_data[temp]=np.nan



#plot_exc_data= exc_data2
plot_exc_data=np.copy( exc_data)

idx=where(plot_exc_data !=1)
plot_exc_data[idx[0]]=np.nan

idx=where((plot_exc_data ==1) & (exc_reset_data<0))
plot_exc_data[idx[0]]=gauge_30ma_data[idx[0]]


idx=where(plot_exc_data ==1)
plot_exc_data[idx[0]]=np.nan



plot_exc_data2= np.copy(exc_data)
idx=where(plot_exc_data2 !=1)
plot_exc_data2[idx[0]]=np.nan
idx=np.where((plot_exc_data2 ==1) & (exc_reset_data >0))
plot_exc_data2[idx[0]]=gauge_30ma_data[idx[0]]


idx=where(plot_exc_data2 ==1)
plot_exc_data2[idx[0]]=np.nan


ax1.plot_date(mpl_date, plot_exc_data, linestyle='-', linewidth = 2, color='darkorange', marker='', label='Exceedance')    
ax1.plot_date(mpl_date, plot_exc_data2,linestyle='-', linewidth = 2, color='darkgrey', marker='', label='Exceedance within 365 Days' )
ax1.legend(loc=9, ncol=2)

dir_str=r'/mfl_'
plt.savefig(output_dir+dir_str+'S79_'+model_run+'.png')
print (" save figure into ",output_dir+dir_str+'S79_'+model_run+'.png')
