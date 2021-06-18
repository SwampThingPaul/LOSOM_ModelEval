# -*- coding: utf-8 -*-


import pydss
import os,sys, csv
import datetime as dt 
import matplotlib.pyplot as plt

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


# find a checking windwo up to 18 months based on the start day 
def find_endofday (julday):

    tmp=jd2cal(julday)
    start_year=tmp[0]
    start_month=tmp[1]
    start_day=tmp[2]
    
    #start 1-10
    if start_month>0 and start_month < 11:
        end_day=30
        end_month=5
        end_year=start_year+1
       
    #SET END DATE FOR NOV
    elif start_month == 11:  
        end_day=start_day
        end_month=5
        end_year=start_year+2
        

    #SET END DATE FOR DEC
    else: #start_month== 12 : 
        end_day=30
        end_month=5
        end_year=start_year+2
    
    return cal2jd(end_year,end_month,end_day)  



#### program start here

ninputs=len(sys.argv)-1

if ninputs !=3:
   print (" program have inputs ", ninputs)
   print (" usage:'/opt/hesm/apps/anaconda3/bin/python3 mflst_LO_v3.py dss_filelist model_runs output_dir")
   print ( "program aborting")
   sys.exit(1)

dss_filelist=sys.argv[1].split(',')
model_labels=sys.argv[2].split(',')
output_dir=sys.argv[3]




### sample inputs
# dss_filelist=['/nw/hesm_nas/projects/LOSOM/Models/RSMBN/Baselines/LSMECB/output_052521_xml17569_losum_dev_5794/RSMBN_output.dss',
               # '/nw/hesm_nas/projects/LOSOM/Models/RSMBN/Baselines/LSM25B/output_052521_xml17565_losum_dev_5791/RSMBN_output.dss',
                # '/nw/hesm_nas/projects/LOSOM/Models/RSMBN/Iteration2/AA/output_053121_xml17636_losum_dev_5794/RSMBN_output.dss',
                # '/nw/hesm_nas/projects/LOSOM/Models/RSMBN/Iteration2/BB/output_053121_xml17634_losum_dev_5794/RSMBN_output.dss',
                # '/nw/hesm_nas/projects/LOSOM/Models/RSMBN/Iteration2/CC/output_052621_xml17592_losum_dev5791/RSMBN_output.dss']
                
# model_labels=['ECBr','NA25','AA','BB','CC']
# output_dir='/nw/hesm_nas/applications/rtopa/mflst/lake_okeechobee/debug'



start_year=1965
start_month=1
start_day=1

end_year=2016
end_month=12
end_day=31


start = dt.datetime(int(start_year),int(start_month),int(start_day))
end = dt.datetime(int(end_year),int(end_month),int(end_day))


all_exceedance=[]

for i, dss_file in enumerate(dss_filelist):


    model_run=model_labels[i]
    ### read data from dss file
    f1 = pydss.openfile(dss_file)
    d_list=[ ['LOK',f1,'/RSMBN/LOK/STAGE/01JAN1965/1DAY/SIMULATED/']
           ]         
    df = pydss.retrieve_df(start, end, d_list)    
    gauge_data=df.iloc[:,0].to_numpy()
    pydss.closefile(f1)


    ##### check the exceedance
    data_days=df.shape[0]
    julian_data=np.empty(shape=(data_days))
    day_data=np.empty(shape=(data_days),dtype='int')
    month_data=np.empty(shape=(data_days),dtype='int')
    year_data=np.empty(shape=(data_days),dtype='int')


    exc_sum_data=np.empty(shape=(data_days),dtype='int')

    exc_data=np.empty(shape=(data_days),dtype='int')
    exc_data2=np.empty(shape=(data_days))             ### this data set will be used to create ouput series, thun define it as float array
    exc_data3=np.empty(shape=(data_days),dtype='int')


    exc_end_data=np.empty(shape=(data_days),dtype='int')   
    exc_reset_data=np.empty(shape=(data_days),dtype='int')   
    exc_ct=np.empty(shape=(data_days))



    exc_sum_data[:]=0
    #checking_window[:]=int(0)    ## checking window up to 18 months
    exc_data[:]=0
    exc_data2[:]=int(0)
    exc_data3[:]=0


    exc_end_data[:]  = 0
    exc_reset_data[:]=0

    exc_ct[:]=int(-9999)

    exc_n=0  



    dates=df.index
    year_data=dates.year
    month_data=dates.month
    day_data=dates.day

    julian_data=np.empty(shape=(data_days))

    for i in range(data_days):
        julian_data[i]=cal2jd(year_data[i],month_data[i],day_data[i])


    #LOK MFL criteria 
    mfl_criteria1=11
    mfl_criteria2=80



    ### check single daily exceedence based on stage and stage criterium that is 11.0 ft
    for bg_ct in range(0,data_days, 1):     
        if gauge_data[bg_ct] <   mfl_criteria1  :   ## mfl_criteria1[0]==11.
                exc_data[bg_ct]=1
              

    #get daily          
    for bg_ct in range(0,data_days, 1):      #skip the first 30 days 
        
          if exc_data[bg_ct]==1:
                                                     
                end_jd=find_endofday(julian_data[bg_ct]+1)
                                    
                ## find the checking window 
                tmp_julian_data=range(int(julian_data[bg_ct]+1),int(end_jd),1)
                
                tmp_len=len(tmp_julian_data)
                                                
                tmp_idx=np.nonzero(np.in1d(julian_data,tmp_julian_data))[0] 
                                  
                #checking_window[bg_ct]=tmp_len    #### used for debug file to check mfl logics
                
                ### get total exceedence days and index or days when stage <11.ft  within the last 18 months
                tmp_idx2=np.where(exc_data[tmp_idx]==1)      
                tmp_idx2= tmp_idx2[0]
                
                tmp_sum2=len(tmp_idx2)
                exc_sum_data[bg_ct]=tmp_sum2
                                                                       
                if tmp_sum2>mfl_criteria2:    # cumulative single daily exceedance >80
                
                    exc_data2[tmp_idx[tmp_idx2[mfl_criteria2-1]]]=1  ### then the 81st single exceedance day is marked as the first exceedence day
                                                                                
          
          #set an end exceedence 
          if exc_data2[bg_ct-2]==1 and exc_data2[bg_ct-1]==0:
                exc_end_data[bg_ct-2]=1     #found the last exceedance dates 
            
    counts=0
                    
    for bg_ct in range(0,data_days, 1):  
                            
         if  exc_data2[bg_ct-1]==0 and exc_data2[bg_ct]==1:
             counts=1
             
        
         if  exc_end_data[bg_ct]==1:
                if exc_reset_data[bg_ct-1]<1:
                    exc_reset_data[bg_ct]=365
                    
                else:    
                    exc_reset_data[bg_ct]=exc_reset_data[bg_ct-1]-1
                    if exc_reset_data[bg_ct]==0 and exc_data2[bg_ct]==1:
                        exc_reset_data[bg_ct]=365
           
         else:
                
                exc_reset_data[bg_ct]=exc_reset_data[bg_ct-1]-1 
                counts=counts+1
                
                if counts>366 and exc_data2[bg_ct]==1:
                    exc_reset_data[bg_ct]=365
                    counts=0
                    
                if exc_reset_data[bg_ct]==0 and exc_data2[bg_ct]==1:
                        exc_reset_data[bg_ct]=365
             
             #identify yearly violation          
         if exc_reset_data[bg_ct]<0:
                 if exc_data2[bg_ct]==1:
                     
                    if exc_data2[bg_ct-1]!=1:
                        exc_data3[bg_ct]=1
                        exc_n=exc_n+1    
                    else:
                         exc_data3[bg_ct]=0   
                 else:
                     exc_data3[bg_ct]=0
                 
         else:
                # exc_data3[bg_ct]=0
                 if exc_reset_data[bg_ct]==365 and exc_end_data[bg_ct]==0:
                    exc_data3[bg_ct]=1
                    exc_n=exc_n+1  
                 else:
                    exc_data3[bg_ct]=0  
        

    exc_n3=len(np.where(exc_data3==1)[0])

    print (' found total exceedance  ',exc_n3)


    all_exceedance.append(exc_n3)
    
    
    
    #save the data out to check MFL logics           
    csvfile=open(output_dir+r'/LO_mfl_check_results_'+model_run+'.csv', 'w',newline='')       
    csvfo = csv.writer(csvfile, dialect='excel')
    csvfo.writerow(["julian","year","month","day","daily_stage","daily single exceedance", \
         "81st trigger real exceedance", "end_exceedance"," count down data", "yearly_exceeedance"])

    for i in range(0,len(gauge_data),1):
       csvfo.writerow([str(julian_data[i])] + [str(year_data[i])] + [str(month_data[i])] + [str(day_data[i])] \
        + ["%.4f" % gauge_data[i]]+["%.4f" % exc_data[i]]+["%.4f" % exc_data2[i]]+["%.4f" % exc_end_data[i]]+["%.4f" % exc_reset_data[i]]+["%.4f" % exc_data3[i]])
    csvfile.close()
    print ('save check data into file', output_dir+r'/LO_mfl_check_results_'+model_run+'.csv')



    #=====================================================================
    # Prepare data for each graphic line
    #=====================================================================

           

    mpl_date=julian_data-1721425
    mpl_ge_x=[min(mpl_date)-20, max(mpl_date)+30]

    miny=8
    maxy=20

    plt.clf()
    plt.close('all')


    fig = plt.figure(figsize=(10,7.5))
    ax1 = fig.add_subplot(111)



    ax1.plot_date(mpl_date, gauge_data, fmt="b-", linewidth = 2, label='Water Elevation')


    plt.title('MFL Recovery Water Body - Minimum Flows and Levels for Lake Okeechobee ' +'\n('+model_run+':'+ str(int(exc_n))+' exceedance events for 52 years of similation)')

    plt.figtext(0.1, 0.025, ' Exceedance Criteria: 80 consecutive or non-consecutive days with daily water elevation below MFL Criteria: < 11 (NGVD29 FT) during  eighteen month period',size='x-small')
    plt.figtext(0.1, 0.0075, 'The recovery designation indicates that exceedances and violations are expected to occur until all of the projects listed in the recovery strategy are completed.', size='x-small')


    ax1.set_ylabel('Stage (feet, NGVD)')
    ax1.set_xlabel('Date')

    ax1.yaxis.grid(color='gray', linestyle=':', linewidth = 2)
    #ax1.set_axisbelow()
    plt.ylim([miny,maxy]) 


    mfl_ge_y=empty(shape=(len(mpl_ge_x)))
    mfl_ge_y[:]=mfl_criteria1
    ax1.plot_date(mpl_ge_x, mfl_ge_y, color='brown', linewidth = 2, linestyle='dashed', marker='', label='MFL Criteria')


    # temp=where(exc_data < -99)
    # temp=temp[0]
    # exc_data[temp]=np.nan


    ###exceedance

    plot_exc_data= np.copy(exc_data2)
    idx=np.where(plot_exc_data<1)
    plot_exc_data[idx[0]]=np.nan
    idx=np.where((plot_exc_data ==1) & (exc_reset_data<0))
    plot_exc_data[idx[0]]=gauge_data[idx[0]]
    idx=np.where(plot_exc_data==1)
    plot_exc_data[idx[0]]=np.nan




    ##### exceedance with 365 days


    plot_exc_data2= np.copy(exc_data2)
    idx=np.where(plot_exc_data2 <1)
    plot_exc_data2[idx[0]]=np.nan
    idx=np.where((plot_exc_data2 ==1) & (exc_reset_data>0) )
    plot_exc_data2[idx[0]]=gauge_data[idx[0]]
    idx=np.where(plot_exc_data2==1)
    plot_exc_data2[idx[0]]=np.nan




    ax1.plot_date(mpl_date, plot_exc_data, linestyle='-', linewidth = 2, color='darkorange', marker='', label='Exceedance')    
    ax1.plot_date(mpl_date, plot_exc_data2,linestyle='-', linewidth = 2, color='darkgrey', marker='', label='Exceedance within 365 Days' )


    ax1.legend(loc=9, ncol=3)

    dir_str=r'/mfl_'
    plt.savefig(output_dir+dir_str+'LOK_'+model_run+'.png')
    print (" save figure into ",output_dir+dir_str+'LOK_'+model_run+'.png')
    
    
# plot a bar chart for a comparison of exceedane times over model runs


nruns=len(model_labels)


plt.clf()
plt.close('all')


fig = plt.figure(1, figsize=(10,7.5))
ax = fig.add_subplot(111)


xdata=np.array(range(0,nruns))+1

#xdata=np.arange(nruns)+1

# Create the barchart

width=0.25

ax.yaxis.grid(linestyle='--',color="lightgrey",linewidth=0.5)


bar=ax.bar(xdata,all_exceedance,width, color='yellow', edgecolor='black', label='# of times stage less than 11 ft for >80 days')

### add label over the bar

for i, v in enumerate(all_exceedance):
    ax.text(xdata[i]-0.05, v+0.25, str(v), color='black')

ax.set_xticks(xdata)
ax.set_xticklabels(model_labels)

ax.set_ylabel('Number of Times')
ax.set_xlabel('Alternatives')
ax.set_title('Number of Times LOK Proposed Minumum Level and Duration '+'\n Criteria Exceeded During the 52 years of Simulation')
plt.xlim(0,nruns+1)
plt.ylim(0, max(all_exceedance)+1)
    
#ax.bar_label(bar, padding=3) 


   
ax.legend()    
    
fig.savefig(output_dir+'/Number_of_Times_LOK_MFL_critieria_Exceedance.png')

print ('save summary plot into',output_dir+'/Number_of_Times_LOK_MFL_critieria_Exceedance.png')


