setInternet2(TRUE)
library(data.table)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(zoo)

snp=read.csv(file.choose(),header = T) #sp500mini_fut file from 
#E:\Drive\Realtime_DD_iteration1\DD_repeated_thrice

SP=xts(snp[,2],order.by=as.Date(snp[,1],format="%m/%d/%Y"))
dates=data.frame(DATE = index(SP))
#Wednesdays=3420
Wed=matrix(NA,nrow=10000,ncol=3)

j=2
Wed[1,3]=1
Wed[1,1]=as.character(snp[1,1])
Wed[1,2]=snp[1,2]

#took dates of all wednesdays and left blank if no wednesday in the week
for(i in 2:nrow(snp)){ #
  if(weekdays(as.Date(snp[i,1],format="%m/%d/%Y"))=="Wednesday"){
    Wed[j,3]=as.integer(i)
    if(as.integer(Wed[j,3])-as.integer(Wed[(j-1),3])<7){
      Wed[j,1]=as.character(snp[i,1])
      Wed[j,2]=snp[i,2]
      j=j+1
    } else{
      j=j+1
      Wed[j,3]=as.integer(i)
      Wed[j,1]=as.character(snp[i,1])
      Wed[j,2]=snp[i,2]
      j=j+1
    }
  }
}

for(i in 1:nrow(Wed)){
  if(is.na(Wed[i,3])==1){t=i
  break}
}
Wed=Wed[1:(t-1),]

#Replacing missing wednesdays with tuesdays
for(j in 2:nrow(Wed)){
  if(is.na(Wed[j,1]==1)){
    for(i in Wed[j-1,3]:Wed[j,3]){
      if(weekdays(as.Date(snp[i,1],format="%m/%d/%Y"))=="Tuesday"){
        Wed[j,3]=i
        if(as.numeric(Wed[j,3])-as.numeric(Wed[j-1,3])<7){
          Wed[j,2]=snp[i,2]
          Wed[j,1]=as.character(snp[i,1])
        }
        break
      }
    }
  }
}


Peak_to_peak_DD=matrix(NA,nrow=nrow(snp),ncol=2)


Peak_prc=snp[1,2]
Trough_prc=snp[1,2]

#code to calculate peak-to-peak drawdowns
#Every week, check if there is new peak. If yes, then replace its DD with 0 and populate DDs before and after that particular date

for(i in 1:nrow(Wed)){ #
  for(j in (as.numeric(Wed[i,3])+1):(as.numeric(Wed[i+1,3]))){
    if(Peak_prc<snp[j,2]){
      Peak_prc1=Peak_prc
      Peak_prc=snp[j,2]
      t=j
    }
  }
  
  Peak_to_peak_DD[t,1]=as.character(snp[t,1])
  Peak_to_peak_DD[t,2]=0
  if(t==j){
  } else{ 
    for(k in (t+1):(as.numeric(Wed[i+1,3]))){
      Peak_to_peak_DD[k,1]=as.character(snp[k,1])
      Peak_to_peak_DD[k,2]=as.numeric(Peak_to_peak_DD[k-1,2])+log(as.numeric(snp[k,2])/as.numeric(snp[k-1,2]))
    }
  }
  
  if(i>1){
    for(l in (as.numeric(Wed[i-1,3])+1):(t-1)){
      if(is.na(Peak_to_peak_DD[l,2])==0){
      } else{
        Peak_to_peak_DD[l,1]=as.character(snp[l,1])
        Peak_to_peak_DD[l,2]=as.numeric(Peak_to_peak_DD[l-1,2])+log(as.numeric(snp[l,2])/as.numeric(snp[l-1,2]))
      }
    }
  }
}

#code to calculate real-time drawdowns & Candidate trough
Realtime_DD=matrix(NA,nrow=nrow(snp),ncol=5)

Peak_prc=snp[1,2]
Trough_prc=snp[1,2]

t1=0
for(i in 1:(nrow(Wed)-1)){ 
  for(j in (as.numeric(Wed[i,3])+1):(as.numeric(Wed[i+1,3]))){
    if(Peak_prc<snp[j,2]){
      Peak_prc1=Peak_prc
      Peak_prc=snp[j,2]
      t=j
    }
  }
  
  if((j-t)>4){ #since j goes upto next Wed, and peak is on day 't', we have to check for troughs b/w t+1 and j-1
    #and atleast two days to find out if there is a trough
    for(m in (t+1):(as.numeric(Wed[i+1,3]))-1){
      
      if(Trough_prc>=snp[m,2]){
        Trough_prc=snp[m,2]
        t1=m
      }
    }
  }else{
    t1=0
    Trough_prc=100000
  }
  
  if(t1==0){
  } else{
    Realtime_DD[(as.numeric(Wed[i+1,3])),2]=as.character(snp[t1,1])
    Realtime_DD[(as.numeric(Wed[i+1,3])),4]=t1  #candidate trough dates
  }
  
  Realtime_DD[t,3]=0
  if(t==j){
  } else{ 
    for(k in (t+1):(as.numeric(Wed[i+1,3]))){
      Realtime_DD[k,3]=as.numeric(Realtime_DD[k-1,3])+log(as.numeric(snp[k,2])/as.numeric(snp[k-1,2]))
    }
  }
  
  if(i>1){
    for(l in (as.numeric(Wed[i-1,3])+1):(t-1)){
      if(is.na(Realtime_DD[l,3])==0){
      } else{
        Realtime_DD[l,3]=as.numeric(Realtime_DD[l-1,3])+log(as.numeric(snp[l,2])/as.numeric(snp[l-1,2]))
      }
    }
  }
}

for(i in 1:nrow(Realtime_DD)){
  Realtime_DD[i,1]=snp[i,2]
}


#if trough is repeated thrice, change drawdowns: Final drawdowns (Real-time)
#Peak_prc=snp[1,2]
a=0
b=0
c=0
for(i in 1:nrow(Realtime_DD)){ 
  for(z in 1:(nrow(Wed)-1)){
    for(j in (as.numeric(Wed[z+1,3])+1):(as.numeric(Wed[z+2,3]))){
      
      if(is.na(Realtime_DD[j,4])==0){
        c=as.numeric(b)
        b=as.numeric(a)
        a=as.numeric(Realtime_DD[j,4])
      }
      if(a==b & b==c & a>0){  #repeated thrice
        Realtime_DD[a,3]=0
        for(k in (a+1):nrow(Realtime_DD)){
          if(Realtime_DD[k,3]==0){
            break
          } else{
            Realtime_DD[k,3]=as.numeric(Realtime_DD[k-1,3])+as.numeric(snp[k,3])
          }
        }
      }
    }
  }
}



Rt_dd=cbind(Peak_to_peak_DD,Realtime_DD[,1:3])
colnames(Rt_dd)=c("Date","Peak-to-peak Drawdowns","Prices","Candidate trough","Real-time Drawdowns")


#TO fill candidate troughs on all wednesdays/tuesday
for(i in 1:nrow(Rt_dd)){
  if(is.na(Rt_dd[i,4])==0){
    t=i
    break
  }
}

for(j in 1:nrow(Wed)){
  for(i in t:nrow(Rt_dd)){
    if(Wed[j,1]==Rt_dd[i,1]){
      
      if(is.na(Rt_dd[i,4])==0){
        t1=Rt_dd[i,4]
        t=i
      }
      
      if(is.na(Rt_dd[i,4])==1){
        Rt_dd[i,4]=t1
      }
    }
  }
}


#Actual trading drawdowns
ADD=matrix(NA,nrow=nrow(Realtime_DD),ncol=1)
a=0
b=0
c=0
for(z in 1:(nrow(Wed)-1)){
  for(j in (as.numeric(Wed[z,3])+1):(as.numeric(Wed[z+1,3]))){
    # for(i in 1:nrow(Realtime_DD)){
    ADD[j,1]=Peak_to_peak_DD[j,2]
    if(is.na(Rt_dd[j,4])==0){
      c=b
      b=a
      a=Rt_dd[j,4]
    }
    
    if(a==b & b==c & a!=0){  #trough repeated thrice
      for(k in (as.numeric(Wed[z,3])+1):(as.numeric(Wed[z+1,3]))){
        ADD[k,1]=Rt_dd[k,5]
      }
    }
  }
}

colnames(ADD)=c("ADD")
Rt_dd1=cbind(Rt_dd,ADD)

write.csv(Rt_dd1,file="Realtime_DD_RepeatedThrice_fut.csv")


############################################################
#Estimation of Max Drawdown

DD_strategy=read.csv('E:/Drive/Realtime_DD_iteration1/DD_repeated_thrice/DD_strategy_fut.csv',header =T)
ADD_strategy=read.csv('E:/Drive/Realtime_DD_iteration1/DD_repeated_thrice/ADD_strategy_fut.csv',header =T)

#Max Drawdown
#t1 gives max drawdown
new=0
t=0
t1=0
for(i in 1:nrow(DD_strategy)){
  if(DD_strategy[i,7]<=0){new=new+DD_strategy[i,7]}
  if(DD_strategy[i,7]>0){
    t=new
    if(t<t1){t1=t}
    new=0
  }
}

#Recovery time
#count gives the number of weeks for recovery
new=0
count=0
sum=0
for(i in 1:nrow(DD_strategy)){
  if(DD_strategy[i,7]<=0){new=new+DD_strategy[i,7]}
  
  if(DD_strategy[i,7]>0){
    if(new==t1){
      z1=i #z1 gives the row where recovery starts
      for(z in i:nrow(DD_strategy)){
        count=count+1
        sum=sum+DD_strategy[z,7]
        if((sum+new)>0){
          break
        }
      }
    } else{new=0}
  }
  if(count>0){break}
}

#Max Drawdown
#t2 gives MDD

new=0
t=0
t2=0
for(i in 1:nrow(DD_strategy)){
  if(DD_strategy[i,8]<=0){new=new+DD_strategy[i,8]}
  if(DD_strategy[i,8]>0){
    t=new
    if(t<t2){t2=t}
    new=0
  }
}

#Recovery time
new=0
count=0
sum=0
for(i in 1:nrow(DD_strategy)){
  if(DD_strategy[i,8]<=0){new=new+DD_strategy[i,8]}
  
  if(DD_strategy[i,8]>0){
    if(new==t2){
      z1=i #z1 gives the row where recovery starts
      for(z in i:nrow(DD_strategy)){
        count=count+1
        sum=sum+DD_strategy[z,7]
        if((sum+new)>0){
          break
        }
      }
    } else{new=0}
  }
  if(count>0){break}
}

#Max Drawdown
new=0
t=0
t1=0
for(i in 1:nrow(ADD_strategy)){
  if(ADD_strategy[i,7]<=0){new=new+ADD_strategy[i,7]}
  if(ADD_strategy[i,7]>0){
    t=new
    if(t<t1){t1=t}
    new=0
  }
}

#Recovery time
new=0
count=0
sum=0
for(i in 1:nrow(ADD_strategy)){
  if(ADD_strategy[i,7]<=0){new=new+ADD_strategy[i,7]}
  
  if(ADD_strategy[i,7]>0){
    if(new==t1){
      z1=i #z1 gives the row where recovery starts
      for(z in i:nrow(ADD_strategy)){
        count=count+1
        sum=sum+DD_strategy[z,7]
        if((sum+new)>0){
          break
        }
      }
    } else{new=0}
  }
  if(count>0){break}
}

#Max Drawdown
new=0
t=0
t2=0
for(i in 1:nrow(ADD_strategy)){
  if(ADD_strategy[i,8]<=0){new=new+ADD_strategy[i,8]}
  if(ADD_strategy[i,8]>0){
    t=new
    if(t<t2){t2=t}
    new=0
  }
}

#Recovery time
new=0
count=0
sum=0
for(i in 1:nrow(ADD_strategy)){
  if(ADD_strategy[i,8]<=0){new=new+ADD_strategy[i,8]}
  
  if(ADD_strategy[i,8]>0){
    if(new==t2){
      z1=i #z1 gives the row where recovery starts
      for(z in i:nrow(ADD_strategy)){
        count=count+1
        sum=sum+DD_strategy[z,7]
        if((sum+new)>0){
          break
        }
      }
    } else{new=0}
  }
  if(count>0){break}
}