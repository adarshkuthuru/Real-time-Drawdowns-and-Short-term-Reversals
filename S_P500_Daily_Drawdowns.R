setInternet2(TRUE)
library(data.table)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(zoo)

snp=read.csv(file.choose(),header = T) #sp500 file


t=0

for(i in 1:nrow(snp)){
  if(weekdays(as.Date(snp[i,1],format="%m/%d/%Y"))=="Wednesday"){
    t=t+1
  }
}

#wday(as.Date(snp[6,1],format="%m/%d/%Y"), label=TRUE)
#weekdays(as.Date(snp[6,1],format="%m/%d/%Y"))

SP=xts(snp[,2],order.by=as.Date(snp[,1],format="%m/%d/%Y"))

df1=SP[endpoints(SP, on = "weeks")+2, ]

DJ.roc <- ROC(SP,n=1,type="discrete")

SP500.RET=monthlyReturn(SP)
dailyDD <- findDrawdowns(SP500.RET)

Drawdowns <- table.Drawdowns(DJ.roc[,1],top=1000)

for(i in 1:nrow(Drawdowns)){
  if(is.na(Drawdowns[i,3])==1){
    Drawdowns[i,3]=as.Date(snp[nrow(snp),1],format="%d/%m/%Y")
  }
}


DD=Drawdowns[order(as.Date(Drawdowns[,1], format="%d/%m/%Y")),]
DD=DD[1:nrow(DD)-1,]

#####
#DECLINE CYCLE
#####
#SP=xts(daily$PRC[t:(t+length(g)-1)],order.by=as.Date(daily$DATE[t:(t+length(g)-1)],format="%d/%m/%Y"))


x <- matrix(NA,nrow=sum(Drawdowns[,6]),ncol=5)
t3=0
for(i in 1:nrow(Drawdowns)){  #
  for(j in t:(t+g-1)){
    if(as.Date(DD[i,1], format="%Y-%m-%d")==as.Date(snp[j,1],format="%m/%d/%Y")){
      t1=j-1 #In package, the drawdown starts one day after it actually starts, so adjusting for it.
      #if(is.na(t1)==1){t1=t}
      for(l in t1:(t+g-1)){
        if(as.Date(DD[i,2], format="%Y-%m-%d")==as.Date(snp[l,1],format="%m/%d/%Y")){
          t2=l-1
          break
        }
      }
      break
    }
  }
  for(k in (t3+1):(t3+t2-t1+1)){
    x[k,2]=as.character(snp[t1+k-t3-1,1])
    x[k,1]=i
    x[k,3]=snp[t1,2]
    x[k,4]=snp[t1+k-t3-1,2]
    x[k,5]=log(as.numeric(x[k,4])/as.numeric(x[k,3]))
  }
  t3=t3+t2-t1+1
}

x=na.omit(x)
colnames(x)=c("DDNO","DATE","PeakPRC","PRC","DD")


#####
#RECOVERY CYCLE
#####

if(nrow(Drawdowns)>1){
  sum=0
  for(i in 1:(nrow(Drawdowns)-2)){
    sum=sum+as.integer(DD[i+1,1]-DD[i,2])
  }
  x3 <- matrix(NA,nrow=sum,ncol=5)

  t3=0
  for(i in 1:nrow(Drawdowns)){
    if(i==nrow(Drawdowns)){
      for(j in t:(t+g-1)){
        if(as.Date(DD[i,2], format="%Y-%m-%d")==as.Date(snp[j,1],format="%m/%d/%Y")){
         t1=j
         for(l in t1:(t+g-1)){
            if(as.Date(DD[i,3], format="%Y-%m-%d")==as.Date(snp[l,1],format="%m/%d/%Y")){
             t2=l
              break
            }
          }
         break
        }
       }
      } else{
        for(j in t:(t+g-2)){
         if(as.Date(DD[i,2], format="%Y-%m-%d")==as.Date(snp[j,1],format="%m/%d/%Y")){
         t1=j
          for(l in t1:(t+g-1)){
          if(as.Date(DD[i+1,1], format="%Y-%m-%d")==as.Date(snp[l,1],format="%m/%d/%Y")){
             t2=l-2
             break
           }
         }
         break
          }
        }
      }
    for(k in (t3+1):(t3+t2-t1+1)){
      x3[k,2]=as.character(snp[t1+k-t3-1,1])
      x3[k,1]=i
      x3[k,3]=snp[t1,2]
      x3[k,4]=snp[t1+k-t3-1,2]
      x3[k,5]=log(as.numeric(x3[k,4])/as.numeric(x3[k,3]))
      }
    t3=t3+t2-t1+1
  }

  x3=na.omit(x3)
  colnames(x3)=c("DDNO","DATE","PeakPRC","PRC","DD")
  } else{
   x2 <- matrix(NA,nrow=Drawdowns[1,5]-Drawdowns[1,6]+30,ncol=5)
   t3=0
   for(i in 1:nrow(Drawdowns)){ 
      for(j in t:(t+g-1)){
       if(as.Date(DD[i,2], format="%Y-%m-%d")==as.Date(snp[j,1],format="%m/%d/%Y")){
         t1=j
         for(l in t1:(t+g-1)){
           if(as.Date(DD[i,3], format="%Y-%m-%d")==as.Date(snp[l,1],format="%m/%d/%Y")){
             t2=l
             break
           }
         }
         break
        }
     }
      for(k in (t3+1):(t3+t2-t1+1)){
        x2[k,2]=as.character(snp[t1+k-t3-1,1])
        x2[k,1]=i
        x2[k,3]=snp[t1,2]
        x2[k,4]=snp[t1+k-t3-1,2]
        x2[k,5]=log(as.numeric(x2[k,4])/as.numeric(x2[k,3]))
      }
     t3=t3+t2-t1+1
   }
  
   x2=na.omit(x2)
   colnames(x2)=c("DDNO","DATE","PeakPRC","PRC","DD")
  }

#####
#End of month
####
if(nrow(Drawdowns)==1){
  df_new=rbind(x,x2)
} else{
  df_new=rbind(x,x3)
}


df_new1=na.omit(df_new[,c(2,4,5)])
write.csv(df_new1,file="Daily_Drawdowns_Sp500.csv")
df_new2=xts(df_new[,2],order.by=as.Date(df_new[,1],format="%d/%m/%Y"))

df=daily[t:(t+length(g)-1),]
df$PERMNO=NULL
  
df1=xts(df$PRC,order.by=as.Date(df$DATE,format="%d/%m/%Y")) #Converted to xts object

df2=df1[endpoints(df1, on = "weeks"), ]

df_final=merge(df2,df_new2,all=c(T,F))
df_final=na.omit(df_final)

if(length(df_final)==0){
  df_new1=matrix(NA,nrow=1,ncol=4)
  colnames(df_new1)=c("DATE", "df2", "df_new2", "PERMNO")
  write.csv(df_new1,sprintf('myarray.%d.csv', z))
} else{
  df_new1=data.frame(DATE = index(df_final), df_final$df2, df_final$df_new2, PERMNO=uq[z],row.names=NULL)
  write.csv(df_new1,sprintf('myarray.%d.csv', z))
  write.csv(df_new,sprintf('DD.%d.csv', z))
}
t=t+length(g)
}
end.time <- proc.time()
cat("Elapsed; ", end.time[3]-start.time[3], "seconds.\n")