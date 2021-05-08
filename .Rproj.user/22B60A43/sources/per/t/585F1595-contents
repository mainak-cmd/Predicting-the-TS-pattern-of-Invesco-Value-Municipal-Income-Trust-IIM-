#Aim of the project is to findout when the pattern of a time serise will repeate useing random walk and simulation

"""ALGORITHUM
1.PREPARE TIME SERISE Data
2.DECOMPOSE BY ADDATIVE AS WLL AS multiplicative Type
3.split into train and test data
4.prepare addative and multiplicative model to forcast on training data
5.compare it with test data
6.prepare a funtion for addative and multiplicative model and replicate it n times to  find out  optimum RMSE,
7.make conclusion that after n years pattern will be repate"""

#LIBARIES INSTALLATION
library(data.table)
library(ggplot2)
library(fpp2)
library(forecast)
library(stats)
library(tseries)
library(expsmooth)
library(tidyverse)
library(Metrics)
library(caret)

#to create the project Invesco Value Municipal Income Trust (IIM) is used
#(NYSE - NYSE Delayed Price. Currency in USD)
data <- read.csv('IIM.csv')
data

#droup unuse colloumns and keep only open price and date features.
df_stock <- data[ -c(7:3) ]
# prepareing the time serise
Df_st<- ts(df_stock[,2], frequency=12, start=c(2016,1),end=c(2021,5))

plot.ts(Df_st)
#decompose timeserise in multiplicative form:
Comp <- decompose(Df_st,type="multiplicative")
plot(Comp)

min(Comp$figure)
#decompose timeserise in additive form:

Comp1 <- decompose(log(Df_st),type="additive")
Comp1

#train test split
Df_st_train<- ts(df_stock[,2], frequency=12, start=c(2016,1),end=c(2019,12))
Df_st_test<- ts(df_stock[,2], frequency=12, start=c(2020,1),end=c(2021,5))

#exponential smoothing considering treand and sesanality are both present in the time serize
hw1<-hw(Df_st_train,h=24,seasonal = "multiplicative")

hw_log<-hw(log(Df_st_train),h=24,seasonal = "additive")

#exponential smoothing plot
plot(forecast(hw_log,methods='rwdrift'))
plot(forecast(hw1,methods='rwdrift'))

forecast_additive =as.data.frame(forecast(hw_log,methods='rwdrift'))

forecast_multiplicative= as.data.frame(forecast(hw1,methods='rwdrift'))

newdata_additive <- forecast_additive[1:17,]
newdata_multiplicative<- forecast_multiplicative[1:17,]

#varience plot between  test and forcast
var_in_additive <- cbind(log(Df_st_test),newdata_additive[,1])
ts.plot(var_in_additive,col=c("blue","red"),main="actual vs forcast in additive type")

var_in_multiplicative <- cbind(Df_st_test,newdata_multiplicative[,1])
ts.plot(var_in_multiplicative,col=c("blue","red"),main="actual vs forcast in multiplicative type")

#Aim to useing reduce the RMSE useing  monto carlo simulation 
#extracting open price features

df_open_additive <- newdata_additive[ c(1) ]
df_open_multiplicative<- newdata_multiplicative[ c(1) ]

#define a funtion to find out the random change in price

#addative scale

funtion_for_simulation_addative<-function(X,Y,df,df_actual){
  
  x=runif(17, min=min(X), max=max(X))
  y= runif(17, min=min(Y), max=max(Y))
  
  # calculate randam change in price:
  
  for (j in df){
    for(i in x){
      for(k in y){
        R_C <-j+i+k ##subtracting seasanality and randomness from serize to get more accurate
        
      }
    }
    
  }
  
  # return result:
  return(rmse(log(df_actual),R_C)) #data.frame(R_C)
}

#simulate 30 times to findout optimal error as well as replicate the treand

year = 12
r1=replicate(year,funtion_for_simulation_addative(Comp1$seasonal,Comp1$figure,df_open_additive,Df_st_test))


df_addative <-data.frame( do.call(cbind, list(r1)))
plot(df_addative[,1], type = "s",col = "red", xlab = "year", ylab = "RMSE",main="year(for 12) vs RMSE in addative type")

#from the graph it is observed thaat the pattern will repate again aafter 2 year in 12 year simulation 


#multivative scale

funtion_for_simulation_multivative<-function(X,Y,df,df_actual){
  
  x=runif(17, min=min(X), max=max(X))
  y= runif(17, min=min(Y), max=max(Y))
  
  # calculate randam change in price:
  
  for (j in df){
    for(i in x){
      for(k in y){
        R_C_m <-j*(i*k)
        
      }
    }
    
  }
  
  # return result:
  return(rmse(df_actual,R_C_m))
}


#simulate 12 times to findout optimal error as well as replicate the treand

r2=replicate(year,funtion_for_simulation_multivative(Comp$seasonal,Comp$figure,df_open_multiplicative,Df_st_test))

df_multivative <-data.frame( do.call(cbind, list(r2)))
plot(df_multivative[,1], type = "s",col = "red", xlab = "year", ylab = "RMSE",main = "year(for 12) vs RMSE in multivative scale ")
#from the graph it is observed thaat the pattern will repate again aafter 5 year in 12 year simulation 


preproc_add <- preProcess(data.frame(df_addative[,1]), method=c("center", "scale"))
preproc_mul <- preProcess(data.frame(df_multivative[,1]), method=c("center", "scale"))

norm1_add <- predict(preproc_add,data.frame(df_addative[,1]))
norm1_mul <- predict(preproc_mul,data.frame(df_multivative[,1]))


l1= list(1:year)
a=data.frame(l1,norm1_add[,1])
names(a) <- c('year', 'RSME')


b=data.frame(l1,norm1_mul[,1])
names(b) <- c('year', 'RSME')

p = ggplot() + 
  geom_line(data = data.frame(a), aes(x =year, y = RSME), color = "blue") +
  geom_line(data = data.frame(b), aes(x = year, y = RSME), color = "red") +
  xlab('year') +
  ylab('RSME')
  


print(p)


#conclusion
"""as forcast on addative and multivative type TS shows different different results so one can refers either addative or  multivative type
   to understand the repetation of timeserise"""

