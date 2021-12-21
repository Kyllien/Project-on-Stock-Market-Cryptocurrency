library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(forecast)

#Initilisation de la crypto sur indice 1 

indice1 <- "ETH-USD"
date <- "2020-01-01"
datef <- "2020-12-24"
getSymbols(indice1,from=date,to=datef)
command1 <- str_c("`",indice1,"`")
indice1 <- eval(parse(text=command1))
indice1 <- indice1[-is.na(indice1)]

#LOG OPERATIONS ON STOCK ##########
#Stock returns in log
indice1_log_returns<-indice1%>%Ad()%>%dailyReturn(type='log')

#Mean of log stock returns 
indice1_mean_log<-mean(indice1_log_returns)

#sd
indice1_sd_log<-sd(indice1_log_returns)

#Auto arima
out <- auto.arima(indice1$`ETH-USD.Adjusted`)
forecast(out,12)

summary(indice1)

#Test
d<-Ad(indice1)
d<-na.omit(d)
MACD.d<-MACD(d)
RSI_d <- RSI(d)


forecast(auto.arima(RSI_d[-c(seq(1:14),351,352,353,354)]),4)

#ChartSeries
indice1%>%Ad()%>%chartSeries()
indice1%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()')#,subset='2020')

#MONTE CARLO#####
#monte carlo simulation: incredibly useful forecasting tool to predict outcomes of events with many random variables
adj_indice <- str_c("as.numeric(indice1$'",names(indice1[,6]),"'[length(indice1$'",names(indice1[,6]),"'),])")
mu<-indice1_mean_log
sig<-indice1_sd_log
probs <- c(0.005,0.025,0.25,0.5,0.75,0.975,0.995)

N<-1000
mc_matrix<-matrix(nrow=275*1,ncol=N)
mc_matrix[1,1]<-eval(parse(text = adj_indice))

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-eval(parse(text = adj_indice))
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name<-str_c("Sim ",seq(1,1000))
name<-c("Day",name)

final_mat<-cbind(1:(275*1),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

dim(final_mat) #1008 501

final_mat%>%gather("Simulation","Price",2:1001)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title=" Stock (indice1): 1000 Monte Carlo Simulations for 4 Years")+theme_bw()

#is it likely? Check the confidence interval



final_mat[84,-1]%>%as.numeric()%>%quantile(probs=probs, na.rm=TRUE)
summary(final_mat)
