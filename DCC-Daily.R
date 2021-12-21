library(PerformanceAnalytics)
library(quantmod)
library(MASS)
library(mvtnorm)
library(mnormt) 
library(fBasics)
library(readxl)
library(tidyverse)
library(ggplot2)
library(tseries)
library(zoo)
library(xts)
library(rugarch)
library(rmgarch)
library(stats)
library("lmtest")
library("FinTS")
library(forecast)
library(car)
library(stringr)

setwd("E:/Crypto/Data/daily")

liste <- c("AAVE","ADA","BAT","BNB","BTC","DASH","DOT","EOS","ETH","LINK","LTC","MATIC","NEO","ONE","QTUM","SOL","TRX","UNI","VET","XLM","XRP")

for (i in 1:length(liste)){
  command <- str_c(liste[i]," <- read.csv('",liste[i],"USDT_d.csv',skip=1)")
  eval(parse(text = command))
  command <- str_c(liste[i],"[,2] <- as.Date(",liste[i],"[,2])")
  eval(parse(text = command))
}

#Plot log

dataPlot <- AAVE[,c(2,3,7)]

for (i in 2:length(liste)){
  command <- str_c("dataPlot <- rbind(dataPlot,",liste[i],"[,c(2,3,7)])")
  eval(parse(text = command))
}

ggplot(dataPlot) + geom_line(aes(x=date,y=log(close),color=symbol)) + ylab("Prix en Log")
#Possibilite de decouper la data pour avoir un graphique plus propre avec moins de courbes

#Calcul des rendements pour chaque crypto
for (i in 1:length(liste)){
  command <- str_c(liste[i],"2 <- ",liste[i],"[-1,]")
  eval(parse(text = command))
  command <- str_c(liste[i],"2$Rd <- diff(log(",liste[i],"$close))")
  eval(parse(text = command))
}

#stat descriptive
for (i in 1:length(liste)){
  command <- str_c("basicStats(",liste[i],"2[,c(7,11)])")
  print(liste[i])
  print(eval(parse(text = command)))
}

#ADF Test
for (i in 1:length(liste)){
  command <- str_c("adf.test(",liste[i],"2$Rd)")
  print(liste[i])
  print(eval(parse(text = command)))
}

#Jarque Bera Test
for (i in 1:length(liste)){
  command <- str_c("jarque.bera.test(",liste[i],"2$Rd)")
  print(liste[i])
  print(eval(parse(text = command)))
}

#Ljung Test Box
for (i in 1:length(liste)){
  command <- str_c("Box.test(",liste[i],"2$Rd, type='Ljung')")
  print(liste[i])
  print(eval(parse(text = command)))
}
for (i in 1:length(liste)){
  command <- str_c("Box.test(",liste[i],"2$Rd^2, type='Ljung')")
  print(liste[i])
  print(eval(parse(text = command)))
}

#Plot de la volatilite
for (i in 1:length(liste)){
  command <- str_c("ggplot(",liste[i],"2) + geom_line(aes(x=date,y=Rd^2)) + ylab('Volatilite de ",liste[i],"')")
  print(eval(parse(text = command)))
}

#Modele ARIMA
for (i in 1:length(liste)){
  command <- str_c("auto.arima(",liste[i],"2$Rd, seasonal = F)")
  print(eval(parse(text = command)))
}
#AAVE : (2,0,2)
#ADA : (1,1,0)
#BAT : (1,0,1)
#BNB : (2,0,2)
#BTC : (0,0,2)
#DASH : (2,0,1)
#DOT : (0,0,0)
#EOS : (0,0,2)
#ETH : (0,0,2)
#


#Modele GARCH
specG_AAVE <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                      c(1,1)), 
                            mean.model = list(armaOrder = c(2,2), 
                                              external.regressors = NULL), 
                            distribution.model = "norm")
GArch <- ugarchfit(spec=specG_AAVE, data = AAVE2$Rd)
GArch

1 - (sum(GArch@fit$residuals^2) / sum((AAVE2$Rd-mean(AAVE2$Rd))^2))

specG_BTC <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                 c(1,1)), 
                         mean.model = list(armaOrder = c(0,2), 
                                           external.regressors = NULL), 
                         distribution.model = "norm")
GArch <- ugarchfit(spec=specG_BTC, data = BTC2$Rd)
GArch

1 - (sum(GArch@fit$residuals^2) / sum((BTC2$Rd-mean(BTC2$Rd))^2))

specG_ETH <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                c(1,1)), 
                        mean.model = list(armaOrder = c(0,2), 
                                          external.regressors = NULL), 
                        distribution.model = "norm")
GArch <- ugarchfit(spec=specG_ETH, data = ETH2$Rd)
GArch

1 - (sum(GArch@fit$residuals^2) / sum((ETH2$Rd-mean(ETH2$Rd))^2))

Rd_Data <- data.frame(cbind(ETH2$Rd,BTC2$Rd))
Rd_Data <- data.frame(cbind(Rd_Data,ETH2$date))
colnames(Rd_Data) <- c("ETH","BTC","Date")
Rd_Data$Date <- as.Date(Rd_Data$Date)
Rd_Data <- xts(Rd_Data,order.by = Rd_Data$Date)
summary(Rd_Data)
Rd_Data$ETH <- as.numeric(Rd_Data$ETH)
Rd_Data$BTC <- as.numeric(Rd_Data$BTC)

uspec.n <- multispec(list(specG_ETH,specG_BTC))
multf <- multifit(uspec.n,Rd_Data[,c(1,2)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Data[,c(1,2)],fit.control=list(eval.se = TRUE), fit=multf)
fit1

cor <- rcor(fit1)
cor

plot(as.xts(cor[1,2,]))
