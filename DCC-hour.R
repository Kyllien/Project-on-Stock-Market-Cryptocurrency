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

setwd("E:/Crypto/Data/hours")

liste <- c("AAVE","ADA","BAT","BNB","BTC","BTT","CELR","CVC","DASH","DOT","EOS","ETC","ETH","FIL","ICP","ICX","LINK","LTC","MATIC","MKR","NEO","ONE","PAX","QTUM","SOL","TRX","UNI","VET","XLM","XMR","XRP","ZEC")

#Mise en place des datas et du format POSiXct a partir de UNIX
UNIX <- function(x){
  if(x>10000000000){
    x <- x/1000
  }
  else{
    x <- x
  }
}

for (i in 1:length(liste)){
  command <- str_c(liste[i]," <- read.csv('",liste[i],"USDT_1h.csv',skip=1)")
  eval(parse(text = command))
  #command <- str_c(liste[i],"[,2] <- as.Date(",liste[i],"[,2])")
  #eval(parse(text = command))
  command <- str_c("dates <- sapply(",liste[i],"[,1],UNIX)")
  eval(parse(text = command))
  class(dates) = c('POSIXt','POSIXct')
  dates <- as.data.frame.POSIXct(dates)
  command <- str_c(liste[i],"[,11] <- dates")
  eval(parse(text = command))
}

#Enlever les doublons de date et remis en ordre par la date
for (i in 1:length(liste)){
  command <- str_c(liste[i],"2 <- distinct(",liste[i],"[,c(3,7,11)])")
  eval(parse(text=command))
  command <- str_c(liste[i],"2 <- arrange(",liste[i],"2,",liste[i],"2$dates)")
  eval(parse(text=command))
}


#Mis en place de la dataPlot pour plot en log les closes
for (i in 1:length(liste)){
  if(i == 1){
    command <- str_c("dataPlot <- ",liste[i],"2")
    eval(parse(text = command))
  }
  else{
    command <- str_c("dataPlot <- rbind(dataPlot,",liste[i],"2)")
    eval(parse(text = command))
  }
}


ggplot(dataPlot) + geom_line(aes(x=dates,y=log(close),color=symbol)) + ylab("Prix en Log")
#Possibilite de decouper la data pour avoir un graphique plus propre avec moins de courbes

#Calcul des rendements pour chaque crypto
for (i in 1:length(liste)){
  command <- str_c(liste[i],"3 <- ",liste[i],"2[-1,]")
  eval(parse(text = command))
  command <- str_c(liste[i],"3$Rd <- diff(log(",liste[i],"2$close))")
  eval(parse(text = command))
}

#stat descriptive
for (i in 1:length(liste)){
  command <- str_c("basicStats(",liste[i],"3[,c(2,4)])")
  print(liste[i])
  print(eval(parse(text = command)))
}

#ADF Test
for (i in 1:length(liste)){
  command <- str_c("adf.test(",liste[i],"3$Rd)")
  print(liste[i])
  print(eval(parse(text = command)))
}

#Jarque Bera Test
for (i in 1:length(liste)){
  command <- str_c("jarque.bera.test(",liste[i],"3$Rd)")
  print(liste[i])
  print(eval(parse(text = command)))
}

#Ljung Test Box
for (i in 1:length(liste)){
  command <- str_c("Box.test(",liste[i],"3$Rd, type='Ljung')")
  print(liste[i])
  print(eval(parse(text = command)))
}
for (i in 1:length(liste)){
  command <- str_c("Box.test(",liste[i],"3$Rd^2, type='Ljung')")
  print(liste[i])
  print(eval(parse(text = command)))
}

#Plot de la volatilite
for (i in 1:length(liste)){
  command <- str_c("ggplot(",liste[i],"3) + geom_line(aes(x=dates,y=Rd^2)) + ylab('Volatilite de ",liste[i],"')")
  print(eval(parse(text = command)))
}

#Mis en place des data en fonction dunb de ligne de chacune afin de teste la max de data

for(i in 1:length(liste)){
  if(i==1){
    command <- str_c("long <- c(nrow(",liste[i],"3))")
    eval(parse(text=command))
  }
  else{
    compteur <- 0
    for(j in 1:length(long)){
      command <- str_c("if(nrow(",liste[i],"3)==",long[j],"){compteur <- compteur + 1}")
      eval(parse(text=command))
    }
    if(compteur != 1){
      command <- str_c("long <- append(long,nrow(",liste[i],"3))")
      eval(parse(text=command))
    }
  }
}
#Mise en place des datas pour chaque Rd et chaque taille de base de donnée differentes
#Ajout dans un premier temps de toute les bases ayant le meme nombre de ligne
for(j in 1:length(long)){
  compteur <- 1
  for(i in 1:length(liste)){
    command <- str_c("if(nrow(",liste[i],"3)==",long[j],"){
                     if(compteur==1){
                      dataRd",long[j]," <- ",liste[i],"3[,c(3,4)]
                     }
                     else{
                      dataRd",long[j]," <- cbind(dataRd",long[j],",",liste[i],"3[,4])
                     }
                     compteur <- compteur + 1
                     names(dataRd",long[j],")[",compteur+1,"]<- '",liste[i],"'
    }")
    eval(parse(text = command))
  }
}
#Ajout dans un deuxieme temps de toute les bases qui ont > de ligne par rapport aux autres
for(j in 1:length(long)){
  for(i in 1:length(liste)){
    command <- str_c("if(nrow(",liste[i],"3)>",long[j],"){
                     dataRd",long[j]," <- left_join(dataRd",long[j],",",liste[i],"3[,c(3,4)],by='dates')
                     names(dataRd",long[j],")[ncol(dataRd",long[j],")] <- '",liste[i],"'
    }")
    eval(parse(text=command))
  }
}
#Cette partie de code peut etre ameliore en creant directment les dataRd dans la premiere boucle 
#cad de mettre direct le premier de la liste qui a ce nombre de ligne ds la data et dans la deuxieme partie mettre le left join avec
#la condition >= ce qui permet d'enlever la 3eme etape (a faire plus tard)


#ARIMA
ARIMA <- function(i,dt){
  command <- str_c(names)
  
}
for (j in 1:length(long)){
  command <- str_c("for(i in 2:ncol(dataRd",long[j],")){
    command2 <- str_c('fitArima')
    fitArima",long[j],"names(data)
  }
                    ")
  command <- str_c("fitArima",liste[i]," <- auto.arima(",liste[i],"3$Rd, seasonal = F)")
  eval(parse(text = command))
}
#Coeftest
for (i in 1:length(liste)){
  command <- str_c("coeftest(fitArima",liste[i],")")
  print(liste[i])
  print(eval(parse(text = command)))
}

#BoxTest
for (i in 1:length(liste)){
  command <- str_c("Box.test(fitArima",liste[i],"$residuals)")
  print(liste[i])
  print(eval(parse(text = command)))
  command <- str_c("Box.test((fitArima",liste[i],"$residuals)^2)")
  print(eval(parse(text = command)))
}



#GARCH
F1_GARCH <- function(x,arma1,arma2,garch1,garch2){
  spec_G <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = 
                                               c(garch1,garch2)), 
                       mean.model = list(armaOrder = c(arma1,arma2), 
                                         external.regressors = NULL), 
                       distribution.model = 'norm')
  GArch <- ugarchfit(spec=spec_G, data = x)
  return(GArch)
}
F2_GARCH <- function(x,arma1,arm2){
  compt<-0
  garch1 <- 1
  garch2 <- 1 
  while(compt!=1){
    if()
  }
  return(z)
}
z <- F2_GARCH(dataRd1839$AAVE)

for (i in 1:length(liste)){
  command <- str_c("specG_",liste[i]," <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = 
                                           c(1,1)), 
                   mean.model = list(armaOrder = c(fitArima",liste[i],"$arma[1],fitArima",liste[i],"$arma[2]), 
                                     external.regressors = NULL), 
                   distribution.model = 'norm') ")
  eval(parse(text = command))
  
  command <- str_c("GArch_",liste[i]," <- ugarchfit(spec=specG_",liste[i],", data = ",liste[i],"3$Rd)")
  eval(parse(text = command))
}


#Les modeles garch crée ne sont que des (1,1), donc ce sont pas forcement les meilleurs modeles, 
#il va falloir mettre en place une fonction qui crée le meilleur garch possible pour chaque serie
#car par exemple un probleme est rencontre avec la serie ETH a cause de la mise en place au niveau de la moyenne (ARMA : (1,3))
#A FAIRE : fonction de chaque garch avec les tests d autocorrelation des residus normalise au carré et des arch effect
#test effectue dans une des boucles qui suit

for (i in 1:length(liste)){
  command <- str_c("specG_",liste[i]," <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = 
                                           c(1,1)), 
                   mean.model = list(armaOrder = c(fitArima",liste[i],"$arma[1],fitArima",liste[i],"$arma[2]), 
                                     external.regressors = NULL), 
                   distribution.model = 'norm') ")
  eval(parse(text = command))
  
  command <- str_c("GArch_",liste[i]," <- ugarchfit(spec=specG_",liste[i],", data = ",liste[i],"3$Rd)")
  eval(parse(text = command))
}
specG_ETH <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                c(1,2)), 
                        mean.model = list(armaOrder = c(0,2), 
                                          external.regressors = NULL), 
                        distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_ETH, data = ETH3$Rd)

#R2 Garch
for (i in 1:length(liste)){
  command <- str_c("1 - (sum(GArch_",liste[i],"@fit$residuals^2) / sum((",liste[i],"3$Rd-mean(",liste[i],"3$Rd))^2))")
  print(liste[i])
  print(eval(parse(text = command)))
}

#Test GARCH
for (i in 1:length(liste)){
  command <- str_c("AutocorTest(residuals(GArch_",liste[i],",standardize=T))")
  print(liste[i])
  print(eval(parse(text = command)))
  command <- str_c("AutocorTest(residuals(GArch_",liste[i],",standardize=T)^2)")
  print(eval(parse(text = command)))
  command <- str_c("ArchTest(residuals(GArch_",liste[i],",standardize=T)^2)")
  print(eval(parse(text = command)))
}

#Xts

#Mise en place du xts pour chaque dataRd
for (j in (1:length(long))){
  command <- str_c("dataRd",long[j],"<- xts(dataRd",long[j],",order.by=dataRd",long[j],"$dates)")
  eval(parse(text=command))
}
#Changement des data et non analyse des dataRd 35041 et 35071 et 6685 pour bug dans la creation des DCC
long <- c(1839,2345,2700,9105,32215)

 #Mise en place des DCC-GARCH
for (j in (1:length(long))){
  command <- str_c("nom <- names(dataRd",long[j],")[2]")
  eval(parse(text = command))
  command <- str_c("title <- str_c('specG_",nom,"')")
  eval(parse(text = command))
  command <- str_c("if(ncol(dataRd",long[j],">2)){for (i in (3:ncol(dataRd",long[j],"))){
  title <- str_c(title,',specG_',names(dataRd",long[j],")[i])
  }
  }")
  eval(parse(text=command))
  print(title)
  command <- str_c("uspec",long[j]," <- multispec(list(",title,"))")
  eval(parse(text=command))
}

for (j in (1:length(long))){
  command <- str_c("multi <- multifit(uspec",long[j],",dataRd",long[j],"[,c(2:ncol(dataRd",long[j],"))])
                   spec <- dccspec(uspec=uspec",long[j],",dccOrder = c(1,1), distribution = 'mvnorm')
                   fit",long[j]," <- dccfit(spec,data=dataRd",long[j],"[,c(2:ncol(dataRd",long[j],"))],fit.control=list(eval.se = TRUE), fit=multi)")
  eval(parse(text=command))
}

#Test de verification des modeles
#Autocorrelation des residus
for(j in (1:length(long))){
  print(str_c("Data ",long[j], " : "))
  command <- str_c("nameAutoCor",long[j]," <- c()
  for (i in (1:ncol(fit",long[j],"@mfit$stdresid))){
        test <- AutocorTest(fit",long[j],"@mfit$stdresid[,i]^2,lag=log(nrow(dataRd",long[j],")))
        if(test$p.value <= 0.05){
          nameAutoCor",long[j]," <- append(nameAutoCor",long[j],", (i))
          print(names(dataRd",long[j],"[,(i+1)]))
          print(AutocorTest(fit",long[j],"@mfit$stdresid[,i]^2,lag=log(nrow(dataRd",long[j],"))))
        }
  }")
  eval(parse(text = command))
}

#ArchTest sur chaque relation
for(j in (1:length(long))){
  command <- str_c("RelationDCC",long[j]," <- c('1','2')
  for( i in (1:(length(nameAutoCor",long[j],")-1))){
                   for( k in ((i+1):length(nameAutoCor",long[j],"))){
                   test <- ArchTest(fit",long[j],"@mfit$stdresid[,c(nameAutoCor",long[j],"[i],nameAutoCor",long[j],"[k])],lag=log(nrow(dataRd",long[j],")))
                   if(test$p.value > 0.05){
                   print(test)
                   }
                   }
  }")
  eval(parse(text =  command))
}
# f <- nameAutoCor",long[j],"[i]
# test <- ArchTest(fit",long[j],"@mfit$stdresid[,c(nameAutoCor",long[j],"[i],nameAutoCor",long[j],"[k])],lag=log(nrow(dataRd",long[j],")))
# if(test$p.value > 0.05){
#   print(str_c(names(dataRd",long[j],")[i+1],' x ',names(dataRd",long[j],")[k+1],' : '))
#   print(test)
z <- c()
Z <- as.data.frame(c("1","2"))
Z <- cbind(Z,c("3","4"))
nameAutoCor1839[3]

#DCC",long[j]," <- append(DCC",long[j],",)

#Il faut faire pour avoir les correlations dynamiques de chaque relation ce qui va suivre pour chaque
#cela n'a pas ete automatisé et il faut le faire seulement pour celle qui ont été print par les boucles precedentes
cor <- rcor(fit9105) #Rajouter le numero du fit a partir des daats etablis precedement
#fit le dcc fit etablis dans la boucle precedentes -> pour chaque data differente et apres plot la correlation de chaque systeme
#de correlation entre les relations non fallacieuses
plot(as.xts(cor[19,10,]))


