getwd()
setwd("H:/ME607/Trabalho")
tvc=read.table("serie_tvc_tabulado.txt",header=T)

# Série Temporal
serie_tvs <- ts(tvc$TVC,start=2000,frequency=12)
plot(serie_tvs,main="Volume de Vendas",xlab="Ano",ylab="%")
summary(serie_tvs)

#Exlcluindo os dois últimos anos para previsão
serie <- serie_tvs[1:120]
serie.ts <- ts(serie,start=2000,frequency=12)
plot(serie.ts,main="Volume de Vendas",xlab="Ano",ylab="%")
acf2(serie,50)

#Estabilizando a variância

##LOG
serieT<-log(serie)
serieT.ts <- ts(serieT,start=2000,frequency=12)
plot(serieT.ts,main="Volume de Vendas - Serie Transformada LOG",xlab="Ano",ylab="%")

## BOX-COX (FOI INEFICIENTE!!!)
# valor 1 está próximo extremo do intervalo de confiança, indicando que pode não ser necessário 
# realizar a transformação
t=seq(1:length(serie))
box=boxcox(serie~t,lambda = seq(0.5, 1.2, 1/10),plotit=T)
maxi=max(box$y)
lamb1=box$x[which(box$y==maxi)]
lamb1 (#0.75...)

serieT2=serie^0.75
serieT2.ts <- ts(serieT2,start=2000,frequency=12) 
plot(serieT2.ts,main="Volume de Vendas - Serie Transformada BOX",xlab="Ano",ylab="%")

par(mfrow=c(1,1))

# Periodograma

pgm <- function(serie){ 
  n <- length(serie)
  m <- n/2-1
  I <- abs(fft(serie)/sqrt(n))^2  # periodogram
  P <- (4/n)*I                    # scaled-periodogram
  f <- 0:m/n
  list(f=f,P=P[1:(m+1)])
}

v = pgm(serie)
v$P
plot(v$f,v$P,type="l",xlab="Frequência",ylab="", main="Periodograma")

plot(v$f[1:60],v$P[1:60],type="l",xlab="Frequência",ylab="")
max(v$P) # estб na posiзгo 16 do vetor v$f

## 1є pico
abline(v=3/140,lty="dotted", col="blue", lwd=2)
140/3
# =46.66667 # a cada 47 meses

#Utilizando a série não transformada
require(astsa)
ajuste.1 = sarima(serieT,0,0,0,1,0,0,12)
ts.plot(serieT,main="Série Original",xlab="Ano",ylab="TVC")
lines(serieT - ajuste.1$fit$residuals[1:120],col="orange",lty=6,lwd=2)
#legend("topleft",legend=c("Série original","Ajuste por SARIMA(0,0,0)(1,0,0)12"),col=c("black","orange"),lwd=c(2,2))


ajuste.2 = sarima(serie,0,0,0,1,0,0,12)
ts.plot(serie,main="Série Original",xlab="Ano",ylab="TVC")
lines(serie - ajuste.2$fit$residuals[1:120],col="orange",lty=6,lwd=2)

ajuste.3 = arima(serieT,order=c(1,0,0))
ts.plot(serieT,main="Série Original",xlab="Ano",ylab="TVC")
lines(serieT - ajuste.3$residuals[1:120],col="orange",lty=6,lwd=2)

ajuste.4 = sarima(serieT,0,0,0,1,1,1,6)
ts.plot(serieT,main="Série Original",xlab="Ano",ylab="TVC")
lines(serieT - ajuste.4$fit$residuals[1:120],col="orange",lty=6,lwd=2)

# POR ENQUANTO O MELHOR#
ajuste.5 = sarima(serie,0,0,0,1,1,1,6)
ts.plot(serie,main="Série Original",xlab="Ano",ylab="TVC")
lines(serie - ajuste.5$fit$residuals[1:120],col="orange",lty=6,lwd=2)

sarima(serie,0,0,0,0,1,1,6) #NAO
sarima(serie,0,0,0,1,1,0,6) #NAO
ajuste.6<-sarima(serie,1,0,1,1,1,0,6) #NAO
ts.plot(serie,main="Série Original",xlab="Ano",ylab="TVC")
lines(serie - ajuste.6$fit$residuals[1:120],col="orange",lty=6,lwd=2)

par(mfrow=c(2,1))

# POR ENQUANTO O MELHOR#
ajuste.7 = sarima(serie,2,1,1,1,1,1,6)
ts.plot(serie,main="Série Original",xlab="Ano",ylab="TVC")
lines(serie - ajuste.7$fit$residuals[1:120],col="orange",lty=6,lwd=2)

auto.arima(serie, seasonal = TRUE) #ARIMA(4,1,2) 

ndiffs(serie)
nsdiffs(serie, m=6)

ajuste.8 = arima(serie,order=c(4,1,2))
ts.plot(serie,main="Série Original",xlab="Ano",ylab="TVC")
lines(serieT - ajuste.8$residuals[1:120],col="orange",lty=6,lwd=2)

ajuste.7 = sarima(serie,4,1,2,0,1,0,6)
ts.plot(serie,main="Série Original",xlab="Ano",ylab="TVC")
lines(serie - ajuste.7$fit$residuals[1:120],col="orange",lty=6,lwd=2)
