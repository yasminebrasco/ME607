###Código para série de tecidos, calçados e vestuário

setwd("~/Documentos/Estatística/2S2016/ME607 - Séries Temporais/Trabalho")
tvc=read.table("serie_tvc.txt",header=T)

# Série Temporal
serie_tvs <- ts(tvc$TVC,start=2000,frequency=12)
summary(serie_tvs)

#Uma diferenciação
par(mfrow=c(3,2))
plot(serie_tvs,main="Volume de Vendas",xlab="Ano",ylab="Vendas")
serie=diff(serie_tvs, lag=1)
plot.ts(serie)

#Duas diferenciações
#par(mfrow=c(1,2))
plot(serie_tvs,main="Volume de Vendas",xlab="Ano",ylab="Vendas")
serie=diff(serie_tvs, lag=2)
plot.ts(serie)

#Três diferenciações
#par(mfrow=c(1,2))
plot(serie_tvs,main="Volume de Vendas",xlab="Ano",ylab="Vendas")
serie=diff(serie_tvs, lag=3)
plot.ts(serie)