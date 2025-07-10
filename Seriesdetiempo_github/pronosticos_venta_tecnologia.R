# Cargar bibliotecas necesarias
library(TSA) #proporciona herramientas y funciones para el an�lisis de series temporales en R.
library(readr)#proporciona funciones eficientes para leer datos en formato rectangular, como CSV o archivos de texto
library(forecast)#ofrece funciones para el an�lisis y pron�stico de series temporales en R. 
# Cargar el archivo Excel
data <- read_excel("C:/Users/juanjoseagudelo/OneDrive/Seriesdetiempo_github/ventas_tecnologia.xlsx")
# Crear una serie de tiempo mensual
ventas_ts <- ts(data$valor_transaccion, start = c(2020, 9), end = c(2022, 8), frequency = 12)
# Visualizar la serie de tiempo y sus funciones de autocorrelaci�n
plot(ventas_ts)  
par(mfrow=c(1,2))
acf(ventas_ts,lag=48)
pacf(ventas_ts,lag=48)

# Funci�n para buscar la mejor combinaci�n de par�metros para SARIMA
sarima.list = function(datos, p, d = 0, q, P = 0, D = 0, Q = 0, S = NA, include.mean = F, criterio = ""){
  M <- matrix(ncol = 10,nrow = (p+1)*(q+1)*(P+1)*(Q+1),dimnames=list(NULL,c("p","d","q","P","D","Q","S","converge","AIC", "BIC")))
  k <- 1
  n <- length(datos)
  for(i in 0:p){
    for(j in 0:q){
      for(l in 0:P){
        for(m in 0:Q){
          if ((i==0)&&(j==0)&&(l==0)&&(m==0)) next #Continua con la siguiente iteracion
          fit <- arima(datos, order = c(i, d, j),seasonal = list(order = c(l, D, m), period = S), include.mean = include.mean)
          M[k,1] <- i
          M[k,2] <- d
          M[k,3] <- j
          M[k,4] <- l
          M[k,5] <- D
          M[k,6] <- m
          M[k,7] <- S
          M[k,8] <- fit$code # 0: Convergencia, 1: No Convergencia
          M[k,9] <- AIC(fit) # AIC
          M[k,10] <- AIC(fit, k = log(length(datos))) # BIC
          k <- k+1
        }
      }
    }
  }
  if(criterio == "AIC"){
    M <- M[order(M[,9]),]
  }
  if(criterio == "BIC"){ M <- M[order(M[,10]),]
  }
  if(criterio == ""){
    M <- M
  }
  rownames(M) = rep("", (p+1)*(q+1)*(P+1)*(Q+1))
  return(M[1:((p+1)*(q+1)*(P+1)*(Q+1)-1),])
}

# Ejecutar la funci�n para buscar los mejores par�metros SARIMA
sarima.listarima<-data.frame(sarima.list(ventas_ts, p=1, d = 1, q=1, P = 1, D = 1, Q = 1, S = 1, include.mean = F, criterio = ""))
sarima.listarima[with(sarima.listarima, order(abs(AIC), abs(BIC))), ]

# Ajustar un modelo ARIMA a los datos
(mod=arima(ventas_ts, order = c(1, 0, 1),seasonal = list(order = c(0, 0, 0), period = 12), method = c("CSS-ML"))) 


# An�lisis de los residuales del modelo ARIMA ajustado
res=residuals(mod)
# C�digo para la prueba de Ljung-Box
Box.Test = function(x, lag = 48, main = "p values for Ljung-Box statistic"){
  B<-vector("numeric")
  for(i in 1:lag){
    B[i]<-Box.test(x, lag = i, type = "Ljung-Box")$p.value
  }
  A<-matrix(cbind(c(1:lag),B), nrow = lag, ncol = 2, byrow=F, dimnames = list(NULL, c("lag", "p.value")))
  plot(A[,1], A[,2], ylim = c(0, max(0.051,(max(A[,2])+.01))), 
       ylab = "p-value", xlab = "Lag", main = main, lwd = 2)
  abline(0.05, 0, col = 4, lty = 2)
}

# Gr�ficos de los residuales y pruebas estad�sticas
par(mfrow=c(3,2))
plot.ts(res, main="Residuales", ylab="")
hist(res, main="Histograma de Residuales")
acf(res, lag.max=48, main="ACF de Residuales")
pacf(res, lag.max=25, main="PACF de Residuales")
Box.test(res)
shapiro.test(res)
qqnorm(res, main="QQ de los Residuales")
qqline(res, col="red")

# Obtener valores ajustados
ajust=ventas_ts-res

# Gr�fico de la serie original y los valores ajustados
par(mfrow=c(1,1))
ts.plot(ventas_ts, ajust,ylab='ventas')
lines(ventas_ts, col="black")
lines(ajust, col="red")
legend("topright", legend = c("Serie original", "Modelo ajustado"), col = c("black", "red"), lty = 1)

# Realizar predicciones con el modelo ARIMA
predicciones <- forecast(mod, h = 3)




# Graficar la serie de tiempo original y las predicciones
ts.plot(ventas_ts,predicciones$mean,col = c("black", "red"), lty = c(1, 1), 
        main = "Serie original y predicciones",
        xlab = "Fecha", ylab = "Ventas")
legend("topright", legend = c("Serie original", "predicciones"), col = c("black", "red"), lty = 1)

