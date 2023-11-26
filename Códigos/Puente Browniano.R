library(here)
source(here("Códigos/MovimientoBrowniano.R"))


#Función que simula un (0,a,T,b)-puente browniano
puente_browniano <- function(a,b,T,N){
  dt = T/N
  X = numeric(N+1)
  X[1] = a
  T_ = seq(0,T,length.out=N+1)
  for(i in 1:N){
    Z = rnorm(1,0,1)
    X[i+1] = X[i] + ((b-X[i])/(T-T_[i]))*dt + sqrt(dt)*Z
  }
  return(X)
}

#Ejemplo

#T=1
#N=1000
#T_ = seq(0,T,length.out=N+1)
#plot(T_,puente_browniano(0,2,T,N), main = "Puente Browniano",
#    xlab = "t",ylab = "Xt",type = "l",lwd = 2, col = "#5DC0B5",ylim = c(-0.5,2.5))
#lines(T_, puente_browniano(0,2,T,N),lwd = 2, col = "#FF8811", type = "l")
#lines(T_, puente_browniano(0,2,T,N),lwd = 2, col = "#EE6881", type = "l")
#lines(T_, puente_browniano(0,2,T,N),lwd = 2, col = "#7CBF5F", type = "l")
#abline(h = 0, col = "#436AB1",lwd = 2.5,type="l",lty=5)
#abline(h = 2, col = "#436AB1",lwd = 2.5,type="l",lty = 5)


#Puente Browniano: Representación Adelantada
puente_browniano2 <- function(T,N){
  T_ = seq(0,T,length.out=N+1)
  mb = MBE(T,N)
  PB <- mb-(T_/T)*mb[N+1]
  return(PB)
}

#Ejemplo

#plot(T_,puente_browniano2(T,N), main = "Puente Browniano: Representación Adelantada",
#     xlab = "t",ylab = "Xt",type = "l",lwd = 2, col = "#5DC0B5",ylim = c(-0.02,0.02))
#lines(T_, puente_browniano2(T,N),lwd = 2, col = "#FF8811", type = "l")
#lines(T_, puente_browniano2(T,N),lwd = 2, col = "#EE6881", type = "l")
#lines(T_, puente_browniano2(T,N),lwd = 2, col = "#7CBF5F", type = "l")
#abline(h = 0, col = "#436AB1",lwd = 2.5,type="l",lty=5)


  