library(here)
source(here("Códigos/MBGeometrico.R"))

#Estimadores máximo verosímiles para Movimiento Browniano Geométrico

MLE_mbg <- function(T_,X){
  n = length(T_)-1
  T = T_[length(T_)]
  dt = T/n
  Y = log(X[2:(n+1)]/X[1:n])
  s2 = (1/(n*dt))*sum((Y-mean(Y))^2)
  mu = mean(Y)/dt + s2/2
  return(c(mu,sqrt(s2)))
}

