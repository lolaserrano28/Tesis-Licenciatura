library(here)
source(here("Códigos/Vasicek.R"))

#Estimadores máximo verosímiles para proceso de Vasicek

MLE_a <- function(T_,X){
  n = length(T_)-1
  T = T_[length(T_)]
  dt = T/n
  suma1 = n*sum(X[1:n]*X[2:(n+1)])
  suma2 = sum(X[1:n])*sum(X[2:(n+1)])
  suma3 = n*sum(X[1:n]^2)
  suma4 = sum(X[1:n])^2
  a = (-1/dt)*log(abs((suma1-suma2))/(suma3-suma4))
  return(a)
}

MLE_b <- function(T_,X,a){
  n = length(T_)-1
  T = T_[length(T_)]
  dt = T/n
  expo = exp(-a*dt)
  suma1 = sum(X[2:(n+1)])
  suma2 = sum(X[1:n])
  b = (1/(n*(1-expo)))*(suma1-expo*suma2)
  return(b)
}

MLE_sigma <- function(T_,X,a,b){
  n = length(T_)-1
  T = T_[length(T_)]
  dt = T/n
  expo = exp(-a*dt)
  expo2 = exp(-2*a*dt)
  suma = sum((X[2:(n+1)]-X[1:n]*expo - b*(1-expo))^2)
  sigma = sqrt((2*a)/(n*(1-expo2))*suma)
  return(sigma)
}

MLE_vasicek <- function(T_,X){
  a = MLE_a(T_,X)
  b = MLE_b(T_, X, a)
  sigma = MLE_sigma(T_, X, a, b)
  return(c(a,b,sigma))
}

