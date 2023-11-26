library(here)
library(matlib)
library(latex2exp)
source(here("Códigos/MBGeometrico.R"))
source(here("Códigos/Puente Difusion.R"))
source(here("Códigos/MLEmbg.R"))

#Estimadores EM para movimiento browniano geométrico
EM_mbg <- function(Xt,theta_0,N,n,T,tam_puente){
  Mu <- c()
  Si <- c()
  theta_k = theta_0
  
  T_ <- seq(0,T,length.out = N)
  t <- seq(0,T,length.out = n)
  
  for(k in 1:1000){
    Mu <- c(Mu,theta_k[1])
    Si <- c(Si,theta_k[2])
    mu_k <- theta_k[1]
    si_k <- theta_k[2]
    Z <- numeric(N)
    for(i in 1:(n-1)){
      puente <- puente_difusion(2,Xt[i],Xt[i+1],t[i+1]-t[i],tam_puente,mu_k,si_k)
      Z[(tam_puente*i-(tam_puente-1)):(tam_puente*i)] = puente[1:(tam_puente)]
    }
    z1 = tam_puente*(n-1)+1
    Z[z1:N]=X[z1:N]
    theta_k <- MLE_mbg(T_, Z)
  }
  data <- data.frame(Mu=Mu,Si=Si)
  m <- mean(Mu[901:1000])
  s <- mean(Si[901:1000])
  
  #return(data) #Output = Dataframe de estimadores obtenidos en cada iteración
  
  return(c(m,s)) #Output = Vector con los estimadores finales, obtenidos promediando las últimas 100 iteraciones
}
