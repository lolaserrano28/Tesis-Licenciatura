library(here)
source(here("Códigos/MLEVasicek.R"))
source(here("Códigos/Vasicek.R"))

#Esimadores EM para proceso de Vasicek

EM_Vasicek <- function(Xt,theta_0,N,n,T,tam_puente){
  A <- c()
  B <- c()
  Si <- c()
  theta_k = theta_0
  
  T_ <- seq(0,T,length.out = N)
  t <- seq(0,T,length.out = n)
  
  for(k in 1:1000){
    A <- c(A,theta_k[1])
    B <- c(B,theta_k[2])
    Si <- c(Si,theta_k[3])
    
    a_k <- theta_k[1]
    b_k <- theta_k[2]
    si_k <- theta_k[3]
    
    Z <- numeric(N)
    for(i in 1:(n-1)){
      puente <- puente_difusion(1,Xt[i],Xt[i+1],t[i+1]-t[i],tam_puente,a_k,b_k,si_k)
      Z[(tam_puente*i-(tam_puente-1)):(tam_puente*i)] = puente[1:(tam_puente)]
    }
    z1 = tam_puente*(n-1)+1
    Z[z1:N]=X[z1:N]
    theta_k <- MLE_vasicek(T_, Z)
  }
  data <- data.frame(A=A,B=B,Si=Si)
  a <- mean(A[901:1000])
  b <- mean(B[901:1000])
  s <- mean(Si[901:1000])
  
  #return(data) #Output = Dataframe de estimadores obtenidos en cada iteración
  
  return(c(a,b,s)) #Output = Vector con los estimadores finales, obtenidos promediando las últimas 100 iteraciones
}


