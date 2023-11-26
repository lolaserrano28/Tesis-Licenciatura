library(here)
library(latex2exp)
source(here("Códigos/MBGeometrico.R"))
source(here("Códigos/MLEmbg.R"))
source(here("Códigos/EM_mbg.R"))


#Fórmula de Black&Scholes
omega <- function(t,s,r,sigma,K,T){
  w <- ((r+sigma^2/2)*(T-t)+log(K/s))/(sigma*sqrt(T-t))
  return(w)
}

C <- function(t,s,r,sigma,K,T){
  w <- omega(t,s,r,sigma,K,T)
  c <- s*pnorm(w) - K*exp(-r*(T-t))*pnorm(w-sigma*sqrt(T-t))
  return(c)
}

#Trayectoria de precios opción
C_trayectoria <- function(T,s0,r,sigma,K,N){
  C_ <- rep(N+1)
  mu = r- sigma^2/2
  St <- MBG(mu,sigma,s0,T,N)
  T_ = seq(0,T,length.out=N+1)
  for(i in 1:(N+1)){
    t <- T_[i]
    s <- St[i]
    C_[i] <- C(t,s,r,sigma,K,T)
  }
  return(C_)
}

act_riesgoso <- function(t,s,r,sigma,K,T){
  w <- omega(t,s,r,sigma,K,T)
  return(s*pnorm(w))
}

act_libre <- function(t,s,r,sigma,K,T){
  w <- omega(t,s,r,sigma,K,T)
  return(- K*exp(-r*(T-t))*pnorm(w-sigma*sqrt(T-t)))
  
}

#Ejemplos y gráficas

#Comparación K < S0, K=30, S0=60
sigma = 0.1
r = 0.1
K = 30
s0 = 60
mu = r- sigma^2/2
T = 10
N = 1000
T_ = seq(0,T,length.out=N+1)

precio1 = C_trayectoria(T,s0,r,sigma,K,N)
precio2 = C_trayectoria(T,s0,r,sigma,K,N)
precio3 = C_trayectoria(T,s0,r,sigma,K,N)
precio4 = C_trayectoria(T,s0,r,sigma,K,N)
precio5 = C_trayectoria(T,s0,r,sigma,K,N)

mini <- min(precio1,precio2,precio3,precio4, precio5)
maxi <- max(precio1,precio2,precio3,precio4, precio5)

plot(T_,precio1, main = "Precio Black-Scholes: K = 30, S0 = 60", xlab = "t", ylab ="g(t,St)",
     type = "l",lwd = 2, col = "#5DC0B5",ylim = c(mini,maxi))
lines(T_, precio2, type = "l",lwd = 2, col = "#FF8811" )
lines(T_, precio3, type = "l",lwd = 2, col = "#EE6881" )
lines(T_, precio4, type = "l",lwd = 2, col = "#7CBF5F" )
lines(T_, precio5, type = "l",lwd = 2, col = "#436AB1" )

#Comparación K > S0, K=60, S0=30
K = 60
s0 = 30

precio1 = C_trayectoria(T,s0,r,sigma,K,N)
precio2 = C_trayectoria(T,s0,r,sigma,K,N)
precio3 = C_trayectoria(T,s0,r,sigma,K,N)
precio4 = C_trayectoria(T,s0,r,sigma,K,N)
precio5 = C_trayectoria(T,s0,r,sigma,K,N)

mini <- min(precio1,precio2,precio3,precio4, precio5)
maxi <- max(precio1,precio2,precio3,precio4, precio5)

plot(T_,precio1, main = "Precio Black-Scholes: K = 60, S0 = 30", xlab = "t", ylab ="g(t,St)",
     type = "l",lwd = 2, col = "#5DC0B5",ylim = c(mini,maxi))
lines(T_, precio2, type = "l",lwd = 2, col = "#FF8811" )
lines(T_, precio3, type = "l",lwd = 2, col = "#EE6881" )
lines(T_, precio4, type = "l",lwd = 2, col = "#7CBF5F" )
lines(T_, precio5, type = "l",lwd = 2, col = "#436AB1" )

#-----------------------------------------------------------------------
#ESTIMACIÓN PARAMÉTRICA MLE
#Lectura datos
voo <- read.csv(here("Bases de datos","VOO.csv"))
length(voo$Close)

X <- voo$Close
N <- length(X)
T_ = seq(0,1,length.out=N)

#Estimadores Máximo Verosímiles
MLE_mbg(T_,X)

#-----------------------------------------------------------------------
#ESTIMACIÓN PARAMÉTRICA EM
source(here("Códigos/Puente Difusion.R"))

#Muestra de datos incompletos usada para cálculo de estimadores EM
tam_puente = 5
Xt <- c(X[seq(1, N, tam_puente)])
n = length(Xt)
t <- seq(0,1,length.out = n)

#Usamos MLE de datos incompletos como parámetro inicial
theta_0 <- MLE_mbg(t,Xt)
parametros_EM <- EM_mbg(Xt,theta_0,N,n,1,tam_puente)
mu_em <- parametros_EM[1]
sigma_em <- parametros_EM[2]

#----------------------------------------------------------------------------
#Validación Modelo

#Calculo residuales

residuales_mle_mbg <- c()
residuales_em_mbg <- c()
for(i in 2:N){
  res <- (1/sigma)*log(X[i]/X[i-1])-(mu-sigma^2/2)*(T_[i]-T_[i-1])
  residuales_mle_mbg <- c(residuales_mle_mbg,res)
}

for(i in 2:n){
  res2 <- (1/sigma_em)*log(Xt[i]/Xt[i-1])-(mu_em-sigma_em^2/2)*(t[i]-t[i-1])
  residuales_em_mbg <- c(residuales_em_mbg,res2)
}

#muestra1_mbg <- rnorm(N,0,sqrt(1/251))
qqplot(residuales_mle_mbg,muestra1_mbg,
       main = "Gráfico QQ Estimador MLE",
       xlab = "Residuales",
       ylab = "Muestra Normal",
       col = "#5DC0B5")

abline(0,1, lwd = 2.5,col="#436AB1")
ks.test(residuales_mle_mbg,muestra1_mbg)
set.seed(muestra1_mbg)

#muestra2_mbg <- rnorm(n,0,sqrt(1/49))
qqplot(residuales_em_mbg,muestra2_mbg,
       main = "Gráfico QQ Estimador EM",
       xlab = "Residuales",
       ylab = "Muestra Normal",
       col = "#7CBF5F")
abline(0,1, lwd = 2.5,col="#436AB1")
ks.test(residuales_em_mbg,muestra2_mbg)
set.seed(muestra2_mbg)


