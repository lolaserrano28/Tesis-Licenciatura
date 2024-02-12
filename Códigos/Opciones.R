library(here)
library(latex2exp)
source(here("Códigos/MBGeometrico.R"))
source(here("Códigos/MLEmbg.R"))
source(here("Códigos/EM_mbg.R"))



#Fórmula de Black-Scholes
omega <- function(St,r,sigma,K,T_,T){
  w <- ((r+sigma^2/2)*(T-T_)+log(St/K))/(sigma*sqrt(T-T_))
  return(w)
}

posicion_corta <- function(K,A0,r,w,T_,T){
  return(-K*A0*exp(-r*(T-T_))*pnorm(w-sigma*sqrt(T-T_)))
}

posicion_larga <- function(St,r,sigma,K,T_,T){
  w <-omega(St,r,sigma,K,T_,T)
  return(St*pnorm(w))
}

C <- function(St,r,sigma,K,T_,T){
  w <-  omega(St,r,sigma,K,T_,T)
  c <- St*pnorm(w) - K*exp(-r*(T-T_))*pnorm(w-sigma*sqrt(T-T_))
  N <- length(St)
  if(c[N]<0){
    c[N]=0
  }
  return(c)
}

#-----------------------------------------------------------------------
#EJEMPLOS Y GRAFICAS
#Comparación S0 y K
A0 = 1
r = 0.15

S0 = 1
mu = 0.15
sigma = 0.05

K = 5
T = 10
N = 1000

precio = C(MBG(mu,sigma,S0,T,N),r,sigma,K,T_,T)
plot(T_, precio, main = "Precio Black-Scholes: K = 5",
     xlab = "t", ylab = "C(t,St)", type = "l",lwd = 2, col = "#5DC0B5",ylim = c(0,0.6))
for(i in 1:20){
  St <- MBG(mu,sigma,S0,T,N)
  precio = C(St,r,sigma,K,T_,T)
  if(precio[1001]>0){
    lines(T_, precio, type = "l",lwd = 2, col = "#5DC0B5")
    cont = cont + 1
  } else{
    lines(T_, precio, type = "l",lwd = 2, col = "#FF8811")
  }
}

legend("topleft" ,legend = c(TeX(sprintf(r'($S_T > K$)')),TeX(sprintf(r'($S_T \leq K$)'))), 
       bty = "n",lwd = c(3,3), cex =1.2,col = c("#5DC0B5","#FF8811"))




#-----------------------------------------------------------------------
#ESTIMACIÓN PARAMÉTRICA MLE
#Lectura datos
voo <- read.csv(here("Bases de datos","VOO.csv"))
length(voo$Close)

X <- voo$Close
N <- length(X)-1
T = 1
T_ = seq(0,T,length.out=N+1)

summary(X)
sd(X)
var(X)
max(X)-min(X)

#Estimadores Máximo Verosímiles
MLE_mbg(T_,X)
mu_mle <- MLE_mbg(T_,X)[1]
sigma_mle <- MLE_mbg(T_,X)[2]

#-----------------------------------------------------------------------
#ESTIMACIÓN PARAMÉTRICA EM
source(here("Códigos/Puente Difusion.R"))

#Muestra de datos incompletos usada para cálculo de estimadores EM
n = 50
Xt <- c(X[seq(1, N+1, N/n)])
t <- seq(0,1,length.out = n+1)

#Usamos MLE de datos incompletos como parámetro inicial
theta_0 <- MLE_mbg(t,Xt)

#Llamada al algoritmo
parametros_EM <- EM_mbg(Xt,theta_0,N,n,T)
mu_em <- parametros_EM[1]
sigma_em <- parametros_EM[2]

#-----------------------------------------------------------------------
#SIMULACIÓN CON PARÁMETROS OBTENIDOS


#Trayectorias precio S&P 500

#Parámetros MLE
sim1 <- MBG(mu_mle,sigma_mle,X[1],1,N)
sim2 <- MBG(mu_mle,sigma_mle,X[1],1,N)
sim3 <- MBG(mu_mle,sigma_mle,X[1],1,N)
sim4 <- MBG(mu_mle,sigma_mle,X[1],1,N)


mini <- min(X, sim1,sim2,sim3,sim4)
maxi <- max(X, sim1,sim2,sim3,sim4)

plot(T_, X, type = "l", col = "#436AB1",lwd = 2.5, 
     main = "Simulaciones Estimador Máximo Verosímil",xlab = "T",ylab = "$",
     ylim = c(mini, maxi),xaxt="n")
axis(1, at = c(0:11)/12,
     labels = c("Ene.","Feb.","Mar.","Abr.","May.","Jun.",
                "Jul.","Ago.","Sep.","Oct.","Nov.","Dic"),srt=45)

lines(T_,sim1, type = "l", col = "#FF8811",lwd = 1.5,lty = 2) 
lines(T_,sim2, type = "l", col = "#7CBF5F",lwd = 1.5,lty = 2)
lines(T_,sim3, type = "l", col = "#5DC0B5",lwd = 1.5,lty = 2)
lines(T_,sim4, type = "l", col = "#EE6881",lwd = 1.5,lty = 2)

legend(0, maxi, legend=c("Sim. 1", "Sim. 2", "Sim. 3","Sim. 4"),
       col=c("#FF8811","#EE6881","#5DC0B5","#7CBF5F"), lty=2, cex=1,bty = "n",lwd=2)

legend(0.2, maxi, legend = "Precios S&P500",
       col="#436AB1", lty=1, cex=1,bty = "n",lwd=2.5)


#Parámetros EM
sim1 <- MBG(mu_em,sigma_em,Xt[1],1,n)
sim2 <- MBG(mu_em,sigma_em,Xt[1],1,n)
sim3 <- MBG(mu_em,sigma_em,Xt[1],1,n)
sim4 <- MBG(mu_em,sigma_em,Xt[1],1,n)


mini <- min(Xt, sim1,sim2,sim3,sim4)
maxi <- max(Xt, sim1,sim2,sim3,sim4)

plot(t, Xt, type = "l", col = "#436AB1",lwd = 2.5, 
     main = "Simulaciones Estimador EM",xlab = "T",ylab = "$",
     ylim = c(mini, maxi),xaxt="n")
axis(1, at = c(0:11)/12,
     labels = c("Ene.","Feb.","Mar.","Abr.","May.","Jun.",
                "Jul.","Ago.","Sep.","Oct.","Nov.","Dic"),srt=45)

lines(t,sim1, type = "l", col = "#FF8811",lwd = 1.5,lty = 2) 
lines(t,sim2, type = "l", col = "#7CBF5F",lwd = 1.5,lty = 2)
lines(t,sim3, type = "l", col = "#5DC0B5",lwd = 1.5,lty = 2)
lines(t,sim4, type = "l", col = "#EE6881",lwd = 1.5,lty = 2)

legend(0, maxi, legend=c("Sim. 1", "Sim. 2", "Sim. 3","Sim. 4"),
       col=c("#FF8811","#EE6881","#5DC0B5","#7CBF5F"), lty=2, cex=1,bty = "n",lwd=2)

legend(0.2, maxi, legend = "Precios S&P500",
       col="#436AB1", lty=1, cex=1,bty = "n",lwd=2.5)



#Precio de una opción

K = 300
r = 0.1
par(mfrow=c(2,1))
St <- MBG(mu_mle, sigma_mle, X[1], T, N)
Ct <- C(St,r,sigma_mle,K,T_,T)
plot(T_, St, type = "l", col = "#436AB1",lwd = 2,xlab = "t",ylab = "St" ,
     main = "Simulaciones precio de activo y opción",ylim = c(min(St),max(max(St),K)))
abline(h=K, col = "#FF8811")
legend("topleft",legend = c("K=300","Sim. activo subyacente"), col=c("#FF8811","#436AB1"),lty=2, cex=1,bty = "n",lwd=2)
plot(T_, Ct,type = "l", col = "#5DC0B5", lwd = 2,xlab = "t",ylab = "C(t,St)")
legend("topleft",legend = "Precio opción", col="#5DC0B5",lty=2, cex=1,bty = "n",lwd=2)




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


muestras_validación_MLE <- data.frame(matrix(nrow=0,ncol=N))
p_val_MLE <- c()
Muestra_MLE <- rep(0,N)
for(i in 1:1000){
  muestra_i <- rnorm(N,0,sqrt(1/N))
  p_val_MLE <- c(p_val_MLE,ks.test(residuales_mle_mbg,muestra_i)$p.value)
  muestras_validación_MLE[nrow(muestras_validación_MLE) + 1,] <- muestra_i
  Muestra_MLE <- Muestra_MLE + muestra_i
}
muestras_validación_MLE$pvalue <- p_val_MLE



qqplot(residuales_mle_mbg,Muestra_MLE/sqrt(1000),
       main = "Gráfico QQ Estimador MLE",
       xlab = "Residuales",
       ylab = "Muestra Normal",
         col = "#EE6881",ylim = c(-0.3,0.3),xlim = c(-0.3,0.3))
abline(0,1, lwd = 2.5,col="#436AB1")
mean(p_val_MLE)

muestras_validación_EM <- data.frame(matrix(nrow=0,ncol=n))
p_val_EM <- c()
Muestra_EM <- rep(0,n)
for(i in 1:1000){
  muestra_i <- rnorm(n,0,sqrt(1/n))
  p_val_EM <- c(p_val_EM,ks.test(residuales_em_mbg,muestra_i)$p.value)
  muestras_validación_EM[nrow(muestras_validación_EM) + 1,] <- muestra_i
  Muestra_EM <- Muestra_EM + muestra_i
}

muestras_validación_EM$pvalue <- p_val_EM


qqplot(residuales_em_mbg,Muestra_EM/sqrt(1000),
       main = "Gráfico QQ Estimador EM",
       xlab = "Residuales",
       ylab = "Muestra Normal",
       col = "#FF8811",
       xlim = c(-0.4,0.4),ylim = c(-0.4,0.4),
       pch = 16)
abline(0,1, lwd = 2.5,col="#436AB1")
mean(p_val_EM)
