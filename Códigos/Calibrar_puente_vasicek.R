library(here)
source(here("Códigos/Puente Difusion.R"))

#Usando el Lemma 3.1 de Bladt 2014 se calibra el puente de Vasicek con el puente exacto
Xi <- function(theta,sigma,x0,T,N){
  dt = T/N
  X <- numeric(N+1)
  X[1] = x0
  var = (sigma^2)*(1-exp(-2*theta*dt))/(2*theta)
  for (i in 1:N){
    W <- rnorm(1,0,sqrt(var))
    X[i+1] <- X[i]*exp(-theta*dt) + W 
  }
  return(X)
}

Zi <- function(theta,sigma,x0,x,T,N){
  X <- Xi(theta, sigma, x0, T, N)
  Z <- numeric(N+1)
  Z[1] <- x0
  xN <- tail(X,1)
  T_ = seq(0,T,length.out=N+1)
  for (i in 1:(N+1)){
    Z[i] <- X[i] + (x-xN)*(exp(theta*T_[i])-exp(-theta*T_[i]))/(exp(theta*tail(T_,1))-exp(-theta*tail(T_,1)))
  }
  return(Z)
}

T = 1
N = 100
theta = -0.1
sigma = 0.5
x = 0
x0 = 0


M = 25000
sim_aproximado <- numeric(M)
sim_exacto <- numeric(M)
for (i in 1:M){
  sim_aproximado[i] <- puente_difusion(1,x0,x,T,N,theta,0,sigma)[N/2+1]
  sim_exacto[i] <- Zi(theta,sigma,x0,x,T,N)[N/2+1]
}

T_ = seq(0,T,length.out=N+1)

plot(T_,puente_difusion(1,x0,x,T,N,theta,0,sigma),type = "l",col = "#5DC0B5",
     ylab="Xt",xlab="T",main="Puentes de Vasicek: Aproximación",lwd=2,ylim=c(-0.75,0.75))
lines(T_,puente_difusion(1,x0,x,T,N,theta,0,sigma),type = "l",col = "#FF8811",lwd=2)
lines(T_,puente_difusion(1,x0,x,T,N,theta,0,sigma),type = "l",col = "#EE6881",lwd=2)
lines(T_,puente_difusion(1,x0,x,T,N,theta,0,sigma),type = "l",col = "#7CBF5F",lwd=2)
abline(h=0, col = "#436AB1",lwd = 2.5,type="l",lty=5)

plot(T_,Zi(theta,sigma,x0,x,T,N),type = "l",col = "#5DC0B5",
     ylab="Xt",xlab="T",main="Puentes de Vasicek: Exacto",lwd=2,ylim=c(-0.75,0.75))
lines(T_,Zi(theta,sigma,x0,x,T,N),type = "l",col = "#FF8811",lwd=2)
lines(T_,Zi(theta,sigma,x0,x,T,N),type = "l",col = "#EE6881",lwd=2)
lines(T_,Zi(theta,sigma,x0,x,T,N),type = "l",col = "#7CBF5F",lwd=2)
abline(h=0, col = "#436AB1",lwd = 2.5,type="l",lty=5)

qqplot(sim_exacto, sim_aproximado, 
       main="Gráfico Q-Q puente Vasicek",
       xlab="Puente Exacto",
       ylab="Puente Aproximado", col = "#7CBF5F")
abline(0,1, lwd = 2.5,col="#436AB1")

