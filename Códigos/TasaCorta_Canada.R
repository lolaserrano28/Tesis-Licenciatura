library(here)
library(latex2exp)
source(here("Códigos/Vasicek.R"))

#Funciones A(t,T) y B(t,T) para valuación bono
B <- function(t,T,a,b,s){
  return((1-exp(-2*a*(T-t)))/(2*a))
}

A <- function(t,T,a,b,s){
  s1 <- (s^2/(2*a)-b)*((T-t)-B(t,T,a,b,s))
  s2 <- s^2*B(t,T,a,b,s)^2/(4*a)
  return(exp(s1-s2))
}

#Precio bono al tiempo t
P <- function(t,T,rt,a,b,s){
  A(t,T,a,b,s)*exp(-rt*B(t,T,a,b,s))
}

#Trayectoria de precios
P_trayectoria <- function(T,a,b,s,r0,N){
  #vector donde vamos a ir guardando los precios
  P_ <- rep(N+1)
  #Generamos la trayectoria Vasicek 
  rt <- vasicek(a,b,s,r0,T,N)
  #Vector con los tiempos
  T_ = seq(0,T,length.out=N+1)
  for(i in 1:(N+1)){
    t <- T_[i]
    r <- rt[i]
    P_[i] <- A(t,T,a,b,s)*exp(-r*B(t,T,a,b,s))
  }
  return(P_)
}


P_determinista <- function(r0,T,N){
  T_ = seq(0,T,length.out=N+1)
  return(exp(-r0*(T-T_)))
  
}

#Ejemplos y gráficas

#media a largo plazo b = 1%
a = 0.5
b = 1
s = 1
r0 = 5
T = 1
N = 1000
T_ = seq(0,T,length.out=N+1)

plot(T_,P_trayectoria(T,a,b,s,r0,N), main = "Trayectoria Precios: b = 1%",
     xlab = "t",ylab = "P(t,T)",type = "l",col = "#FF8811",lwd=2,ylim = c(0,1))

lines(T_,P_determinista(r0,T,N),type = "l", col = "#7CBF5F",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#EE6881",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#5DC0B5",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#436AB1",lwd=2)

legend(0.2, 1, legend = TeX(sprintf(r'($e^{-r_0(T-t)}$)')),
       col="#7CBF5F", lty=1, cex=1,bty = "n",lwd=2)

legend(0, 1, legend=c(TeX(sprintf(r'($P(t,T)$)')),"", "",""),
       col=c("#FF8811","#EE6881","#5DC0B5","#436AB1"), lty=1, cex=1,bty = "n",lwd=2)


#media a largo plazo b = 5%
b = 5
plot(T_,P_trayectoria(T,a,b,s,r0,N), main = "Trayectoria Precios: b = 5%",
     xlab = "t",ylab = "P(t,T)",type = "l",col = "#FF8811",lwd=2,ylim = c(0,1))

lines(T_,P_determinista(r0,T,N),type = "l", col = "#7CBF5F",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#EE6881",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#5DC0B5",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#436AB1",lwd=2)

legend(0.2, 1, legend = TeX(sprintf(r'($e^{-r_0(T-t)}$)')),
       col="#7CBF5F", lty=1, cex=1,bty = "n",lwd=2)

legend(0, 1, legend=c(TeX(sprintf(r'($P(t,T)$)')),"", "",""),
       col=c("#FF8811","#EE6881","#5DC0B5","#436AB1"), lty=1, cex=1,bty = "n",lwd=2)



#media a largo plazo b = 10%
b = 10
plot(T_,P_trayectoria(T,a,b,s,r0,N), main = "Trayectoria Precios: b = 10%",
     xlab = "t",ylab = "P(t,T)",type = "l",col = "#FF8811",lwd=2,ylim = c(0,1))

lines(T_,P_determinista(r0,T,N),type = "l", col = "#7CBF5F",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#EE6881",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#5DC0B5",lwd=2)
lines(T_,P_trayectoria(T,a,b,s,r0,N),type = "l", col = "#436AB1",lwd=2)

legend(0.2, 1, legend = TeX(sprintf(r'($e^{-r_0(T-t)}$)')),
       col="#7CBF5F", lty=1, cex=1,bty = "n",lwd=2)

legend(0, 1, legend=c(TeX(sprintf(r'($P(t,T)$)')),"", "",""),
       col=c("#FF8811","#EE6881","#5DC0B5","#436AB1"), lty=1, cex=1,bty = "n",lwd=2)


#------------------------------------------------------------------------
#Estimación MLE
source(here("Códigos/MLEVasicek.R"))

#Lectura datos
tbill <- read.csv(here("Bases de datos","Tasas_Canada.csv"))


X <- tbill$ZC100YR
X <- as.numeric(X[!X==" na"])*100
N <- length(X)
T_ = seq(0,1,length.out=N)

#Estimadores Máximo Verosímiles
MLE_vasicek(T_,X)


#------------------------------------------------------------------------
#Estimación EM
source(here("Códigos/EM_Vasicek.R"))
source(here("Códigos/Puente Difusion.R"))
N = 249
Xt <- c(X[seq(1, 249, 5)])
n = 50
t <- seq(0,1,length.out = n)


theta_0 <- MLE_vasicek(t,Xt)
EM_vas <- EM_Vasicek(Xt,theta_0,N,n,T,tam_puente)

#------------------------------------------------------------------------------
#Proyección precios
integral <- function(vec,x1,x2){
  Ni <- length(vec)-1
  I <- (sum(vec[2:Ni])*2+vec[1]+vec[Ni+1])*(x2-x1)/(2*Ni)
  return(I)
}

P_real <- c()

for(i in 1:(N-1)){
  vec = X[i:N]
  ex <- integral(vec,T_[i],1)
  precio <- exp(-ex)
  P_real <- c(P_real, precio)
  
}
P_real[N]=1
plot(T_, P_real, type = "l", col = "#436AB1",lwd = 2.5, 
     main = "Precio Bono",xlab = "T",ylab="$",xaxt="n")
axis(1, at = c(0:11)/12,
     labels = c("Ene.","Feb.","Mar.","Abr.","May.","Jun.",
                "Jul.","Ago.","Sep.","Oct.","Nov.","Dic"),srt=45)

a <- MLE_vasicek(T_,X)[1]
b <- MLE_vasicek(T_,X)[2]
s <- MLE_vasicek(T_,X)[3]

a_EM <- EM_vas[1]
b_EM <- EM_vas[2]
s_EM <- EM_vas[3]


lines(T_,P_trayectoria(1,a,b,s,X[1],N-1),type = "l", col ="#FF8811",lwd=3,lty=2)
lines(T_,P_trayectoria(1,a_EM,b_EM,s_EM,X[1],N-1),type = "l", col = "#7CBF5F",lwd=3,lty=2)

legend(0, 1, legend=c("Precio Real","Precio Simulado MLE","Precio Simulado EM"),
       col=c("#436AB1","#FF8811","#7CBF5F"), lty=c(1,2,2), cex=1,bty = "n",lwd=2)


#-----------------------------------------------------------------------------------------------------
#Validación Modelo

#Calculo de los residuales
residuales_mle_vas <- c()
residuales_em_vas <- c()

for(i in 2:N){
  res <- 1/s*(X[i]-X[i-1]-a*(b-X[i])*(T_[i]-T_[i-1]))
  residuales_mle_vas <- c(residuales_mle_vas,res)
}

for(i in 2:n){
  res2 <- 1/s_EM*(Xt[i]-Xt[i-1]-a_EM*(b_EM-Xt[i])*(t[i]-t[i-1]))
  residuales_em_vas <- c(residuales_em_vas,res2)
}

eff_seed <- sample(1:2^15, 1)
print(sprintf("Seed for session: %s", eff_seed))


#muestra1 <- rnorm(N,0,sqrt(1/N))

qqplot(residuales_mle_vas,muestra1,
       main = "Gráfico QQ Estimador MLE",
       xlab = "Residuales",
       ylab = "Muestra Normal",
       col = "#EE6881")
abline(0,1, lwd = 2.5,col="#436AB1")
ks.test(residuales_mle_vas,muestra1)
set.seed(muestra1)

#muestra2 <- rnorm(n,0,sqrt(1/49))
qqplot(residuales_em_vas,muestra2,
       main = "Gráfico QQ Estimador EM",
       xlab = "Residuales",
       ylab = "Muestra Normal",
       col = "#FF8811",
       pch = 16)
abline(0,1, lwd = 2.5,col="#436AB1")
ks.test(residuales_em_vas,muestra2)

set.seed(muestra2)


