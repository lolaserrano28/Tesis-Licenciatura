#Movimiento Browniano Estandar
MBE <- function(T,N){
  l <- c(0,rnorm(N,0,T/N))
  mb <- cumsum(l)
  return(mb)
}


#Movimiento Browniano con deriva = mu, difusion = sigma y condición inicial = x
MB <- function(mu,sigma,x,T,N){
  l <- c(x,rnorm(N,mu*(T/N),sigma^2*(T/N)))
  mb <- cumsum(l)
  return(mb)
}

#Ejemplos 

#T = 1
#N = 1000
#T_ = seq(0,T,length.out=N+1)

#plot(T_,MBE(T,N),type="l",main = "Movimiento Browniano",xlab = "T",ylab = "Mov. Browniano",lwd = 2, col = "#FF8811",ylim = c(-0.08,0.08))
#lines(T_,MBE(T,N),type="l",lwd = 2, col = "#5DC0B5")
#lines(T_,MBE(T,N),type="l",lwd = 2, col = "#436AB1")

#plot(T_,MB(1,1.5,0,T,N),type="l",main = "Movimiento Browniano: mu = 1, sigma = 1.5",xlab = "T",ylab = "Mov. Browniano",lwd = 2, col = "#FF8811")
#lines(T_,MB(1,1.5,0,T,N),type="l",lwd = 2, col = "#5DC0B5")
#lines(T_,MB(1,1.5,0,T,N),type="l",lwd = 2, col = "#436AB1")

