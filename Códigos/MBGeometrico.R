library("latex2exp")


#Movimiento Browniano Geométrico: Milstein
MBG_milstein <- function(mu,sigma,X0,T,N){
  dt = T/N
  X <- numeric(N+1)
  X[1] = X0
  for(i in 1:N){
    Z = rnorm(1,0,1)
    X[i+1] = X[i] + mu*X[i]*dt + sigma*X[i]*sqrt(dt)*Z + 0.5*sigma*X[i]*dt*(Z^2-1)
  }
  return(X)
}


#Movimiento Browniano Geométrico: Lamperti
MBG <- function(mu,sigma,X0,T,N){
  dt = T/N
  X <- numeric(N+1)
  X[1] = X0
  for(i in 1:N){
    Z = rnorm(1,0,1)
    ln_xi = log(X[i]) + (mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*Z
    X[i+1] = exp(ln_xi)
  }
  return(X)
}

#Ejemplo 

#T = 1
#N = 1000
#T_ = seq(0,T,length.out=N+1)


#plot(T_, MBG(0,0.1,1,T,N), type = "l", 
#     main = "Movimiento Browniano Geométrico: Milstein", 
#     xlab = "T", ylab = "M.B. Geométrico", col = "#436AB1",lwd=2, ylim = c(0,3))  
#lines(T_, MBG(0,0.5,1,T,N),type = "l",lwd = 2, col = "#EE6881")
#lines(T_, MBG(0,1.2,1,T,N),type = "l",lwd = 2, col = "#7CBF5F")

#legend(0, 3, legend=c(TeX(sprintf(r'($\sigma = 0.1$)')), TeX(sprintf(r'($\sigma = 0.5$)')),TeX(sprintf(r'($\sigma = 1.2$)'))),
#       col=c("#436AB1", "#EE6881", "#7CBF5F"), lty=1, cex=1,bty = "n")


