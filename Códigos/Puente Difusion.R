library(here)
source(here("Códigos/Vasicek.R"))
source(here("Códigos/MBGeometrico.R"))

#La función cruce(x1,x2) indica si hay un cruce en las trayectorias y la posición

#En esta funcion x1[1] = alpha y x2[N] = beta 
#(es decir, la trayectoria x2 ya debe estar a tiempo reverso)
cruce <- function(x1,x2){
  v <- c(x1>x2)
  #Caso donde no hay cruce
  if(sum(v)==0|sum(v)==length(v)){
    bool = FALSE
    nu = 0
  }else{
    #Caso donde x1[1]>x2[1]
    if(v[1]==TRUE){
      nu = which(v==FALSE)[1]
    #Caso donde x1[1]<x2[1]
    }else{
      nu = which(v==TRUE)[1]
    }
    bool = TRUE
  }
  return(c(bool,nu))
}

#Si fun==1 la función devuelve un puente de vasicek
puente_difusion <- function(fun,alpha,beta,T,N,theta1,theta2,theta3=NULL){
  if(fun==1){
    bool = FALSE
    while(bool == FALSE){
      x1 <- vasicek(theta1,theta2,theta3,alpha,T,N)
      x2 <- rev(vasicek(theta1,theta2,theta3,beta,T,N))
      if(cruce(x1,x2)[1]==0){
        bool = FALSE
      }else{
        nu = cruce(x1,x2)[2]
        y1 = x1[0:(nu-1)]
        y2 = x2[nu:(N+1)]
        X = c(y1,y2)
        bool = TRUE
      }
    }
  }
  else if(fun==2){
    bool = FALSE
    while(bool == FALSE){
      x1 <- MBG(theta1,theta2,alpha,T,N)
      x2 <- rev(MBG(theta1,theta2,beta,T,N))
      if(cruce(x1,x2)[1]==0){
        bool = FALSE
      }else{
        nu = cruce(x1,x2)[2]
        y1 = x1[0:(nu-1)]
        y2 = x2[nu:(N+1)]
        X = c(y1,y2)
        bool = TRUE
      }
    }
  }
  return(X)
}



#Ejemplo puente Vasicek

#plot(T_,puente_difusion(1,alpha,beta,T,N,a,b,sigma),type = "l",col = "#FF6699",
#     ylab="X",xlab="T",main="Puentes de Vasicek",ylim = c(0.8,1.2))
#lines(T_,puente_difusion(1,alpha,beta,T,N,a,b,sigma),type = "l",col = "#660099")
#lines(T_,puente_difusion(1,alpha,beta,T,N,a,b,sigma),type = "l",col = "#00CC88")
#lines(T_,puente_difusion(1,alpha,beta,T,N,a,b,sigma),type = "l",col = "#FF9933")
#abline(h=alpha, lwd = 2,lty=2)
#abline(h=beta,lwd=2,lty=2)


#Ejemplo puente MBG

#plot(T_,puente_difusion(2,alpha,beta,T,N,mu,sigma),type = "l",col = "#5DC0B5",
#    ylab="X",xlab="T",main="Puentes Mov. Browniano Geométrico",ylim = c(0.75,1.25),lwd = 2)
#lines(T_,puente_difusion(2,alpha,beta,T,N,mu,sigma),type = "l",col = "#FF8811",lwd=2)
#lines(T_,puente_difusion(2,alpha,beta,T,N,mu,sigma),type = "l",col = "#EE6881",lwd=2)
#lines(T_,puente_difusion(2,alpha,beta,T,N,mu,sigma),type = "l",col = "#7CBF5F",lwd=2)
#abline(h=1, col = "#436AB1",lwd = 2.5,type="l",lty=5)
#abline(h=beta,lwd=2,lty=2)
