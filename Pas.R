#Les fonction pour trouver les pas

f = function(t,y){
  return(t^2+y)
}
f_exa = function(t){
  return(2*exp(t)-(t+1)^2-1)
}

k1 = function(t,y,h){
  return(f(t,y))
}

k2 = function(t,y,h){
  return(f(t+h/2,y+h/2*k1(t,y)))
}

k3 = function(t,y,h){
  return(f(t+2*h/3,y+2*h/3*k2(t,y,h)))
}

k4 = function(t,y,h){
  return(f(t+h,y+h*k3(t,y,h)))
}

f_appRK2 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  for (i in 2:length(t)) {
    y[i] = y[i-1]+h[i-1]*k2(t[i-1],y[i-1],h[i-1])
  }
  return(y)
}

f_appRK3 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  if(length(t)>1)
  for (i in 2:length(t)) {
    y[i] = y[i-1]+h/4*(k1(t[i-1],y[i-1],h)+3*k3(t[i-1],y[i-1],h))
  }
  return(y)
}


Pas = function(tol,hmax,Tn,t0,y0,epsi){
  t = c(t0)
  h = c(tol)
  i = 1
  y=c(y0)
  Tp = Tn + t0
  #Tp = 20
  Eli1 = h[i]/3*(k1(t[i],y[i],h[i])+k2(t[i],y[i],h[i])-2*k3(t[i],y[i],h[i]))
  #print(Eli1)
  hnew = h[i]*abs(tol/Eli1)^(1/3)
  while(t[i]<Tp){
    #t = c(t,t[i]+h[i])
    #cat(h[2],t[2],'\n')
    Eli1 = h[i]/3*(k1(t[i],y[i],h[i])+k2(t[i],y[i],h[i])-2*k3(t[i],y[i],h[i]))
    hnew = h[i]*abs(tol/Eli1)^(1/3)
    #cat(Eli1)
    if((1-epsi)*tol <= abs(Eli1) & abs(Eli1)<=(1+epsi)*tol){
      h = c(h,min(h[i],Tp - t[i])) 
      t = c(t,t[i]+h[i+1])
      y[i+1] = y[i] + h[i]/2*k2(t[i],y[i],h[i])
      i =i+1
    }
    else if(abs(Eli1)<(1-epsi)*tol){ 
      h = c(h,min(max(hnew,hmax),Tp-t[i]))
      t = c(t,t[i]+h[i+1])
      y[i+1] = y[i] + h[i]/2*k2(t[i],y[i],h[i])
      i =i+1
    }
    else if((1+epsi)*tol < abs(Eli1) & abs(Eli1)<=(1+2*epsi)*tol){
      h = c(h,min(hnew,Tp-t[i]))
      t = c(t,t[i]+h[i+1])
      y[i+1] = y[i] + h[i]/2*k2(t[i],y[i],h[i])
      i =i+1
    }
    else {
      hnew = h[i]*abs(tol/Eli1)^(1/3)
      h[i] = hnew
    }
    #cat(i,':','h',h[i],'t',t[i],'y',y[i],'Eln1',Eli1,'\n')
  }
  #cat(i,':','h',h[i],'t',t[i],'y',y[i],'Eln1',Eli1,'\n')
  return(list(t,y,h))
  
}
tol = seq(0.00001,0.01,length=5)
hmax = 0.015
Tn = 1
t0 = 0
y0 = 0
epsi = 0.01*tol

resultats = mapply(Pas,tol = tol,hmax=hmax, Tn = Tn, t0 = t0, y0 = y0,epsi=epsi, SIMPLIFY = FALSE)
solex = mapply(f_exa,t =resultats[1] , SIMPLIFY = FALSE)
f_appl2 = mapply(f_appRK2,y0=y0,t =resultats[1,] ,h = resultats[3,], SIMPLIFY = FALSE)

erreur = function(fex, fapp){
  max(abs(fex-fapp))
}

erreurG = mapply(erreur, fex=solex,fap = f_appl2)
# resutats[[1]][[1]] = Pas(tol,hmax,Tn,t0,y0,epsi)[[1]]
# resutats[[1]][[2]] = Pas(tol,hmax,Tn,t0,y0,epsi)[[2]]
# resutats[[1]][[3]] = Pas(tol,hmax,Tn,t0,y0,epsi)[[3]]
# for (j in 1:5) {
#   tol = tol/10
#   resutats = list(resutats,)
#   for(i in 1:3){
#     resutats[[j+1]][[i]] =Pas(tol,hmax,Tn,t0,y0,epsi)[[i]]
#     
#   }
# }
# solex = mapply(f_exa,t =resutat[[1]] )
# f_appl2 = f_appRK2(y0,resutat[[1]],resutat[[3]])
# resutat = Pas(tol,hmax,Tn,t0,y0,epsi)
# err = abs(solex-f_appl2)
# plot(resutat[[1]],solex,type = 'l',col='green')
# points(resutat[[1]],f_appl2,type = 'l',lwd=3)
# print(max(err))
#print(resutat)
