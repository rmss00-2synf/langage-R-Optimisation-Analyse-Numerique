#LE SCHEMA EXPLICITE D'EULEUR
library(kableExtra)

##############2-) Calcul de l'erreur e4
#Calcul de la fonction f

Derive_seconde = function(t,y){
  return((-1-4*t+4*t)*y)
}

calcul_pas =  function(t0,y0,theta,Tol.Tn,hmax,hmin){
  Tt = t0 + Tn
  t = t0
  i=1
  h0=h=sqrt(2*Tol)
  Eln1 = h0^2/2*Derive_seconde(t0,y0)
  while (t[i]<Tt) {
    
  if(Tol-theta<= abs(Eln1) && abs(Eln1)<=Tol+theta){
    h = min(h,Tt - t[i])
  }
    else if(abs(Eln1)>Tol+theta){
      h = min(max(h,hmax))
    }
    else{
      h = h*sqrt(abs(Tol/Eln1))
      h = min(h,Tt - t[1])
    }
    t = c(t,t[i]+h)
    Eln1 = h^2/2*Derive_seconde(t[i],y0)
    i =i +1
    
  }
}

f = function(t,y){
  return((1-2*t)*y)
}

#Calcul de la fonction reelle

f_exa = function(t){
  return(exp(0.25-(t-0.5)^2))
}

# Pour TS1
f_app1 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  for (i in 2:length(t)) {
    y[i] = y[i-1]+h*f(t[i-1],y[i-1])
  }
  return(y)
}

# Pour TS2
f_app2 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  for (i in 2:length(t)) {
    y[i] = y[i-1]+h*(f(t[i-1],y[i-1])+h/2*(-1-4*t[i-1]+4*t[i-1]^2)*y[i-1])
  }
  return(y)
}

#Des conditions initiales

h = 0.30
Tn = 1.2
y0 = 1
t=seq(0,Tn,by=h)

f_appl1 = f_app1(y0,t,h)
f_appl2 = f_app2(y0,t,h)
f_exal = mapply(f_exa,t = t)
erro1 = abs(f_exal-f_appl1)
erro2 = abs(f_exal-f_appl2)
result1 = data.frame(temps = t, sol_app1=f_appl1, sol_app2=f_appl2, sol_exa1=f_exal, erro1 = erro1, erro2 = erro2)
#dev.new()
View(result1)

plot(t,f_exal,type = 'l',lwd=3,ylim = c(0.5, 1.5))
points(t,f_appl1,type='l',col='red',lwd=2)

points(t,f_appl2,type='l',col='green',lwd=2)

legend("topleft",legend = c("Courbe exacte","Courbe TS(1)","Courbe TS(2)"), col = c("black","red","green"),lty = 1, cex = 0.8)
##################################################################
