#LE SCHEMA EXPLICITE D'EULEUR
library(kableExtra)

##############1-) Calcult des Ti(t,y)
#T1(t,y) = (1-2*t)y
#T2(t,y) = T1(t,y) + h/2(-1-4t+4t²)y
#T3(t,y) = T2(t,y) + h²/6(1-2t)(-5-4t+4t²)y

##############2-) Calcul de l'erreur e4
#Calcul de la fonction f

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
