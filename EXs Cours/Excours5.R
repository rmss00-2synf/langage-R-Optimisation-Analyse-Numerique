#LE SCHEMA EXPLICITE D'EULEUR
library(kableExtra)
#Calcul de la fonction f

f = function(t,y1,y2){
  return(c(-3*y1+y2,-2*y2))
}

#Calcul de la fonction reelle

f_exa = function(t){
  return(c(exp(-3*t)+exp(-2*t),exp(-2*t)))
}

#Euleur

f_app = function(y01,y02,t,h){
  y = matrix(numeric(2*length(t)),2,length(t))
  y[1,1] = y01
  y[2,1] = y02
  for (i in 2:length(t)) {
    y[1,i] = y[1,i-1]+h*f(t[i-1],y[1,i-1],y[2,i-1])[1]
    y[2,i] = y[2,i-1]+h*f(t[i-1],y[1,i-1],y[2,i-1])[2]
  }
  return(y)
}

#Des conditions initiales

h = 0.01
Tn = 1
y01 = 2
y02 = 1
t=seq(0,Tn,by=h)

f_appl = f_app(y01,y02,t,h)
f_exal = mapply(f_exa,t = t)
result1 = data.frame(temps = t, sol_app1=f_appl[1,], sol_app2=f_appl[2,], sol_exa1=f_exal[1,], sol_exa2=f_exal[2,])
#dev.new()
View(result1)

##################################################################
