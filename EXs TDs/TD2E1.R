#LE SCHEMA EXPLICITE D'EULEUR
library(kableExtra)
# y(t) = 2exp(t)-(t+1)²-1
#Calcul de la fonction f

f = function(t,y){
  return(t^2+y)
}


#

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

#Calcul de la fonction reelle

f_exa = function(t){
  return(2*exp(t)-(t+1)^2-1)
}


#Des conditions initiales

h = 0.1
Tn = 1
y0 = 0
t=seq(0,Tn,by=h)

##################################################################
#LE SCHEMA RK2

f_appRK2 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  for (i in 2:length(t)) {
    y[i] = y[i-1]+h*k2(t[i-1],y[i-1],h)
  }
  return(y)
}

##################################################################
#LE SCHEMA TS2

f_appTS2 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  for (i in 2:length(t)) {
    y[i] = y[i-1]+h*(f(t[i-1],y[i-1])+h/2*(-1-4*t[i-1]+4*t[i-1]^2)*y[i-1])
  }
  return(y)
}
##################################################################
#LE SCHEMA RK3

f_appRK3 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  if(length(t)>1)
    for (i in 2:length(t)) {
      y[i] = y[i-1]+h/4*(k1(t[i-1],y[i-1],h)+3*k3(t[i-1],y[i-1],h))
    }
  return(y)
}
##################################################################
#LE SCHEMA RK4

f_appRK4 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  for (i in 2:length(t)) {
    y[i] = y[i-1]+h/6*(k1(t[i-1],y[i-1],h)+2*k2(t[i-1],y[i-1],h)+2*k3(t[i-1],y[i-1],h)+k4(t[i-1],y[i-1],h))
  }
  return(y)
}

f_appl = f_appTS2(y0,t,h)
f_exal = mapply(f_exa,t = t)
err_g= abs(f_exal - f_appl)/h
result1 = data.frame(temps = t, sol_appTS2=f_appl, sol_exa=f_exal, err_gh=err_g)
#dev.new()
plot(t,f_exal,type = 'l',lwd=3)
points(t,f_appl,type='l',col='red',lwd=2)
f_appl2 = f_appRK2(y0,t,h)
f_appl3 = f_appRK3(y0,t,h)
f_appl4 = f_appRK4(y0,t,h)
err_g2= abs(f_exal - f_appl2)/h
err_g3= abs(f_exal - f_appl3)/h
err_g4= abs(f_exal - f_appl4)/h
result1 = data.frame(result1,f_applRK2=f_appl2,err_g2=err_g2,f_applRK3=f_appl3,err_g3=err_g3,f_applRK4=f_appl4,err_g4=err_g4)
points(t,f_appl2,type='l',col='green',lwd=2)
points(t,f_appl3,type='l',col='blue',lwd=2)
points(t,f_appl4,type='l',col='yellow',lwd=2)

col_names=c("ti","y_i_explicite","y(t_i)","Eg_i1/h",'y_i_implicite',"Eg_i2/h","RK2","Eg_i3/h","RK4","Eg_i4/h")  # ("Pas_i","sum El_i","max Eg_i","Ordre")
resultat_kable <- kable(result1,caption = 'Resultats numeriques obtenus pour un pas h = 0.5 sur [0,4] par les sch\'emas d\'Euler', "html", col.names = NULL,digits=4)%>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))%>%
  add_header_above(col_names)%>%
  column_spec(1, bold = TRUE)%>%
  row_spec(1:nrow(result1), align = "c")
print(resultat_kable)
legend("topright",legend = c("Courbe exacte","Courbe TS2","Courbe RK2","Courbe RK3","Courbe RK4"), col = c("black","red","green","yellow","blue"),lty = 1, cex = 0.5)
View(result1)