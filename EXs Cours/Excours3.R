#LE SCHEMA EXPLICITE D'EULEUR
library(kableExtra)
# y(t) = 2exp(t)-1-t
#Calcul de la fonction f

f = function(t,y){
  return(t+y)
}

#Calcul de la fonction reelle

f_exa = function(t){
  return(2*exp(t)-t-1)
}

#Euleur

f_app = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  for (i in 2:length(t)) {
    y[i] = y[i-1]+h*f(t[i-1],y[i-1])
  }
  return(y)
}

#Des conditions initiales

h = 0.1
Tn = 10
y0 = 1
t=seq(0,Tn,by=h)

f_appl = f_app(y0,t,h)
f_exal = mapply(f_exa,t = t)
err_g= abs(f_exal - f_appl)/h
result1 = data.frame(temps = t, sol_app1=f_appl, sol_exa=f_exal, err_gh=err_g)
#dev.new()
plot(t,f_exal,type = 'l',lwd=3)
points(t,f_appl,type='l',col='red',lwd=2)

##################################################################
#LE SCHEMA IMPLICITE D'EULEUR

#Euleur


f_app1 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  
  for (i in 2:length(t)) {
    y[i] <- uniroot( function(yn1) {yn1 - y[i-1] - h * f(t[i],yn1) }, interval = c(-100000, 100000))$root
  }
  
  return(y)
}

##################################################################
#LE SCHEMA RK2

# f_app2 = function(y0,t,h){
#   y = numeric(length(t))
#   y[1] = y0
#   for (i in 2:length(t)) {
#     k1 = f(t[i-1],y[i-1])
#     k2 = f(t[i-1]+h/2,y[i-1]+h/2*k1)
#     y[i] = y[i-1]+h*k2
#   }
#   return(y)
# }

##################################################################
#LE SCHEMA RK4

f_app4 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  for (i in 2:length(t)) {
    k1 = f(t[i-1],y[i-1])
    k2 = f(t[i-1]+h/2,y[i-1]+h/2*k1)
    k3 = f(t[i-1]+h/2,y[i-1]+h/2*k2)
    k4 = f(t[i-1]+h,y[i-1]+h*k3)
    y[i] = y[i-1]+h/6*(k1+2*k2+2*k3+k4)
  }
  return(y)
}


##################################################################
#LE SCHEMA RK4
f_app2 <- function( y,t, h) {
  n <- length(t)
  y_next <- numeric(n)
  y_next[1] <- y  # Valeur initiale de y
  
  for (i in 1:(n - 1)) {
    k1 <- h * f(t[i], y_next[i])
    k2 <- h * f(t[i] + h/5, y_next[i] + k1/5)
    k3 <- h * f(t[i] + 3*h/10, y_next[i] + 3*k1/40 + 9*k2/40)
    k4 <- h * f(t[i] + 3*h/5, y_next[i] + 3*h/10 - 9*k2/10 + 6*k3/5)
    k5 <- h * f(t[i] + h, y_next[i] - 11*k1/54 + 5*k2/2 - 70*k3/27 + 35*k4/27)
    k6 <- h * f(t[i] + 7*h/8, y_next[i] + 1631*k1/55296 + 175*k2/512 + 575*k3/13824 + 44275*k4/110592 + 253*k5/4096)
    
    y_next[i + 1] <- y_next[i] + (37*k1/378 + 250*k3/621 + 125*k4/594 + 512*k6/1771)
  }
  
  return(y_next)
}
##################################################################
#LE SCHEMA RK4

f_appl2 = f_app1(y0,t,h)
f_appl3 = f_app2(y0,t,h)
f_appl4 = f_app4(y0,t,h)
err_g2= abs(f_exal - f_appl2)/h
err_g3= abs(f_exal - f_appl3)/h
err_g4= abs(f_exal - f_appl4)/h
result1 = data.frame(result1,f_appl2=f_appl2,err_g2=err_g2,f_appl3=f_appl3,err_g3=err_g3,f_appl4=f_appl4,err_g4=err_g4)
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
legend("topleft",legend = c("Courbe exacte","Courbe explicite","Courbe implicite","RK2","RK4"), col = c("black","red","green","yellow","blue"),lty = 1, cex = 0.8)
View(result1)