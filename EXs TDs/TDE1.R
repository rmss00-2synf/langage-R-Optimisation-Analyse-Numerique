#LE SCHEMA EXPLICITE D'EULEUR
library(kableExtra)
#Calcul de la fonction f

f = function(t,y){
  return(-2*t+1)
}

#Calcul de la fonction reelle

f_exa = function(t){
  return(-t^2+t+2)
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

h = 0.5
Tn = 4
y0 = 2
t=seq(0,4,by=h)

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
    y[i] <- uniroot( function(yn1) {yn1 - y[i-1] - h * f(t[i],yn1) }, interval = c(-1000, 1000))$root
  }
  
  return(y)
}

f_appl2 = f_app1(y0,t,h)
err_g2= abs(f_exal - f_appl2)/h
result1 = data.frame(result1,f_appl2=f_appl2,err_g2=err_g2)
points(t,f_appl2,type='l',col='green',lwd=2)


col_names=c("ti","y_i_explicite","y(t_i)","Eg_i1/h",'y_i_implicite',"Eg_i2/h")  # ("Pas_i","sum El_i","max Eg_i","Ordre")
resultat_kable <- kable(result1,caption = 'Resultats numeriques obtenus pour un pas h = 0.5 sur [0,4] par les sch\'emas d\'Euler', "html", col.names = NULL,digits=4)%>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))%>%
  add_header_above(col_names)%>%
  column_spec(1, bold = TRUE)%>%
  row_spec(1:nrow(result1), align = "c")
print(resultat_kable)
legend("topright",legend = c("Courbe exacte","Courbe explicite","Courbe implicite"), col = c("black","red","green"),lty = 1, cex = 0.8)
View(result1)