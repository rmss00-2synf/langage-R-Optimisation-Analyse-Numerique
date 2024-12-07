library(ggplot2)
library(geometry)
library(plotly)
library(dplyr)
#
# Définition de la fonction
f=function(x,y,z) {
  return(3*x^2 + y^2 + z^2 + x*y + x*z -2*y -3*z)
}

# Gradient de la fonction
grad_f=function(x,y,z) {
  return(c(6*x + y + z, x + 2*y - 2, x + 2*z - 3))
}

#Recherche du pas optimal
Gradien_pas_opt=function(x,y,z, c1, c2, max_iter) {
  alpha = 0.5
  beta = 1.5
  alpha_i = 1
  x_values=c(x)
  y_values=c(y)
  z_values=c(z)
  i=0
  while(i <= max_iter & norm(grad_f(x,y,z),'2')>1e-10 ) {
    prod_i1=dot(grad_f(x,y,z),grad_f(x,y,z))
    prod_i2=dot(grad_f(x - alpha_i * grad_f(x,y,z)[1],y - alpha_i * grad_f(x,y,z)[2],z - alpha_i * grad_f(x,y,z)[3]),grad_f(x,y,z))
    j=0
    cond1 = f(x - alpha_i * grad_f(x,y,z)[1],y - alpha_i * grad_f(x,y,z)[2],z - alpha_i * grad_f(x,y,z)[3]) <= f(x,y,z) - c1 * alpha_i * prod_i1
    cond2 = (-prod_i2) <= (-c2 * prod_i1)
    while((j <= max_iter) & !(cond1 & cond2)){
      if(!cond1){
        alpha_i=alpha_i*alpha
      }
      if (cond1 & !cond2) {
        #print(alpha_g)
        alpha_i=alpha_i*beta
      }
      j=j+1
      cond1 = f(x - alpha_i * grad_f(x,y,z)[1],y - alpha_i * grad_f(x,y,z)[2],z - alpha_i * grad_f(x,y,z)[3]) <= f(x,y,z) - c1 * alpha_i * prod_i1
      cond2 = (-prod_i2) <= (-c2 * prod_i1)
    }
    #print(alpha_i)
    x=x - alpha_i * grad_f(x,y,z)[1]
    y=y - alpha_i * grad_f(x,y,z)[2]
    z=z - alpha_i * grad_f(x,y,z)[3]
    x_values=c(x_values, x)
    y_values=c(y_values, y)
    z_values=c(z_values, z)
    #
    i=i + 1
  }
  cat(i," iterations\n")
  return(list(x_values,y_values,z_values))
}

# Point initial
x_0=5
y_0=-10
z_0 =10
max_iter = 40
# Calcul des points x_i
sol_values=Gradien_pas_opt(x_0,y_0,z_0,c1 = 1e-10, c2 = 0.7, max_iter = max_iter)
# Affichage du graphique
x_values=sol_values[[1]]
y_values=sol_values[[2]]
z_values=sol_values[[3]]
cat("solx : ",x_values[length(x_values)],"\nsoly : ",y_values[length(x_values)],"\nsolz : ",z_values[length(x_values)])
#cat(x_values[length(x_values)],y_values[length(y_values)])
# Création de la surface pour la fonction f
cat("\n\n")
cat("solx : ",x_values[6],"   soly : ",y_values[6],"   solz : ",z_values[6])
cat("\n")
cat("solx : ",x_values[11],"   soly : ",y_values[11],"   solz : ",z_values[11])
cat("\n")
cat("solx : ",x_values[36],"   soly : ",y_values[36],"   solz : ",z_values[36])