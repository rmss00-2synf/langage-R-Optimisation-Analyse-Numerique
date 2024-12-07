library(ggplot2)
library(geometry)
library(plotly)
library(dplyr)
#
# Définition de la fonction
f=function(x,y) {
  return(x^2/2+7*y^2/2)
}

# Gradient de la fonction
grad_f=function(x,y) {
  return(c(x,7*y))
}

#Recherche du pas optimal
Gradien_pas_opt=function(x,y, c1, c2, max_iter) {
  alpha_d0=.2
  alpha_g0 =0
  alpha_i = mean(c(alpha_g0,alpha_d0))
  x_values=c(x)
  y_values=c(y)
  i=0
  while(i <= max_iter & norm(grad_f(x,y),'2')>1e-10 ) {
    prod_i1=dot(grad_f(x,y),grad_f(x,y))
    prod_i2=dot(grad_f(x - alpha_i * grad_f(x,y)[1],y - alpha_i * grad_f(x,y)[2]),grad_f(x,y))
    j=0
    alpha_d = alpha_d0
    alpha_g = alpha_g0
    cond1 = f(x - alpha_i * grad_f(x,y)[1],y - alpha_i * grad_f(x,y)[2]) <= f(x,y) - c1 * alpha_i * prod_i1
    cond2 = (-prod_i2) <= (-c2 * prod_i1)
    while((j <= max_iter) & !(cond1 & cond2)){
      if(!cond1){
        alpha_d = alpha_i
        alpha_i=mean(c(alpha_g,alpha_d))
      }
      if (cond1 & !cond2) {
        #print(alpha_g)
        alpha_g = alpha_i
        alpha_i=mean(c(alpha_g,alpha_d))
      }
      j=j+1
      cond1 = f(x - alpha_i * grad_f(x,y)[1],y - alpha_i * grad_f(x,y)[2]) <= f(x,y) - c1 * alpha_i * prod_i1
      cond2 = (-prod_i2) <= (-c2 * prod_i1)
    }
    #print(alpha_i)
    x=x - alpha_i * grad_f(x,y)[1]
    y=y - alpha_i * grad_f(x,y)[2]
    x_values=c(x_values, x)
    y_values=c(y_values, y)
    #
    i=i + 1
  }
  cat(i," iterations\n")
  return(list(x_values,y_values))
}

# Point initial
x_0=5
y_0=5
max_iter = 100
# Calcul des points x_i
sol_values=Gradien_pas_opt(x_0,y_0,c1 = 1e-14, c2 = 0.1, max_iter = max_iter)
# Affichage du graphique
x_values=sol_values[[1]]
y_values=sol_values[[2]]
cat("solx : ",x_values[length(x_values)],"\nsoly : ",y_values[length(x_values)])
#cat(x_values[length(x_values)],y_values[length(y_values)])
# Création de la surface pour la fonction f
x=y=seq(-5, 5, length.out = 100)
grid=expand.grid(x = x, y = y)
z=f(grid$x, grid$y)

# Visualisation en 3D
p=plot_ly(z = ~matrix(z, nrow = length(x)), x = ~matrix(grid$x, nrow = length(x)), 
          y = ~matrix(grid$y, ncol = length(y)), type ="surface") %>%
  add_trace(data = data.frame(x = x_values, y = y_values),
            x = ~x, y = ~y, z = ~f(x, y), type = "scatter3d", mode = "markers", marker = list(size = 4, color = "red"))
p=p %>% layout(scene = list(
  xaxis = list(title = "x"),
  yaxis = list(title = "y"),
  zaxis = list(title = "z")
))
print(p)

i=0

# Tracer les contours de f et de la contrainte g
# Create a contour plot
x_endval= x_values[-1]
y_endval= y_values[-1]
x_endval[length(x_endval)+1]=x_endval[length(x_endval)]
y_endval[length(y_endval)+1]=y_endval[length(y_endval)]
contour_p = ggplot(grid, aes(x, y)) +
  geom_contour(aes(z = z), bins = 10) +
  geom_point(data = data.frame(x = x_values, y = y_values), aes(x, y), color = "red", size = 2) +
  labs(title = " ", x = "x", y = "y") +
  theme_minimal()+ 
  geom_segment(data =  data.frame(x = x_values, y = y_values,xend=x_endval,yend=y_endval),
                                      aes(x = x, y = y, xend = xend, yend = yend),
                                      arrow = arrow(length = unit(0.1, "inches")),
                                      color = "green")

print(contour_p)
