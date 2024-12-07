library(ggplot2)
library(geometry)
library(plotly)

#
# Définition de la fonction
f=function(x,y) {
  return(exp(x - 3*y - 1/10) + exp(x + 3*y - 1/10) + exp(- x - 1/10))
}

# Gradient de la fonction
grad_f=function(x,y) {
  return(c(exp(x - 3*y - 1/10) + exp(x + 3*y - 1/10) - exp(- x - 1/10),3*exp(x + 3*y - 1/10) - 3*exp(x - 3*y - 1/10)))
}

#Recherche du pas optimal
Gradien_pas_opt=function(x,y, c1, c2, max_iter) {
  alpha_i=1  
  x_values=c(x)
  y_values=c(y)
  i=0
  while(i <= max_iter & norm(grad_f(x,y),'2')>1e-10 ) {
    prod_i1=dot(grad_f(x,y),grad_f(x,y),d=TRUE)
    prod_i2=dot(grad_f(x - alpha_i * grad_f(x,y)[1],y - alpha_i * grad_f(x,y)[2]),grad_f(x,y),d=TRUE)
    j=0
    while((j <= max_iter) & (f(x - alpha_i * grad_f(x,y)[1],y - alpha_i * grad_f(x,y)[2]) > f(x,y) - c1 * alpha_i * prod_i1) ){
      alpha_i=0.1*alpha_i 
      # if ( -prod_i2 >= -c2 * prod_i1) {
      #   alpha_i=1.2*alpha_i
      # }
      j=j+1
    }
    x=x - alpha_i * grad_f(x,y)[1]
    y=y - alpha_i * grad_f(x,y)[2]
    x_values=c(x_values, x)
    y_values=c(y_values, y)
    #
    i=i + 1
  }
  print(i)
  return(list(x_values,y_values))
}

# Point initial
x_0=1
y_0=1
# Calcul des points x_i
sol_values=Gradien_pas_opt(x_0,y_0,c1 = 1e-14, c2 = 0.1, max_iter = 100000)

# Affichage du graphique
x_values=sol_values[[1]]
y_values=sol_values[[2]]
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

cat("x* = ",sol_values[[1]][length(sol_values[[2]])],"\ny* = ",sol_values[[2]][length(sol_values[[2]])])

# Tracer les contours de f et de la contrainte g
contour_p=ggplot(grid, aes(x, y)) +
  geom_contour(aes(z = z), bins = 10) +
  geom_point(data = data.frame(x = x_values, y = y_values), aes(x, y), color = "red", size = 2) +
  labs(title = " ", x = "x", y = "y")+
  theme_minimal()

print(contour_p)

rm(list = ls())
