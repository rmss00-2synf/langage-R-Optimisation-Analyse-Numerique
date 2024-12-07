library(plotly)
library(ggplot2)
# Fonction à deux variables
f=function(x, y) {
  return(exp(3*y+x-0.1)+exp(-3*y+x-0.1)+exp(-x-0.1))  # Exemple : fonction quadratique
}
#return(x^2/2+7*y^2/2)#
# Gradient de la fonction à deux variables
gradient=function(x, y) {
   return(c(exp(x - 3*y - 1/10) + exp(x + 3*y - 1/10) - exp(- x - 1/10), 3*exp(x + 3*y - 1/10) - 3*exp(x - 3*y - 1/10)))
  #return(c(x,7*y))
}
# Méthode du gradient avec pas fixe pour deux variables
gradient_descent_2D=function(gradient, x_initial, y_initial, learning_rate, iterations) {
  x=x_initial
  y=y_initial
  x_values=c(x)
  y_values=c(y)
  k=0
  while(k<iterations & norm(gradient(x, y),'2')>1e-10){
    x = x - learning_rate * gradient(x, y)[1]
    y = y - learning_rate * gradient(x, y)[2]
    x_values = c(x_values, x)
    y_values = c(y_values, y)
    k=k+1
  }
  cat(k," iterations\n")
  return(list(x_values,y_values))
}
x_0=.6053
y_0=.6053
# Valeurs initiales et paramètres de l'algorithme
x_initial = 1  # Point de départ pour x
y_initial = 1  # Point de départ pour y
learning_rate = 0.0001  # Pas d'apprentissage
iterations = 10000  # Nombre d'itérations

# Appliquer la méthode du gradient pour deux variables
sol_values = gradient_descent_2D(gradient, x_initial, y_initial, learning_rate, iterations)
#print(result)  # Afficher le minimum trouvé pour x et y
# Création du graphique
x_values = sol_values[[1]]
y_values = sol_values[[2]]
cat("solx : ",x_values[length(x_values)],"\nsoly : ",y_values[length(x_values)])
# Création de la surface pour la fonction f
x = y=seq(-5, 5, length.out = 100)
grid = expand.grid(x = x, y = y)
z = f(grid$x, grid$y)

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
# Tracer les contours de f et de la contrainte g
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

#print(contour_p)

print(contour_p)