library(pracma)# Chargement de la bibliothèque nécessaire pour les opérations matricielles
library(ggplot2)
library(geometry)
library(plotly)
# Définition de la fonction f(x, y)
f=function(x,y) {
  return(x^2 + y^2 + x*y - x - 2*y)
}
grad_f=function(x,y) {
  return(c(2*x  +y - 1, 2*y + x - 2))
}

# Forme quadratique de f(x, y)
A=matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE)  # Matrice A
b=c(1, 2)  # Vecteur b

# Vérification de la définie positivité de A
if(all(eigen(A)$values > 0)) {
  print("La matrice A est bien définie positive.")
  
  # Résolution du système AX = b en inversant la matrice A
  X=solve(A, b)
  print("La solution du système AX = b est :")
  print(X)
  
  # Méthode du gradient conjugué pour trouver le minimum de f(x, y)
  n=20
  X=c(-5, 5)  # Point initial
  tol=1e-10
  #fonction de R : 
  r=b-A%*%X
  d=r
  x_values=c(X[1])
  y_values=c(X[2])
  for(k in 1:n){
    if(norm(r,'2')<tol){
      break
    }
    else{
      rho=dot(r,d)/dot(A%*%d,d) 
      X=X+rho*d
      r=b-A%*%X
      alpha=-dot(A%*%r,d)/dot(A%*%d,d)
      d=r+alpha*d
    }
    x_values=c(x_values,X[1])
    y_values=c(y_values,X[2])
  }
  
  sol_values = list(x_values=x_values,y_values=y_values)
  cat(k-1," iterations\n")
  # Affichage
  print("Valeur approchée du point X* pour le minimum global de f(x, y) :")
  #print(list(x_values=x_values,y_values=y_values))
  cat("solx : ",sol_values[[1]][length(sol_values[[2]])],"\nsoly : ",sol_values[[2]][length(sol_values[[2]])])
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
  
  print(contour_p)
  

} else {
  print("La matrice A n'est pas définie positive.")
}