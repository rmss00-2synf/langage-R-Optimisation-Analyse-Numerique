library(pracma)# Chargement de la bibliothèque nécessaire pour les opérations matricielles
library(ggplot2)
library(geometry)
library(plotly)
# Définition de la fonction f(x, y)
f=function(x,y,z) {
  return(3*x^2 + y^2 + z^2 + x*y + x*z -2*y -3*z)
}
grad_f=function(x,y,z) {
  return(c(6*x + y + z, x + 2*y - 2, x + 2*z - 3))
}

# Forme quadratique de f(x, y)
A=matrix(c(6, 1, 1, 1,2,0,1,0,2), nrow = 3, byrow = TRUE)  # Matrice A
b=c(0, 2,3)  # Vecteur b

# Vérification de la définie positivité de A
if(all(eigen(A)$values > 0)) {
  print("La matrice A est bien définie positive.")
  
  # Résolution du système AX = b en inversant la matrice A
  X=solve(A, b)
  print("La solution du système AX = b est :")
  print(X)
  
  # Méthode du gradient conjugué pour trouver le minimum de f(x, y)
  n=20
  X=c(5, -10,10)  # Point initial
  tol=1e-10
  #fonction de R : 
  r=b-A%*%X
  d=r
  x_values=c(X[1])
  y_values=c(X[2])
  z_values=c(X[3])
  for(k in 1:n){
    if(norm(r,'2')<tol){
      break
    }
    else{
      rho=dot(r,r)/dot(A%*%d,d) 
      X=X+rho*d
      r1 = r
      r=b-A%*%X
      alpha=dot(r,r)/dot(r1,r1)
      d=r+alpha*d
    }
    x_values=c(x_values,X[1])
    y_values=c(y_values,X[2])
    z_values=c(z_values,X[3])
  }
  
  sol_values = list(x_values=x_values,y_values=y_values,z_values=z_values)
  cat(k-1," iterations\n")
  # Affichage
  print("Valeur approchée du point X* pour le minimum global de f(x, y) :")
  #print(list(x_values=x_values,y_values=y_values))
  cat("solx : ",sol_values[[1]][length(sol_values[[2]])],"\nsoly : ",sol_values[[2]][length(sol_values[[2]])],"\nsolz : ",sol_values[[3]][length(sol_values[[3]])])
  
  print(sol_values)
  
  
} else {
  print("La matrice A n'est pas définie positive.")
}