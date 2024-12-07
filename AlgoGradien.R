# Définition de la fonction f
f <- function(x,y) {
  return(exp(3*y+x-0.1)+exp(-3*y+x-0.1)+exp(-x-0.1))
}

# Définition de la dérivée de la fonction f
df <- function(x,y) {
  return(c(exp(3*y+x-0.1)+exp(-(3*y)+x-0.1)-exp(-x-0.1),3*exp(3*y+x-0.1)-3*exp(-(3*y)+x-0.1)))
}

# Algorithme de descente de gradient
gradient_descent <- function(learning_rate, epochs, initial_x) {
  x <- initial_x
  cat(x)
  for (i in 1:epochs) {
    gradient <- df(x[1],x[2])
    x <- x - learning_rate * gradient
    cat("Étape", i, "- x =", x[1], "- y =", x[2], "\n")
  }
  
  return(x)
}

# Paramètres de l'algorithme
learning_rate <- 0.1
epochs <- 20
initial_x <- c(1,1)

# Exécution de l'algorithme de descente de gradient
result <- gradient_descent(learning_rate, epochs, initial_x)
cat("Valeur minimale trouvée pour x:", result, "\n")
# cat("Valeur minimale réelle pour f(x):", f(,), "\n")
