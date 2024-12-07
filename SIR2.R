# Installer et charger le package deSolve
if (!require("deSolve")) {
  install.packages("deSolve")
  library(deSolve)
}

# Définir l'équation différentielle (à adapter à votre problème)
f <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    dy <- x^2 - y  # Équation différentielle à résoudre (à adapter)
    return(c(dy))  # Retourner une liste de dérivées correspondant au nombre de variables d'état
  })
}

# Condition initiale
y0 <- c(y = 1)  # Condition initiale sous forme de vecteur

# Plage de valeurs pour x
x_range <- seq(0, 10, by = 0.1)  # De 0 à 10 avec un pas de 0.1

# Résolution de l'EDO avec RK4 (Runge-Kutta d'ordre 4)
solution <- ode(y = y0, times = x_range, func = f, parms = NULL, method = "rk4")

# Tracer la solution
plot(solution, main = "Solution de l'EDO avec RK4", xlab = "x", ylab = "y")
