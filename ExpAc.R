# Définition de la fonction du système d'équations
SIRsystem <- function(t, X, ...) {
  Xpoint <- numeric(length = 4)
  b <- 1/(3 * X[1]) # Calcul de beta
  g <- 1/20
  d <- 5/1000
  n <- 0
  
  Xpoint[1] <- -b * X[1] * X[2] + n * X[3]
  Xpoint[2] <- b * X[1] * X[2] - g * X[2] - d * X[2]
  Xpoint[3] <- g * X[2] - n * X[3]
  Xpoint[4] <- d * X[2]
  
  return(list(Xpoint))
}

# Définition des conditions initiales
S0 <- 1000000
I0 <- 10
R0 <- 0
M0 <- 0
Xinit <- c(S0, I0, R0, M0)


b <- 1/(3 * S0) # Calcul de beta
g <- 1/20
d <- 5/1000
n <- 0
# Définition des bornes du temps et du nombre de points
borneSup <- 200
nbrPts <- 5000
Temps <- seq(0, borneSup, length.out = nbrPts)

# Résolution du système
library(deSolve) # Assure que le package de résolution des équations différentielles est chargé
X <- ode(y = Xinit, times = Temps, func = SIRsystem)

# Affichage
guerison <- paste("taux de guérison (gamma) :",  g)
infection <- paste("taux d'infection (beta) :", b)
mortalite <- paste("taux de mortalité (delta) :", d)

plot(Temps, X[,1], type = "l", col = "blue", xlab = "Temps en jours", ylab = "Population")
lines(Temps, X[,2], col = "red")
lines(Temps, X[,3], col = "green")
lines(Temps, X[,4], col = "orange")
legend("topright", legend = c("Personnes saines", "Personnes infectées", "Personnes guéries", "Personnes mortes"), col = c("blue", "red", "green", "orange"))
