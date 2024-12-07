# Modèle SIR pour la propagation de WannaCry en R

# Paramètres du modèle
beta <- 0.5  # Taux de propagation de WannaCry
gamma <- 0.1 # Taux de rétablissement ou de nettoyage des systèmes

# Conditions initiales
N <- 10000    # Nombre total de systèmes
I <- 1       # Systèmes initialement infectés par WannaCry
R <- 0       # Systèmes rétablis ou protégés
S <- N - I - R # Systèmes non infectés mais vulnérables

# Stockage des valeurs au fil du temps
S_values <- S
I_values <- I
R_values <- R

# Nombre de jours à simuler
days <- 200
# Simulation du modèle SIR pour WannaCry
for (i in 1:days) {
  dS <- -beta * S * I / N
  dR <- gamma * I
  dI <- -(dS + dR)
  
  S <- S + dS
  I <- I + dI
  R <- R + dR
  
  S_values <- c(S_values, S)
  I_values <- c(I_values, I)
  R_values <- c(R_values, R)
}

# Visualisation des résultats
plot(1:(days+1), S_values, type="l", col="blue", ylim=c(0,N), xlab="Jours", ylab="Systèmes", main="Modèle SIR pour WannaCry")
lines(1:(days+1), I_values, col="red")
lines(1:(days+1), R_values, col="green")
legend("topright", legend=c("Systèmes vulnérables", "Systèmes infectés", "Systèmes rétablis/protégés"), col=c("blue", "red", "green"), lty=1)
