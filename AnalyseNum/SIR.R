library(ggplot2)
library(reshape2)
# Les fonctions dérivées
S_derive <- function(S, I, beta, gamma){ #Definition de S'
return(-beta * S * I + gamma * I)
}
I_derive <- function(S, I, beta, gamma){ #Definition de I'
return(beta * S * I - gamma * I)
}
# Les fonctions ki (Ki1,ki2) de Runge-Kutta pour l'ordre 6
#K1
k11 <- function(S, I, beta, gamma) {
S_derive(S, I, beta, gamma)
}
k12 <- function(S, I, beta, gamma) {
I_derive(S, I, beta, gamma)
}
#K2
k21 <- function(S, I, beta, gamma) {
S_derive(S + 1/4 * k11(S, I, beta, gamma), I + 1/4 * k12(S, I, beta, gamma), beta, gamma)
}
k22 <- function(S, I, beta, gamma) {
I_derive(S + 1/4 * k11(S, I, beta, gamma), I + 1/4 * k12(S, I, beta, gamma), beta, gamma)
}
#K3
k31 <- function(S, I, beta, gamma) {
S_derive(S + 1/4 * k21(S, I, beta, gamma), I + 1/4 * k22(S, I, beta, gamma), beta, gamma)
}
k32 <- function(S, I, beta, gamma) {
I_derive(S + 1/4 * k21(S, I, beta, gamma), I + 1/4 * k22(S, I, beta, gamma), beta, gamma)
}
#K4
k41 <- function(S, I, beta, gamma) {
S_derive(S + 1/2 * k31(S, I, beta, gamma), I + 1/2 * k32(S, I, beta, gamma), beta, gamma)
}
k42 <- function(S, I, beta, gamma) {
I_derive(S + 1/2 * k31(S, I, beta, gamma), I + 1/2 * k32(S, I, beta, gamma), beta, gamma)
}
#K5
k51 <- function(S, I, beta, gamma) {
S_derive(S + 3/4 * k41(S, I, beta, gamma), I + 3/4 * k42(S, I, beta, gamma), beta, gamma)
}
k52 <- function(S, I, beta, gamma) {
I_derive(S + 3/4 * k41(S, I, beta, gamma), I + 3/4 * k42(S, I, beta, gamma), beta, gamma)
}
#K6
k61 <- function(S, I, beta, gamma) {
S_derive(S + k51(S, I, beta, gamma), I + k52(S, I, beta, gamma), beta, gamma)
}
k62 <- function(S, I, beta, gamma) {
I_derive(S + k51(S, I, beta, gamma), I + k52(S, I, beta, gamma), beta, gamma)
}
# Les fontions de Runge-Kutta à chaque instant:
Runge_kutta5 <- function(S, I, beta, gamma, pas, T){
t <- seq(0, T, by = pas)
S_result <- c(S)
I_result <- c(I)
for(i in 2:length(t)){
k1_1 <- k11(S_result[i-1], I_result[i-1], beta, gamma)
k1_2 <- k12(S_result[i-1], I_result[i-1], beta, gamma)
k2_1 <- k21(S_result[i-1], I_result[i-1], beta, gamma)
k2_2 <- k22(S_result[i-1], I_result[i-1], beta, gamma)
k3_1 <- k31(S_result[i-1], I_result[i-1], beta, gamma)
k3_2 <- k32(S_result[i-1], I_result[i-1], beta, gamma)
k4_1 <- k41(S_result[i-1], I_result[i-1], beta, gamma)
k4_2 <- k42(S_result[i-1], I_result[i-1], beta, gamma)
k5_1 <- k51(S_result[i-1], I_result[i-1], beta, gamma)
k5_2 <- k52(S_result[i-1], I_result[i-1], beta, gamma)
k6_1 <- k51(S_result[i-1], I_result[i-1], beta, gamma)
k6_2 <- k52(S_result[i-1], I_result[i-1], beta, gamma)
S_new <- S_result[i-1] + pas/90 * (7*k1_1 + 32*k3_1 + 12*k4_1 + 32*k5_1 + 7*k6_1)
I_new <- I_result[i-1] + pas/90 * (7*k1_2 + 32*k3_2 + 12*k4_2 + 32*k5_2 + 7*k6_2)
S_result[i] <- S_new
I_result[i] <- I_new
}
#Retourner le tableau des resultats
return(data.frame(t = t, S = S_result, I = I_result))
}
# Exemple d'utilisation du model SIS:
# Les conditions initials
S <- 1000 #1000 subsbectible
I <- 100 #100 infecté
pas <- 5 #Pas de 5 jours
T <- 180 #Arrivant jusqu'a 180jours
#Initialiser ,t les taux de transmitions
beta <- 0.0001
gamma <- 0.01
#Stocker les résultats sous forme de tableau
result <- Runge_kutta5(S, I, beta, gamma, pas, T)
View(result)
# Transformer les données en format long pour ggplot
result_long <- melt(result, id.vars = "t")
col_plots <- c("S" = "blue", "I" = "red", "R" = "green")
# Utiliser ggplot pour créer le graphique
p<- ggplot(data = result_long, aes(x = t, y = value, colour = variable, group = variable)) +
geom_line() +
scale_y_continuous(limits = c(0, max(c(S, I) * 1.1))) + # Ajuster la limite de l'axe y
labs(x = "Temps (t)", y = "Population", title = "Evolution de S, I, S en fonction du temps") +
theme_minimal() +
scale_colour_manual(values = col_plots) +
theme(legend.title = element_blank())
print(p)