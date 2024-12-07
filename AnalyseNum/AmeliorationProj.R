library(plotly)
library(ggplot2)
library(reshape2)
# Les fonctions dérivées
S_derive <- function(S, I,R, beta, gamma){
  return(-beta * S * I )
}
I_derive <- function(S, I,R, beta, gamma){
  return(beta * S * I - gamma * I)
}
R_derive <- function(S,I,R,beta,gamma){
  return(gamma*I)
}
# Les fonctions ki (Ki1,ki2, ki3) de Runge-Kutta pour l'ordre 6
# K1
k11 <- function(S, I,R, beta, gamma) {
  S_derive(S, I,R ,beta, gamma)
}
k12 <- function(S, I,R, beta, gamma) {
  I_derive(S, I,R, beta, gamma)
}
k13 <- function(S, I,R, beta, gamma) {
  R_derive(S, I,R, beta, gamma)
}
# K2
k21 <- function(S, I, R, beta, gamma) {
  S_derive(S + 1/4 * k11(S, I, R, beta, gamma), I + 1/4 * k12(S, I, R, beta, gamma), R + 1/4 *
             k13(S, I, R, beta, gamma), beta, gamma)
}
k22 <- function(S, I, R, beta, gamma) {
  I_derive(S + 1/4 * k11(S, I, R, beta, gamma), I + 1/4 * k12(S, I, R, beta, gamma), R + 1/4 *
             k13(S, I, R, beta, gamma), beta, gamma)
}
k23 <- function(S, I, R, beta, gamma) {
  R_derive(S + 1/4 * k11(S, I, R, beta, gamma), I + 1/4 * k12(S, I, R, beta, gamma), R + 1/4 *
             k13(S, I, R, beta, gamma), beta, gamma)
}
# K3
k31 <- function(S, I,R, beta, gamma) {
  S_derive(S + 1/4 * k21(S, I, R, beta, gamma), I + 1/4 * k22(S, I, R, beta, gamma), R + 1/4 *
             k23(S, I,R, beta, gamma), beta, gamma)
}
k32 <- function(S, I,R, beta, gamma) {
  I_derive(S + 1/4 * k21(S, I,R, beta, gamma), I + 1/4 * k22(S, I,R, beta, gamma),R + 1/4 *
             k23(S, I,R, beta, gamma), beta, gamma)
}
k33 <- function(S, I,R, beta, gamma) {
  R_derive(S + 1/4 * k21(S, I,R, beta, gamma), I + 1/4 * k22(S, I,R, beta, gamma),R + 1/4 *
             k23(S, I,R, beta, gamma), beta, gamma)
}
# K4
k41 <- function(S, I,R, beta, gamma) {
  S_derive(S + 1/2 * k31(S, I,R, beta, gamma), I + 1/2 * k32(S, I,R, beta, gamma),R + 1/2 *
             k33(S, I,R, beta, gamma), beta, gamma)
}
k42 <- function(S, I,R, beta, gamma) {
  I_derive(S + 1/2 * k31(S, I,R, beta, gamma), I + 1/2 * k32(S, I,R, beta, gamma),R + 1/2 *
             k33(S, I,R, beta, gamma), beta, gamma)
}
k43 <- function(S, I,R, beta, gamma) {
  R_derive(S + 1/2 * k31(S, I,R, beta, gamma), I + 1/2 * k32(S, I,R, beta, gamma),R + 1/2 *
             k33(S, I,R, beta, gamma),beta, gamma)
}
# K5
k51 <- function(S, I,R, beta, gamma) {
  S_derive(S + 3/4 * k41(S, I,R, beta, gamma), I + 3/4 * k42(S, I,R, beta, gamma), R + 3/4 *
             k43(S, I,R, beta, gamma),beta, gamma)
}
k52 <- function(S, I,R, beta, gamma) {
  I_derive(S + 3/4 * k41(S, I,R, beta, gamma), I + 3/4 * k42(S, I,R, beta, gamma),R + 3/4 *
             k43(S, I,R, beta, gamma), beta, gamma)
}
k53 <- function(S, I,R, beta, gamma) {
  R_derive(S + 3/4 * k41(S, I,R, beta, gamma), I + 3/4 * k42(S, I,R, beta, gamma),R + 3/4 *
             k43(S, I,R, beta, gamma), beta, gamma)
}
# K6
k61 <- function(S, I,R, beta, gamma) {
  S_derive(S + k51(S, I,R, beta, gamma), I + k52(S, I, R,beta, gamma),R + k53(S, I, R,beta,
                                                                              gamma), beta, gamma)
}
k62 <- function(S, I,R, beta, gamma) {
  I_derive(S + k51(S, I,R, beta, gamma), I + k52(S, I,R, beta, gamma),R + k53(S, I,R, beta,
                                                                              gamma), beta, gamma)
}
k63 <- function(S, I,R, beta, gamma) {
  R_derive(S + k51(S, I,R, beta, gamma), I + k52(S, I,R, beta, gamma), R + k53(S, I,R, beta,
                                                                               gamma),beta, gamma)
}
# Les fontions de Runge-Kutta à chaque instant:
Runge_kutta6 <- function(S, I,R, beta, gamma, pas, T){
  t <- seq(0, T, by = pas)
  S_result <- c(S)
  I_result <- c(I)
  R_result <- c(R)
  for(i in 2:length(t)){
    k1_1 <- k11(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k1_2 <- k12(S_result[i-1], I_result[i-1], R_result[i-1],beta, gamma)
    k1_3 <- k13(S_result[i-1], I_result[i-1], R_result[i-1],beta, gamma)
    k2_1 <- k21(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k2_2 <- k22(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k2_3 <- k23(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k3_1 <- k31(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k3_2 <- k32(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k3_3 <- k33(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k4_1 <- k41(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k4_2 <- k42(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k4_3 <- k43(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k5_1 <- k51(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k5_2 <- k52(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k5_3 <- k53(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k6_1 <- k61(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k6_2 <- k62(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    k6_3 <- k63(S_result[i-1], I_result[i-1],R_result[i-1], beta, gamma)
    S_new <- S_result[i-1] + pas/90 * (7*k1_1 + 32*k3_1 + 12*k4_1 + 32*k5_1 + 7*k6_1)
    I_new <- I_result[i-1] + pas/90 * (7*k1_2 + 32*k3_2 + 12*k4_2 + 32*k5_2 + 7*k6_2)
    R_new <- R_result[i-1] + pas/90 * (7*k1_3 + 32*k3_3 + 12*k4_3 + 32*k5_3 + 7*k6_3)
    S_result[i] <- S_new
    I_result[i] <- I_new
    R_result[i] <- R_new
  }
  #Retourner le tableau des resultats
  return(data.frame(t = t, S = S_result, I = I_result, R = R_result))
}
# Exemple d'utilisation du model SIS:
# Les conditions initials:
S <- 1000 #1000 subsbectible
I <- 100 #100 infecté
R <- 0 #0 recover
pas <- 5 #Pas de 5 jours
T <- 180 #Arrivant jusqu'a 180jours
gamma <- 0.04 # Taux de guérison (guérison en 10 jours en moyenne)
beta = 0.0001
R0 = 3 # On souhaite avoir un R0 autour de 3 pour une épidémie rapide mais réaliste
#Stocker les résultats sous forme de tableau
result <- Runge_kutta6(S, I,R, beta, gamma, pas, T)






x <-  Runge_kutta6(S, I,R, beta, gamma, pas, T)$S

# create data
aval <- list()
for(step in 1:11){
  aval[[step]] <-list(visible = FALSE,
                      name = paste0('v = ', step),
                      x=x,
                      beta=c(0.1,0.2),
                      y= Runge_kutta6(S, I,R, beta, gamma, pas, T)$I)
}
aval[3][[1]]$visible = TRUE

# create steps and plot all traces
steps <- list()
fig <- plot_ly()
for (i in 1:11) {
  fig <- add_lines(fig,x=aval[i][[1]]$x,  y=aval[i][[1]]$y, visible = aval[i][[1]]$visible, 
                   name = aval[i][[1]]$name, type = 'scatter', mode = 'lines', hoverinfo = 'name', 
                   line=list(color='00CED1'), showlegend = FALSE)
  
  step <- list(args = list('visible', rep(FALSE, length(aval))),
               method = 'restyle')
  step$args[[2]][i] = TRUE  
  steps[[i]] = step 
}  

# add slider control to plot
fig <- fig %>%
  layout(sliders = list(list(active = 3,
                             currentvalue = list(prefix = "Frequency: "),
                             steps = steps)))

print(fig)
