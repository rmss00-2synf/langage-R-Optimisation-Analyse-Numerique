library(kableExtra)
library(knitr)
library(dplyr)
###
# Fonctions du systeme
#y1=s,,,,y2=I

beta <- 0.5  
gamma <- 0.1
M = 1000 #Nombre total de population

f1=function(t, y1,y2,y3) {
  return(-beta * y1 * y2 / M)
}
f2=function(t, y1,y2,y3) {
  return(beta * y1 * y2 / M -gamma * y2)
}
f3=function(t, y1,y2,y3) {
  return(gamma * y2)
}


# Méthode Runge-Kutta d'ordre 2 Heun
k1=function(t, h,y1,y2,y3) {
  k11=f1(t, y1,y2,y3)
  k21=f2(t, y1,y2,y3)
  k31=f3(t, y1,y2,y3)
  return(c(k11,k21,k31))
}
k2=function(t, h,y1,y2,y3) {
  k12=f1(t + h/2, y1 + (h/2)*k1(t,h,y1,y2,y3)[1],y2 + (h/2)*k1(t,h,y1,y2,y3)[2] + (h/2)*k1(t,h,y1,y2,y3)[3])
  k22=f2(t + h/2, y1 + (h/2)*k1(t,h,y1,y2,y3)[1],y2 + (h/2)*k1(t,h,y1,y2,y3)[2] + (h/2)*k1(t,h,y1,y2,y3)[3])
  k32=f3(t + h/2, y1 + (h/2)*k1(t,h,y1,y2,y3)[1],y2 + (h/2)*k1(t,h,y1,y2,y3)[2] + (h/2)*k1(t,h,y1,y2,y3)[3])
  return(c(k12,k22,k32))
}
k3=function(t,h,y1,y2,y3){
  k13=f1(t + h/2, y1 + (h/2)*k2(t,h,y1,y2,y3)[1],y2 + (h/2)*k2(t,h,y1,y2,y3)[2] + (h/2)*k2(t,h,y1,y2,y3)[3])
  k23=f2(t + h/2, y1 + (h/2)*k2(t,h,y1,y2,y3)[1],y2 + (h/2)*k2(t,h,y1,y2,y3)[2] + (h/2)*k2(t,h,y1,y2,y3)[3])
  k33=f3(t + h/2, y1 + (h/2)*k2(t,h,y1,y2,y3)[1],y2 + (h/2)*k2(t,h,y1,y2,y3)[2] + (h/2)*k2(t,h,y1,y2,y3)[3])
  return(c(k13,k23,k33))
}
k4=function(t,h,y1,y2,y3){
  k14=f1(t + h, y1 + h*k3(t,h,y1,y2,y3)[1],y2 + h*k3(t,h,y1,y2,y3)[2] + h*k3(t,h,y1,y2,y3)[3])
  k24=f2(t + h, y1 + h*k3(t,h,y1,y2,y3)[1],y2 + h*k3(t,h,y1,y2,y3)[2] + h*k3(t,h,y1,y2,y3)[3])
  k34=f3(t + h, y1 + h*k3(t,h,y1,y2,y3)[1],y2 + h*k3(t,h,y1,y2,y3)[2] + h*k3(t,h,y1,y2,y3)[3])
  return(c(k14,k24,k34))
}
RK2=function(t,h,y1_0,y2_0,y3_0) {
  y1 = numeric(length(t))
  y1[1] = y1_0
  y2 = numeric(length(t))
  y2[1] = y2_0
  y3 = numeric(length(t))
  y3[1] = y3_0
  for (i in 2:length(t)) {
    y1[i] = y1[i - 1] + (h/6) * (k1(t[i - 1], h, y1[i - 1], y2[i - 1],y3[i - 1])[1] + 2*k2(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1])[1]+ 2*k3(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1])[1] + k4(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1])[1])
    y2[i] = y2[i - 1] + (h/6) * (k1(t[i - 1], h, y1[i - 1], y2[i - 1],y3[i - 1])[2] + 2*k2(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1])[2]+ 2*k3(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1]) [2]+ k4(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1])[2])
    y3[i] = y3[i - 1] + (h/6) * (k1(t[i - 1], h, y1[i - 1], y2[i - 1],y3[i - 1])[3] + 2*k2(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1])[3]+ 2*k3(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1]) [3]+ k4(t[i - 1], h, y1[i - 1], y2[i - 1], y3[i - 1])[3])
  }
  return(list(y1,y2,y3))
}



# ... (Your function definitions remain the same)

# Afichage des resultats en tableau
resultats_tab <- function(titre, nom_col, pas, y_list) {
  col_names <- nom_col
  resultat_kable <- list()
  
  # Pad shorter lists with NA values
  max_length <- max(lengths(y_list))
  y_list <- lapply(y_list, function(x) c(x, rep(NA, max_length - length(x))))
  
  for (i in 1:length(pas)) {
    resultats <- data.frame(
      Pas = rep(pas[i], max_length),
      Y1 = y_list[[i]][[1]],
      Y2 = y_list[[i]][[2]],
      Y3 = y_list[[i]][[3]]
    )
    resultats$Y1 <- format(resultats$Y1, scientific = TRUE)
    resultats$Y2 <- format(resultats$Y2, scientific = TRUE)
    resultats$Y3 <- format(resultats$Y3, scientific = TRUE)
    
    resultat_kable[[i]] <- resultats
  }
  
  combined_result <- bind_rows(resultat_kable)
  kable_table <- kable(
    combined_result, "html",
    caption = titre, col.names = col_names, digits = 10
  ) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))
  print(kable_table)
}


resultats_graph <- function(t, h, y_i, col_plots1) {
  # Create an empty plot
  plot(t[[length(h)]], y_i[[length(h)]][[1]], type = "l", col = "black", xlab = "Temps", ylab = "Y(t)", lwd = 3)
  
  # Add lines to the plot for y2 and y3
  lines(t[[length(h)]], y_i[[length(h)]][[2]], col = "blue", lwd = 3)
  lines(t[[length(h)]], y_i[[length(h)]][[3]], col = "green", lwd = 3)
  
  # Add legends
  legend("topright", legend = c(paste("pas=", h), "y1(t)", "y2(t)", "y3(t)"), col = c("black", "blue", "green"), lwd = 3, lty = 1, cex = 0.8)
}

# Example usage


# Example usage


# Paramètres initiales
t0 = 0
y1_0 = 980
y2_0 = 20
y3_0 = M -(y1_0+y2_0)
T = 100
h_values = c(0.02)
N = T / h_values

# le maillage pour chaque pas de temps
t_list = lapply(N, function(n) seq(t0, t0 + T, by = T / n))
print(t_list)
RK2_list = mapply(
  RK2, t = t_list, h = h_values, y1_0 = y1_0, y2_0 = y2_0, y3_0 = y3_0, SIMPLIFY = FALSE
)

# Example usage
col_plots1 = c("green", "blue", "yellow3")
resultats_tab("RK2 Values for Different Time Steps", c("Pas", 'Y1', 'Y2', 'Y3'), h_values, RK2_list)
resultats_graph(t_list, h_values, RK2_list, col_plots1)
mtext("Resolution du system EDO par RK2 pour differents pas de temps", side = 3, line = -2, outer = TRUE)

rm(list = ls())