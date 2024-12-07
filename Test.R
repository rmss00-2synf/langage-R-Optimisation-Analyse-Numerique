library(kableExtra)
library(knitr)
library(dplyr)

#Equation différentielle dy/dt = -2t+1
f <- function(y, t) {
  return(-8*y+800)
}

# Solution analytique
y_exact= function(y0,t){
  return(95*exp(-8*t)+100)
}
# Schema d'Euler explicite
sol_euler = function(y0,t,h) {
  y = numeric(length(t))
  y[1] = y0
  
  for (i in 2:length(t)) {
    y[i] = y[i - 1] + h * f(y[i - 1], t[i - 1])
    
  }
  
  return(y)
}
# Schema d'Euler implicite dans ce cas
sol_euler_implicite = function(y0, t, h) {
  y = numeric(length(t))
  y[1] = y0
  
  for (i in 2:length(t)) {
    y[i] = y[i - 1] + h * (-2*t[i]+1)
    
  }
  
  return(y)
}

sol_euler_implicite2 = function(y0,t,h){
  y = numeric(length(t))
  y[1] = y0
  
  for (i in 2:length(t)) {
    y[i] <- uniroot( function(yn1) {yn1 - y[i-1] - h * (-8*yn1+800) }, interval = c(-1000, 1000))$root
  }
  
  return(y)
}

# Erreur globale :
Erreur_G= function(sol_app,sol_exact) {
  return(abs(sol_app - sol_exact))
}

# Paramètres initiales
y0 = 195
T = 2
h_values = c(0.5,0.2,0.1,0.05) 
N= T / h_values 

# le maillage pour chaque pas de temps
t_list <- lapply(N, function(n) seq(0, T, by=T/n))
#print(t_list)

# solution exacte pour chaque  pas de temps 
sol_exact_list=lapply(t_list,function(t) y_exact(y0,t))
#print(sol_exact_list)

# solutions approchees
# solution par Euler explicite pour chaque pas de temps sur [0,T]
sol_app_list <- mapply(sol_euler, y0=y0, t = t_list, h = h_values, SIMPLIFY = FALSE)
#print(sol_app_list) 

# solution par Euler implicite pour chaque pas de temps sur [0,T]
sol_app_imp_list <- mapply(sol_euler_implicite2, y0=y0, t = t_list, h = h_values, SIMPLIFY = FALSE)
#print(sol_app_imp_list) 

# Calcul des erreurs globales pour chaque  pas pour schema d'euler explicite 
global_errors_list <- mapply(Erreur_G, sol_app=sol_app_list,sol_exact=sol_exact_list, SIMPLIFY = FALSE)
#print(global_errors_list[[1]])

# Calcul des erreurs globales pour chaque  pas pour schema d'euler implicite 
global_errorsimp_list<- mapply(Erreur_G, sol_app=sol_app_imp_list,sol_exact=sol_exact_list, SIMPLIFY = FALSE)

# max de les erreurs globales sur [0,T] pour chaque pas de temps
global_errors_exp_T <- sapply(global_errors_list, function(err) max(err))
global_errors_imp_T <- sapply(global_errorsimp_list, function(err) max(err))

# affichage des resultats  en fenetre Viewer
# col_names=c("y(t_i)","yexp_i","yimp_i","Egexp_i","Egimp_i") #,"max Eg_i/h")  # ("Pas_i","sum El_i","max Eg_i","Ordre")
#  for(i in 1:length(N)){
#   resultats=data.frame(Sol_exact=sol_exact_list[[i]],Sol_app_exp=sol_app_list[[i]],Sol_app_imp=sol_app_imp_list[[i]],Egexp=global_errors_list[[i]],Egimp=global_errorsimp_list[[i]])
#   resultat_kable <- kable(resultats, "html", col.names = NULL,digits=7)%>%
#   kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))%>%
#   add_header_above(col_names)%>%
#   column_spec(1, bold = TRUE)%>%
#   row_spec(1:nrow(resultats), align = "c")
#   print(resultat_kable)
#  }
col_names <- c("Pas h", "Egexp_i", "Egimp_i")
resultat_kable <- list() # pour mettre tous les resultats concernant les differents pas de temps en meme tableau
for (i in 1:length(N)) {
  resultats <- data.frame(Pas = h_values[i], Eg_exp = global_errors_exp_T[i], Eg_imp = global_errors_imp_T[i])
  resultat_kable[[i]] <- resultats
}

# Combinaison de tous les dataframes en un seul
combined_result <- bind_rows(resultat_kable)

# Utilisation de kable sur le dataframe combiné
kable_table <- kable(combined_result, "html", col.names = col_names) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))

# Afficher la table
print(kable_table)

# Affichages des graphes
par(mfrow = c(2, 2))  
col_plots1=c("green","blue","pink","darkgreen")
col_plots2="red"
for (i in 1:length(N)) {
  plot(t_list[[length(N)]], sol_exact_list[[length(N)]], type = "l", col="black", xlab = "Temps", ylab = " y(t)", lwd = 3)#, ylim = c(-0.2, 1))
  lines(t_list[[i]], sol_app_list[[i]], col = col_plots1, lwd = 2,lty =i)
  lines(t_list[[i]], sol_app_imp_list[[i]], col = col_plots2, lwd = 2,lty =i)
  legend("topright", legend = c(paste("Schema explicite, pas =", h_values[i]),paste("Schema implicite, pas =", h_values[i]), "Solution Exacte"), col = c(col_plots1,col_plots2,"black"), lty = i, cex = 0.3)
}
mtext("Schema d'Euler Explicite vs Implicite pour differents pas de temps", side = 3, line = -2, outer = TRUE)
