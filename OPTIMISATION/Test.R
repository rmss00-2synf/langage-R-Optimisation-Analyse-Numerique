# f = function(x,y){
#   n = length(x)
#   a = n/(n-1)*cov(x,y)/var(x)
#   b = mean(y)-a*mean(x)
#   return(c(a,b))
# }
# 
x = c(1,3,4,5,6,7,8,10);
y = c(0,2,4,7,8,9,8,11);
# 
# c=f(x,y)
# 
# y0 = c[1]*seq(0,10,length=8) + c[2]
# 
# plot(y0,type="l",lwd="3",xlim = c(0,11))
# points(x,y)

library(ggplot2)
# Création de données aléatoires pour l'exemple
# set.seed(123)  # Pour la reproductibilité
# x <- 1:100  # Création de valeurs pour x
# y <- 2 * x + rnorm(100, mean = 0, sd = 10)  # Relation linéaire entre x et y avec un bruit aléatoire

modele_lineaire <- lm(y ~ x)
summary(modele_lineaire)
# Création du nuage de points avec la droite de régression
graphique <- ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_point() +  # Nuage de points
  geom_smooth(method = "lm", se = FALSE) +  # Ajout de la droite de régression
  labs(title = "Régression linéaire", x = "Variable x", y = "Variable y")  # Ajout des titres des axes et du graphique

# Affichage du graphique
print(graphique)
