# # Définition de la fonction
# f <- function(x, y) {
#   return(x^2 + y^3 -2*x*y -y)
# }
# x^2 + y^2 +3*x*y -y
# # Évaluation de la fonction
# x <- seq(-2, 1, 0.1)
# y <- seq(-2, 1, 0.1)
# z <- outer(x, y, f)
# 
# # Représentation graphique de la fonction
# plot(x, z)


library(rgl)

# Création des données
x <- seq(-2, 2, length = 100)
y <- seq(-2, 2, length = 100)
z <- outer(x, y, FUN = function(x, y) exp(x - 3*y-0.1) + exp(x - 3*y - 0.1) + exp(-x - 0.1))

# Création du graphique 3D
persp3d(x, y, z, col = "skyblue", xlab = "X", ylab = "Y", zlab = "f(X,Y)", main = "Représentation 3D de f(x, y) ")
