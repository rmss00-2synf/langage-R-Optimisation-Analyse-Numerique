# x <- runif(12); y <- runif(12)
#  i <- order(x,y); x <- x[i]; y <- y[i]
#  plot(x,y)
#  s <- seq(length(x)-1)
#  arrows


# library(ggplot2)
# 
# # Exemple de données
# data <- data.frame(
#   x = c(1, 2, 3),
#   y = c(2, 4, 1)
# )
# 
# # Création du graphique avec geom_point et geom_segment
# courbe = ggplot(data, aes(x = x, y = y)) +
#   geom_point() +
#   geom_segment(aes(x = x[1], y = y[1], xend = x[2], yend = y[2]),
#                arrow = arrow(length = unit(0.1, "inches")), color = "blue") +
#   geom_segment(aes(x = x[2], y = y[2], xend = x[3], yend = y[3]),
#                arrow = arrow(length = unit(0.1, "inches")), color = "red")+
#   theme_minimal()
# 

library(ggplot2)

# Exemple de données
grid <- expand.grid(x = seq(0, 10, length.out = 100), y = seq(0, 10, length.out = 100))
grid$z <- with(grid, sin(x) + cos(y))

x_values <- c(3, 7, 9)
y_values <- c(2, 5, 8)

# Création du graphique avec geom_contour et geom_point
contour_p <- ggplot(grid, aes(x, y)) +
  geom_contour(aes(z = z), bins = 10) +
  geom_point(data = data.frame(x = x_values, y = y_values), aes(x, y), color = "red", size = 2) +
  labs(title = " ", x = "x", y = "y") +
  theme_minimal()

# Ajout des flèches avec geom_segment
arrows_data <- data.frame(
  x_start = x_values[-length(x_values)],
  y_start = y_values[-length(y_values)],
  x_end = x_values[-1],
  y_end = y_values[-1]
)

contour_p + 
  geom_segment(data = arrows_data, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.1, "inches")), color = "blue")


print(contour_p)
