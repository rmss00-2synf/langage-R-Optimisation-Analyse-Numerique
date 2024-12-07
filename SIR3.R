# Function to define the differential equation dy/dt = f(t, y)
f <- function(t, y) {
  return(-y)
}

# Function to perform sixth-order Runge-Kutta integration
runge_kutta_sixth_order <- function(f, y0, t0, t_end, h) {
  times <- seq(t0, t_end, by = h)
  y <- numeric(length(times))
  y[1] <- y0
  
  for (i in 2:length(times)) {
    k1 <- h * f(times[i - 1], y[i - 1])
    k2 <- h * f(times[i - 1] + 1/4 * h, y[i - 1] + 1/4 * k1)
    k3 <- h * f(times[i - 1] + 1/2 * h, y[i - 1] + 1/8 * k1 + 1/8 * k2)
    k4 <- h * f(times[i - 1] + 3/4 * h, y[i - 1] - 1/2 * k2 + k3)
    k5 <- h * f(times[i - 1] + h, y[i - 1] + 3/16 * k1 - 3/8 * k2 + 3/8 * k3 + 9/16 * k4)
    k6 <- h * f(times[i - 1] + 1/2 * h, y[i - 1] - 3/7 * k1 + 2/7 * k2 + 12/7 * k3 - 12/7 * k4 + 8/7 * k5)
    
    y[i] <- y[i - 1] + 1/90 * (7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k6)
  }
  
  return(data.frame(time = times, value = y))
}

# Initial conditions
y0 <- 1.0
t0 <- 0
t_end <- 5
h <- 0.1

# Perform sixth-order Runge-Kutta integration
result <- runge_kutta_sixth_order(f, y0, t0, t_end, h)

# Plot the results
plot(result$time, result$value, type = "l", col = "blue", xlab = "Time", ylab = "y", main = "Runge-Kutta 6th Order")