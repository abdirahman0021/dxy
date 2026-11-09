# 1. Definisi fungsi Interpolasi Lagrange
lagrange <- function(x_data, y_data, x) {
  n <- length(x_data)
  result <- 0
  for (i in 1:n) {
    L <- 1
    for (j in 1:n) {
      if (j != i) {
        L <- L * (x - x_data[j]) / (x_data[i] - x_data[j])
      }
    }
    result <- result + y_data[i] * L
  }
  return(result)
}

# 2. Pengujian fungsi pada data
x_data <- c(1, 2, 4)
y_data <- c(2, 3, 5)
lagrange(x_data, y_data, x = 3)

# 3. Visualisasi Kurva Polinom Lagrange
x_grid <- seq(min(x_data), max(x_data), length.out = 300)
y_grid <- sapply(x_grid, function(x) lagrange(x_data, y_data, x))

plot(
  x_grid, y_grid,
  type = "l",
  lwd = 2,
  xlab = "x",
  ylab = "P(x)",
  main = "Interpolasi Polinomial Lagrange"
)
points(x_data, y_data, pch = 19)
points(3, lagrange(x_data, y_data, 3), pch = 19)