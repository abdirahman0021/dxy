# 1. Fungsi pembentuk tabel Divided Differences
divided_difference <- function(x, y) {
  n <- length(x)
  table <- matrix(0, nrow = n, ncol = n)
  table[, 1] <- y
  
  for (j in 2:n) {
    for (i in 1:(n - j + 1)) {
      table[i, j] <- (table[i + 1, j - 1] - table[i, j - 1]) / (x[i + j - 1] - x[i])
    }
  }
  return(table)
}

# 2. Fungsi Evaluasi Polinom Newton
newton <- function(x_data, y_data, x) {
  n <- length(x_data)
  dd <- divided_difference(x_data, y_data)
  result <- dd[1, 1]
  product <- 1
  
  if (n >= 2) {
    for (j in 2:n) {
      product <- product * (x - x_data[j - 1])
      result <- result + dd[1, j] * product
    }
  }
  return(result)
}

# Pengujian
x_data <- c(1, 2, 4)
y_data <- c(2, 3, 5)

# Tampilkan tabel divided difference
divided_difference(x_data, y_data)

# Tampilkan hasil estimasi nilai pada x = 3
newton(x_data, y_data, x = 3)