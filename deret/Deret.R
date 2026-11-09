# 1. Definisi fungsi aproksimasi deret e^x
exp_series <- function(x, n) {
  result <- 0
  for (k in 0:n) {
    result <- result + x^k / factorial(k)
  }
  return(result)
}

# 2. Evaluasi konvergensi (n = 0 sampai 10)
x <- 0.5
n <- 0:10

approximation <- sapply(
  n,
  function(k) exp_series(x, k)
)

# 3. Perhitungan galat absolut dibanding nilai eksak exp(x)
true_value <- exp(x)
error <- abs(true_value - approximation)

# Menampilkan tabel hasil aproksimasi dan galat
data.frame(
  n = n,
  approximation = approximation,
  error = error
)

# 4. Membuat grafik konvergensi galat (skala logaritmik)
plot(
  n,
  error,
  type = "b",
  log = "y",
  xlab = "Orde polinom",
  ylab = "Galat absolut",
  main = "Konvergensi Deret Maclaurin untuk exp(x)"
)