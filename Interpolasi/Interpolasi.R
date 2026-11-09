# --- 1. Visualisasi Konsep Dasar Interpolasi Linear ---
x <- c(1, 3)
y <- c(2, 6)

plot(
  x, y,
  pch = 19,
  xlab = "x",
  ylab = "f(x)",
  main = "Interpolasi Linear"
)
abline(lm(y ~ x), lwd = 2)
points(2, 4, pch = 19)
text(1, 2, "(1, 2)", pos = 2)
text(3, 6, "(3, 6)", pos = 4)
text(2, 4, "(2, 4)", pos = 3)

# --- 2. Perhitungan Manual Interpolasi Linear ---
x0 <- 1
x1 <- 3
y0 <- 2
y1 <- 6
x <- 2

y_manual <- y0 + (y1 - y0) / (x1 - x0) * (x - x0)
y_manual

# --- 3. Definisi Fungsi Reusable Interpolasi Linear ---
interpolasi_linear <- function(x0, y0, x1, y1, x) {
  y <- y0 + (y1 - y0) / (x1 - x0) * (x - x0)
  return(y)
}

# Pengujian fungsi
interpolasi_linear(x0 = 1, y0 = 2, x1 = 3, y1 = 6, x = 2)