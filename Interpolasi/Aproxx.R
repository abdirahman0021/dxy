# --- 1. Penggunaan Dasar approx() ---
x <- c(1, 2, 4, 5)
y <- c(2, 3, 5, 8)

# Estimasi 1 titik
approx(x, y, xout = 3)

# Estimasi beberapa titik sekaligus
approx(x, y, xout = c(1.5, 2.5, 3, 3.5))

# --- 2. Aplikasi Interpolasi Data Suhu ---
waktu <- c(8, 10, 12, 14, 16)
suhu <- c(24.0, 27.0, 30.5, 32.0, 31.0)

# Estimasi suhu pukul 11.00
approx(waktu, suhu, xout = 11)

# Visualisasi grafik data suhu dan kurva interpolasi
plot(
  waktu, suhu,
  pch = 19,
  xlab = "Waktu",
  ylab = "Suhu (°C)",
  main = "Interpolasi Data Suhu"
)

x_grid <- seq(min(waktu), max(waktu), length.out = 300)
y_grid <- approx(waktu, suhu, xout = x_grid)$y

lines(x_grid, y_grid, lwd = 2)
points(11, approx(waktu, suhu, xout = 11)$y, pch = 19)