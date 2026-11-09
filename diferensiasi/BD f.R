bw_diff <- function(f, x, h){
  (f(x)-f(x-h))/h
}

f <- function(x){
  x^2
}
bw_diff(f, 2, 0.1)