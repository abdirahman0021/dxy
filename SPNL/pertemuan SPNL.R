F <- function(x){
  c(
    x[1]^2+x[2]^2-4,
    x[1]^2-x[2]-1
  )
}


J <- function(x){
  matrix(
    c(
      2*x[1], 2*x[2],
      2*x[1], -1
    ),
    nrow=2,
    byrow = TRUE
  )
}


x<- c(1,1)

F(x)

J(x)

delta <- solve(
  J(x),
  -F(x)
)

delta

x_new <- x +delta


#IMPLEMENTASI
newton_spnl <- function(F, J, x0,
                        tol = 1e-8,
                        max_iter = 100) {
  x <- x0
  
  for(k in 1:max_iter) {
    
    Fx <- F(x)
    Jx <- J(x)
    
    delta <- solve(Jx, -Fx)
    
    x_new <- x + delta
    
    if (sqrt(sum(F(x_new)^2)) < tol) {
      return(list(
        solution = x_new,
        iterations = k,
        converged = TRUE
      ))
    }
    x <- x_new
  }
  
  list(
    solution = x,
    iterations = max_iter,
    converged = FALSE
  )
}

hasil <-newton_spnl(
  F=F,
  J=J,
  x0=c(1,1)
)

hasil$solution
hasil$iterations
hasil$converged
