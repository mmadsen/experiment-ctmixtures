library(Rglpk)
library(ggplot2)

dim <- 2

N1 <- 5
N2 <- 5

# the points of sets A and B
P <- matrix(
  runif(dim*N1 + dim*N2,0,1),
  ncol=dim,byrow=T
)

# the matrix A defining the lhs of the conditions
A <- cbind(P * c(rep(-1,N1),rep(1,N2)), c(rep(1,N1),rep(-1,N2)))

# the objective function - no optimization necessary
obj <- rep(0, dim+1)

# the vector b defining the rhs of the conditions
b <- rep(-1, N1+N2)

# by default GLPK assums positive boundaries for the
# variables. but we need the full set of real numbers.
bounds <- list(
  lower = list(ind = 1:(dim+1), val = rep(-Inf, dim+1)),
  upper = list(ind = 1:(dim+1), val = rep(Inf, dim+1))
)

# solving the linear program
s <- Rglpk_solve_LP(obj, A, rep("<=", N1+N2), b, bounds=bounds)

df <- as.data.frame(P)


par(pty="s")
p <- plot(P,col=c(rep("red",N1),rep("blue",N2)), xlab="x", ylab="y", 
     cex=1, pch=16, xlim=c(0,1), ylim=c(0,1))

p

plt <- ggplot()




# status 0 means that a solution was found
if(s$status == 0) {
  h1 = s$solution[1]
  h2 = s$solution[2]
  beta = s$solution[3]
  
  # drawing the separating line
  if(h2 != 0) {
    abline(beta/h2,-h1/h2)
  } else {
    abline(v=-beta/h1)
  }
} else {
  cat("Not linearly separable.")
}