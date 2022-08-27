install.packages("UsingR")

library(UsingR)
data("father.son",package="UsingR")

typeof(father.son)

mean(as.data.frame(father.son)[round(father.son$fheight) == 71,]$sheight)
, function(x){range(x$fheight) == 71})
mean(father.son$sheight)

X = matrix(c(3,4,-5,1,2,2,2,-1,1,-1,5,-5,5,0,0,1), nrow = 4, byrow = TRUE)
c = matrix(c(10,5,7,4), nrow=4, byrow= TRUE)

solve(X) %*% c
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)

(a %*% b)[3,2]
a[3,] %*% b[,2]

X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")

beta <- c(5, 2)

X %*% beta

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
y = h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)

X = cbind(1,tt,tt^2)
A = solve(crossprod(X)) %*% t(X) %*% y

-2 * (A %*% y) [3]

set.seed(1)
B = 100000
g = 9.8 ## meters per second
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)

betahat = replicate(B,{
  y = 56.67  - 0.5*g*tt^2 + rnorm(n,sd=1)
  betahats = -2*A%*%y
  return(betahats[3])
})
sqrt(mean( (betahat-mean(betahat) )^2))