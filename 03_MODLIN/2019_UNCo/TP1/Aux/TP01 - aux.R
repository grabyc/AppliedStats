V <- matrix(c(6,2,3,2,15,6,3,6,3),nrow=3,byrow=TRUE)
D <- diag(diag(V))
D_inv <- sqrt(solve(D))
D_raiz <- sqrt(D)

R <-D_inv %*% V %*% D_inv
Sigma <- D_raiz %*% R %*% D_raiz

I_3 <- diag(nrow = 3)

require(devtools)
install_github("markheckmann/mat2tex", force=TRUE)

W_12 <- matrix(c(1,1),nrow=1,byrow=FALSE)
W_22 <- matrix(c(3,0,0,4),nrow=2,byrow=TRUE)
W_12 %*% solve(W_22)

mu <- matrix(c(0,0,0),nrow=3,byrow=TRUE)
S <- matrix(c(3,1,0,1,2,1,0,1,4),nrow=3,byrow=TRUE)
S <- S[c(2,1,3),c(2,1,3)]

Y1 <- c(2)
Y2 <-  c(1,3)
mu_Y1 <- mu[Y1] 
mu_Y2 <- mu[Y2] 

data <- read_excel("calefaccion.xlsx")

S_11 <- S[Y1,Y1]
S_12 <- S[Y1,Y2]
S_21 <- S[Y2,Y1]
S_22 <- S[Y2,Y2]

mu_Y1_Y2 <- round(mu_Y1 + S_12 %*% solve(S_22), 3)
S_Y1_Y2 <- round(S_11 - S_12 %*% solve(S_22) %*% S_21, 3)

#######################
## ejemplos

# matrices auxiliares
m_unos <-  function(x) {
  matrix(rep(1, times = x), ncol = 1, byrow = FALSE)
}

m_jota <- function(x){
  uno <- m_unos(x)
  uno %*% t(uno)
}

m_hat <- function(x){
  I <- diag(x)
  J <- m_jota(x)
  
  I - (1/x) * J
}


# datos calefaccion
calef <- matrix(c(1.67,-1.67,2.22,15.56,18.33,-1.11,-12.22,-13.89,-6.11,12.78,
                  12.22,8.89,-6.67,3.89,15.56,-6.67,14.44,4.44,-2.78,-1.11,7.62,
                  10.16,17.78,15.24,12.7,12.7,15.24,25.4,22.86,5.08,30.48,12.7,
                  12.7,10.16,20.32,12.7,17.78,20.32,22.86,17.78,
                  6,10,3,9,6,5,7,10,11,5,4,1,15,7,6,8,3,11,8,5),ncol=3,byrow=FALSE)

n <- nrow(calef)
X <- calef

# datos acciones
stock <- matrix(c(1, 3.4, 89.7, 30.2, 2, 5.1, 55.7, 9.9, 3, 4.5, 52.3, 11.5, 
                  4, 3.5, 47, 11.2, 5, 5.9, 42.7, 7, 6, 5.1, 30.6, 6.9), ncol=4, byrow =  TRUE)
colnames(stock) <- c("Obs", "X1", "X2", "X3")
n <- nrow(stock)
X <- stock

# datos alcornoques
alcornoques <- matrix(c(72, 66, 76, 77, 60, 53, 66, 63, 56, 56, 64, 58, 41, 29, 36, 38, 32, 32, 35, 36,
                        30, 35, 34, 26, 39, 39, 31, 27, 42, 43, 21, 25, 37, 40, 31, 25, 33, 29, 27, 36,
                        32, 30, 34, 28, 63, 45, 74, 63, 54, 46, 60, 52, 47, 51, 52, 43, 91, 79, 100, 75,
                        56, 68, 47, 50, 79, 65, 70, 61, 81, 80, 68, 58, 78, 55, 67, 60, 46, 38, 37, 38,
                        39, 35, 34, 37, 32, 30, 30, 32, 60, 50, 67, 54, 35, 37, 48, 39, 39, 36, 39, 31,
                        50, 34, 37, 40, 43, 37, 39, 50, 48, 54, 57, 43), ncol=4, byrow =  TRUE)
colnames(alcornoques) <- c("N", "E", "S", "W")
n <- nrow(alcornoques)
X <- alcornoques

n <- 4

#matriz de centrado
unos <- m_unos(n)
J <- m_jota(n)
H <- m_hat(n)

X_means <- (1/n) * t(unos) %*% X
colMeans(X)

X_centro <- H %*% X
X_cov <- (1/n) * t(X) %*% H %*% X
X_cov_2 <- (1/(n-1)) * t(X) %*% H %*% X
cov(X)

D_int <- diag(diag(X_cov_2))
D <- sqrt(solve(D_int))
R <- D %*% X_cov %*% D
cov2cor(cov(X))
