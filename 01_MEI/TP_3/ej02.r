## muestra aleatoria de n observaciones tomadas de una población normal con sd = 4 arroja como resultado una media igual a 33

# d) estimación de mu mediante intervalos del 95% de confianza

## constants
mean <- 33
sd <- 4
alpha <- 0.05
norm_score <- qnorm(alpha/2, 0, 1, lower.tail = F) 

#  - i) n=5
n <- 5

std_error <- sd/sqrt(n)
margin_error <- norm_score * std_error

lower_bound <- mean - margin_error  
upper_bound <- mean + margin_error  

range_i <- upper_bound - lower_bound

#  - ii) n=15
n <- 15

std_error <- sd/sqrt(n)
margin_error <- norm_score * std_error

lower_bound <- mean - margin_error  
upper_bound <- mean + margin_error  

range_ii <- upper_bound - lower_bound

#  - iii) n=25
n <- 25

std_error <- sd/sqrt(n)
margin_error <- norm_score * std_error

lower_bound <- mean - margin_error  
upper_bound <- mean + margin_error  

range_iii <- upper_bound - lower_bound

# e) precisión de los intervalos obtenidos

#  - i) n=5
precision <- range_i / 2

#  - ii) n=15
precision <- range_ii / 2

#  - iii) n=25
precision <- range_iii / 2
