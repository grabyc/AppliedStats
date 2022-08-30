## muestra aleatoria de 25 mediciones tomadas de una población normal arroja como resultado una media igual a 300

## constants
n <- 25
mean <- 300
alpha <- 0.05
norm_score <- qnorm(alpha/2, 0, 1, lower.tail = F) 

# a) sd(x) = 20
sd <- 20

std_error <- sd/sqrt(n)
margin_error <- norm_score * std_error

lower_bound <- mean - margin_error  
upper_bound <- mean + margin_error  

# b) sd(x) = 40
sd <- 40

std_error <- sd/sqrt(n)
margin_error <- norm_score * std_error

lower_bound <- mean - margin_error  
upper_bound <- mean + margin_error  

# c) sd(x) = 80
sd <- 80

std_error <- sd/sqrt(n)
margin_error <- norm_score * std_error

lower_bound <- mean - margin_error  
upper_bound <- mean + margin_error  

