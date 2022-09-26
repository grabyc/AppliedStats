## muestra aleatoria de 12 calificaciones tomadas de una población normal
##     6.71 5.90 7.12 7.20 6.83 6.05 7.26 7.45 6.66 7.16 5.64 7.05

# a) intervalo del 90 % de confianza para la media poblacional

## sample
sample <- c(6.71,5.90,7.12,7.20,6.83,6.05,7.26,7.45,6.66,7.16,5.64,7.05)
n <- length(sample)

## stimators
mean_s <- mean(sample)
sd_s <- sd(sample)

## t-score
alpha <- 0.10
df <- n -1
t_score_inf <- qt(alpha/2, df, lower.tail = T) 
t_score_sup <- qt(alpha/2, df, lower.tail = F) 

#  confidence interval
std_error <- sd_s / sqrt(n)

lower_bound <- mean_s + t_score_inf * std_error  
upper_bound <- mean_s + t_score_sup * std_error

print(c(lower_bound, upper_bound))

# b) intervalo del 95% de confianza para la varianza poblacional y para el desvío estándar poblacional.

#  var

## sample
sample <- c(6.71,5.90,7.12,7.20,6.83,6.05,7.26,7.45,6.66,7.16,5.64,7.05)
n <- length(sample)

## stimators
var_s <- var(sample)

## chisq-score
alpha <- 0.05
df <- n -1
chisq_score_inf <- qchisq(alpha/2, df, lower.tail = T) 
chisq_score_sup <- qchisq(alpha/2, df, lower.tail = F) 

#  confidence interval
lower_bound <- df * var_s / chisq_score_inf  
upper_bound <- df * var_s / chisq_score_sup

print(c(upper_bound, lower_bound))

#  sd

print(c(sqrt(upper_bound), sqrt(lower_bound)))
