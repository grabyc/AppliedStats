## proporción de cigüeñales de motor defectuosos 
## muestra aleatoria de 800 cigueñales y 80 defectuosos de proveedor A

# a) intervalo del 95% de confianza para la proporción poblacional de cigüeñales defectuosos

## sample
fail <-  80
n <- 800

## estimators
p_s <- fail / n

## z-score
alpha <- 0.05
z_score_inf <- qnorm(alpha/2, 0, 1, lower.tail = T) 
z_score_sup <- qnorm(alpha/2, 0, 1, lower.tail = F) 

#  confidence interval
std_error <- sqrt( (p_s * (1 - p_s)) / n )

lower_bound <- p_s + z_score_inf * std_error  
upper_bound <- p_s + z_score_sup * std_error

print(c(lower_bound, upper_bound))

# b) muestra aleatoria de 200 cigueñales y 30 defectuosos de proveedor B; comparar prov A con prov a un nivel de significacion del 5%

# H_0: pA = pB  <--> pA - pB = 0 
# H_a: pA <> pB  <--> pA - pB <> 0

## sample A
fail_A <-  80
n_A <- 800
p_A <- fail_A / n_A

## sample B
fail_B <-  30
n_B <- 200
p_B <- fail_B / n_B

## estimators
p_hat <- (fail_A + fail_B) / (n_A + n_B)
std_error <- p_hat * (1 - p_hat) * sqrt( 1/n_A + 1/n_B)
z_obs <- (p_A - p_B) / std_error 

# rejection zone
alpha <- 0.05

rej_inf <- qnorm(alpha/2, 0, 1, lower.tail = T) 
rej_sup <- qnorm(alpha/2, 0, 1, lower.tail = F) 


# decision rule
#  -  z_obs <= rej_inf || z_obs >= rej_sup  => reject H_0
#  -  rej_inf < z_obs < rej_sup  => don't reject H_0

z_obs <= rej_inf || z_obs >= rej_sup

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pnorm(z_obs, 0, 1, lower.tail = T) 

p_value < alpha
