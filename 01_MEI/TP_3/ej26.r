## comparar las características de llenado del equipo envasador de aceite refrigerador de dos destilerías diferentes
## poblaciones se distribuyen en forma normal 
##          | Dest A  | n = 12 | mean_s = 75.2 | sd_s = 0.36 |
##          | Dest B  | n = 16 | mean_s = 75.5 | sd_s = 0.40 |

# a) existe diferencia entre las variancias

## sample A
n_A <- 12
mean_A <- 75.2
sd_A <- 0.36

## sample B
n_B <- 16
mean_B <- 75.5
sd_B <- 0.4

# H_0: varA = varB  <--> varA / varB = 1 
# H_a: varA <> varB  <--> varA / varB <> 1

## estimators
f_obs <- sd_A^2 / sd_B^2

# rejection zone
alpha <- 0.05
df_A <- n_A -1 
df_B <- n_B -1 

rej_inf <- qf(alpha/2, df_A, df_B, lower.tail = T) 
rej_sup <- qf(alpha/2, df_A, df_B, lower.tail = F) 

# decision rule
#  -  f_obs <= rej_inf || f_obs >= rej_sup  => reject H_0
#  -  rej_inf < f_obs < rej_sup  => don't reject H_0

f_obs <= rej_inf || f_obs >= rej_sup

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pf(f_obs, df_A, df_B, lower.tail = T) 

p_value < alpha

# varA = varB? can't reject!!


# b) medias poblacionales iguales? nivel de significación del 5%. Calcule el valor P
# H_0: meanA = meanB  <--> meanA - meanB = 0 
# H_a: meanA <> meanB  <--> meanA - meanB <> 0

## sample A
n_A <- 12
mean_A <- 75.2
sd_A <- 0.36

## sample B
n_B <- 16
mean_B <- 75.5
sd_B <- 0.4

## estimators
df_A <- n_A -1 
df_B <- n_B -1 

sd_mixed <- sqrt( ( (sd_A^2 * df_A) + (sd_B^2 * df_B) ) / (n_A + n_B - 2) )
std_error <- sd_mixed * sqrt( (1/n_A) + (1/n_B) ) 
t_obs <- (mean_A - mean_B) / std_error 

# rejection zone
alpha <- 0.05

rej_inf <- qt(alpha/2, df_A + df_B, lower.tail = T) 
rej_sup <- qt(alpha/2, df_A + df_B, lower.tail = F) 


# decision rule
#  -  t_obs <= rej_inf || t_obs >= rej_sup  => reject H_0
#  -  rej_inf < t_obs < rej_sup  => don't reject H_0

t_obs <= rej_inf || t_obs >= rej_sup 

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pt(t_obs, df_A + df_B, lower.tail = T) * 2

p_value < alpha


# c) intervalo de confianza del 95% para la diferencia verdadera entre los contenidos medio poblacional; resultado compatible con b)?
## t-score
alpha <- 0.05
t_score_inf <- qt(alpha/2, df_A + df_B, lower.tail = T) 
t_score_sup <- qt(alpha/2, df_A + df_B, lower.tail = F) 

#  confidence interval
lower_bound <- (mean_A - mean_B) + t_score_inf * std_error  
upper_bound <- (mean_A - mean_B) + t_score_sup * std_error

print(c(lower_bound, upper_bound))

# meanA - meanB = 0 --> lower_bound < 0 < upper_bound

# b) <--> c)
