## comparar las actitudes de dos grupos de estudiantes de medicina
## poblaciones se distribuyen en forma normal y que las varianzas se pueden considerar iguales 
##          | Grupo A  | n = 10 | mean_s = 60.3 | sd_s = 5.5 |
##          | Grupo B  | n = 15 | mean_s = 67.2 | sd_s = 4.1 |

# a) intervalo del 95% de confianza para la diferencia de medias poblacionales

## sample A
n_A <- 10
mean_A <- 60.3
sd_A <- 5.5

## sample B
n_B <- 15
mean_B <- 67.2
sd_B <- 4.1

# pre-req: var_A = var_B?

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


## t-score
alpha <- 0.05
t_score_inf <- qt(alpha/2, df_A + df_B, lower.tail = T) 
t_score_sup <- qt(alpha/2, df_A + df_B, lower.tail = F) 

#  confidence interval
sd_mixed <- sqrt( ( (sd_A^2 * df_A) + (sd_B^2 * df_B) ) / (n_A + n_B - 2) )
std_error <- sd_mixed * sqrt( (1/n_A) + (1/n_B) ) 

lower_bound <- (mean_A - mean_B) + t_score_inf * std_error  
upper_bound <- (mean_A - mean_B) + t_score_sup * std_error

print(c(lower_bound, upper_bound))
