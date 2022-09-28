## curso para estudiantes para mejorar la memoria, prueba antes y despues, calificación tiene una distribución normal
## muestra aleatoria de 10 estudiantes
## | Estudiante |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
## | Antes      | 93 | 86 | 72 | 54 | 92 | 65 | 80 | 81 | 62 | 73 |
## | Después    | 98 | 92 | 80 | 62 | 91 | 78 | 89 | 78 | 71 | 80 |

# a) el curso fue beneficioso, a un nivel de significación del 1%? Calcule el valor P

## sample before study (A)
sample_A <-  c(93 , 86 , 72 , 54 , 92 , 65 , 80 , 81 , 62 , 73)
n_A <- length(sample_A)

## sample after study (B)
sample_B <-  c(98 , 92 , 80 , 62 , 91 , 78 , 89 , 78 , 71 , 80)
n_B <- length(sample_B)

## difference A-B
sample_D <- sample_A - sample_B
n_D <- length(sample_D)
mean_D <- mean(sample_D)
sd_D <- sd(sample_D)

# H_0: meanD = 0  <--> meanA = meanB 
# H_a: meanD < 0  <--> meanA < meanB    --- calificaciones post estudio mejoran

## estimators
df_D <- n_D -1 

std_error <- sd_D / sqrt(n_D) 
t_obs <- mean_D / std_error 

# rejection zone
alpha <- 0.01

rej_inf <- qt(alpha, df_D, lower.tail = T) 


# decision rule
#  -  t_obs <= rej_inf   => reject H_0
#  -  rej_inf < t_obs    => don't reject H_0

t_obs <= rej_inf  

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pt(t_obs, df_D, lower.tail = T) 

p_value < alpha
