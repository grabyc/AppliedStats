ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("readxl", "dplyr", "ggplot2")
ipak(packages)

## resistencia a la flexión de dos muestras independientes de 
##   muestra A: 20 juntas seleccionadas al azar sin recubrimiento y 
##   muestra B: 25 juntas seleccionadas al azar con recubrimiento
## archivo resistenciaflexion.xlxs  

data <- read.csv("./01_MEI/TP_3/Data/resistenciaflexion.csv", header = T, sep = ";", dec = ",")

# sample A - sin recubr
sample_A <- data[data$tipo.junta == 'sin recubrimeinto',1]
n_A <- length(sample_A)
mean_A <- mean(sample_A)
sd_A <- sd(sample_A)

# sample B - con recubr
sample_B <- data[data$tipo.junta == 'con recubrimiento',1]
n_B <- length(sample_B)
mean_B <- mean(sample_B)
sd_B <- sd(sample_B)

# a) resistencia a la flexión tiene distribución normal según tipo de junta

# como el tamaño de la muestra n<30, no puede aplicarse Teorema Central del Limite y asumir normalidad
# por lo tanto, se debe hacer una prueba de hipotesis de normalidad (p.ej. Shapiro-Wilk)

shapiro.test(sample_A)  ## don't reject  --> is normal
shapiro.test(sample_B)  ## don't reject  --> is normal


# b) intervalo de confianza del 95% para la resistencia promedio verdadera para cada tipo de junta

alpha <- 0.05

norm_score_inf <- qnorm(alpha/2, 0, 1, lower.tail = T) 
norm_score_sup <- qnorm(alpha/2, 0, 1, lower.tail = F) 

# confidence interval A
std_error_A <- sd_A / sqrt(n_A) 

lower_bound_A <- mean_A + norm_score_inf * std_error_A  
upper_bound_A <- mean_A + norm_score_sup * std_error_A

print(c(lower_bound_A, upper_bound_A))

# confidence interval B
std_error_B <- sd_B / sqrt(n_B) 

lower_bound_B <- mean_B + norm_score_inf * std_error_B  
upper_bound_B <- mean_B + norm_score_sup * std_error_B

print(c(lower_bound_B, upper_bound_B))

# c) intervalo de confianza del 95% para la diferencia entre las resistencias promedio verdaderas de los dos tipos de juntas

# pre-req: var_A = var_B?
## two methods
##   - 1) hypothesis test
##   - 2) F test to compare two variances

## method 1
# H_0: varA = varB  <--> varA / varB = 1 
# H_a: varA <> varB  <--> varA / varB <> 1

# estimators
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

## method 2
# F-test is very sensitive to departure from the normal assumption
# normality tests in a) eliminate this risk 

var.test(sample_A, sample_B, alternative = "two.sided") ## don't reject  --> same variances

# dif means
## two methods
##   - 1) t-score confidence interval
##   - 2) two unpaired samples t test

## method 1
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

## method 2
## pre-reqs
##   - normality of both samples --> both normals a)
##   - same variances  --> same variances c) F test

res <- t.test(sample_A, sample_B, alternative = "two.sided", var.equal = T)
res$conf.int

# d) probar que la resistencia de las juntas es mayor cuando no tienen recubrimiento lateral; nivel de significación del 0,05

## two methods
##   - 1) t-score hypothesis test
##   - 2) two unpaired samples t test

# method 1
# H_0: mean_A >= mean_B  <--> resistencia es mayor sin recubr 
# H_a: mean_A < mean_B  <--> resistencia es menor sin recubr

## estimators
t_obs <- (mean_A - mean_B) / std_error 

# rejection zone
alpha <- 0.05

rej_inf <- qt(alpha, df_A + df_B, lower.tail = T) 


# decision rule
#  -  t_obs <= rej_inf || t_obs >= rej_sup  => reject H_0
#  -  rej_inf < t_obs < rej_sup  => don't reject H_0

t_obs <= rej_inf 

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pt(t_obs, df_A + df_B, lower.tail = T) 

p_value < alpha

# method 2
t.test(sample_A, sample_B, alternative = "less", var.equal = T)
