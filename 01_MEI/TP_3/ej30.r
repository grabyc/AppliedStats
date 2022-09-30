ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("readxl", "dplyr", "ggplot2")
ipak(packages)

## desviación estándar de la tensión a la ruptura es de 15 lb como máximo
## archivo tensionruptura.xlxs  -- muestra aleatoria de 16 piezas

# a) test de hipotesis 

# H_0: sigma^2 <= 15^2 lb     <-->  sd <= 15 lb 
# H_a: sigma^2 > 15^2 lb     <-->  sd > 15 lb

# population
sd <- 15

# sample
data <- read.csv("./01_MEI/TP_3/Data/tensionruptura.csv", header = T, sep = "", dec = ",")
n <- nrow(data)
var_s <- var(data$tensionruptura)

# rejection zone
alpha <- 0.05
df <- n -1 

chisq_score_sup <- qchisq(alpha,df, lower.tail = F) 

# estimator
chisq_obs <- (var_s * (n - 1)) / sd^2

# decision rule
#  -  chisq_obs >= chisq_score_sup  => reject H_0
#  -  chisq_obs < chisq_score_sup  => don't reject H_0
chisq_obs >= chisq_score_sup

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pchisq(chisq_obs, df, lower.tail = F) 

p_value < alpha

# b) supuestos necesarios para que la prueba anterior sea estadísticamente correcta

# distrib poblacional de donde se obtiene la muestra --> normal

# como el tamaño de la muestra n<30, no puede aplicarse Teorema Central del Limite y asumir normalidad
# por lo tanto, se debe hacer una prueba de hipotesis de normalidad (p.ej. Shapiro-Wilk)

shapiro.test(data$tensionruptura)

# c) intervalo de confianza del 95% para la tensión media a la ruptura

# sample
mean_s <- mean(data$tensionruptura)
sd_s <- sqrt(var_s)

## z-score
alpha <- 0.05
std_error <- sd / sqrt(n) 

norm_score_inf <- qnorm(alpha/2, 0, 1, lower.tail = T) 
norm_score_sup <- qnorm(alpha/2, 0, 1, lower.tail = F) 

#  confidence interval
lower_bound <- mean_s + norm_score_inf * std_error  
upper_bound <- mean_s + norm_score_sup * std_error

print(c(lower_bound, upper_bound))
