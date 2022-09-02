ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## Un torno automático produce pieza con longitud X (X es variable aleatoria con distribución normal con sd = 0.12mm)

# b) alpha = 5% 

# H_0 (está bien ajustado): mu = 20mm 
# H_a (posición incorrecta): mu <> 20mm
# muestra aleatoria de 9 piezas x_muestral = 20.04mm

# population
#   - normal distrib
mu <- 20
sd <- 0.12

# sample
n <- 9
mean_s <- 20.04

# rejection zone
alpha <- 0.05

norm_score_inf <- qnorm(alpha/2, 0, 1, lower.tail = T) 
norm_score_sup <- qnorm(alpha/2, 0, 1, lower.tail = F) 

# validations
# pnorm(norm_score_inf, 0, 1, lower.tail = T) ¿<> alpha/2?
# pnorm(norm_score_sup, mu, sd, lower.tail = F) ¿<> alpha/2?

# estimator
std_error <- sd/sqrt(n)
z_obs <- (mean_s - mu) / std_error

# decision rule
#  -  z_obs <= norm_score_inf || z_obs >= norm_score_sup  => reject H_0
#  -  norm_score_inf < z_obs < norm_score_sup  => don't reject H_0
z_obs <= norm_score_inf || z_obs >= norm_score_sup

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pnorm(z_obs, 0, 1, lower.tail = F) 

p_value < alpha

# graphical representation
x_inf <- -3
x_sup <- 3
mu <- 0
sd <- 1
events <- seq(x_inf,x_sup,0.05)
df <- data.frame(events, density = dnorm(x = events, mu, sd))  

ggplot(df, aes(x = events)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = sd
                ),
                geom = "line") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = sd
                ),
                geom = "area",
                alpha = 0.8,
                xlim = c(norm_score_sup,x_sup)) + 
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = sd
                ),
                geom = "area",
              alpha = 0.8,
              xlim = c(x_inf,norm_score_inf)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = sd
                ),
                geom = "area",
                alpha = 0.6,
                fill = "steelblue",
                xlim = c(z_obs,x_sup)) + 
  geom_segment(aes(x = z_obs, xend = z_obs, y = 0, yend = dnorm(z_obs, mu, sd)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("P-value = ", round(p_value,5)), collapse = ""), x = z_obs, y = dnorm(z_obs, mu, sd) + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Hypothesis: mu = 20",
       subtitle = "alpha = 5%",
       x = "z_score",
       y = "Density")



# c) mu_real = 19.95mm ; prob de aceptar H_0?

# H_0 (está bien ajustado): mu = 20mm 
# H_a (posición incorrecta): mu = 19.95mm
# muestra aleatoria de 9 piezas x_muestral = 20.04mm

# e) potencia de la prueba