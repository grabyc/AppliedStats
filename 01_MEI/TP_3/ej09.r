ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## resistencia al rompimiento de una fibra textil X (X es variable aleatoria con distribución normal con mu = 150psi y sd = 4.08psi)

# a) hipótesis nula y alternativa adecuada

# H_0 (está bajo control): mu = 150psi 
# H_a (fuera de control): mu <> 150psi

# b) alpha = 5% 

# muestra aleatoria de 16 piezas 

# population
#   - normal distrib
mu <- 150
sd <- 4.08

# sample
n <- 16
std_error <- sd/sqrt(n)

# rejection zone
alpha <- 0.05

rej_inf <- qnorm(alpha/2, mu, std_error, lower.tail = T) 
rej_sup <- qnorm(alpha/2, mu, std_error, lower.tail = F) 


# decision rule
#  -  z_obs <= rej_inf || z_obs >= rej_sup  => reject H_0
#  -  rej_inf < z_obs < rej_sup  => don't reject H_0

# graphical representation
x_inf <- mu - std_error * 3
x_sup <- mu + std_error * 3

events <- seq(x_inf,x_sup,0.01)
df <- data.frame(events, density = dnorm(x = events, mu, std_error))  

ggplot(df, aes(x = events)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "line") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.8,
                xlim = c(rej_sup,x_sup)) + 
  geom_segment(aes(x = rej_sup, xend = rej_sup, y = 0, yend = dnorm(rej_sup, mu, std_error)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("alpha/2 = ", round(rej_sup,5)), collapse = ""), x = rej_sup, y = dnorm(rej_sup, mu, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
              alpha = 0.8,
              xlim = c(x_inf,rej_inf)) +
  geom_segment(aes(x = rej_inf, xend = rej_inf, y = 0, yend = dnorm(rej_inf, mu, std_error)), 
             lty = "dashed") + 
  geom_text(
    aes(label = paste(c("-alpha/2 = ", round(rej_inf,5)), collapse = ""), x = rej_inf, y = dnorm(rej_inf, mu, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) + 
  labs(title = "Hypothesis: mu = 150 ",
       subtitle = "alpha = 5%",
       x = "Res (psi)",
       y = "Density")

# c) test con mean_s = 152.18psi

# population
#   - normal distrib
mu <- 150
sd <- 4.08

# sample
n <- 16
mean_s  <-  152.18
std_error <- sd/sqrt(n)

# rejection zone
alpha <- 0.05

rej_inf <- qnorm(alpha/2, mu, std_error, lower.tail = T) 
rej_sup <- qnorm(alpha/2, mu, std_error, lower.tail = F) 


# decision rule
#  -  z_obs <= rej_inf || z_obs >= rej_sup  => reject H_0
#  -  rej_inf < z_obs < rej_sup  => don't reject H_0

mean_s <= rej_inf || mean_s >= rej_sup

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pnorm(mean_s, mu, std_error, lower.tail = F) 

p_value < alpha

# graphical representation
x_inf <- mu - std_error * 3
x_sup <- mu + std_error * 3

events <- seq(x_inf,x_sup,0.01)
df <- data.frame(events, density = dnorm(x = events, mu, std_error))  

ggplot(df, aes(x = events)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "line") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.8,
                xlim = c(rej_sup,x_sup)) + 
  geom_segment(aes(x = rej_sup, xend = rej_sup, y = 0, yend = dnorm(rej_sup, mu, std_error)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("alpha/2 = ", round(rej_sup,5)), collapse = ""), x = rej_sup, y = dnorm(rej_sup, mu, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.8,
                xlim = c(x_inf,rej_inf)) +
  geom_segment(aes(x = rej_inf, xend = rej_inf, y = 0, yend = dnorm(rej_inf, mu, std_error)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("-alpha/2 = ", round(rej_inf,5)), collapse = ""), x = rej_inf, y = dnorm(rej_inf, mu, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) + 
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.6,
                fill = "steelblue",
                xlim = c(mean_s,x_sup)) + 
  geom_segment(aes(x = mean_s, xend = mean_s, y = 0, yend = dnorm(mean_s, mu, std_error)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("sample mean = ", round(mean_s,5)), collapse = ""), x = mean_s, y = dnorm(mean_s, mu, std_error) + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  geom_text(
    aes(label = paste(c("P-value = ", round(p_value,5)), collapse = ""), x = mean_s + 0.5 * std_error, y = dnorm(mean_s + 0.5 * std_error, mu, std_error) + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Hypothesis: mu = 150 ",
       subtitle = "alpha = 5%",
       x = "Res (psi)",
       y = "Density")



# d) intervalo del 95% de confianza para la resistencia media de rompimiento
mu <- 150
sd <- 4.08
std_error <- sd/sqrt(n)
norm_score_inf <- qnorm(alpha/2, 0, 1, lower.tail = T) 
norm_score_sup <- qnorm(alpha/2, 0, 1, lower.tail = F) 


lower_bound <- mean_s + norm_score_inf * std_error  
upper_bound <- mean_s + norm_score_sup * std_error  


# e) proceso de fabricación no está bajo control, produciendo fibras con una resistencia media de 153 psi (igual desvío) 
#    ¿Cuál es la probabilidad de no efectuar el ajuste? (error tipo II = beta)

# population
# H_0  - normal distrib
mu <- 150
sd <- 4.08

# real  - normal distrib
mu_real <- 153
sd <- 4.08

# sample
n <- 16
std_error <- sd/sqrt(n)

# rejection zone
alpha <- 0.05

rej_inf <- qnorm(alpha/2, mu, std_error, lower.tail = T) 
rej_sup <- qnorm(alpha/2, mu, std_error, lower.tail = F) 

# type II error - beta
beta <- pnorm(rej_sup, mu_real, std_error, lower.tail = T)

# graphical representation
x_inf <- mu - std_error * 4
x_sup <- mu + std_error * 4
events <- seq(x_inf,x_sup,0.01)
df <- data.frame(events, density = dnorm(x = events, mu, std_error))  

ggplot(df, aes(x = events)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "line") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.8,
                xlim = c(rej_sup,x_sup)) + 
  geom_segment(aes(x = rej_sup, xend = rej_sup, y = 0, yend = dnorm(rej_sup, mu, std_error)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("alpha/2 = ", round(rej_sup,5)), collapse = ""), x = rej_sup, y = dnorm(rej_sup, mu, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.8,
                xlim = c(x_inf,rej_inf)) +
  geom_segment(aes(x = rej_inf, xend = rej_inf, y = 0, yend = dnorm(rej_inf, mu, std_error)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("-alpha/2 = ", round(rej_inf,5)), collapse = ""), x = rej_inf, y = dnorm(rej_inf, mu, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu_real,
                  sd = std_error
                ),
                geom = "line") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu_real,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.6,
                fill = "steelblue",
                xlim = c(x_inf,rej_sup)) + 
  geom_text(
    aes(label = paste(c("beta = ", round(beta,5)), collapse = ""), x = rej_sup, y = dnorm(rej_sup, mu_real, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Hypothesis: mu = 150 (real mu = 153)",
       subtitle = "alpha = 5%",
       x = "Res (psi)",
       y = "Density")

# f) potencia de la prueba

power <- 1 - beta
