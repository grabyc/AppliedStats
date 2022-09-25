ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## accidentes automovilísticos registrados diariamente en una ciudad

# a) frecuencia teórica del número de días (100 días) suponiendo que la variable número de accidentes presenta una distribución de Poisson

accidents <- 0:6

# sample freqs
days_obs <- c(19, 26, 26, 15, 9, 4, 1)
n <- sum(days_obs)
lambda <- sum(accidents * days_obs) / n

# theorical freqs
density <- dpois(x = accidents, lambda)
density[7] <- ppois(5, lambda, lower.tail = F)    # P(last_group) = P(X>=6)
days_esp <- round(n * density, 2)

data <- data.frame(accidents, days_obs, days_esp)  

# regroup all accidents_i where days_esp < 5, with accidents_{i-1}
data_group <- rbind(data[1:4,], 
                  c(4, 
                    sum(data[5:7,2]), 
                    sum(data[5:7,3]))
                  )


# b) prueba de bondad de ajuste correspondiente y concluya con un alpha = 5% 

# H_0: nro de accidentes tiene distrib Poisson
# H_a: nro de accidentes no tiene distrib Poisson

# observed chi-square
chi_obs <- sum(data_group$days_obs^2 / data_group$days_esp) - n

# rejection zone
alpha <- 0.05

k <- nrow(data_group)    # number of groups in freq
p <- 1                 # number of estimated parameters (lambda)
df <- k - 1 - p        # degrees of freedom
rej_sup <- qchisq(1 - alpha, df) 

# decision rule
#  -  chi_obs >= rej_sup  => reject H_0
#  -  chi_obs < rej_sup  => don't reject H_0
chi_obs >= rej_sup

# decision rule
#  -  p_value < alpha  => reject H_0
#  -  p_value >= alpha  => don't reject H_0
p_value <- pchisq(chi_obs, df, lower.tail = F)

p_value < alpha

# graphical representation
x_inf <- 0
x_sup <- 12

events <- seq(x_inf,x_sup,0.01)
data <- data.frame(events, density = dchisq(x = events, df))  

ggplot(data, aes(x = events)) +
  stat_function(fun = dchisq,
                args = list(
                  df = df
                ),
                geom = "line") +
  stat_function(fun = dchisq,
                args = list(
                  df = df
                  ),
                geom = "area",
                alpha = 0.8,
                xlim = c(rej_sup,x_sup)) + 
  geom_segment(aes(x = rej_sup, xend = rej_sup, y = 0, yend = dchisq(rej_sup, df)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("alpha = ", round(rej_sup,5)), collapse = ""), x = rej_sup, y = dchisq(rej_sup, df) + 0.01),
    size = 3,
    vjust = 0
  ) +
  stat_function(fun = dchisq,
                args = list(
                  df = df
                ),
                geom = "area",
                alpha = 0.6,
                fill = "steelblue",
                xlim = c(chi_obs,x_sup)) + 
  geom_segment(aes(x = chi_obs, xend = chi_obs, y = 0, yend = dchisq(chi_obs, df)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("P-value = ", round(p_value,5)), collapse = ""), x = chi_obs, y = dchisq(chi_obs, df) + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  geom_text(
    aes(label = paste(c("chi_obs = ", round(chi_obs,5)), collapse = ""), x = chi_obs, y = dchisq(chi_obs, df) + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = -2
  ) +
  labs(title = "Hypothesis: X (# of accidents) is Chi-Square",
       subtitle = "alpha = 5%; lambda = 1.85, df = 3",
       x = "Accidents",
       y = "Density")

