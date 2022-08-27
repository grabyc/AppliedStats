ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## 6 solicitudes por hora en un departamento de reparación de maquinarias

# a) probabilidad de que en 15 minutos no se reciba ninguna solicitud P(X = 0)
lambda <- 6 / 4
x <- 0
result <- dpois(x, lambda)

events <- 0:10
df <- data.frame(events, density = dpois(x = events, lambda)) %>%
  mutate(Trials = ifelse((events == x), "result", "other")) 
ggplot(df, aes(x = factor(events), y = dpois(x = events, lambda), fill = Trials)) +
  geom_col() +
  geom_text(
    aes(label = round(density,3), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r = 0 with Lambda = 1.5",
       subtitle = "P(1.5).",
       x = "Events (x)",
       y = "Density")

# b) probabilidad de que en media hora se reciban 2 o 3 solicitudes. P(2 <= X <= 3)

lambda <- 6 / 2
x1 <- 1
x2 <- 3
result <- ppois(x2, lambda, lower=TRUE) - ppois(x1, lambda, lower=TRUE)

events <- 0:10
df <- data.frame(events, density = dpois(x = events, lambda)) %>%
  mutate(Trials = ifelse((events == 2 | events == 3), "result", "other")) 
ggplot(df, aes(x = factor(events), y = dpois(x = events, lambda), fill = Trials)) +
  geom_col() +
  geom_text(
    aes(label = round(density,3), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of 2 <= r <= 3 with Lambda = 3",
       subtitle = "P(3)",
       x = "Events (x)",
       y = "Density")

# c) probabilidad de que en las próximas tres horas se reciban más de 15 solicitudes. P(X >= 15)

lambda <- 6 * 3
x <- 15
result <- 1 - ppois(x, lambda, lower.tail = TRUE) 

events <- 0:30
df <- data.frame(events, density = dpois(x = events, lambda)) %>%
  mutate(Trials = ifelse((events >= 15), "result", "other")) 
ggplot(df, aes(x = factor(events), y = dpois(x = events, lambda), fill = Trials)) +
  geom_col() +
  geom_text(
    aes(label = round(density,3), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r >= 15 with Lambda = 18",
       subtitle = "P(3)",
       x = "Events (x)",
       y = "Density")

# d) probabilidad de que transcurran mas de 20 minutos hasta la llegada de la primera solicitud 

rate <- 6
x <- 20 / 60
result <- 1 - pexp(x, rate) 

data.frame(x = 0:60 / 100, prob = pexp(q = 0:60 / 100, rate = rate, lower.tail = FALSE)) %>%
  mutate(Interval = ifelse(x >= 0.2 , "> 20m", "other")) %>%
  ggplot(aes(x = x, y = prob, fill = Interval)) +
  geom_area(alpha = 0.3) +
  labs(title = "Cumulative Probability of Interval X until First Success",
       subtitle = "X ~ Exponential(6)",
       x = "Interval (x)",
       y = "Cum Probability") 
