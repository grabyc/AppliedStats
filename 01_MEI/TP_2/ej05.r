ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## cantidad de interferencias en un sistema eléctrico sigue una ley de Poisson con una media de 2 interferencias por día

# a) P(X > 5)
lambda <- 2
x <- 5
result <- ppois(x, lambda, lower=FALSE)

events <- 0:10
df <- data.frame(events, density = dpois(x = events, lambda)) %>%
  mutate(Trials = ifelse((events > 5), "result", "other")) 
ggplot(df, aes(x = factor(events), y = dpois(x = events, lambda), fill = Trials)) +
  geom_col() +
  geom_text(
    aes(label = round(density,3), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r > 5 with Lambda = 2",
       subtitle = "P(2).",
       x = "Events (x)",
       y = "Density")

# b) a lo sumo 2 interferencias en el transcurso de tres días consecutivos elegidos al azar. P(Y ≤ 2)

lambda <- 6
x <- 2
result <- ppois(x, lambda, lower=TRUE)

events <- 0:10
df <- data.frame(events, density = dpois(x = events, lambda)) %>%
  mutate(Trials = ifelse((events <= 2), "result", "other")) 
ggplot(df, aes(x = factor(events), y = dpois(x = events, lambda), fill = Trials)) +
  geom_col() +
  geom_text(
    aes(label = round(density,3), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r <= 2 with Lambda = 6",
       subtitle = "P(6)",
       x = "Events (x)",
       y = "Density")

# c) probabilidad de que transcurran entre 1 y 3 días hasta la ocurrencia de alguna interferencia. P(1 ≤ X ≤ 3)

rate <- 2
x1 <- 1
x2 <- 3
result <- pexp(x2, rate) - pexp(x1, rate)

data.frame(x = 0:1000 / 100, prob = pexp(q = 0:1000 / 100, rate = rate, lower.tail = TRUE)) %>%
  mutate(Interval = ifelse(x >= 1 & x <= 3, "1 to 3", "other")) %>%
  ggplot(aes(x = x, y = prob, fill = Interval)) +
    geom_area(alpha = 0.3) +
    labs(title = "Cumulative Probability of Interval X until First Success",
       subtitle = "X ~ Exponential(2)",
       x = "Interval (x)",
       y = "Cum Probability") 
