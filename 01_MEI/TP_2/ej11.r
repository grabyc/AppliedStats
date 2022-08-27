ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## tiempo que transcurre entre dos llamadas consecutivas a cierto teléfono sigue una distribución exponencial con una 
## media igual a 5 minutos

# a) graficar

media <- 5
lambda <- 1 / media


data.frame(x = 0:20, prob = dexp(x = 0:20, rate = lambda)) %>%
  ggplot(aes(x = x, y = prob)) +
  geom_area(alpha = 0.3) +
  labs(title = "Probability of X until First Success",
       subtitle = "X ~ Exponential(0.2)",
       x = "Interval (x)",
       y = "Probability") 

# b) probabilidad de que transcurran más de 4 minutos entre dos llamadas consecutivas P(X < 4)

x <-  4
result <- pexp(x, lambda, lower.tail = FALSE) 

data.frame(x = 0:30, prob = pexp(q = 0:30, rate = lambda, lower.tail = FALSE)) %>%
  mutate(Interval = ifelse(x > 4 , "> 4", "other")) %>%
  ggplot(aes(x = x, y = prob, fill = Interval)) +
  geom_area(alpha = 0.3) +
  labs(title = "Cumulative Probability of Interval X until First Success",
       subtitle = "X ~ Exponential(0.2)",
       x = "Interval (x)",
       y = "Cum Probability") 

# c) probabilidad de que en un intervalo de 10 minutos lleguen exactamente 3 llamadas  P(X = 3)

lambda <- (1 / media) * 10
x <- 3

result <- dpois(x, lambda)

data.frame(x = 0:10, prob = dpois(0:10, lambda)) %>%
  mutate(Interval = ifelse(x == 3, "result", "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Interval)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,3), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r = 3 with lambda = 2",
       subtitle = "P(2)",
       x = "X",
       y = "Probability") 


# d) probabilidad de que en un intervalo de 1 (una) hora lleguen más de 10 llamadas P(X > 10)

lambda <- (1 / media) * 60
x <- 10

result <- ppois(x, lambda, lower.tail = FALSE)

data.frame(x = 0:20, prob = dpois(0:20, lambda)) %>%
  mutate(Interval = ifelse(x > 10, "result", "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Interval)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,3), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r > 10 with lambda = 12",
       subtitle = "P(12)",
       x = "X",
       y = "Probability") 
