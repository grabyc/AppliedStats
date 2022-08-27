ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## cantidad de buques que llegan cada día a cierta refinería tiene una distribución de Poisson con parámetro 
## lambda = 2. Las actuales instalaciones portuarias pueden despachar tres buques al día. Si llegan más de tres 
## buques en un día, los que están en exceso deben enviarse a otro puerto.

# a) probabilidad de tener que enviar buques a otro puerto P(X > 3)
lambda <- 2
x <- 3
result <- ppois(x, lambda, lower.tail = FALSE)

events <- 0:10
df <- data.frame(events, density = dpois(x = events, lambda)) %>%
  mutate(Trials = ifelse((events > x), "result", "other")) 
ggplot(df, aes(x = factor(events), y = dpois(x = events, lambda), fill = Trials)) +
  geom_col() +
  geom_text(
    aes(label = round(density,3), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r > 3 with Lambda = 2",
       subtitle = "P(2).",
       x = "Events (x)",
       y = "Density")

# b) cantidad esperada de buques atendidos diariamente E(X)

E <- lambda 

# c) cantidad esperada de buques enviados diariamente a otro puerto E(X > 3)

???
  
# d) T: tiempo transcurrido hasta la llegada de un buque (a partir de la llegada anterior)

# d2) probabilidad de que transcurran entre 1 y 2 días hasta la llegada del próximo buque P(1 <= X <= 2)

rate <- 2
x1 <- 1
x2 <- 2
result <- pexp(x2, rate) - pexp(x1, rate) 

data.frame(x = 0:500 / 100, prob = pexp(q = 0:500 / 100, rate = rate, lower.tail = TRUE)) %>%
  mutate(Interval = ifelse(x >= 1 & x <= 2 , "1 <= X <= 2", "other")) %>%
  ggplot(aes(x = x, y = prob, fill = Interval)) +
  geom_area(alpha = 0.3) +
  labs(title = "Cumulative Probability of Interval X until First Success",
       subtitle = "X ~ Exponential(6)",
       x = "Interval (x)",
       y = "Cum Probability") 

