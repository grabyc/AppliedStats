ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## resistencia (en mm.) de probetas de un determinado material cuando son sometidas a
##   cargas de 1 tn., viene dada por una variable aleatoria X con función de densidad
##
##              x         si 0 <= x <= 1
##    f(x) =    2 - x     si 1 < x <= 2
##              0         para todo otro x
##
##              0                             si x < 0
##              (x**2) / 2                    si 0 <= x <= 1
##    F(x) =    -[(x**2) / 2] + 2x - 1      si 1 < x <= 2
##              1                             si 2 < x
##

# b) P(0.5 < X < 1.5)
result <- (-((1.5**2) / 2) + 2*1.5 - 1) - ((0.5**2) / 2)   ## F(1.5) - F(0.5)

# c) resistencia ideal entre 0.5 y 1.5; muestra de 10 probetas seleccionadas al azar, cuál es la probabilidad de que 
#     al menos el 90% de ellas tengan resistencia ideal  P(X>=90)

success <- result
n <- 10
prob <- 0.8

result <- 1 - pbinom(8, n, success, lower.tail = TRUE)

data.frame(x = 0:n, prob = dbinom(x = 0:n, size = n, prob = success)) %>%
  mutate(Failures = ifelse(x >= 9, "result", "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,3), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r >= 9 Successes in X = 10 Trials",
       subtitle = "B(10,.75)",
       x = "Trials (X)",
       y = "Probability") 


# c) número medio de extracciones hasta encontrar una que no cumple con las especificaciones. X -> G(0.25); E(X)

prob <- 1 - success
E <- 1 / prob 
