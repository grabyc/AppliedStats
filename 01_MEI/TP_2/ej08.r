ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## cantidad de polvo en la atmósfera se distribuye según una ley de Poisson con 0,1 partículas por mm3

# a) probabilidad de que al examinar un cm3 de aire se encuentre entre 85 y 112 partículas de polvo inclusive P(85 <= X <= 112)
lambda <- 0.1 * 1000
x1 <- 85
x2 <- 112
result <- ppois(x2, lambda, lower.tail = TRUE) - ppois(x1, lambda, lower.tail = TRUE)

events <- 75:125
df <- data.frame(events, density = dpois(x = events, lambda)) %>%
  mutate(Trials = ifelse((events >= x1 & events <= x2), "result", "other")) 
ggplot(df, aes(x = factor(events), y = dpois(x = events, lambda), fill = Trials)) +
  geom_col() +
  geom_text(
    aes(label = round(density,3), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of 85 <= r <= 112 with Lambda = 100",
       subtitle = "P(100).",
       x = "Events (x)",
       y = "Density")

