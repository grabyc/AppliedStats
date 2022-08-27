ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## Lanzar un dado repetidas veces; éxito si sale 5 o 6 al tirar un dado

# a) dado se lanza 10 veces, ¿cuál es la probabilidad de obtener por lo menos 2 éxitos?

success <- 1/3
n <- 10

result <- 1 - pbinom(1, n, success)

data.frame(x = 0:n, prob = dbinom(x = 0:n, size = n, prob = success)) %>%
  mutate(Failures = ifelse(x >= 2, "result", "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,2), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of r >= 2 Successes in X = 10 Trials",
       subtitle = "B(10,.333)",
       x = "Trials (X)",
       y = "Probability") 

# b) ¿Cuál es la probabilidad de tener que lanzar el dado 15 veces hasta obtener el 3º éxito?

r <- 3
n <- 15 - r

result <- dnbinom(n, r, success)

data.frame(x = 0:15, prob = dnbinom(x = 0:15, size = r, prob = success)) %>%
  mutate(Failures = ifelse(x == n, n, "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
    geom_col() +
    geom_text(
      aes(label = round(prob,2), y = prob + 0.01),
      position = position_dodge(0.9),
      size = 3,
      vjust = 0
      ) +
    labs(title = "Probability of r = 3 Successes in X = 15 Trials",
        subtitle = "NB(3,.333)",
        x = "Failed Trials (X - r)",
        y = "Probability") 

# c) ¿Cuál es el número medio de lanzamientos necesarios hasta la obtención del 1º éxito?

result <- dgeom(3, success)


data.frame(x = 0:15, prob = dgeom(x = 0:15, prob = success)) %>%
  ggplot(aes(x = factor(x), y = prob)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,2), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of X Failed Trails until First Success",
       subtitle = "G(.333)",
       x = "Failed Trials (X)",
       y = "Probability") 

E <- 1 / success

