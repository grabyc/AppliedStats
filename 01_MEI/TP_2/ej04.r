ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## clase consiste de 10 varones y 15 mujeres, y que se selecciona al azar un grupo de 5 estudiantes de la clase. X: número de varones seleccionados

# c) P(1 ≤ X < 3)

m <- 10      ## varones
n <- 15      ## mujeres
k <- 5       ## intentos sin reposicion

result <- phyper(2, m, n, k) - phyper(0, m, n, k)

data.frame(x = 0:5, prob = dhyper(x = 0:5, m, n, k)) %>%
  mutate(Trials = ifelse((x > 0) & (x < 3), "result", "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Trials)) +
    geom_col() +
    geom_text(
      aes(label = round(prob,3), y = prob + 0.01),
      position = position_dodge(0.9),
      size = 3,
      vjust = 0
      ) +
    labs(title = "Probability of 1 <= r < 3 Successes in X = 5 Trials without Reposition",
        subtitle = "H(5,10,15)",
        x = "Trials (Xr)",
        y = "Probability") 

# d) Calcule E(X). Calcule la V(X).

E <- k * ( m / (m+n) )
V <- k * ( m / (m+n) ) * ( n / (m+n) ) * ( ((m+n) - k) / ((m+n) - 1) )
