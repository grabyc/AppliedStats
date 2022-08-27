ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## Un embarque de 12 televisores contiene 3 aparatos defectuosos. Un hotel compra 5 de esos televisores.

# c) probabilidad de recibir al menos 2 aparatos defectuosos

m <- 3      ## TVs defectuosos
n <- 9      ## TVs no defectuosos
k <- 5      ## intentos sin reposicion

result <- 1 - phyper(1, m, n, k)

data.frame(x = 0:5, prob = dhyper(x = 0:5, m, n, k)) %>%
  mutate(Failures = ifelse(x >= 2, "result", "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
    geom_col() +
    geom_text(
      aes(label = round(prob,3), y = prob + 0.01),
      position = position_dodge(0.9),
      size = 3,
      vjust = 0
      ) +
    labs(title = "Probability of r >= 2 Successes in X = 5 Trials without Reposition",
        subtitle = "H(5,9,3)",
        x = "Trials (Xr)",
        y = "Probability") 

# d) número esperado de televisores defectuosos en la compra

E <- k * ( m / (m+n) )
