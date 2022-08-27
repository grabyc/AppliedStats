ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## unidades terminadas de una línea de ensamble; defectuosas 25%

# a) probabilidad de que en la décima unidad inspeccionada se encuentre la segunda defectuosa

p <- 0.25
n <-  10
r <- 2
x <- n - r

result <- dnbinom(x, r, p)

data.frame(x = 0:10, prob = dnbinom(x = 0:10, size = r, prob = p)) %>%
  mutate(Failures = ifelse(x == 8, x, "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
    geom_col() +
    geom_text(
      aes(label = round(prob,3), y = prob + 0.01),
      position = position_dodge(0.9),
      size = 3,
      vjust = 0
      ) +
    labs(title = "Probability of r = 2 Successes in X = 10 Trials",
        subtitle = "NB(2,.25)",
        x = "Failed Trials (X - r)",
        y = "Probability") 

# b) probabilidad de que la tercera unidad inspeccionada resulte ser la primera defectuosa

result <- dgeom(3, p)


data.frame(x = 0:10, prob = dgeom(x = 0:10, prob = p)) %>%
  ggplot(aes(x = factor(x), y = prob)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,3), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of X Failed Trails until First Success",
       subtitle = "G(.25)",
       x = "Failed Trials (X - 1)",
       y = "Probability") 


