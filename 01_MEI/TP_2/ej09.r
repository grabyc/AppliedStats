ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## la temperatura de destilación del aceite en grados centígrados
##
##    f(x) = 1 / 150      si 150 < x < 300
##              0         para todo otro x
##

# a) grafique f(x) y F(x)
min <- 150
max <- 300

events <- 100:350

qplot(x = events, y = dunif(x = events, min, max)) + geom_area(fill = "cyan")
qplot(x = events, y = punif(events, min, max, lower.tail =TRUE, log.p = FALSE)) + geom_area(fill = "cyan")

# b) E(x), V(x) y sd(x)

E <- (min + max) / 2
V <- ((max - min) ** 2) / 12
sd <- sqrt(V)

# c) P(X < x) = 0.3

x <- 0.3
qunif(x, min, max, lower.tail =TRUE, log.p = FALSE)
