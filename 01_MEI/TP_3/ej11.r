ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## dependencia entre la práctica de algún deporte y el estado de ánimo; muestra aleatoria de 100 jóvenes;
## tabla de contingencia
##    |               |  Sin depresion  |  Con depresion  |
##    | Deportista    |        38       |        9        |
##    | No Deportista |        31       |       22        |

# a) tabla de frecuencias relativas condicionales por fila
ct <- matrix( c(c(38, 9),
                c(31, 22)),
              nrow = 2, ncol = 2,
              byrow = T)
rownames(ct) <- c("Deportista", "No Deportista")
colnames(ct) <- c("Sin Depresion", "Depresion")

rel_ct <- round(prop.table(ct, margin = 1),3) 
rel_ct_m <- addmargins(rel_ct, 2)

# b) gráfico para las frecuencias relativas condicionales por fila
rel_freqs <- as.vector(t(rel_ct))
depression <- gl(n = 2, k = 1, length = 4, labels = c("Sin Depresion", "Depresion"))
sport <- gl(n = 2, k = 2, length = 4, labels = c("Deportista", "No Deportista"))

df <- data.frame(rel_freqs, depression, sport)

ggplot(df, aes(x = sport, y = rel_freqs, fill = depression)) + 
  geom_bar(stat = "identity") +
  labs(y = "proportion")

# c) Intuitivamente, hay asociación entre las variables

# intuitivamente (grafico) hay diferencia entre estado de animo si la persona practica deporte (menos depresion) o no (mas depresion)

# d) prueba de hipótesis no paramétrica independencia entre factores

# H_0: estado de animo es independiente de deporte
# H_a: estado de animo no es independiente de deporte

n <- sum(ct)
# tasa global con depresion
margins_sp <- marginSums(ct,1)
margins_dep <- marginSums(ct,2)
gl_depression <- margins_dep[2] / n

# sample freqs
ct

# theorical freqs
exp_ct <- matrix( c(margins_sp * (1 - gl_depression),
                margins_sp * gl_depression),
              nrow = 2, ncol = 2,
              byrow = F)

# observed chi-square
chi_obs <- sum(ct^2 / exp_ct) - n

# rejection zone
alpha <- 0.05

r <- nrow(ct)          # number of levels in factor #1
c <- ncol(ct)          # number of levels in factor #2
df <- (r-1) * (c-1)    # degrees of freedom
rej_sup <- qchisq(1 - alpha, df) 

# decision rule
#  -  chi_obs >= rej_sup  => reject H_0
#  -  chi_obs < rej_sup  => don't reject H_0
chi_obs >= rej_sup

# decision rule
#  -  p_value < alpha  => reject H_0
#  -  p_value >= alpha  => don't reject H_0
p_value <- pchisq(chi_obs, df, lower.tail = F)

p_value < alpha

# graphical representation
x_inf <- 0
x_sup <- 7

events <- seq(x_inf,x_sup,0.01)
data <- data.frame(events, density = dchisq(x = events, df))  

ggplot(data, aes(x = events)) +
  stat_function(fun = dchisq,
                args = list(
                  df = df
                ),
                geom = "line") +
  stat_function(fun = dchisq,
                args = list(
                  df = df
                  ),
                geom = "area",
                alpha = 0.6,
                xlim = c(rej_sup,x_sup)) + 
  geom_segment(aes(x = rej_sup, xend = rej_sup, y = 0, yend = dchisq(rej_sup, df)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("alpha = ", round(rej_sup,5)), collapse = ""), x = rej_sup, y = dchisq(rej_sup, df) + 0.01),
    size = 3,
    vjust = 0
  ) +
  stat_function(fun = dchisq,
                args = list(
                  df = df
                ),
                geom = "area",
                alpha = 0.6,
                fill = "red",
                xlim = c(chi_obs,x_sup)) + 
  geom_segment(aes(x = chi_obs, xend = chi_obs, y = 0, yend = dchisq(chi_obs, df)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("P-value = ", round(p_value,5)), collapse = ""), x = chi_obs, y = dchisq(chi_obs, df) + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  geom_text(
    aes(label = paste(c("chi_obs = ", round(chi_obs,5)), collapse = ""), x = chi_obs, y = dchisq(chi_obs, df) + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = -2
  ) +
  labs(title = "Hypothesis: depression is independent of sport",
       subtitle = "alpha = 5%; df = 1",
       x = "X",
       y = "Density")

