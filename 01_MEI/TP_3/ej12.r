ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "ggplot2")
ipak(packages)

## comparar las cualidades de dos procesos de fabricación; muestra aleatoria de 100 unidades de cada proceso;
## tabla de contingencia
##    | Proceso  |  No defectuoso  |  Defectuoso  |
##    | A        |        94       |      6       |
##    | B        |        90       |     10       |

# a) conclusiones pueden extraerse de los datos? prueba de hipótesis apropiada
ct <- matrix( c(c(94, 6),
                c(90, 10)),
              nrow = 2, ncol = 2,
              byrow = T)
rownames(ct) <- c("A", "B")
colnames(ct) <- c("Sin Defectos", "Con Defectos")

rel_ct <- round(prop.table(ct, margin = 1),3) 
rel_ct_m <- addmargins(rel_ct, 2)

# d) prueba de hipótesis homogeneidad de poblaciones

# H_0: p_{Aj} = p_{Bj}        j:1,2    -- procesos son homogeneos
# H_0: p_{Aj} <> p_{Bj}       j:1,2    -- procesos no son homogeneos

n <- sum(ct)
# tasa global con depresion
margins_rows <- marginSums(ct,1)
margins_cols <- marginSums(ct,2)
glob_defect <- margins_cols[2] / n

# sample freqs
ct

# theorical freqs
exp_ct <- matrix( c(margins_rows * (1 - glob_defect),
                    margins_rows * glob_defect),
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
                alpha = 0.8,
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
                alpha = 0.4,
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
  labs(title = "Hypothesis: process A and B are homogeneus",
       subtitle = "alpha = 5%; df = 1",
       x = "X",
       y = "Density")

