ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("readxl", "dplyr", "ggplot2")
ipak(packages)

## entre el hecho de que una gestante fume durante el embarazo y que el bebé presente bajo peso al nacer 
##   muestra: cohorte de 2000 gestantes
## archivo Gestante.xlsx 

data <- read.csv("./01_MEI/TP_3/Data/Gestante.csv", header = T, sep = ";")

# a) prueba chi-cuadrado para resolver el problema

## two methods
##   - 1) no parametric hypothesis test
##   - 2) Pearson's chi squared test (R function)

# method 1
# A: fumadora
# B: no fumadora

# H_0: condicion de fumadora de la gestante es independiente de la condicion de bajo peso del nacido
# H_0: condicion de fumadora de la gestante no es independiente de la condicion de bajo peso del nacido

# sample
tbl <- table(data)
n <- sum(tbl)

# tasa global con depresion
margins_rows <- marginSums(tbl,1)
margins_cols <- marginSums(tbl,2)
global <- margins_cols[2] / n

# sample freqs
tbl

# theorical freqs
exp_tbl <- matrix( c(margins_rows * (1 - global),
                    margins_rows * global),
                  nrow = 2, ncol = 2,
                  byrow = F)

# observed chi-square
chi_obs <- sum(tbl^2 / exp_tbl) - n

# rejection zone
alpha <- 0.05

r <- nrow(tbl)          # number of levels in factor #1
c <- ncol(tbl)          # number of levels in factor #2
df <- (r-1) * (c-1)     # degrees of freedom
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

# method 2

chisq.test(tbl, correct = F)
