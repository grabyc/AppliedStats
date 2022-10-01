

## límite al que las personas perciben olores industriales en cierta región 
##   muestra de personas de cada una de las tres regiones cercanas a instalaciones industriales: Región A, B y C
##   resultados: percibía olores: Todos los días, al menos una vez a la semana, al menos una vez al mes, menos de una vez al mes, o nada en absoluto
## archivo Olores.csv 

data <- read.csv("./01_MEI/TP_3/Data/Olores.csv", header = T, sep = ";")

# a) hipótesis apropiadas y prueba correspondiente

## two methods
##   - 1) no parametric hypothesis test
##   - 2) Pearson's chi squared test (R function)

# method 1

# H_0: percepcion de olores es independiente de la region
# H_a: percepcion de olores no es independiente de la region

# sample
tbl <- table(data)
n <- sum(tbl)

# tasa global con depresion
margins_rows <- marginSums(tbl,1)
margins_cols <- marginSums(tbl,2)
global <- sapply(1:length(margins_cols), function(i) margins_cols[i] / n)

# sample freqs
tbl

# theorical freqs
exp_tbl <- sapply(1:length(global), function(i) margins_rows * global[i])

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
