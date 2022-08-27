ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("descriptr", "readxl", "moments")
ipak(packages)

## número de plantas Larrea divaricata encontradas en cada uno de 48 bloques de muestreo

data <- read_xls("Larrea.xls")

# b) tabla de distribución de frecuencias

freq <- ds_freq_table(data, "Num de plantas", 7)

# c) grafico

plot(freq)

# d) h(3)

freq$percent[4]

# e) F(X ≥ 1)

freq$cumulative[7] - freq$cumulative[1] 

# f) F(2 <= X <= 6)

freq$cumulative[7] - freq$cumulative[2] 

# g) media, mediana y moda

mean(data$`Num de plantas`)
median(data$`Num de plantas`)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data$`Num de plantas`)

# h) rango, variancia, desvío estándar y coeficiente de variación

range(data$`Num de plantas`)
var(data$`Num de plantas`)
sd(data$`Num de plantas`)
cv <- sd(data$`Num de plantas`) / mean(data$`Num de plantas`) * 100

# i) cuartil primero y el tercero

quantile(data$`Num de plantas`)[2]
quantile(data$`Num de plantas`)[4]

# j) diagrama de cajas y coeficiente de asimetría

boxplot(data$`Num de plantas`)

skewness(data$`Num de plantas`)



