ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("descriptr", "readxl", "moments")
ipak(packages)

## deflexión en milímetros ante una presión de 1 tn de 120 probetas

data <- read_xlsx("probeta.xlsx")

# b) tabla de distribución de frecuencias y grafico

freq <- ds_freq_table(data, probeta, 10)
plot(freq)


# c) diagrama de cajas

boxplot(data$probeta)

# d) coeficiente de asimetría

skewness(data$probeta)

# e) media, mediana

mean(data$probeta)
median(data$probeta)

# f) cuartil inferior y el superior

quantile(data$probeta)[2]
quantile(data$probeta)[4]

# g) desvío estándar y coeficiente de variación

sd(data$probeta)
cv <- sd(data$probeta) / mean(data$probeta) * 100

# h) estadístico de apuntamiento es -0.8351 

kurtosis(data$probeta) ## 2.132429 ???
skewness(data$probeta)
