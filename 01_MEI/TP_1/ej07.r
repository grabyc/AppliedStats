ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("descriptr", "readxl", "moments")
ipak(packages)

## número de transductores en cada lote que no cumple con las especificaciones de diseño

data <- read_xlsx("transductores.xlsx")

# b) tabla de distribución de frecuencias

freq <- ds_freq_table(data, transductores, 9)
plot(freq)


# c) media, mediana, moda, desvío estándar y coeficiente de variación

mean(data$transductores)
median(data$transductores)
sd(data$transductores)
cv <- sd(data$transductores) / mean(data$transductores) * 100
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data$transductores)

# d) P(X <= 5), P(X <= 4), P(X >= 5)

cumsum(freq$percent)[5]
cumsum(freq$percent)[4]
100-cumsum(freq$percent)[4]

# e) diagrama de cajas

boxplot(data$transductores)
skewness(data$transductores)

