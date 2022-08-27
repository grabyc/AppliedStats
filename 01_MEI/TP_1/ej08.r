ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("descriptr", "readxl", "moments")
ipak(packages)

## cantidad de fallas encontradas en las 2000 observaciones realizadas al cabo de un mes

data <- read_xlsx("NroFallas.xlsx")

# b) tabla de distribución de frecuencias

freq <- ds_freq_table(data, CantFallas, 7)
plot(freq)


# c) media, mediana, moda y desvío estándar

mean(data$CantFallas)
median(data$CantFallas)
sd(data$CantFallas)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data$CantFallas)

# d) EEUU: media=4.13 y desvío estándar=2.64 , ¿Cuál de las dos distribuciones de frecuencias es más homogénea?

cvARG <- sd(data$CantFallas) / mean(data$CantFallas) * 100
cvEEUU <- 2.64 / 4.13 * 100

# e) diagrama de cajas

boxplot(data$transductores)
skewness(data$transductores)

