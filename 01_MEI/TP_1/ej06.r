ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("descriptr")
ipak(packages)

## niveles de tensión en (KV) de una muestra de 50 clientes industriales

data <- c(15.30, 12.32, 13.13, 13.25, 14.19, 13.84, 13.73, 13.41, 13.12, 14.29,
 13.42, 14.81, 12.57, 13.98, 14.63, 14.83, 13.55, 15.22, 12.83, 14.51,
 13.82, 13.82, 12.26, 14.67, 13.79, 15.18, 14.51, 12.46, 13.23, 13.84,
 13.75, 14.23, 13.84, 14.41, 14.07, 13.74, 12.91, 12.63, 11.70, 13.64,
 12.83, 13.33, 14.35, 14.46, 13.75, 13.27, 14.32, 14.13, 13.63, 13.89)
data_df <- data.frame(data)

# b) tabla de distribución de frecuencias

breaks <- seq(11.5,15.5,by=0.5)

hist(data, breaks , plot = FALSE)

hist(data, plot=TRUE, right=FALSE, breaks=breaks, main = "Distribución de niveles de tensión", 
     xlab = "KV", ylab = "Clientes", freq=TRUE)

# or with descriptr library

freq <- ds_freq_table(data_df, data, 10)
plot(freq)


# d) moda

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data)

# e) media, mediana, desvío estándar muestral y coeficiente de variación

mean(data)
median(data)
sd(data)
cv <- sd(data) / mean(data) * 100

# g) diagrama de cajas

boxplot(data)

# h) media y desvío estándar de variable transformada y = (x + 10)

y <- data + 10

mean(y)
sd(y)
cv <- sd(y) / mean(y) * 100

# i) media y desvío estándar de variable transformada y = (1.05 * x)

y <- data * 1.05

mean(y)
sd(y)
cv <- sd(y) / mean(y) * 100
