ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("readxl", "dplyr", "ggplot2")
ipak(packages)

## tiempo hasta el fallo (en horas) de dos muestras independientes de 
##   muestra A: 10 componentes del proveedor A 
##   muestra B: 15 componentes del proveedor B
## archivo iempoHastaFallo.xlxs  

data <- read.csv("./01_MEI/TP_3/Data/tiempohastaFallo.csv", header = T, sep = ";", dec = ",")

# sample A 
sample_A <- data[data$Proveedor == 'A',1]
n_A <- length(sample_A)
mean_A <- mean(sample_A)
sd_A <- sd(sample_A)

# sample B 
sample_B <- data[data$Proveedor == 'B',1]
n_B <- length(sample_B)
mean_B <- mean(sample_B)
sd_B <- sd(sample_B)

# a) dos gráficos de caja superpuestos y dos gráficos Q-Q plot del tiempo hasta el fallo según proveedor

ggplot(data, aes(x=Proveedor, y=TiempoFallo)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Proveedor")

ggplot(data, aes(sample = TiempoFallo, colour = Proveedor)) +
  stat_qq() +
  stat_qq_line()

# b) En base a estos gráficos puede suponer poblaciones normales?

# normal, en ambos casos

# c) prueba de Bondad de ajuste respecto a la normalidad del tiempo hasta el fallo de los componentes electrónicos de cada proveedor

# como el tamaño de la muestra n<30, no puede aplicarse Teorema Central del Limite y asumir normalidad
# por lo tanto, se debe hacer una prueba de hipotesis de normalidad (p.ej. Shapiro-Wilk)

res_A <- shapiro.test(sample_A)  ## don't reject  --> is normal
res_B <- shapiro.test(sample_B)  ## don't reject  --> is normal

# por tener multiples hipotesis conjuntas (family-wise error rate), podrian ajustarse los p-values con metodos 
#   como el de Holm-Bonferroni

p.adjust(sort(c(res_A$p.value, res_B$p.value)), "holm")

# d) homogeneidad de las variancias poblaciones de ambos proveedores?

var.test(sample_A, sample_B, alternative = "two.sided") ## don't reject  --> same variances

# e) existe diferencia significativa entre los tiempos medios hasta el fallo de los componentes de los dos proveedores?

res <- t.test(sample_A, sample_B, alternative = "two.sided", var.equal = T)

# f) intervalo de confianza para la diferencia verdadera entre los tiempos medios hasta el fallo de ambos proveedores

res$conf.int

# intervalo de confianza contiene el 0 --> puede considerarse que mean_A == mean_B

# g) intervalo de confianza para el valor medio del tiempo hasta el fallo de los componentes de cada proveedor

alpha <- 0.05

norm_score_inf <- qnorm(alpha/2, 0, 1, lower.tail = T) 
norm_score_sup <- qnorm(alpha/2, 0, 1, lower.tail = F) 

# confidence interval A
std_error_A <- sd_A / sqrt(n_A) 

lower_bound_A <- mean_A + norm_score_inf * std_error_A  
upper_bound_A <- mean_A + norm_score_sup * std_error_A

print(c(lower_bound_A, upper_bound_A))

# confidence interval B
std_error_B <- sd_B / sqrt(n_B) 

lower_bound_B <- mean_B + norm_score_inf * std_error_B  
upper_bound_B <- mean_B + norm_score_sup * std_error_B

print(c(lower_bound_B, upper_bound_B))

