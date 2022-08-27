## número de lesiones en la piel de una muestra de 30 manzanas

data <- c(0,0,0,0,0,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,4,4,4,5,5,6)

# b) tabla de distribución de frecuencias

FreqAbs <- table(data)
FreqRel <- round(FreqAbs/length(data),3)

# c) representación gráfico f2, h1%,
breaks=seq(11.5,15.5,by=1)
colors = c("cyan","cyan","blue","cyan","cyan","cyan","cyan")
hist(data,plot=TRUE, breaks = -0.5:6.5, right=FALSE, main = "Distribución de lesiones en piel", 
	xlab = "Cantidad de lesiones", ylab = "Frecuencia absoluta", col=colors, freq=TRUE)
 
hist(data,plot=TRUE, breaks = -0.5:6.5, right=FALSE, main = "Distribución de lesiones en piel", xlab = "Cantidad de lesiones", ylab = "Frecuencia relativa", col=colors, freq=FALSE)


# d) número medio de golpes por manzana
mean(data)

# e) mediana y la moda
median(data)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data)

# f) rango, varianza, desvío estándar muestral y coeficiente de variación
diff(range(data))

var(data)

sd(data)

cv <- sd(data) / mean(data) * 100

# h) diagrama de cajas

boxplot(data)

# i) media y desvío estándar de variable transformada y = (x − m̅ean(x))

y <- data - mean(data)

mean(y)
sd(y)

# j) media y desvío estándar z = y/sd(y)

z <- y / sd(y)

mean(z)
sd(z)
