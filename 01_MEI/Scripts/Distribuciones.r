x<-c(13,13 13,25 14,19 13,84 13,73 13,41 13,12 14,29
13,42 14,81 12,57 13,98 14,63 14,83 13,55 15,22 12,83 14,51
13,82 13,82 12,26 14,67 13,79 15,18 14,51 12,46 13,23 13,84
13,75 14,23 13,84 14,41 14,07 13,74 12,91 12,63 11,70 13,64
12,83 13,33 14,35 14,46 13,75 13,27 14,32 14,13 13,63 13,89

## Cargar CSV
nroFallas <- read.table( file="C:\\Users\\admin\\Documents\\NroFallas.csv", skipNul = TRUE, header = TRUE)

##Estadisticos
mean(x)
sd(x)
range(x)
skewness(x)  //package moments (https://cran.r-project.org/bin/windows/contrib/3.6/moments_0.14.zip)
kurtosis(x)  //package moments (https://cran.r-project.org/bin/windows/contrib/3.6/moments_0.14.zip)


##Factors & Levels
#Renaming levels of a factor
library(plyr)
x <- factor(c("alpha","beta","gamma","alpha","beta"))
revalue(x, c("beta"="two", "gamma"="three"))

#1 factor y varios niveles
tapply(y, x , sd)

##Tabla de Frecuencias
#Var Discreta
FreqAbs <- table(x)

#Var Continua
breaks=seq(11.5,15.5,by=1)
FreqAbs2 <- table(cut(x,,right=FALSE))

#Frec Relativa
FreqRel <- FreqAbs/length(x)

##Histogramas
colors = c("cyan","cyan","blue","cyan","cyan","cyan","cyan")
hist(x,plot=TRUE, breaks, right=FALSE, main = "Distribución de lesiones en piel", 
	xlab = "Cantidad de lesiones", ylab = "Frecuencia absoluta", col=colors, freq=TRUE)

colors = c("cyan","blue","cyan","cyan","cyan","cyan","cyan")
hist(x,plot=TRUE, breaks = -0.5:6.5, right=FALSE, main = "Distribución de lesiones en piel", xlab = "Cantidad de lesiones", ylab = "Frecuencia relativa", col=colors, freq=FALSE)

#new ticks
hist(x, ..., xaxt ="n")
axis(1, at = breaks, tick = TRUE)

##Frecuencias acumuladas (escalera)
plot(0:6,cumsum(Freq), type="s", xlab="", ylab="",main="")

##diagrama de caja
boxplot(TiempoFallo~Proveedor,datos)

##QQ Plots
library(car)
qqPlot(TiempoFallo~Proveedor, datos)

##Tests
#normalidad Pearson  (con o sin correccion de Yates)
chisq.test(Matriz, correct=TRUE) 

#normalidad shapiro
shapiro.test(x)

#F para dos varianzas (son iguales? se supone normalidad de las distribs)
var.test(res$resistencia[res$tipo.junta=='con recubrimiento'], res$resistencia[res$tipo.junta=='sin recubrimeinto'])

#t para dos muestras (medias iguales? se supone normalidad de las distribs y varianzas iguales)
t.test(resistencia ~ tipo.junta, data=res,
       var.equal=TRUE,
       conf.level=0.95)

#t para dos muestras (medias iguales? se supone normalidad de las distribs y varianzas distintas)
t.test(resistencia ~ tipo.junta, data=res,
       var.equal=FALSE,  #Welch test
       conf.level=0.95)