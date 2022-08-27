#Requirements:
#  - ggplot2
#  - library("ggplot2")
ej3 <- read.csv2( file="C:\\Users\\admin\\Documents\\EdadPesoGrasas.csv", header = TRUE)

# - alternativa de datos creados
#datos <- data.frame(X=seq(0,8,by=2), Y=c(9,11,11,14,15))

#diagrama de dispersion
gg <- ggplot(data=ej3, mapping=aes(edad, grasas, label = rownames(ej3)))
gg + 
  geom_point() +
  geom_text(hjust=0, nudge_x = 0.4)

#regresion lineal simple
reg <- lm(grasas ~ edad, data = ej3)

#resumen de datos
#  - incluye prueba t y prueba F
summary(reg)

# - alternativa de desc
# 	- install.package(pastecs)
# 	- library(pastecs)
# stat.desc(datos) 

#  - otros
coef(reg)
summary.aov(reg)

#recta de regresion
gg_reg <- ggplot(data=ej3, mapping=aes_string(x = names(ej3)[3], y=names(ej3)[4]))
gg_reg + 
  geom_point() +
  stat_smooth(method ="lm", se = F)

#analisis de residuales
par(mfrow = c(2,2))
plot(reg)

#intervalos de confianza
inc_g <-data.frame(edad=32)
predict(reg, newdata=inc_g, interval="confidence", level=0.95)

#intervalos de prediccion
inc_g <-data.frame(edad=50)
predict(reg, newdata=inc_g, interval="prediction", level=0.95)

#intervalos de confianza de B1 estimado
confint(reg.lm)

##correlacion de variables aleatorias
cor(reg) 			##coeficiente de correlacion de Pearson r
cor.test(x, y, method = "pearson", conf.level=0.95)