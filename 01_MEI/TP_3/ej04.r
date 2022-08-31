## diámetro de las varillas de una caja de cambios tiene una distribución normal con un
## desvío estándar = = 1,5 SS. Se seleccionó al azar 16 varillas y resultó una media de 20 mm

# a) Verifique que el intervalo del 90% de confianza para la media es: 19.38 ≤ mu ≤ 20.61
n <- 16
mean <- 20
sd <- 1.5

alpha <- 0.10
norm_score <- qnorm(alpha/2, 0, 1, lower.tail = F) 

std_error <- sd/sqrt(n)
margin_error <- norm_score * std_error

lower_bound <- mean - margin_error  
upper_bound <- mean + margin_error  
