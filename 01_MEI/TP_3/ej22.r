## proporción de cigüeñales de motor defectuosos 
## muestra aleatoria de 800 cigueñales y 80 defectuosos de proveedor A

# a) intervalo del 95% de confianza para la proporción poblacional de cigüeñales defectuosos

## sample
fail <-  80
n <- 800

## estimators
p_s <- fail / n

## z-score
alpha <- 0.05
z_score_inf <- qnorm(alpha/2, 0, 1, lower.tail = T) 
z_score_sup <- qnorm(alpha/2, 0, 1, lower.tail = F) 

#  confidence interval
std_error <- sqrt( (p_s * (1 - p_s)) / n )

lower_bound <- p_s + z_score_inf * std_error  
upper_bound <- p_s + z_score_sup * std_error

print(c(lower_bound, upper_bound))

# b) muestra aleatoria de 200 cigueñales y 30 defectuosos de proveedor B; 
#    comparar prov A con prov B a un nivel de significacion del 5%: es A mejor que B? pA < pB?

# H_0: pA = pB  <--> pA - pB = 0 
# H_a: pA < pB  <--> pA - pB < 0

## sample A
fail_A <-  80
n_A <- 800
p_A <- fail_A / n_A

## sample B
fail_B <-  30
n_B <- 200
p_B <- fail_B / n_B

## estimators
p_hat <- (fail_A + fail_B) / (n_A + n_B)
std_error <- sqrt( p_hat * (1 - p_hat) * (1/n_A + 1/n_B) )
z_obs <- (p_A - p_B) / std_error 

# rejection zone
alpha <- 0.05

rej_inf <- qnorm(alpha, 0, 1, lower.tail = T) 


# decision rule
#  -  z_obs <= rej_inf  => reject H_0
#  -  rej_inf < z_obs   => don't reject H_0

z_obs <= rej_inf 

# decision rule
#  -  p_value < alpha  => reject H_0
p_value <- pnorm(z_obs, 0, 1, lower.tail = T) 

p_value < alpha

# graphical representation
x_inf <- -3.5
x_sup <- 3.5

mu <- 0
std_error <- 1

events <- seq(x_inf,x_sup,0.01)
df <- data.frame(events, density = dnorm(x = events, mu, std_error))  

ggplot(df, aes(x = events)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "line") +
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.8,
                xlim = c(x_inf,rej_inf)) +
  geom_segment(aes(x = rej_inf, xend = rej_inf, y = 0, yend = dnorm(rej_inf, mu, std_error)), 
               lty = "dashed") + 
  geom_text(
    aes(label = c("alpha = 0.05"), x = rej_inf, y = dnorm(rej_inf, mu, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) + 
  geom_text(
    aes(label = paste(c("rej_inf = ", round(rej_inf,5)), collapse = ""), x = rej_inf, y = dnorm(rej_inf, mu, std_error) + 0.01),
    size = 3,
    vjust = 1
  ) + 
  stat_function(fun = dnorm,
                args = list(
                  mean = mu,
                  sd = std_error
                ),
                geom = "area",
                alpha = 0.6,
                fill = "red",
                xlim = c(x_inf,z_obs)) + 
  geom_segment(aes(x = z_obs, xend = z_obs, y = 0, yend = dnorm(z_obs, mu, std_error)), 
               lty = "dashed") + 
  geom_text(
    aes(label = paste(c("P-value = ", round(p_value,5)), collapse = ""), x = z_obs, y = dnorm(z_obs, mu, std_error) + 0.01),
    size = 3,
    vjust = 0
  ) +
  geom_text(
    aes(label = paste(c("z_obs = ", round(z_obs,5)), collapse = ""), x = z_obs, y = dnorm(z_obs, mu, std_error) + 0.01),
    size = 3,
    vjust = 1
  ) +
  labs(title = "Hypothesis: p_A = p_B \n Alternative: p_A < p_B ",
       subtitle = "alpha = 5%",
       x = "z_score",
       y = "Density")
