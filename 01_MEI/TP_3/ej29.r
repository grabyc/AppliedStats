## el tiempo de ensamblaje (en minutos) tiene una distribución aproximadamente normal?
## muestra aleatoria de 200 registros
## | i   |    1    |    2    |    3    |    4    |    5    |    6    |    7    |
## | x_i | (22-26] | (26-30] | (30-34] | (34-38] | (38-42] | (42-46] | (46-50] |
## | f_i |    6    |   28    |   48    |   65    |   38    |   13    |    2    |

# a) distribución aproximadamente normal?

inf <- c(22, 26, 30, 34, 38, 42, 46)
sup <- c(26, 30, 34, 38, 42, 46, 50)
avg <- (sup + inf) / 2      # to calculate mean and sd
x <- data.frame(inf, avg, sup)

# sample freqs
freq_obs <- c(6, 28, 48, 65, 38, 13, 2)
n <- sum(freq_obs)
mean_s <- sum(x$avg * freq_obs) / n
sd_s <- sqrt( (sum(x$avg^2 * freq_obs) - (sum(x$avg * freq_obs)^2 / n)) / (n-1) )

# theorical freqs
bin_prob <- function(inf, sup, mean, sd, n){
  z_inf <- (inf - mean) / sd
  z_sup <- (sup - mean) / sd
  
  p_inf <- pnorm(z_inf, 0, 1, lower.tail = T)
  p_sup <- pnorm(z_sup, 0, 1, lower.tail = T)
  
  prob <- p_sup - p_inf
  
  freq <- prob * n
}

# to properly calculate extreme bins probs 
x$inf[1] <- -Inf
x$sup[7] <- Inf

freq_esp <- sapply(1:nrow(x), function(i) bin_prob(x[i,1], x[i,3], mean_s, sd_s, n) )

# regroup all bins_i where freq_esp < 5, with bins_{i-1}
freq_esp <- c(freq_esp[1:5], sum(freq_esp[6:7]))
freq_obs <- c(freq_obs[1:5], sum(freq_obs[6:7]))


# H_0: tiempo de ensamblaje tiene distrib normal
# H_a: tiempo de ensamblaje no tiene distrib normal

# observed chi-square
chi_obs <- sum(freq_obs^2 / freq_esp) - n

# rejection zone
alpha <- 0.05

k <- length(freq_obs)  # number of groups in freq
p <- 2                 # number of estimated parameters (mean, sd)
df <- k - 1 - p        # degrees of freedom
rej_sup <- qchisq(1 - alpha, df) 

# decision rule
#  -  chi_obs >= rej_sup  => reject H_0
#  -  chi_obs < rej_sup  => don't reject H_0
chi_obs >= rej_sup

# decision rule
#  -  p_value < alpha  => reject H_0
#  -  p_value >= alpha  => don't reject H_0
p_value <- pchisq(chi_obs, df, lower.tail = F)

p_value < alpha
