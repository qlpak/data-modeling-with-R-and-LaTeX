setwd("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt")

allegro <- read.csv("ale_d.csv")
lpp <- read.csv("lpp_d.csv")

allegro$Data <- as.Date(allegro$Data)
lpp$Data <- as.Date(lpp$Data)

allegro$log_returns <- c(NA, diff(log(allegro$Zamkniecie)))
lpp$log_returns <- c(NA, diff(log(lpp$Zamkniecie)))

library(ggplot2)

plot_allegro_closing <- ggplot(allegro, aes(x = Data, y = Zamkniecie)) +
  geom_line(color = "blue") +
  labs(x = "Data", y = "Cena zamknięcia") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_line(color = "grey90")
  )
ggsave("allegro_closing_prices.png", plot = plot_allegro_closing)

plot_allegro_log_returns <- ggplot(na.omit(allegro), aes(x = Data, y = log_returns)) +
  geom_line(color = "red") +
  labs(x = "Data", y = "Log-zwroty") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_line(color = "grey90")
  )
ggsave("allegro_log_returns.png", plot = plot_allegro_log_returns)

plot_lpp_closing <- ggplot(lpp, aes(x = Data, y = Zamkniecie)) +
  geom_line(color = "green") +
  labs(x = "Data", y = "Cena zamknięcia") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_line(color = "grey90")
  )
ggsave("lpp_closing_prices.png", plot = plot_lpp_closing)

plot_lpp_log_returns <- ggplot(na.omit(lpp), aes(x = Data, y = log_returns)) +
  geom_line(color = "purple") +
  labs(x = "Data", y = "Log-zwroty") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_line(color = "grey90")
  )
ggsave("lpp_log_returns.png", plot = plot_lpp_log_returns)

allegro$log_returns <- c(NA, diff(log(allegro$Zamkniecie)))
lpp$log_returns <- c(NA, diff(log(lpp$Zamkniecie)))

compute_stats <- function(data) {
  log_returns <- na.omit(data$log_returns)
  mean_value <- mean(log_returns)
  variance <- var(log_returns)
  std_dev <- sd(log_returns)
  quantiles <- quantile(log_returns, probs = c(0.05, 0.5, 0.95))
  
  list(
    mean = mean_value,
    variance = variance,
    std_dev = std_dev,
    q_5 = quantiles[1],
    q_50 = quantiles[2],
    q_95 = quantiles[3]
  )
}

allegro_stats <- compute_stats(allegro)
lpp_stats <- compute_stats(lpp)

results <- data.frame(
  Spółka = c("Allegro", "LPP"),
  Średnia = c(allegro_stats$mean, lpp_stats$mean),
  Wariancja = c(allegro_stats$variance, lpp_stats$variance),
  Odchylenie_std = c(allegro_stats$std_dev, lpp_stats$std_dev),
  Kwantyl_5 = c(allegro_stats$q_5, lpp_stats$q_5),
  Kwantyl_50 = c(allegro_stats$q_50, lpp_stats$q_50),
  Kwantyl_95 = c(allegro_stats$q_95, lpp_stats$q_95)
)

print(results)

png("allegro_histogram.png", width = 800, height = 600)
hist(
  allegro$log_returns, breaks = 50, col = "lightblue", main = "Histogram log-zwrotów Allegro", 
  xlab = "Log-zwroty", ylab = "Częstość"
)
abline(v = allegro_stats$mean, col = "red", lwd = 2, lty = 2) # Średnia
abline(v = allegro_stats$q_5, col = "green", lwd = 2, lty = 2) # Kwantyl 5%
abline(v = allegro_stats$q_95, col = "green", lwd = 2, lty = 2) # Kwantyl 95%
dev.off()

png("lpp_histogram.png", width = 800, height = 600)
hist(
  lpp$log_returns, breaks = 50, col = "slateblue2", main = "Histogram log-zwrotów LPP", 
  xlab = "Log-zwroty", ylab = "Częstość"
)
abline(v = lpp_stats$mean, col = "red", lwd = 2, lty = 2) # Średnia
abline(v = lpp_stats$q_5, col = "green", lwd = 2, lty = 2) # Kwantyl 5%
abline(v = lpp_stats$q_95, col = "green", lwd = 2, lty = 2) # Kwantyl 95%
dev.off()


library(ggplot2)

setwd("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt")

allegro <- read.csv("ale_d.csv")
lpp <- read.csv("lpp_d.csv")

allegro$log_returns <- c(NA, diff(log(allegro$Zamkniecie)))
lpp$log_returns <- c(NA, diff(log(lpp$Zamkniecie)))

allegro_data <- na.omit(allegro$log_returns)
lpp_data <- na.omit(lpp$log_returns)

plot_ecdf <- function(data, output_file) {
  p <- ggplot(data.frame(x = data), aes(x = x)) +
    stat_ecdf(geom = "step", color = "blue") +
    labs(
      x = "Log-zwroty",
      y = "Dystrybuanta empiryczna"
    ) +
    theme_minimal()
  
  ggsave(output_file, plot = p, width = 7, height = 5)
}

plot_ecdf(allegro_data, "allegro_cdf.png")
plot_ecdf(lpp_data, "lpp_cdf.png")
setwd("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt")

allegro <- read.csv("ale_d.csv")
lpp <- read.csv("lpp_d.csv")
library(fitdistrplus)
setwd("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt")

allegro <- read.csv("ale_d.csv")
lpp <- read.csv("lpp_d.csv")
library(fitdistrplus)
# mc z ks ale bez ks.test
# Funkcja obliczająca statystykę KS "na około"
compute_ks_stat <- function(data, dist_func, ...) {
  n <- length(data)
  sorted_data <- sort(data)
  
  # Dystrybuanta empiryczna
  F_empirical <- (1:n) / n
  
  # Dystrybuanta teoretyczna
  F_theoretical <- dist_func(sorted_data, ...)
  
  # Statystyka KS
  max(abs(F_empirical - F_theoretical))
}

# Funkcja do testu Monte Carlo bez użycia ks.test
mc_test_manual <- function(data, n_sim = 10000) {
  n <- length(data)
  
  # Dopasowanie rozkładu normalnego
  fit <- fitdist(data, "norm")
  mean_fit <- fit$estimate[1]
  sd_fit <- fit$estimate[2]
  
  # Obliczenie statystyki KS dla danych rzeczywistych
  dn <- compute_ks_stat(data, pnorm, mean = mean_fit, sd = sd_fit)
  
  # Symulacje Monte Carlo
  D <- numeric(n_sim)
  for (i in 1:n_sim) {
    sim_data <- rnorm(n, mean = mean_fit, sd = sd_fit)
    D[i] <- compute_ks_stat(sim_data, pnorm, mean = mean_fit, sd = sd_fit)
  }
  
  # Obliczenie p-wartości
  p_value <- mean(D > dn)
  
  list(statistic = dn, p_value = p_value, simulated_stats = D)
}

# Obliczenie log-zwrotów dla Allegro i LPP
allegro$log_returns <- c(NA, diff(log(as.numeric(allegro$Zamkniecie))))
lpp$log_returns <- c(NA, diff(log(as.numeric(lpp$Zamkniecie))))

allegro_data <- allegro$log_returns[is.finite(allegro$log_returns)]
lpp_data <- lpp$log_returns[is.finite(lpp$log_returns)]

# Test Monte Carlo dla Allegro
mc_result_allegro <- mc_test_manual(allegro_data)
cat("Allegro - Statystyka KS:", mc_result_allegro$statistic, ", p-wartość:", mc_result_allegro$p_value, "\n")

# Test Monte Carlo dla LPP
mc_result_lpp <- mc_test_manual(lpp_data)
cat("LPP - Statystyka KS:", mc_result_lpp$statistic, ", p-wartość:", mc_result_lpp$p_value, "\n")
# Funkcja do rysowania histogramu i zapisywania do pliku
plot_mc_histogram_to_file <- function(simulated_stats, observed_stat, title, filename, color) {
  # Otwórz urządzenie graficzne (plik PNG)
  png(filename, width = 800, height = 600)
  
  # Histogram wartości symulowanych
  hist(simulated_stats, probability = TRUE, breaks = 30,
       col = color, border = "white",
       main = title, xlab = "Statystyka KS", ylab = "Gęstość")
  
  # Dodanie linii dla statystyki obserwowanej
  abline(v = observed_stat, col = "red", lwd = 2, lty = 2)
  
  # Dodanie legendy
  legend("topright", legend = c("Obserwowana statystyka KS"),
         col = "red", lwd = 2, lty = 2, bty = "n")
  
  # Zamknij urządzenie graficzne
  dev.off()
}

# Zapis histogramu dla Allegro
plot_mc_histogram_to_file(mc_result_allegro$simulated_stats,
                          mc_result_allegro$statistic,
                          "Monte Carlo: Allegro",
                          "MonteCarlo_Allegro.png",
                          "lightblue")

# Zapis histogramu dla LPP
plot_mc_histogram_to_file(mc_result_lpp$simulated_stats,
                          mc_result_lpp$statistic,
                          "Monte Carlo: LPP",
                          "MonteCarlo_LPP.png",
                          "slateblue2")


allegro$log_returns <- c(NA, diff(log(as.numeric(allegro$Zamkniecie))))
lpp$log_returns <- c(NA, diff(log(as.numeric(lpp$Zamkniecie))))

log_returns <- data.frame(
  Allegro = na.omit(allegro$log_returns),
  LPP = na.omit(lpp$log_returns)
)

mu <- colMeans(log_returns)

cov_matrix <- cov(log_returns)

cor_coefficient <- cor(log_returns$Allegro, log_returns$LPP)

cor_matrix <- cor(log_returns)

cat("Wektor średnich:\n")
print(mu)

cat("\nMacierz kowariancji:\n")
print(cov_matrix)

cat("\nWspółczynnik korelacji:\n")
print(cor_coefficient)

cat("\nMacierz korelacji:\n")
print(cor_matrix)

library(ggplot2)
library(ggExtra)

allegro$log_returns <- c(NA, diff(log(as.numeric(allegro$Zamkniecie))))
lpp$log_returns <- c(NA, diff(log(as.numeric(lpp$Zamkniecie))))

log_returns <- data.frame(
  Allegro = na.omit(allegro$log_returns),
  LPP = na.omit(lpp$log_returns)
)

scatter_plot <- ggplot(log_returns, aes(x = Allegro, y = LPP)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(x = "Log-zwroty Allegro", y = "Log-zwroty LPP") +
  theme_minimal()

scatter_with_marginals <- ggMarginal(scatter_plot, type = "histogram", fill = "steelblue")

library(mnormt)
library(ggplot2)

allegro$log_returns <- c(NA, diff(log(as.numeric(allegro$Zamkniecie))))
lpp$log_returns <- c(NA, diff(log(as.numeric(lpp$Zamkniecie))))

log_returns <- data.frame(
  Allegro = na.omit(allegro$log_returns),
  LPP = na.omit(lpp$log_returns)
)

mu <- colMeans(log_returns)
Sigma <- cov(log_returns)

x <- seq(min(log_returns$Allegro), max(log_returns$Allegro), length.out = 100)
y <- seq(min(log_returns$LPP), max(log_returns$LPP), length.out = 100)

f <- function(x, y) dmnorm(cbind(x, y), mean = mu, varcov = Sigma)
z <- outer(x, y, f)

persp(
  x, y, z, theta = -30, phi = 25,
  shade = 0.75, col = "lightblue", expand = 0.5, r = 2,
  ltheta = 25, ticktype = "detailed",
  xlab = "Log-zwroty Allegro", ylab = "Log-zwroty LPP", zlab = "Gęstość"
)

par(mfrow = c(2, 1))
curve(dnorm(x, mean = mu[1], sd = sqrt(Sigma[1, 1])), xlim = c(-3 * sqrt(Sigma[1, 1]), 3 * sqrt(Sigma[1, 1])),
      col = "blue", xlab = "Log-zwroty Allegro", ylab = "Gęstość", main = "Gęstość brzegowa Allegro")
grid()

curve(dnorm(x, mean = mu[2], sd = sqrt(Sigma[2, 2])), xlim = c(-3 * sqrt(Sigma[2, 2]), 3 * sqrt(Sigma[2, 2])),
      col = "green", xlab = "Log-zwroty LPP", ylab = "Gęstość", main = "Gęstość brzegowa LPP")
grid()

setwd("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt")

png("joint_density_persp.png", width = 800, height = 600)
persp(
  x, y, z, theta = -30, phi = 25,
  shade = 0.75, col = "lightblue", expand = 0.5, r = 2,
  ltheta = 25, ticktype = "detailed",
  xlab = "Log-zwroty Allegro", ylab = "Log-zwroty LPP", zlab = "Gęstość"
)
dev.off()

png("marginal_density_allegro.png", width = 800, height = 400)
curve(dnorm(x, mean = mu[1], sd = sqrt(Sigma[1, 1])), xlim = c(-3 * sqrt(Sigma[1, 1]), 3 * sqrt(Sigma[1, 1])),
      col = "blue", xlab = "Log-zwroty Allegro", ylab = "Gęstość", main = "Gęstość brzegowa Allegro")
grid()
dev.off()

png("marginal_density_lpp.png", width = 800, height = 400)
curve(dnorm(x, mean = mu[2], sd = sqrt(Sigma[2, 2])), xlim = c(-3 * sqrt(Sigma[2, 2]), 3 * sqrt(Sigma[2, 2])),
      col = "green", xlab = "Log-zwroty LPP", ylab = "Gęstość", main = "Gęstość brzegowa LPP")
grid()
dev.off()

allegro <- read.csv("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt/ale_d.csv")
lpp <- read.csv("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt/lpp_d.csv")

allegro$Data <- as.Date(allegro$Data)
lpp$Data <- as.Date(lpp$Data)

allegro$log_returns <- c(NA, diff(log(allegro$Zamkniecie)))
lpp$log_returns <- c(NA, diff(log(lpp$Zamkniecie)))

library(mnormt)
library(MASS)
library(ggplot2)

allegro$log_returns <- c(NA, diff(log(as.numeric(allegro$Zamkniecie))))
lpp$log_returns <- c(NA, diff(log(as.numeric(lpp$Zamkniecie))))

log_returns <- data.frame(
  Allegro = na.omit(allegro$log_returns),
  LPP = na.omit(lpp$log_returns)
)

mu <- colMeans(log_returns)
Sigma <- cov(log_returns)

n <- nrow(log_returns)
sample_generated <- rmnorm(n, mean = mu, varcov = Sigma)

sample_generated_df <- as.data.frame(sample_generated)
colnames(sample_generated_df) <- c("Allegro", "LPP")

plot_real <- ggplot(log_returns, aes(x = Allegro, y = LPP)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(x = "Log-zwroty Allegro", y = "Log-zwroty LPP") +
  theme_minimal()

setwd("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt")
ggsave("scatter_real_data.png", plot = plot_real, width = 6, height = 6)

plot_generated <- ggplot(sample_generated_df, aes(x = Allegro, y = LPP)) +
  geom_point(alpha = 0.6, color = "red") +
  labs(x = "Log-zwroty Allegro", y = "Log-zwroty LPP") +
  theme_minimal()

ggsave("scatter_generated_sample.png", plot = plot_generated, width = 6, height = 6)

allegro <- read.csv("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt/ale_d.csv")
lpp <- read.csv("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt/lpp_d.csv")

allegro$log_returns <- c(NA, diff(log(allegro$Zamkniecie)))
lpp$log_returns <- c(NA, diff(log(lpp$Zamkniecie)))

allegro_data <- na.omit(allegro$log_returns)
lpp_data <- na.omit(lpp$log_returns)

bootstrap_ci <- function(data, B = 1000, alpha = 0.05) {
  n <- length(data)
  bootstrap_means <- numeric(B)
  
  for (i in 1:B) {
    sample_data <- sample(data, size = n, replace = TRUE)
    bootstrap_means[i] <- mean(sample_data)
  }
  
  ci <- quantile(bootstrap_means, probs = c(alpha / 2, 1 - alpha / 2))
  return(ci)
}

ci_allegro <- bootstrap_ci(allegro_data, B = 1000, alpha = 0.05)
cat("Przedział ufności dla Allegro (95%):\n", ci_allegro, "\n")

ci_lpp <- bootstrap_ci(lpp_data, B = 1000, alpha = 0.05)
cat("Przedział ufności dla LPP (95%):\n", ci_lpp, "\n")

library(ggplot2)
bootstrap_plot <- function(bootstrap_means, ci, fill_color) {
  df <- data.frame(bootstrap_means = bootstrap_means)
  
  ggplot(df, aes(x = bootstrap_means)) +
    geom_histogram(binwidth = 0.0005, fill = fill_color, color = "black", alpha = 0.7) +
    geom_vline(xintercept = ci, color = "#9400D3", linetype = "dashed", linewidth = 1) +
    labs(
      x = "Średnie bootstrapowe",
      y = "Częstość"
    ) +
    theme_minimal()
}

bootstrap_means_allegro <- replicate(1000, mean(sample(allegro_data, size = length(allegro_data), replace = TRUE)))
ci_allegro <- quantile(bootstrap_means_allegro, probs = c(0.025, 0.975))
g1 <- bootstrap_plot(bootstrap_means_allegro, ci_allegro, fill_color = '#B2DFEE')
ggsave("bootstrap_allegro.png", g1)

bootstrap_means_lpp <- replicate(1000, mean(sample(lpp_data, size = length(lpp_data), replace = TRUE)))
ci_lpp <- quantile(bootstrap_means_lpp, probs = c(0.025, 0.975))
g2 <- bootstrap_plot(bootstrap_means_lpp, ci_lpp, fill_color = '#D1EEEE')
ggsave("bootstrap_lpp.png", g2)

###### 3.2
library(ggplot2)
allegro <- read.csv("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt/ale_d.csv")
lpp <- read.csv("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt/lpp_d.csv")

allegro$log_returns <- c(NA, diff(log(allegro$Zamkniecie)))
lpp$log_returns <- c(NA, diff(log(lpp$Zamkniecie)))

log_returns_data <- na.omit(data.frame(R1 = allegro$log_returns, R2 = lpp$log_returns))

linear_model <- lm(R2 ~ R1, data = log_returns_data)

summary_model <- summary(linear_model)
print(summary_model)
x_mean <- mean(log_returns_data$R1)
cat("Średnia R1:", x_mean, "\n")
manual_predicted_R2 <- 0.0001968 + 0.4022814 * x_mean
cat("Ręczna predykcja R2:", manual_predicted_R2, "\n")
predicted_R2_check <- predict(linear_model, newdata = data.frame(R1 = x_mean))
cat("Predykcja R2 z `predict()`: ", predicted_R2_check, "\n")

scatter_plot <- ggplot(log_returns_data, aes(x = R1, y = R2)) +
  geom_point(color = "#00868B", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#473C8B", se = TRUE) +
  labs(
    x = "Log-zwroty Allegro (R1)",
    y = "Log-zwroty LPP (R2)"
  ) +
  theme_minimal()

print(scatter_plot)  

ggsave("scatter_plot_regression.png", scatter_plot, width = 20, height = 2, dpi = 300)


residuals <- resid(linear_model)

png("residuals_histogram.png")
hist(residuals, breaks = 30, col = "#7AC5CD", main = "", xlab = "Reszty")
dev.off()

png("residuals_qqplot.png")
qqnorm(residuals, main = "")
qqline(residuals, col = "#00868B")
dev.off()

library(nortest)
shapiro_test <- shapiro.test(residuals)
ad_test <- ad.test(residuals)
ks_test <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))

cat("Wyniki testów normalności:\n")
print(shapiro_test)
print(ad_test)
print(ks_test)

cat("\nWspółczynniki regresji:\n")
print(coef(summary_model))
cat("\nWnioski:\n")
if (coef(summary_model)[2, "Pr(>|t|)"] < 0.05) {
  cat("Współczynnik nachylenia (b1) jest istotny statystycznie.\n")
} else {
  cat("Współczynnik nachylenia (b1) nie jest istotny statystycznie.\n")
}

r_squared <- summary_model$r.squared
cat("\nWspółczynnik determinacji (R^2):", r_squared, "\n")
cat("R^2 wskazuje, jaka część zmienności R2 jest wyjaśniana przez zmienność R1.\n")

mean_R1 <- mean(log_returns_data$R1)
predicted_R2 <- predict(linear_model, newdata = data.frame(R1 = mean_R1))
cat("\nPredykcja dla średniej R1 (", mean_R1, "):", predicted_R2, "\n")


#------------ na 4+ i wyżej
summary_model <- summary(linear_model)

if (summary_model$coefficients[1, 4] > 0.05) {
  cat("Wyraz wolny b0 jest nieistotny. Dopasowanie modelu uproszczonego R2 ~ R1 - 1 (bez b0).\n")
  simplified_model <- lm(R2 ~ R1 - 1, data = log_returns_data)
} else if (summary_model$coefficients[2, 4] > 0.05) {
  cat("Nachylenie b1 jest nieistotne. Dopasowanie modelu uproszczonego R2 ~ 1 (tylko b0).\n")
  simplified_model <- lm(R2 ~ 1, data = log_returns_data)
} else {
  cat("Oba współczynniki b0 i b1 są istotne. Model pełny jest właściwy.\n")
  simplified_model <- linear_model
}

simplified_summary <- summary(simplified_model)
print(simplified_summary)

mean_R1 <- mean(log_returns_data$R1)
predicted_R2_simple <- predict(simplified_model, newdata = data.frame(R1 = mean_R1))
cat("Predykcja R2 dla średniego R1 (", mean_R1, ") w uproszczonym modelu:", predicted_R2_simple, "\n")
library(ggplot2)

linear_model <- lm(R2 ~ R1, data = log_returns_data)

x_mean <- mean(log_returns_data$R1)
cat("Średnia R1:", x_mean, "\n")

predicted_R2 <- predict(linear_model, newdata = data.frame(R1 = x_mean))
cat("Predykcja R2:", predicted_R2, "\n")

n <- nrow(log_returns_data)  # Liczba obserwacji
x_var <- sum((log_returns_data$R1 - x_mean)^2)  
sigma_squared <- sum(residuals(linear_model)^2) / (n - 2) 

standard_error <- sqrt(
  sigma_squared * (1 / n + (x_mean - mean(log_returns_data$R1))^2 / x_var)
)

t_quantile <- qt(0.975, df = n - 2)

ci_analytical <- c(
  predicted_R2 - t_quantile * standard_error,
  predicted_R2 + t_quantile * standard_error
)
cat("Przedział ufności (analityczny):", ci_analytical, "\n")

set.seed(123)  
B <- 1000
predictions <- numeric(B)

for (i in 1:B) {
  sample_data <- log_returns_data[sample(1:nrow(log_returns_data), replace = TRUE), ]
  boot_model <- lm(R2 ~ R1, data = sample_data)
  predictions[i] <- predict(boot_model, newdata = data.frame(R1 = x_mean))
}

ci_bootstrap <- quantile(predictions, probs = c(0.025, 0.975))
cat("Przedział ufności (bootstrap):", ci_bootstrap, "\n")

bootstrap_plot <- ggplot(data.frame(predictions = predictions), aes(x = predictions)) +
  geom_histogram(binwidth = 0.00001, fill = "#BBFFFF", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_bootstrap, color = "#CAE1FF", linetype = "dashed", size = 1) +
  labs(
    x = "Predykcje R2",
    y = "Częstość",
    title = "Histogram predykcji bootstrapowych"
  ) +
  theme_minimal()

print(bootstrap_plot)

ggsave("/Users/lukaszkulpaczynski/Desktop/modelowanie/projekt/bootstrap_prediction.png", bootstrap_plot, width = 8, height = 6)

