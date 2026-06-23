# ==============================================================================
# PROJEKT 3 - STATYSTYKA (SAD)
# Skrypt: regresja_liniowa_prosta.R
# Budowanie i podstawowa analiza modelu dla zmiennych: diameter (Y) i H (X)
# Autor: Wiktoria Awramik
# ==============================================================================
#install.packages("lmtest")

library(dplyr)

df <- read.csv("asteroidy_clean.csv")

model_prosty <- lm(diameter ~ H, data = df)

summary_results <- summary(model_prosty)
print(summary_results)

n <- nrow(df)
r_squared <- summary_results$r.squared

t_cor <- sqrt(r_squared) / sqrt((1 - r_squared) / (n - 2))
cat("\n--- TEST ISTOTNOŚCI KORELACJI ---\n")
cat("Statystyka t-empiryczna dla korelacji:", t_cor, "\n")

t_krytyczne <- qt(1 - 0.05/2, df = n - 2)
cat("Wartość krytyczna t-Studenta (alfa=0.05):", t_krytyczne, "\n")

if (abs(t_cor) > t_krytyczne) {
  cat("Decyzja: Odrzucamy hipotezę H0. Współczynnik korelacji jest ISTOTNY statystycznie.\n")
} else {
  cat("Decyzja: Brak podstaw do odrzucenia H0. Współczynnik korelacji jest NIEISTOTNY statystycznie.\n")
}


# ==============================================================================
# DIAGNOSTYKA RESZT (Sprawdzenie założeń modelu)
# ==============================================================================

par(mfrow = c(2, 2))
plot(model_prosty)
par(mfrow = c(1, 1))

# Testy statystyczne założeń
reszty <- residuals(model_prosty)
test_normalnosci <- ks.test(reszty, "pnorm", mean = mean(reszty), sd = sd(reszty))
cat("\n--- TEST NORMALNOŚCI RESZT (Kolmogorov-Smirnov) ---\n")
print(test_normalnosci)

# Test homoskedastyczności (stałości wariancji reszt) - Test Breuscha-Pagana
# Wymaga zainstalowanej biblioteki lmtest (jeśli nie masz, wpisz najpierw: install.packages("lmtest"))
library(lmtest)
test_bptest <- bptest(model_prosty)
cat("\n--- TEST HOMOSKEDASTYCZNOŚCI RESZT (Breusch-Pagan) ---\n")
print(test_bptest)

cat("\n--- TEST AUTOKORELACJI RESZT (Box-Pierce) ---\n")
print(Box.test(reszty, lag = 1))
print(Box.test(reszty, lag = 2))
print(Box.test(reszty, lag = 3))






# ==============================================================================
# PRZEDZIAŁY UFNOŚCI I PREDYKCJI (95%) ORAZ AUTOKORELACJA - MODEL PROSTY
# ==============================================================================

# 1. Test Autokorelacji reszt (Box-Pierce) dla modelu prostego
reszty_proste <- residuals(model_prosty)
cat("\n--- TEST AUTOKORELACJI RESZT (Box-Pierce) DLA MODELU PROSTEGO ---\n")
print(Box.test(reszty_proste, lag = 1, type = "Box-Pierce"))
print(Box.test(reszty_proste, lag = 2, type = "Box-Pierce"))
print(Box.test(reszty_proste, lag = 3, type = "Box-Pierce"))

# 2. Obliczenie przedziałów ufności i predykcji dla średniej wartości H
nowy_obiekt_prosty <- data.frame(H = mean(df$H))

pu_prosty <- predict(model_prosty, newdata = nowy_obiekt_prosty, interval = "confidence", level = 0.95)
pp_prosty <- predict(model_prosty, newdata = nowy_obiekt_prosty, interval = "prediction", level = 0.95)

cat("\n--- MODEL PROSTY (Dla średniej wartości jasności H) ---\n")
cat("Przedział ufności dla linii regresji (confidence):\n")
print(pu_prosty)
cat("\nPrzedział predykcji dla nowej obserwacji (prediction):\n")
print(pp_prosty)




# ==============================================================================
# WYKRYWANIE WARTOŚCI WPŁYWOWYCH I ODSTAJĄCYCH (Wykres pudełkowy i Odległość Cooka)
# ==============================================================================

# 1. Wykres pudełkowy reszt
boxplot(reszty, 
        main = "Wykres pudełkowy reszt modelu prostego", 
        col = "lightblue", 
        ylab = "Wartości reszt (e_i)",
        horizontal = TRUE)

# 2. Obliczenie odległości Cooka
cooks_dist <- cooks.distance(model_prosty)
prog_cooka <- 4 / nrow(df)

plot(cooks_dist, type = "h", 
     main = "Odległość Cooka dla poszczególnych obserwacji", 
     ylab = "Odległość Cooka", xlab = "Indeks obserwacji", col = "darkgray")
abline(h = prog_cooka, col = "red", lty = 2, lwd = 2)

# Identyfikacja i usunięcie
wplywowe_indeksy <- which(cooks_dist > prog_cooka)
cat("Liczba zidentyfikowanych obserwacji wpływowych (kryterium 4/n):", length(wplywowe_indeksy), "\n")

top_anomalie <- head(sort(cooks_dist, decreasing = TRUE), 5)
cat("\n5 najwyższych wartości odległości Cooka w próbie:\n")
print(top_anomalie)

df_no_outliers <- df[-wplywowe_indeksy, ]

# Budowa nowego modelu
model_nowy <- lm(diameter ~ H, data = df_no_outliers)
cat("\n--- PODSUMOWANIE NOWEGO MODELU PO USUNIĘCIU ANOMALII ---\n")
print(summary(model_nowy))




# ==============================================================================
# KROSWALIDACJA (5-fold Cross-Validation)
# ==============================================================================
set.seed(111)

n_nowy <- nrow(df_no_outliers)
df_no_outliers_shuffled <- df_no_outliers[sample(n_nowy), ]

# Podział zbioru na 5 równych podzbiorów
folds <- cut(seq(1, n_nowy), breaks = 5, labels = FALSE)

r2_train_vec <- numeric(5)
r2_test_vec  <- numeric(5)
rse_train_vec <- numeric(5)
rse_test_vec  <- numeric(5)

for(i in 1:5) {
  # Wyznaczenie indeksów podzbioru testowego (20%) i treningowego (80%)
  test_indices <- which(folds == i, arr.ind = TRUE)
  test_data   <- df_no_outliers_shuffled[test_indices, ]
  train_data  <- df_no_outliers_shuffled[-test_indices, ]
  
  # Budowa modelu na zbiorze treningowym
  model_cv <- lm(diameter ~ H, data = train_data)
  summary_cv <- summary(model_cv)
  
  # Zapis metryk dla zbioru treningowego
  r2_train_vec[i]  <- summary_cv$r.squared
  rse_train_vec[i] <- summary_cv$sigma
  
  # Predykcja na zbiorze testowym
  pred_test <- predict(model_cv, newdata = test_data)
  
  # Liczenie metryk dla zbioru testowego
  actual_test <- test_data$diameter
  sse_test    <- sum((actual_test - pred_test)^2)
  sst_test    <- sum((actual_test - mean(actual_test))^2)
  
  r2_test_vec[i]  <- 1 - (sse_test / sst_test)
  rse_test_vec[i] <- sqrt(sse_test / (nrow(test_data) - 2))
  
  # Wyświetlenie wyników dla konkretnej iteracji
  cat(paste0("\n--- Model ", i, " --- \n"))
  cat("Trening -> R^2:", round(r2_train_vec[i], 4), "| RSE:", round(rse_train_vec[i], 4), "\n")
  cat("Test    -> R^2:", round(r2_test_vec[i], 4), "| RSE:", round(rse_test_vec[i], 4), "\n")
}

# Uśrednienie wyników kroswalidacji
cat("\n==============================================\n")
cat("ŚREDNIE WYNIKI KROSWALIDACJI:\n")
cat("Średni R^2 (Trening):", round(mean(r2_train_vec), 4), "\n")
cat("Średni R^2 (Test):   ", round(mean(r2_test_vec), 4), "\n")
cat("Średni RSE (Trening):", round(mean(rse_train_vec), 4), "\n")
cat("Średni RSE (Test):   ", round(mean(rse_test_vec), 4), "\n")
cat("==============================================\n")



