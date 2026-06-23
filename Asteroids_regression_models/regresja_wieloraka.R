# ==============================================================================
# PROJEKT 3 - REGRESJA WIELORAKA (MODEL PEŁNY)
# Skrypt: regresja_wieloraka.R, oparty na metodologii z Lab 12 i 13
# Autor: Wiktoria Awramik
# ==============================================================================

library(dplyr)
library(corrplot)
library(lmtest)
library(car)
library(olsrr)

# Wczytanie oczyszczonego wcześniej zbioru (n=10000)
df <- read.csv("asteroidy_clean.csv")

# 1. Macierz korelacji dla wszystkich cech ilościowych
macierz_korelacji <- cor(df %>% select_if(is.numeric))
corrplot(macierz_korelacji, method = "number", number.cex = 0.6, tl.cex = 0.7)

# 2. Budowa pierwszego pełnego modelu (diameter jako Y, reszta jako X)
model_pelny <- lm(diameter ~ . - q, data = df)

cat("\n--- PODSUMOWANIE PEŁNEGO MODELU WIELORAKIEGO ---\n")
print(summary(model_pelny))

# 3. Wyciągnięcie reszt i sprawdzenie założeń (Analiza reszt)
reszty_wielorakie <- residuals(model_pelny)

# Wykresy diagnostyczne bazowe
par(mfrow = c(2, 2))
plot(model_pelny)
par(mfrow = c(1, 1))

# Test normalności (używamy KS, bo Shapiro-Wilk ma limit do 5000 obserwacji!)
test_normalnosci <- ks.test(reszty_wielorakie, "pnorm", mean = mean(reszty_wielorakie), sd = sd(reszty_wielorakie))
cat("\n--- TEST NORMALNOŚCI RESZT (Kolmogorov-Smirnov) ---\n")
print(test_normalnosci)

# Test homoskedastyczności (Breusch-Pagan)
cat("\n--- TEST HOMOSKEDASTYCZNOŚCI RESZT (Breusch-Pagan) ---\n")
print(bptest(model_pelny))

# Test autokorrelation reszt (Box-Pierce dla lag = 1, 2, 3)
cat("\n--- TEST AUTOKORELACJI RESZT (Box-Pierce) ---\n")
print(Box.test(reszty_wielorakie, lag = 1))
print(Box.test(reszty_wielorakie, lag = 2))
print(Box.test(reszty_wielorakie, lag = 3))

# 4. Analiza współliniowości wskaźnikiem VIF
cat("\n--- ANALIZA WSPÓŁLINIOWOŚCI (VIF) ---\n")
print(vif(model_pelny))












# ==============================================================================
# REDUKCJA WYMIAROWOŚCI – REGRESJA KROKOWA
# ==============================================================================

# 1. Eliminacja wsteczna (Backward Selection) oparta o p-value
eliminacja_w_tyl <- ols_step_backward_p(model_pelny)
cat("\n--- PRZEBIEG ELIMINACJI WSTECZNEJ ---\n")
print(eliminacja_w_tyl)

# 2. Selekcja postępowa (Forward Selection) oparta o p-value
selekcja_w_przod <- ols_step_forward_p(model_pelny)
cat("\n--- PRZEBIEG SELEKCJI POSTĘPOWEJ ---\n")
print(selekcja_w_przod)














# ==============================================================================
#  MODEL BAZOWY I ZAAWANSOWANA DIAGNOSTYKA
# ==============================================================================

library(dplyr)
library(olsrr)

# 1. Budowa modelu bazowego (zredukowanego) na podstawie wyników krokowej
model_bazowy <- lm(diameter ~ . - q - ma - w, data = df)

cat("\n--- PODSUMOWANIE MODELU BAZOWEGO ---\n")
print(summary(model_bazowy))

# 2. Wykresy Odległości Cooka
ols_plot_cooksd_chart(model_bazowy)

# 3. Wykres DFFITS (Wpływ obserwacji na dopasowanie modelu)
ols_plot_dffits(model_bazowy)

# 4. Wykresy Reszt Studentyzowanych i Standaryzowanych (Wykrywanie punktów odstających)
ols_plot_resid_stud(model_bazowy)
ols_plot_resid_stand(model_bazowy)

# 5. Wykres Reszt względem Dźwigni (Cztery strefy decyzyjne)
ols_plot_resid_lev(model_bazowy)

# 6. Panel DFBETAs (Wpływ obserwacji na poszczególne współczynniki Beta)
dfb <- as.data.frame(dfbetas(model_bazowy))
cat("\n--- TOP 5 ANOMALII DLA WSPÓŁCZYNNIKA ALBEDO (DFBETAs) ---\n")
print(head(dfb[order(abs(dfb$albedo), decreasing = TRUE), "albedo", drop = FALSE], 5))

cat("\n--- TOP 5 ANOMALII DLA WSPÓŁCZYNNIKA JASNOŚCI H (DFBETAs) ---\n")
print(head(dfb[order(abs(dfb$H), decreasing = TRUE), "H", drop = FALSE], 5))





# ==============================================================================
# CZYSZCZENIE I BUDOWA MODELU KOŃCOWEGO
# ==============================================================================

cooks_d <- cooks.distance(model_bazowy)
prog_cooka <- 4 / nrow(df)
indeksy_wplywowe <- which(cooks_d > prog_cooka)

cat("Liczba wartości wpływowych do usunięcia (4/n):", length(indeksy_wplywowe), "\n")

# Oczyszczenie zbioru z anomalii
df_clean <- df[-indeksy_wplywowe, ]

# Budowa ostatecznego, czystego modelu wielorakiego
model_koncowy <- lm(diameter ~ . - q - ma - w, data = df_clean)

cat("\n--- PODSUMOWANIE OSTATECZNEGO MODELU KOŃCOWEGO ---\n")
print(summary(model_koncowy))




# ==============================================================================
# PRZEDZIAŁY UFNOŚCI I PREDYKCJI (95%) - MODEL WIELORAKI KOŃCOWY
# ==============================================================================

nowy_obiekt_wieloraki <- df_clean %>% 
  summarise(across(everything(), mean))

# Wyznaczenie obu przedziałów dla modelu wielorakiego końcowego
pu_wieloraki <- predict(model_koncowy, newdata = nowy_obiekt_wieloraki, interval = "confidence", level = 0.95)
pp_wieloraki <- predict(model_koncowy, newdata = nowy_obiekt_wieloraki, interval = "prediction", level = 0.95)

cat("\n--- MODEL WIELORAKI KOŃCOWY (Dla średnich wartości cech) ---\n")
cat("Przedział ufności dla płaszczyzny regresji (fit, lwr, upr):\n")
print(pu_wieloraki)
cat("\nPrzedział predykcji dla nowej obserwacji (fit, lwr, upr):\n")
print(pp_wieloraki)