# ==============================================================================
# PROJEKT 3 - STATYSTYKA (SAD)
# Skrypt: czyszczenie_danych.R
# Autor: Wiktoria Awramik
# ==============================================================================

library(dplyr)

# KROK 1: Wczytanie danych i praca na kopii roboczej

if(!file.exists("asteroidy_nasa.csv")) {
  stop("Błąd: Plik 'asteroidy_nasa.csv' nie został znaleziony w katalogu roboczym!")
}

raw_data <- read.csv("asteroidy_nasa.csv", stringsAsFactors = FALSE)

df_worker <- raw_data

cat("Wczytano surowy zbiór. Liczba rekordów:", nrow(df_worker), "\n")


# KROK 2: Sprawdzanie braków, konwersja typów i filtracja

# Upewnienie się o numerycznym charakterze wszystkich kolumn
df_worker <- df_worker %>%
  mutate(across(everything(), as.numeric))

# Analiza braków danych (NA) przed czyszczeniem
braki_przed <- colSums(is.na(df_worker))
cat("\nLiczba braków danych (NA) w poszczególnych kolumnach przed czyszczeniem:\n")
print(braki_przed)

# Obsługa braków danych - usunięcie wierszy z NA (na.omit)
df_clean <- df_worker %>%
  na.omit()

# Usunięcie skrajnych wartości (Gigantów) dla lepszej stabilności regresji
# Eliminujemy obiekty o średnicy powyżej 100 km (bardzo rzadkie, zaburzające model liniowy)
df_clean <- df_clean %>%
  filter(diameter > 0 & diameter < 100)

cat("\nLiczba rekordów po usunięciu NA i odfiltrowaniu gigantów:", nrow(df_clean), "\n")



# KROK 3: Wybór reprezentatywnej próby i zapis do nowego pliku

set.seed(111)

# Losujemy 10 000 obserwacji do ostatecznych analiz
df_final <- df_clean %>% 
  sample_n(10000)

cat("\nWylosowano ostateczną próbę o wielkości:", nrow(df_final), "obserwacji.\n")

# Sprawdzenie struktury i braków w pliku wynikowym
cat("\nLiczba braków danych w pliku finalnym:\n")
print(colSums(is.na(df_final)))

# Zapisujemy oczyszczone dane do nowego pliku
write.csv(df_final, "asteroidy_clean.csv", row.names = FALSE)
cat("\nOczyszczone dane zostały zapisane do pliku: 'asteroidy_clean.csv'\n")