# ---------------------------------------------------------
# 1. FUNKCJE POMOCNICZE (Dominanta, Skośność, Kurtoza)
# ---------------------------------------------------------

# dominanta
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(na.omit(v), uniqv)))]
}

# skośność
get_skewness <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
}

# kurtoza
get_kurtosis <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  (sum((x - mean(x))^4) / n) / (sum((x - mean(x))^2) / n)^2 - 3
}

# ---------------------------------------------------------
# 2. TABELA STATYSTYK DLA ZMIENNYCH ILOŚCIOWYCH
# ---------------------------------------------------------

oblicz_statystyki_ilosciowe <- function(x) {
  x <- na.omit(x)
  c(
    Srednia = round(mean(x), 2),
    Mediana = round(median(x), 2),
    Dominanta = get_mode(x),
    Minimum = min(x),
    Maksimum = max(x),
    Rozstep = max(x) - min(x),
    Kwartyl_Dolny_Q1 = quantile(x, 0.25),
    Kwartyl_Gorny_Q3 = quantile(x, 0.75),
    Rozstep_Miedzykwartylowy_IQR = IQR(x),
    Wariancja = round(var(x), 2),
    Odchylenie_Standardowe = round(sd(x), 2),
    Wspolczynnik_Zmiennosci_Proc = round((sd(x) / mean(x)) * 100, 2),
    Skosnosc = round(get_skewness(x), 2),
    Kurtoza = round(get_kurtosis(x), 2)
  )
}


statystyki_ilosciowe <- data.frame(
  Miara_Statystyczna = names(oblicz_statystyki_ilosciowe(df$Price)),
  Cena_Mln_USD = unname(oblicz_statystyki_ilosciowe(df$Price)),
  Rok_Startu = unname(oblicz_statystyki_ilosciowe(df$Year))
)

# ---------------------------------------------------------
# 3. TABELE STATYSTYK DLA ZMIENNYCH JAKOŚCIOWYCH
# ---------------------------------------------------------

tabela_status <- table(df$MissionStatus)
statystyki_jakosc <- data.frame(
  Status = names(tabela_status),
  Liczebnosc = as.integer(tabela_status),
  Czestosc = round(as.numeric(prop.table(tabela_status)), 4),
  Procent = round(as.numeric(prop.table(tabela_status)) * 100, 2)
)

# ---------------------------------------------------------
# 4. WYŚWIETLENIE TABEL
# ---------------------------------------------------------

View(statystyki_ilosciowe)
View(statystyki_jakosc)

cat("\n--- DOMINANTY JAKOŚCIOWE ---\n")
cat("Dominanta dla Statusu Misji:", statystyki_status$Status[which.max(statystyki_status$Liczebnosc)], "\n")

