# install.packages("ggplot2")
library(dplyr)
library(ggplot2)


# ---------------------------------------------------------
# PRZYGOTOWANIE DANYCH DO ANALIZY
# ---------------------------------------------------------

df <- read.csv("space_missions.csv", stringsAsFactors = FALSE)

df$Price <- as.numeric(gsub(",", "", df$Price))

df$Date <- as.Date(df$Date)
df$Year <- as.numeric(format(df$Date, "%Y"))

head(df[, c("Date", "Year", "Price")])

summary(df$Price)

write.csv(df, "space_missions_clean.csv", row.names = FALSE)




# ---------------------------------------------------------
# WCZYTANIE PLIKU
# ---------------------------------------------------------

df <- read.csv("space_missions_clean.csv", stringsAsFactors = FALSE)

str(df)






# ---------------------------------------------------------
# PUNKT 3: STATYSTYKI OPISOWE
# ---------------------------------------------------------


# 1. FUNKCJE POMOCNICZE (Dominanta, Skośność, Kurtoza)


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


# 2. TABELA STATYSTYK DLA ZMIENNYCH ILOŚCIOWYCH

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


# 3. TABELE STATYSTYK DLA ZMIENNYCH JAKOŚCIOWYCH

tabela_status <- table(df$MissionStatus)
statystyki_jakosc <- data.frame(
  Status = names(tabela_status),
  Liczebnosc = as.integer(tabela_status),
  Czestosc = round(as.numeric(prop.table(tabela_status)), 4),
  Procent = round(as.numeric(prop.table(tabela_status)) * 100, 2)
)

View(statystyki_ilosciowe)
View(statystyki_jakosc)





# ------------------------------------------------------------------
# WIZUALIZACJA DANYCH DO PUNKTU ZE STATYSTYKAMI OPISOWYMI
# ------------------------------------------------------------------


# --- HISTOGRAM (Rozkład cen misji) ---

wykres_1 <- ggplot(df, aes(x = Price)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  scale_x_log10(breaks = c(3, 10, 63, 100, 128, 450, 1000, 5000), labels = scales::comma) + # Używam skali logarytmicznej dla lepszej czytelności
  labs(
    title = "Rozkład kosztów misji kosmicznych",
    subtitle = "Zastosowano skalę logarytmiczną na osi X z uwagi na silną skośność prawostronną",
    x = "Koszt misji (mln USD) - Skala logarytmiczna",
    y = "Liczba misji"
  ) +
  theme_minimal()
print(wykres_1)

# --- BOXPLOT (Koszty misji dla topowych firm) ---
# 5 najczęstszych firm, by wykres był czytelny

top_5_firm <- names(sort(table(df$Company[!is.na(df$Price)]), decreasing = TRUE)[1:5])
df_top_firmy <- subset(df, Company %in% top_5_firm)

wykres_2 <- ggplot(df_top_firmy, aes(x = Company, y = Price, fill = Company)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Zróżnicowanie kosztów misji (Top 5 agencji/firm)",
    x = "Agencja / Firma",
    y = "Koszt misji (mln USD) - Skala log",
    fill = "Firma"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(wykres_2)


# --- SCATTER PLOT (Zależność kosztu od roku startu) ---

wykres_4 <- ggplot(df, aes(x = Year, y = Price)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # linia trendu
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Wykres rozrzutu: Koszt misji a Rok startu",
    x = "Rok startu",
    y = "Koszt misji (mln USD) - Skala log"
  ) +
  theme_minimal()
print(wykres_4)


# --- PIE CHART (Status misji) ---

status_count <- as.data.frame(table(df$MissionStatus))
colnames(status_count) <- c("Status", "Count")

wykres_6 <- ggplot(status_count, aes(x = "", y = Count, fill = Status)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  labs(
    title = "Procentowy udział statusów misji",
    fill = "Status"
  ) +
  theme_void()
print(wykres_6)







# ---------------------------------------------------------
# PUNKT 4: ESTYMACJA PUNKTOWA I PRZEDZIAŁOWA
# ---------------------------------------------------------

# Estymacja przedziałowa dla proporcji (szansa na sukces misji)
sukcesy <- sum(df$MissionStatus == "Success")
wszystkie <- nrow(df)
estymacja_prop <- prop.test(x = sukcesy, n = wszystkie, conf.level = 0.95)

prop_est <- round(estymacja_prop$estimate, 4)
prop_dol <- round(estymacja_prop$conf.int[1], 4)
prop_gora <- round(estymacja_prop$conf.int[2], 4)

# Estymacja przedziałowa dla średniej (koszt misji w mln USD)
ceny <- na.omit(df$Price)
estymacja_sredniej <- t.test(ceny, conf.level = 0.95)

sred_est <- round(estymacja_sredniej$estimate, 2)
sred_dol <- round(estymacja_sredniej$conf.int[1], 2)
sred_gora <- round(estymacja_sredniej$conf.int[2], 2)

# Wyniki
{
cat("\n")
cat("          WYNIKI ESTYMACJI PRZEDZIALOWEJ (95%)           \n")
cat(" 1. PROPORCJA SUKCESOW MISJI KOSMICZNYCH                 \n")
cat("    - Estymator punktowy: ", prop_est, "\n")
cat("    - Przedzial ufnosci:  [", prop_dol, ",", prop_gora, "]\n")
cat("---------------------------------------------------------\n")
cat(" 2. SREDNIA CENA MISJI KOSMICZNEJ (MLN USD)              \n")
cat("    - Estymator punktowy: ", sred_est, "mln USD\n")
cat("    - Przedzial ufnosci:  [", sred_dol, ",", sred_gora, "] mln USD\n")
cat("\n")
}









# ---------------------------------------------------------
# PUNKT 5: TESTOWANIE HIPOTEZ
# ---------------------------------------------------------

{
  # --- Przygotowanie zmiennych grupowych do testów ---
  # 1. nowa zmienna Era
  df$Era <- ifelse(df$Year < 2010, "Historyczna", "New Space")
  
  # 2. Wybieram Top 5 agencji, żeby test Chi-kwadrat miał sens
  top_5 <- names(sort(table(df$Company), decreasing = TRUE)[1:5])
  df_top <- df[df$Company %in% top_5, ]
  
  # --- Testy statystyczne ---
  ceny <- na.omit(df$Price)
  # 1. Test Shapiro-Wilka (Normalność)
  test_shapiro <- shapiro.test(ceny)
  
  # 2. Test Manna-Whitneya (Ceny wg Ery)
  test_mann <- wilcox.test(Price ~ Era, data = subset(df, !is.na(Price)))
  
  # 3. Test Chi-Kwadrat (Firma a Sukces)
  test_chi <- chisq.test(table(df_top$Company, df_top$MissionStatus), simulate.p.value = TRUE)
  
  
  # --- Wypisywanie wyników w estetycznej ramce ---
  cat("\n")
  cat("          WYNIKI TESTOWANIA HIPOTEZ           \n")
  cat(" 1. TEST SHAPIRO-WILKA (Badanie normalnosci cen)         \n")
  cat("    - Statystyka W: ", round(test_shapiro$statistic, 4), "\n")
  cat("    - p-value:      ", format.pval(test_shapiro$p.value), "\n")
  cat("---------------------------------------------------------\n")
  cat(" 2. TEST U MANNA-WHITNEYA (Ceny: Hist. vs New Space)     \n")
  cat("    - Statystyka W: ", test_mann$statistic, "\n")
  cat("    - p-value:      ", format.pval(test_mann$p.value), "\n")
  cat("---------------------------------------------------------\n")
  cat(" 3. TEST CHI-KWADRAT (Niezaleznosc: Agencja vs Status)   \n")
  cat("    - Statystyka X2:", round(test_chi$statistic, 4), "\n")
  cat("    - p-value:      ", format.pval(test_chi$p.value), "\n")
  cat("\n")
}

# --- WIZUALIZACJA DO TESTU MANNA-WHITNEYA ---
wykres_mann <- ggplot(subset(df, !is.na(Price)), aes(x = Era, y = Price, fill = Era)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  labs(
    title = "Test Manna-Whitneya: Spadek cen w Erze New Space",
    x = "Epoka rynkowa",
    y = "Koszt misji (mln USD) - Skala log",
    fill = "Era"
  ) +
  theme_minimal()
print(wykres_mann)

# --- WIZUALIZACJA DO TESTU CHI-KWADRAT ---

# wyliczam procent awarii dla każdej firmy
df_awaryjnosc <- df_top %>%
  group_by(Company) %>%
  summarise(
    Liczba_Startow = n(),
    Liczba_Awarii = sum(MissionStatus != "Success"),
    Procent_Awarii = (Liczba_Awarii / Liczba_Startow) * 100
  ) %>%
  arrange(desc(Procent_Awarii))

wykres_chi_poprawiony <- ggplot(df_awaryjnosc, aes(x = reorder(Company, -Procent_Awarii), y = Procent_Awarii, fill = Company)) +
  geom_col(color = "black", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Procent_Awarii)), vjust = -0.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  labs(
    title = "Zróżnicowanie niezawodności w Top 5 agencji",
    subtitle = "Odsetek misji zakończonych statusem innym niż 'Success' (Awarie i anomalie)",
    x = "Agencja Kosmiczna",
    y = "Wskaźnik awaryjności (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(wykres_chi_poprawiony)












# ---------------------------------------------------------
# PUNKT 6: BOOTSTRAP DLA ŚREDNIEJ CENY MISJI
# ---------------------------------------------------------
{
  ceny <- na.omit(df$Price)
  
  set.seed(111) 
  
  B <- 10000 
  
  boot_srednie <- replicate(B, mean(sample(ceny, replace = TRUE)))
  
  # Obliczanie wyników
  boot_estymator <- mean(boot_srednie)
  boot_dol <- quantile(boot_srednie, 0.025)
  boot_gora <- quantile(boot_srednie, 0.975)
  
  cat("\n")
  cat("           WYNIKI METODY BOOTSTRAP (10 000 prob)         \n")
  cat(" ESTYMACJA SREDNIEJ CENY MISJI KOSMICZNEJ                \n")
  cat("    - Estymator punktowy: ", round(boot_estymator, 2), "mln USD\n")
  cat("    - Przedzial ufnosci:  [", round(boot_dol, 2), ",", round(boot_gora, 2), "] mln USD\n")
  cat("=========================================================\n")
  cat(" (Porownanie z klasycznym testem t-Studenta z Pkt. 4):   \n")
  cat("    - Klasyczny estymator: ", sred_est, "mln USD\n")
  cat("    - Klasyczny przedzial: [", sred_dol, ",", sred_gora, "] mln USD\n")
  cat("\n")
}

# --- WIZUALIZACJA BOOTSTRAP ---

# konwersja wektora 10 000 średnich na ramkę danych
df_boot <- data.frame(Srednie = boot_srednie)

wykres_boot <- ggplot(df_boot, aes(x = Srednie)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 50, alpha = 0.8) +
  # Linia dla średniej (estymatora)
  geom_vline(xintercept = boot_estymator, color = "orange", linetype = "solid", linewidth = 1) +
  # Linie dla granic przedziału ufności
  geom_vline(xintercept = boot_dol, color = "darkred", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = boot_gora, color = "darkred", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Empiryczny rozkład średnich (Bootstrap, B = 10 000)",
    subtitle = "Czerwona linia ciągła: estymator punktowy. Linie przerywane: 95% przedział ufności",
    x = "Symulowana średnia cena misji (mln USD)",
    y = "Częstość występowania"
  ) +
  theme_minimal()

print(wykres_boot)











# ---------------------------------------------------------
# PUNKT 7: JEDNOCZYNNIKOWA ANALIZA WARIANCJI (ANOVA)
# ---------------------------------------------------------

{
  # 1. Przygotowanie danych (Wybieram Top 5 agencji z podaną ceną)
  df_anova <- subset(df, !is.na(Price))
  top_5 <- names(sort(table(df_anova$Company), decreasing = TRUE)[1:5])
  df_top <- subset(df_anova, Company %in% top_5)
  
  # Wymuszenie typu factor dla firmy
  df_top$Company <- as.factor(df_top$Company)
  
  # Z racji, że dane nie mają rozkładu normalnego,
  # robię logarytm naturalny od ceny, by spłaszczyć odstające wartości
  
  df_top$LogPrice <- log(df_top$Price)
  
  # 2. Wykonanie testów
  test_bartlett <- bartlett.test(LogPrice ~ Company, data = df_top)
  model_anova <- aov(LogPrice ~ Company, data = df_top)
  wynik_anova <- summary(model_anova)
  test_tukey <- TukeyHSD(model_anova)
  
  cat("\n")
  cat("          WYNIKI ANALIZY WARIANCJI (ANOVA)               \n")

  cat(" 1. TEST BARTLETTA (Zalozenie jednorodnosci wariancji)   \n")
  cat("    - p-value: ", format.pval(test_bartlett$p.value), "\n")
  cat("---------------------------------------------------------\n")
  cat(" 2. WYNIK TESTU ANOVA (Dla zlogarytmowanych cen)         \n")
  cat("    - p-value modelu: ", format.pval(wynik_anova[[1]][["Pr(>F)"]][1]), "\n")
  cat("\n--- SZCZEGOLOWE WYNIKI TESTU TUKEYA (POST-HOC) ---\n")
  print(test_tukey)
  cat("\n")
}

# --- WIZUALIZACJA DO ANOVY / TESTU TUKEYA ---
par(mar=c(5, 9, 4, 1))
plot(
  test_tukey, 
  las = 1, 
  col = "darkred", 
  # main = "Test Tukeya - Różnice między cennikami Agencji (LogPrice)"
)
par(mar=c(5, 4, 4, 2) + 0.1)





