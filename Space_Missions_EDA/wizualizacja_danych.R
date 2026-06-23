# install.packages("ggplot2")
library(ggplot2)

# --- 1. HISTOGRAM (Rozkład cen misji) ---

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

# --- 2. BOXPLOT (Koszty misji dla topowych firm) ---
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

# --- 3. VIOLIN PLOT (Rozkład misji w czasie dla Statusów) ---

wykres_3 <- ggplot(df, aes(x = MissionStatus, y = Year, fill = MissionStatus)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  labs(
    title = "Skrzypcowy wykres rozkładu lat startów wg statusu misji",
    x = "Status Misji",
    y = "Rok",
    fill = "Status"
  ) +
  theme_minimal()
print(wykres_3)

# --- 4. SCATTER PLOT (Zależność kosztu od roku startu) ---

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

# --- 5. BARPLOT (Liczba startów wg dekad) ---
# nowa zmienna - Dekada

df$Decade <- floor(df$Year / 10) * 10
wykres_5 <- ggplot(df, aes(x = as.factor(Decade))) +
  geom_bar(fill = "pink", color = "black") +
  labs(
    title = "Liczba startów rakiet w poszczególnych dekadach",
    x = "Dekada",
    y = "Liczba startów"
  ) +
  theme_minimal()
print(wykres_5)

# --- 6. PIE CHART (Status misji) ---

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