library(dplyr)
library(ggplot2)

# 1. TWORZĘ WŁASNĄ ZMIENNĄ JAKOŚCIOWĄ (Era kosmiczna)
df_era <- df %>%
  filter(!is.na(Price)) %>%
  mutate(Era = ifelse(Year < 2010, 
                      "1. Era Historyczna (przed 2010)", 
                      "2. Era New Space (po 2010)"))

# 2. TABELA STATYSTYK W GRUPACH
tabela_ery <- df_era %>%
  group_by(Era) %>%
  summarise(
    Liczba_Misji = n(),
    Srednia_Cena = round(mean(Price), 2),
    Mediana_Cena = round(median(Price), 2),
    Min_Cena = min(Price),
    Max_Cena = max(Price),
    Odchylenie = round(sd(Price), 2)
  )

View(tabela_ery)

# 3. WYKRES PORÓWNAWCZY
wykres_era <- ggplot(df_era, aes(x = Price, fill = Era)) +
  geom_density(alpha = 0.6) +
  scale_x_log10(labels = scales::comma) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(
    title = "Porównanie kosztów misji: Era Historyczna vs New Space",
    subtitle = "Rozkład gęstości prawdopodobieństwa ceny startu rakiety",
    x = "Koszt misji (mln USD) - Skala log",
    y = "Gęstość",
    fill = "Epoka rynkowa"
  ) +
  theme_minimal()

print(wykres_era)