# --- ROZKŁAD KOSZTÓW MISJI -------
# ---- dodatek do histogramu z pkt 5


library(dplyr)
library(ggplot2)

# Wybieram 4 najaktywniejsze organizacje, które podawały ceny
top_organizacje <- df %>% 
  filter(!is.na(Price)) %>% 
  count(Company, sort = TRUE) %>% 
  top_n(4, n) %>% 
  pull(Company)

# Filtruję dane tylko dla tych 4 firm
df_top <- df %>% filter(Company %in% top_organizacje)

# Tworzę wykres dekompozycji (skumulowany)
wykres_sledczy <- ggplot(df_top, aes(x = Price, fill = Company)) +
  geom_histogram(color = "black", bins = 50, position = "stack", alpha=0.8) +
  scale_x_log10(
    breaks = c(3, 10, 25, 50, 100, 250, 500, 1000, 5000),
    labels = scales::comma
  ) + 
  scale_fill_brewer(palette = "Set1") + 
  labs(
    title = "Dekompozycja rozkładu kosztów misji (Top 4 organizacje)",
    subtitle = "Identyfikacja 3 głównych segmentów cenowych na rynku",
    x = "Koszt misji (mln USD) - Skala log",
    y = "Liczba misji",
    fill = "Organizacja"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(wykres_sledczy)




# --- SZUKANIE WARTOŚCI ODSTAJĄCYCH ---
# --- dodatek do boxplota z pkt 5 - identyfikacja wart odst

library(dplyr)

# Wyciągam z danych konkretne wartości odstające, które zauważyliśmy na boxplocie
tabela_outlierow <- df_top_firmy %>%
  filter(
    (Company == "NASA" & Price > 1000) |
      (Company == "ULA" & Price > 300) |
      (Company %in% c("SpaceX", "CASC") & Price < 10)
  ) %>%
  select(Company, Rocket, Mission, Price, Year) %>% 
  arrange(Company, Price)

View(tabela_outlierow)