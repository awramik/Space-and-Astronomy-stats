# 1. Tworzę tabelę krzyżową
tabela_krzyzowa <- table(df_top_firmy$Company, df_top_firmy$MissionStatus)

# 2. Przeliczam na procenty
tabela_procentowa <- round(prop.table(tabela_krzyzowa, 1) * 100, 2)

tabela_finalna <- as.data.frame.matrix(tabela_procentowa)
print(tabela_finalna)
View(tabela_finalna)

# 3. Wykres słupkowy skumulowany (procentowy)
library(ggplot2)
wykres_7 <- ggplot(df_top_firmy, aes(x = Company, fill = MissionStatus)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Porównanie skuteczności misji (Top 5 agencji)",
    x = "Agencja / Firma",
    y = "Procentowy udział statusu",
    fill = "Status misji"
  ) +
  theme_minimal()

print(wykres_7)