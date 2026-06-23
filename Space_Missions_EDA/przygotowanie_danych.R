# 1. Wczytanie pliku
df <- read.csv("space_missions.csv", stringsAsFactors = FALSE)

# 2. Czyszczenie kolumny Price
df$Price <- as.numeric(gsub(",", "", df$Price))

# 3. Tworzenie kolumny Year
df$Date <- as.Date(df$Date)
df$Year <- as.numeric(format(df$Date, "%Y"))

head(df[, c("Date", "Year", "Price")])

summary(df$Price)

write.csv(df, "space_missions_clean.csv", row.names = FALSE)