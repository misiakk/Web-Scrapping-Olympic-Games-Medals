
install.packages("rvest")
install.packages("dplyr")
install.packages("ggplot2")

library(rvest)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggplot2movies)
# Adres URL strony z tabelą
url <- "https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table"

# Pobieranie danych z HTML
webpage <- read_html(url)

# Wybieranie tabeli z danymi
table <- html_element(webpage, "table.sortable") %>%
  html_table()

str(table)
# Usunięcie pierwszego wiersza z tabeli
table <- table[-1, , drop = FALSE]
# Nowe nazwy kolumn dla tabeli
colnames(table) <- c("Country","Summer_Games_Rank","Summer_Games_Gold","Summer_Games_Silver","Summer_Games_Bronze",
    "Summer_Games_Total" ,
    "Winter_Games_Rank",
    "Winter_Games_Gold",
    "Winter_Games_Silver",
    "Winter_Games_Bronze",
    "Winter_Games_Total" ,
    "Combined_Total_Rank",
    "Combined_Total_Gold",
    "Combined_Total_Silver",
    "Combined_Total_Bronze",
    "Combined_Total_Total")

# Usunięcie z nazw państw dodatkowych nazw w nawiasach
table$Country <- sub("\\(.*", "", table$Country)

# Usunięcie 4 ostatnich wierszy, ponieważ zawierały podsumowania itp.
table <- head(table, n = nrow(table) - 4)

any(is.na(table))
# Zmiana chr na int
table <- table %>%
  mutate_at(vars(-"Country"), ~ as.integer(.))

na_values <- table
na_values[] <- lapply(table, function(x) ifelse(is.na(x), "NA", x))

# Usunięcie białych znaków z tabeli
table$Country <- str_trim(table$Country, side = c("both", "left", "right"))

#Kraje do usunięcia, ponieważ już zostały przekształcone w inne państwa
countries_to_remove <- c("Bohemia", "Czechoslovakia", "East Germany", "West Germany", "Soviet Union", "Olympic Athletes from Russia", "Netherlands Antilles", "Russian Empire", "Soviet Union", "Unified Team", "ROC", "Serbia and Montenegro", "Yugoslavia", "United Team of Germany", "British West Indies", "Australasia")

table <- table %>%
  filter(!Country %in% countries_to_remove)

# Link do strony która zawiera listę państw i kontynentów do jakich należą
url_countries <- "https://statisticstimes.com/geography/countries-by-continents.php"

# Pobieranie danych z HTML
webpage_countries <- read_html(url_countries)

# Wybieranie tabeli z danymi
countries <- html_nodes(webpage_countries, "table") %>% html_table()
# Wybranie 3 tabeli ze strony, która jest aktualnie potrzebna
countries<-countries[[3]]

str(countries)

# Wybór tylko 2 i 7 kolumny
countries<-countries[c(2,7)]

any(is.na(table))

# Zmiana nazw kolumn
colnames(countries) <- c("Country","Continent")

# Usunięcie nazw w nawiasach
countries$Country <- sub("\\(.*", "", countries$Country)

# Usunięcie białych znaków z tabeli countries
countries$Country <- str_trim(countries$Country, side = c("both", "left", "right"))

na_count<-sum(is.na(result["Continent"]))

result$Continent

#result<- table %>% left_join(countries, by = c('Country'='Country'))

non_na_rows <- result[complete.cases(result$Continent), ]

na_rows <- result[is.na(result$Continent), ]

# Lista przekształceń aby nazwy z obu tabel były spójne
transformations <- c(
  "Czechia" = "Czech Republic",
  "United Kingdom of Great Britain and Northern Ireland" = "Great Britain",
  "China, Hong Kong Special Administrative Region" = "Hong Kong",
  "Democratic People's Republic of Korea" = "South Korea",
  "Republic of Korea" = "North Korea",
  "Republic of Moldova" = "Moldova",
  "Russian Federation" = "Russia",
  "Syrian Arab Republic" = "Syria",
  "United Republic of Tanzania" = "Tanzania",
  "United States of America" = "United States",
  "Viet Nam" = "Vietnam",
  "United States Virgin Islands" = "Virgin Islands"
)


# Zamiana nazw krajów zgodnie z listą przekształceń
countries <- countries %>%
  mutate(Country = ifelse(Country %in% names(transformations), transformations[Country], Country))

# Nowe rekordy, których brakuje w tabeli countries
new_rows <- data.frame(
  Country = c("Kosovo", "Ivory Coast", "Chinese Taipei"),
  Continent = c("Europe", "Africa", "Asia"))

# Dodanie nowych rekordów do tabeli countries 
countries <- rbind(countries, new_rows)

# Połączenie dwóch tabel 'tabela' oraz 'countries'
result <- left_join(table, countries, by = "Country")

# Uzupełnienie danych dla USA
result <- result %>%
  mutate(
    Summer_Games_Gold = ifelse(Country == "United States", 1061, Summer_Games_Gold),
    Summer_Games_Total = ifelse(Country == "United States", 2629, Summer_Games_Total),
    Combined_Total_Gold = ifelse(Country == "United States", 1174, Combined_Total_Gold),
    Combined_Total_Total = ifelse(Country == "United States", 2959, Combined_Total_Total)
  )
------------------------------------#DATA VISUALIZATION------------------------------
# Wykres słupkowy dla letnich igrzysk olimpijskich
summer_plot <- ggplot(result, aes(x = Continent, y = Summer_Games_Total, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Liczba medali letnich igrzysk olimpijskich",
       x = "Kontynent",
       y = "Liczba medali") +
  theme_minimal()

# Wykres słupkowy dla zimowych igrzysk olimpijskich
winter_plot <- ggplot(result, aes(x = Continent, y = Winter_Games_Total, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Liczba medali zimowych igrzysk olimpijskich",
       x = "Kraj",
       y = "Liczba medali") +
  theme_minimal()

----------------------------#Scatter plot pokazujący średnią liczbę medali dla państw z Europy
# Przygotowanie danych (filtrowanie tylko dla Europe)
ratio_data_europe <- result %>%
  filter(Continent == "Europe") %>%
  mutate(Ratio_Medals_to_Participation = Combined_Total_Total / (Summer_Games_Rank + Winter_Games_Rank))%>%
  mutate(Gold_Medals_to_Participation = Combined_Total_Gold / (Summer_Games_Rank + Winter_Games_Rank))%>%
  mutate(Silver_Medals_to_Participation = Combined_Total_Silver / (Summer_Games_Rank + Winter_Games_Rank))%>%
  mutate(Bronze_Medals_to_Participation = Combined_Total_Bronze / (Summer_Games_Rank + Winter_Games_Rank))

# Wykres Stosunek Medali do Liczby Uczestnictwa (dla Europe)
ggplot(ratio_data_europe, aes(x = Country, y = Ratio_Medals_to_Participation, size = Combined_Total_Total)) +
  geom_point(color = "blue", alpha = 0.7) +
  scale_size_continuous(range = c(2, 15)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Stosunek Medali do Liczby Uczestnictwa w Obu Igrzyskach (dla Europy)",
       subtitle = "Dane z igrzysk olimpijskich",
       x = "Kraj",
       y = "Stosunek Medali do Liczby Uczestnictwa")

------------------------------#Scatter plot pokazujący średnią liczbę medali z podziałem na kolory dla państw z Europy 
  #(wielkość określa liczbę medali dla danego koloru)------------------------

# Wykres Stosunek Medali do Liczby Uczestnictwa z podziałem na rodzaje medali
ggplot(ratio_data_europe, aes(x = Country, color = as.factor(Continent))) +
  geom_point(aes(y = Gold_Medals_to_Participation, size = Combined_Total_Gold, fill = "Złote"), shape = 21) +
  geom_point(aes(y = Silver_Medals_to_Participation, size = Combined_Total_Silver, fill = "Srebrne"), shape = 21) +
  geom_point(aes(y = Bronze_Medals_to_Participation, size = Combined_Total_Bronze, fill = "Brązowe"), shape = 21) +
  scale_size_continuous(range = c(2, 15)) +
  scale_fill_manual(values = c("Złote" = "gold", "Srebrne" = "gray", "Brązowe" = "brown")) +
  scale_color_discrete(guide = FALSE) +
  facet_wrap(~Continent, scales = "free", ncol = 1) +
  labs(title = "Stosunek Medali do Liczby Uczestnictwa w Obu Igrzyskach (dla Europe)",
       subtitle = "Dane z igrzysk olimpijskich",
       x = "Kraj",
       y = "Liczba Medali",
       fill = "Rodzaj Medalu",
       size = "Liczba Medali") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

------------------------------#TABELA ZAWIERAJĄCA LISTĘ PAŃSTW Z EUROPY W KOLEJNOŚCI OD NAJWIĘKSZEJ LICZBY MEDALI DO NAJMNIEJSZEJ WRAZ Z PODZIAŁEM NA ZŁOTE, SREBRNE I BRĄZOWE MEDALE---
install.packages('kableExtra')
library(kableExtra)
# Przygotowanie danych (filtrowanie tylko dla Europe)
medals_data_europe <- result %>%
  filter(Continent == "Europe")

# Tworzenie tabeli rankingowej
ranking_table <- medals_data_europe %>%
  select(Country, Combined_Total_Total, Combined_Total_Gold, Combined_Total_Silver, Combined_Total_Bronze) %>%
  arrange(desc(Combined_Total_Total)) %>%
  mutate(Rank = row_number())

# Wyświetlanie tabeli rankingowej
library(kableExtra)

ranking_table %>%
  kable("html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  #add_header_above(c(" ", "Zdobyte Medale" = 2), bold = TRUE) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "darkblue") %>%
  collapse_rows(columns = 1:2, valign = "middle") %>%
  pack_rows("Rank", 1, 3, bold = TRUE, background = "lightgray")


--------------------------#TOP 10 PAŃSTW POD WZGLĘDEM LICZBY MEDALI Z OBU IGRZYSK--------
  
  # Przygotowanie danych
  top_countries <- result %>%
  arrange(desc(Combined_Total_Total)) %>%
  head(10)

# Wykres słupkowy
library(ggplot2)

ggplot(top_countries, aes(x = reorder(Country, -Combined_Total_Total), y = Combined_Total_Total, fill = as.factor(Continent))) +
  geom_col() +
  scale_fill_manual(values = c("Asia" = "red", "Africa" = "orange", "Europe" = "blue", "North America" = "green", "Oceania" = "purple", "South America" = "pink")) +
  labs(title = "Top 10 Państw ze Wszystkich Kontynentów - Liczba Zdobytych Medali",
       x = "Kraj",
       y = "Liczba Medali",
       fill = "Kontynent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
----------------------#TOP 10 PAŃSTW POD WZGLĘDEM ŚREDNIEJ LICZBY MEDALI Z OBU IGRZYSK---------

  # Przygotowanie danych
  top_countries_avg <- result %>%
  mutate(Avg_Medals_Per_Rank = Combined_Total_Total / (Summer_Games_Rank + Winter_Games_Rank)) %>%
  arrange(desc(Avg_Medals_Per_Rank)) %>%
  head(10)

# Wykres słupkowy
library(ggplot2)

ggplot(top_countries_avg, aes(x = reorder(Country, -Avg_Medals_Per_Rank), y = Avg_Medals_Per_Rank, fill = as.factor(Continent))) +
  geom_col() +
  scale_fill_manual(values = c("Asia" = "red", "Africa" = "orange", "Europe" = "blue", "North America" = "green", "Oceania" = "purple", "South America" = "pink")) +
  labs(title = "Top 10 Państw ze Wszystkich Kontynentów - Średnia Medali na Miejsce",
       x = "Kraj",
       y = "Średnia Medali na Miejsce",
       fill = "Kontynent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
------------------------------------------#TOP 10 PAŃSTW POD WZGLĘDEM ŚREDNIEJ LICZBY ZŁOTYCH MEDALI Z OBU IGRZYSK-----
# Przygotowanie danych
top_countries_avg_gold <- result %>%
  mutate(Avg_Medals_Per_Rank_Gold = Combined_Total_Gold / (Summer_Games_Rank + Winter_Games_Rank)) %>%
  arrange(desc(Avg_Medals_Per_Rank_Gold)) %>%
  head(10)

# Wykres słupkowy
library(ggplot2)

ggplot(top_countries_avg_gold, aes(x = reorder(Country, -Avg_Medals_Per_Rank_Gold), y = Avg_Medals_Per_Rank_Gold, fill = as.factor(Continent))) +
  geom_col() +
  scale_fill_manual(values = c("Asia" = "red", "Africa" = "orange", "Europe" = "blue", "North America" = "green", "Oceania" = "purple", "South America" = "pink")) +
  labs(title = "Top 10 Państw ze Wszystkich Kontynentów - Średnia Medali na Miejsce",
       x = "Kraj",
       y = "Średnia Medali na Miejsce",
       fill = "Kontynent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
----------------#Wykres skumulowany z podziałem na letnie i zimowe igrzyska per kontynent
  # Przygotowanie danych
  medals_by_continent <- result %>%
  group_by(Continent) %>%
  summarise(Summer_Medals = sum(Summer_Games_Total),
            Winter_Medals = sum(Winter_Games_Total))

# Wykres skumulowany
library(ggplot2)

ggplot(medals_by_continent, aes(x = Continent, y = Summer_Medals, fill = "Letnie")) +
  geom_col(position = "stack") +
  geom_col(aes(y = Winter_Medals, fill = "Zimowe"), position = "stack") +
  labs(title = "Wykres Skumulowany Medali z Igrzysk Letnich i Zimowych per Kontynent",
       x = "Kontynent",
       y = "Liczba Medali",
       fill = "Rodzaj Igrzysk") +
  scale_fill_manual(values = c("Letnie" = "gold", "Zimowe" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


