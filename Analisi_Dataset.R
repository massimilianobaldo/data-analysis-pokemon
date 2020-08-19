library(readr)
library("dplyr")
library("tidyr")

dataset_pokemon <- read_csv("dataset_pokemon.csv")

# Prima domanda: Which type is the strongest overall? Which is the weakest?

## Il più forte, vuol dire avere meno debolezze
types <- dataset_pokemon %>%
  select(name, type1, type2, contains("against")) %>%
  filter(is.na(type2)) %>%
  select(type1, contains("against"))

weak_table <- types %>%
  distinct()

## La weak table rappresenta le debolezze dei diversi tipi
## Le colonne sono attaccanti, le righe sono difensivi

## Possiam ora calcolare quale tipo ha un numero di danni inferiore

## Lista di una dimensione

total_weak <- weak_table %>%
  select(-type1) %>%
  rowSums()

final <- weak_table %>%
  select(type1) %>%
  mutate(total_weak)

## Risposta: con 15 abbiamo lo steel mentre con 21.5 abbian ice
## Registell >> Articuno

# Seconda doamnda: E la miglior coppia di tipi con meno debolezze?

double_type <- dataset_pokemon %>%
  select(type1, type2, contains("against")) %>%
  filter(!is.na(type2)) %>%
  distinct()

double_weak <- double_type %>%
  select(-type1, -type2) %>%
  rowSums()

final_double <- double_type %>%
  select(type1, type2) %>%
  mutate(double_weak)

View(final_double)

# Risposta: Steel Fairy per il meno debole e infine Rock Ice il più debole
# Mawhile (che non era fata) >> Aurorus


# Domanda tre: Which type is the most likely to be a legendary Pokemon?

legend_pokemon <- dataset_pokemon %>%
  select(type1, type2, is_legendary) %>%
  filter(is_legendary == 1)

legend_type1 <- count(legend_pokemon, type1) %>%
  arrange(type1)

legend_type2 <- count(legend_pokemon, type2) %>%
  filter(!is.na(type2)) %>%
  arrange(type2)

legend_table_number_type <- full_join(legend_type1, legend_type2, by = c("type1" = "type2")) %>%
  rename(Type = type1)
  
legend_row_number <- legend_table_number_type %>%
  select(-Type) %>%
  rowSums(na.rm = TRUE)
  
final_leg <- legend_table_number_type %>%
  select(Type) %>%
  mutate(legend_row_number) %>%
  rename(Number = legend_row_number) %>%
  arrange(desc(Number))
  
## Risposta: 21 di tipo Psico e 11 di Drago
# 30 per cento (21/70) dei leggendari è almeno di tipo Pisco
  

