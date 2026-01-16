# Import nécessaires

library(dplyr)
library(ggplot2)
library(tidyr)

# Travail préalable sur la base de données

load("fiche_wa.rdata")
summary(fiche_wa)
df_fiche_wa_sans_NA <- fiche_wa[!is.na(fiche_wa$Epreuve_championnat), ]
summary(df_fiche_wa_sans_NA)
df_fiche_wa_sans_NA$Epreuve_championnat <- as.factor(df_fiche_wa_sans_NA$Epreuve_championnat)
df_fiche_wa_sans_NA$discipline <- as.factor(df_fiche_wa_sans_NA$discipline)
df_fiche_wa_sans_NA$season_factor <- as.factor(df_fiche_wa_sans_NA$season)
df_fiche_wa_sans_NA$ID <- as.factor(df_fiche_wa_sans_NA$ID)
df_fiche_wa_sans_NA$Nom <- as.factor(df_fiche_wa_sans_NA$Nom)
df_fiche_wa_sans_NA$Nat <- as.factor(df_fiche_wa_sans_NA$Nat)
df_fiche_wa_sans_NA$Epreuve <- as.factor(df_fiche_wa_sans_NA$Epreuve)
df_fiche_wa_sans_NA$Sex <- as.factor(df_fiche_wa_sans_NA$Sex)
summary(df_fiche_wa_sans_NA)

# Première vision du déséquilibre par sexe 

df_fiche_wa_sans_NA %>%
  count(Sex) %>%
  mutate(pourcentage = n / sum(n) * 100)
# On a pas 50-50 alors qu'en théorie on devrait 

# On regarde maintenant par sexe et par saison

df_fiche_wa_sans_NA %>%
  count(season, Sex) %>%
  group_by(season) %>%
  mutate(pourcentage = n / sum(n) * 100) %>% 
  print(n = 60)
# On remarque pour les années avant 1997, c'est là où il y a le plus 
# d'écarts entre la proportion d'hommes et celle de femmes 

ggplot(df_fiche_wa_sans_NA, aes(x = season, fill = Sex)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Répartition des athlètes par saison et par sexe",
    x = "Saison",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# On regarde maintenant par discipline et par sexe

epreuve_sexe <- df_fiche_wa_sans_NA %>%
  count(Epreuve_championnat, Sex)

ggplot(epreuve_sexe, aes(x = Epreuve_championnat, y = n, fill = Sex)) +
  geom_col(position = "dodge") +
  labs(
    title = "Répartition des athlètes par Epreuve_championnat et par sexe",
    x = "Epreuve_championnat",
    y = "Nombre d’athlètes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# On regarde plus les différences hommes/femmes par discipline

df_fiche_wa_sans_NA %>%
  count(Epreuve_championnat, Sex) %>%
  pivot_wider(names_from = Sex, values_from = n, values_fill = 0) %>%
  mutate(ratio_femmes = women / (women + men)) %>% 
  print(n = 30)
# On identifie les disciplines purement feminines et les disciplines purement masculines
# De plus, on identifie des disciplines où les femmes sont moins représentées (disque par ex)

# On regarde epreuve, saison, sexe en même temps 

df_fiche_wa_sans_NA %>%
  count(season, Epreuve_championnat, Sex) %>%
  group_by(season, Epreuve_championnat) %>%
  mutate(pourcentage = n / sum(n) * 100) %>% 
  filter(Epreuve_championnat == "discus-throw") %>% 
  print(n = 70)
# Sur toutes les années, il y a un écart important entre les femmes et les hommes pour le 
# disque

################################################################################

# Maintenant, regardons pourquoi il n'y a pas autant d'ID que de noms

length(unique(df_fiche_wa_sans_NA$ID))
length(unique(df_fiche_wa_sans_NA$Nom))
# On a 7 noms de plus que d'ID, pourquoi ?

df_fiche_wa_sans_NA_nom_ID_problematiques <- df_fiche_wa_sans_NA %>%
  group_by(ID) %>%
  filter(n_distinct(Nom) > 1) %>%
  arrange(ID, Nom)

df_fiche_wa_sans_NA_nom_ID_problematiques2 <- df_fiche_wa_sans_NA %>%
  group_by(Nom) %>%
  filter(n_distinct(ID) > 1) %>%
  arrange(ID, Nom)

df_fiche_wa_sans_NA_nom_ID_problematiques %>% 
  distinct(ID, Nom) %>% 
  print(n = 50)

df_fiche_wa_sans_NA_nom_ID_problematiques2 %>% 
  distinct(ID, Nom) %>% 
  print(n = 50)




