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

#### Arthur
# Analyse du ResultScore par Discipline
# Objectif : Voir si le score est comparable entre les épreuves
score_summary <- df_fiche_wa_sans_NA %>%
  filter(resultScore > 0) %>% # On exclut les scores nuls pour l'analyse de distribution
  group_by(Epreuve_championnat) %>%
  summarise(
    moyenne = mean(resultScore),
    mediane = median(resultScore),
    sd = sd(resultScore),
    n = n()
  ) %>%
  arrange(desc(moyenne))

print(score_summary)

# Visualisation des disparités (Boxplot)
ggplot(df_fiche_wa_sans_NA %>% filter(resultScore > 0), 
       aes(x = reorder(Epreuve_championnat, resultScore, FUN = median), y = resultScore, fill = Sex)) +
  geom_boxplot(outlier.size = 0.5) +
  coord_flip() +
  labs(title = "Distribution des ResultScore par discipline",
       x = "Discipline", y = "Score") +
  theme_minimal()

# Résolution du problème ID vs Nom
# On crée une table de correspondance unique ID -> Nom (le plus fréquent)
# Cela permet de "nettoyer" les noms qui auraient changé pour un même ID
id_clean_table <- df_fiche_wa_sans_NA %>%
  group_by(ID, Nom) %>%
  count() %>%
  group_by(ID) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(ID, Nom_Unique = Nom)

df_final <- df_fiche_wa_sans_NA %>%
  left_join(id_clean_table, by = "ID") %>%
  filter(resultScore > 0) # On retire définitivement les scores nuls

# --- 3. Analyse de la structure longitudinale (pour Modèles Mixtes) ---
# Combien de performances par athlète ?
ath_count <- df_final %>%
  group_by(ID, Epreuve_championnat) %>%
  summarise(n_perf = n(), .groups = "drop")

# Visualisation de la "profondeur" des données
ggplot(ath_count, aes(x = n_perf)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  xlim(0, 20) +
  labs(title = "Nombre de performances par athlète/discipline",
       x = "Nombre de saisons/performances", y = "Nombre d'athlètes") +
  theme_minimal()

# Filtrage : On garde uniquement ceux qui ont au moins 5 performances 
# (Seuil minimal pour que l'effet aléatoire individuel ait du sens)
df_mixed_model <- df_final %>%
  group_by(ID, Epreuve_championnat) %>%
  filter(n() >= 5) %>%
  ungroup()

# --- 4. Analyse du déséquilibre Hommes/Femmes post-1997 ---
# On crée un indicateur de période pour tester l'hypothèse historique
df_final <- df_final %>%
  mutate(periode = ifelse(season < 1997, "Avant 1997", "Après 1997"))

# Tableau croisé Période / Sexe
table_sexe_periode <- df_final %>%
  group_by(periode, Sex) %>%
  tally() %>%
  group_by(periode) %>%
  mutate(prop = n / sum(n))

print(table_sexe_periode)

# TEST

library(lubridate)

df_compare <- df_final %>%
  filter(Epreuve_championnat %in% c("100-metres", "200-metres", "shot-put", "discus-throw")) %>%
  mutate(
    Famille = ifelse(Epreuve_championnat %in% c("100-metres", "200-metres"), "Sprint", "Lancers"),
    
    # parse_date_time est plus robuste car il teste plusieurs formats
    dob_parsed = parse_date_time(DOB, orders = c("ymd", "dmy", "mdy")),
    year_birth = year(dob_parsed),
    
    season_num = as.numeric(as.character(season)),
    Age = season_num - year_birth
  ) %>%
  # On filtre les valeurs où l'âge n'a pas pu être calculé ou est aberrant
  filter(!is.na(Age), Age > 12, Age < 50)

# On relance le graphique
ggplot(df_compare, aes(x = Age, y = resultScore, color = Famille)) +
  geom_point(alpha = 0.05, size = 0.5) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.2) + 
  facet_wrap(~Sex) +
  labs(
    title = "Relation Âge-Performance : Sprinteurs vs Lanceurs",
    subtitle = "Modélisation de la trajectoire de carrière moyenne",
    x = "Âge de l'athlète",
    y = "ResultScore (Points)",
    color = "Discipline"
  ) +
  scale_color_manual(values = c("Sprint" = "#E41A1C", "Lancers" = "#377EB8")) +
  theme_minimal()

# 1. Préparation des données filtrées par famille
df_evol_compare <- df_final %>%
  filter(Epreuve_championnat %in% c("100-metres", "200-metres", "shot-put", "discus-throw")) %>%
  mutate(
    Famille = ifelse(Epreuve_championnat %in% c("100-metres", "200-metres"), "Sprint", "Lancers"),
    season_num = as.numeric(as.character(season))
  ) %>%
  group_by(season_num, Famille) %>%
  summarise(
    moyenne = mean(resultScore, na.rm = TRUE),
    minimum = min(resultScore, na.rm = TRUE),
    maximum = max(resultScore, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Graphique comparatif avec facettes
ggplot(df_evol_compare, aes(x = season_num, group = Famille)) +
  # Zone d'étendue (Min-Max)
  geom_ribbon(aes(ymin = minimum, ymax = maximum, fill = Famille), alpha = 0.2) +
  # Ligne de la moyenne
  geom_line(aes(y = moyenne, color = Famille), linewidth = 1.2) +
  # Points pour marquer chaque année
  geom_point(aes(y = moyenne, color = Famille), size = 1) +
  # Séparation en deux colonnes : Sprint vs Lancers
  facet_wrap(~Famille) +
  labs(
    title = "Comparaison de l'évolution des performances : Sprint vs Lancers",
    subtitle = "Évolution de la moyenne et de l'étendue (Min-Max) des scores par saison",
    x = "Saison",
    y = "ResultScore (Points)",
    fill = "Famille",
    color = "Famille"
  ) +
  scale_color_manual(values = c("Sprint" = "#E41A1C", "Lancers" = "#377EB8")) +
  scale_fill_manual(values = c("Sprint" = "#E41A1C", "Lancers" = "#377EB8")) +
  theme_minimal() +
  theme(legend.position = "none")

