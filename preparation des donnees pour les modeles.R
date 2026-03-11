library(tidyverse)
install.packages("minpack.lm")
library(minpack.lm)
library(broom)
library(ggplot2)
library(dplyr)

# I. On récupére le top 39 des meilleurs athlètes utilisés dans la partie stat desc

table_finale <- df_final %>%
  # On ne garde que les lignes dont le duo (athlete, discipline) 
  # existe dans le Top 39
  inner_join(
    stats_pic_equilibre %>% select(ID, discipline), 
    by = c("ID", "discipline")
  )

table_finale <- table_finale %>%
  select(ID, Nom, discipline, mark_numeric, Sex, everything())

# II. On récupère la meilleure perf pour chaque age

df_meilleure_par_age <- table_finale %>%
  filter(Age >= 16 & Age <= 40) %>%
  # Sélection du record par âge 
  group_by(discipline, Sex, Age) %>%
  summarise(
    meilleure_perf = max(mark_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Assurer la continuité (16 à 40 ans sans trous)
  complete(Age = 16:40, nesting(discipline, Sex)) %>%
  group_by(discipline, Sex) %>%
  mutate(meilleure_perf = zoo::na.approx(meilleure_perf, na.rm = FALSE))

# III. On fait un df par (sexe, disicipline)

df_list <- df_meilleure_par_age %>%
  filter(!is.na(meilleure_perf), !is.na(Age)) %>%
  group_by(Sex, discipline) %>%
  mutate(
    direction = if_else(median(meilleure_perf, na.rm = TRUE) < 0, -1, 1),
    Age_Rel = Age - min(Age),
    n_points = n()
  ) %>%
  # On filtre pour garder les groupes avec assez de points
  filter(n_points >= 10) %>%
  # On calcule max_perf par groupe
  mutate(max_perf = max(meilleure_perf, na.rm = TRUE)) %>%
  # On enlève les max_perf invalides
  filter(!is.na(max_perf), is.finite(max_perf)) %>%
  # 2. On sépare le dataframe en une liste de dataframes
  group_split()

names(df_list) <- map_chr(df_list, ~ {
  paste0("df_", unique(.x$Sex), "_", gsub(" ", "_", unique(.x$discipline)))
})

list2env(df_list, envir = .GlobalEnv)

# Pour les courses, on convertit les perf négatives en vitesse moyenne sur la distance

df_men_100_Metres$meilleure_perf <- -df_men_100_Metres$meilleure_perf
df_men_100_Metres$meilleure_perf <- 100/df_men_100_Metres$meilleure_perf
df_men_100_Metres$meilleure_perf_km_par_h <- df_men_100_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_women_100_Metres$meilleure_perf <- -df_women_100_Metres$meilleure_perf
df_women_100_Metres$meilleure_perf <- 100/df_women_100_Metres$meilleure_perf
df_women_100_Metres$meilleure_perf_km_par_h <- df_women_100_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_men_200_Metres$meilleure_perf <- -df_men_200_Metres$meilleure_perf
df_men_200_Metres$meilleure_perf <- 200/df_men_200_Metres$meilleure_perf
df_men_200_Metres$meilleure_perf_km_par_h <- df_men_200_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_women_200_Metres$meilleure_perf <- -df_women_200_Metres$meilleure_perf
df_women_200_Metres$meilleure_perf <- 200/df_women_200_Metres$meilleure_perf
df_women_200_Metres$meilleure_perf_km_par_h <- df_women_200_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_men_400_Metres$meilleure_perf <- -df_men_400_Metres$meilleure_perf
df_men_400_Metres$meilleure_perf <- 400/df_men_400_Metres$meilleure_perf
df_men_400_Metres$meilleure_perf_km_par_h <- df_men_400_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_women_400_Metres$meilleure_perf <- -df_women_400_Metres$meilleure_perf
df_women_400_Metres$meilleure_perf <- 400/df_women_400_Metres$meilleure_perf
df_women_400_Metres$meilleure_perf_km_par_h <- df_women_400_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_men_5000_Metres$meilleure_perf <- -df_men_5000_Metres$meilleure_perf
df_men_5000_Metres$meilleure_perf <- 5000/df_men_5000_Metres$meilleure_perf
df_men_5000_Metres$meilleure_perf_km_par_h <- df_men_5000_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_women_5000_Metres$meilleure_perf <- -df_women_5000_Metres$meilleure_perf
df_women_5000_Metres$meilleure_perf <- 5000/df_women_5000_Metres$meilleure_perf
df_women_5000_Metres$meilleure_perf_km_par_h <- df_women_5000_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_men_1500_Metres$meilleure_perf <- -df_men_1500_Metres$meilleure_perf
df_men_1500_Metres$meilleure_perf <- 1500/df_men_1500_Metres$meilleure_perf
df_men_1500_Metres$meilleure_perf_km_par_h <- df_men_1500_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_women_1500_Metres$meilleure_perf <- -df_women_1500_Metres$meilleure_perf
df_women_1500_Metres$meilleure_perf <- 1500/df_women_1500_Metres$meilleure_perf
df_women_1500_Metres$meilleure_perf_km_par_h <- df_women_1500_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_men_800_Metres$meilleure_perf <- -df_men_800_Metres$meilleure_perf
df_men_800_Metres$meilleure_perf <- 800/df_men_800_Metres$meilleure_perf
df_men_800_Metres$meilleure_perf_km_par_h <- df_men_800_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_women_800_Metres$meilleure_perf <- -df_women_800_Metres$meilleure_perf
df_women_800_Metres$meilleure_perf <- 800/df_women_800_Metres$meilleure_perf
df_women_800_Metres$meilleure_perf_km_par_h <- df_women_800_Metres$meilleure_perf*(10^(-3)/(1/3600))

df_men_3000_Metres_Steeplechase$meilleure_perf <- -df_men_3000_Metres_Steeplechase$meilleure_perf
df_men_3000_Metres_Steeplechase$meilleure_perf <- 3000/df_men_3000_Metres_Steeplechase$meilleure_perf
df_men_3000_Metres_Steeplechase$meilleure_perf_km_par_h <- df_men_3000_Metres_Steeplechase$meilleure_perf*(10^(-3)/(1/3600))

df_women_3000_Metres_Steeplechase$meilleure_perf <- -df_women_3000_Metres_Steeplechase$meilleure_perf
df_women_3000_Metres_Steeplechase$meilleure_perf <- 3000/df_women_3000_Metres_Steeplechase$meilleure_perf
df_women_3000_Metres_Steeplechase$meilleure_perf_km_par_h <- df_women_3000_Metres_Steeplechase$meilleure_perf*(10^(-3)/(1/3600))

df_men_Marathon$meilleure_perf <- -df_men_Marathon$meilleure_perf
df_men_Marathon$meilleure_perf <- 42195/df_men_Marathon$meilleure_perf
df_men_Marathon$meilleure_perf_km_par_h <- df_men_Marathon$meilleure_perf*(10^(-3)/(1/3600))

df_women_Marathon$meilleure_perf <- -df_women_Marathon$meilleure_perf
df_women_Marathon$meilleure_perf <- 42195/df_women_Marathon$meilleure_perf
df_women_Marathon$meilleure_perf_km_par_h <- df_women_Marathon$meilleure_perf*(10^(-3)/(1/3600))

`df_men_10,000_Metres`$meilleure_perf <- -`df_men_10,000_Metres`$meilleure_perf
`df_men_10,000_Metres`$meilleure_perf <- 10000/`df_men_10,000_Metres`$meilleure_perf
`df_men_10,000_Metres`$meilleure_perf_km_par_h <- `df_men_10,000_Metres`$meilleure_perf*(10^(-3)/(1/3600))

`df_women_10,000_Metres`$meilleure_perf <- -`df_women_10,000_Metres`$meilleure_perf
`df_women_10,000_Metres`$meilleure_perf <- 10000/`df_women_10,000_Metres`$meilleure_perf
`df_women_10,000_Metres`$meilleure_perf_km_par_h <- `df_women_10,000_Metres`$meilleure_perf*(10^(-3)/(1/3600))

df_men_110_Metres_Hurdles$meilleure_perf <- -df_men_110_Metres_Hurdles$meilleure_perf
df_men_110_Metres_Hurdles$meilleure_perf <- 110/df_men_110_Metres_Hurdles$meilleure_perf
df_men_110_Metres_Hurdles$meilleure_perf_km_par_h <- df_men_110_Metres_Hurdles$meilleure_perf*(10^(-3)/(1/3600))

df_women_100_Metres_Hurdles$meilleure_perf <- -df_women_100_Metres_Hurdles$meilleure_perf
df_women_100_Metres_Hurdles$meilleure_perf <- 100/df_women_100_Metres_Hurdles$meilleure_perf
df_women_100_Metres_Hurdles$meilleure_perf_km_par_h <- df_women_100_Metres_Hurdles$meilleure_perf*(10^(-3)/(1/3600))


df_men_400_Metres_Hurdles$meilleure_perf <- -df_men_400_Metres_Hurdles$meilleure_perf
df_men_400_Metres_Hurdles$meilleure_perf <- 110/df_men_400_Metres_Hurdles$meilleure_perf
df_men_400_Metres_Hurdles$meilleure_perf_km_par_h <- df_men_400_Metres_Hurdles$meilleure_perf*(10^(-3)/(1/3600))

df_women_400_Metres_Hurdles$meilleure_perf <- -df_women_400_Metres_Hurdles$meilleure_perf
df_women_400_Metres_Hurdles$meilleure_perf <- 100/df_women_400_Metres_Hurdles$meilleure_perf
df_women_400_Metres_Hurdles$meilleure_perf_km_par_h <- df_women_400_Metres_Hurdles$meilleure_perf*(10^(-3)/(1/3600))


