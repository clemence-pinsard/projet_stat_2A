library(ggplot2)
library(dplyr)

# ── Figure 1 ── Effet de a et c sur l'amplitude ───────────────────────────────

t_f1 <- seq(0, 10, length.out = 500)
b_ref <- 0.5; d_ref <- 0.2

scenarios_f1 <- list(
  list(a = 8,  c = 0.5, label = "a grand"),
  list(a = 4,  c = 0.5, label = "a petit"),
  list(a = 6,  c = 0.3, label = "c petit"),
  list(a = 6,  c = 2.0, label = "c grand")
)

courbes_f1 <- bind_rows(lapply(scenarios_f1, function(s) {
  data.frame(
    t     = t_f1,
    P     = s$a * (1 - exp(-b_ref * t_f1)) + s$c * (1 - exp(d_ref * t_f1)),
    label = s$label,
    a     = s$a
  )
}))

pics_f1 <- bind_rows(lapply(scenarios_f1, function(s) {
  R    <- (s$a * b_ref) / (s$c * d_ref)
  tpic <- log(R) / (b_ref + d_ref)
  Ppic <- s$a * (1 - exp(-b_ref * tpic)) + s$c * (1 - exp(d_ref * tpic))
  data.frame(label = s$label, tpic = tpic, Ppic = Ppic, plateau = s$a)
}))

fig1 <- ggplot(courbes_f1, aes(x = t, y = P, color = label)) +
  geom_line(linewidth = 0.9) +
  geom_hline(data = pics_f1, aes(yintercept = plateau, color = label),
             linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
  geom_point(data = pics_f1, aes(x = tpic, y = Ppic, color = label),
             size = 3, show.legend = FALSE) +
  geom_text(data = pics_f1,
            aes(x = tpic + 0.4, y = Ppic + 0.15,
                label = paste0("pic = ", round(Ppic, 1))),
            size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("#3266ad","#c85a30","#1d9e75","#ba7517")) +
  labs(title = "Effet de a et c sur l'amplitude",
       x = "t", y = "P(t)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# ── Figure 2 ── Effet de b et d sur la dynamique temporelle ───────────────────

t_f2 <- seq(0, 20, length.out = 500)
a_ref <- 6; c_ref <- 0.5

scenarios_f2 <- list(
  list(b = 0.8,  d = 0.4,  label = "b+d grand (pic étroit, précoce)"),
  list(b = 0.5,  d = 0.2,  label = "b+d moyen (référence)"),
  list(b = 0.2,  d = 0.08, label = "b+d petit (pic large, tardif)")
)

courbes_f2 <- bind_rows(lapply(scenarios_f2, function(s) {
  data.frame(
    t     = t_f2,
    P     = a_ref * (1 - exp(-s$b * t_f2)) + c_ref * (1 - exp(s$d * t_f2)),
    label = s$label
  )
}))

pics_f2 <- bind_rows(lapply(scenarios_f2, function(s) {
  R      <- (a_ref * s$b) / (c_ref * s$d)
  tpic   <- log(R) / (s$b + s$d)
  Ppic   <- a_ref * (1 - exp(-s$b * tpic)) + c_ref * (1 - exp(s$d * tpic))
  pente0 <- a_ref * s$b - c_ref * s$d
  data.frame(label = s$label, tpic = tpic, Ppic = Ppic, pente0 = pente0)
}))

tangentes_f2 <- bind_rows(lapply(1:nrow(pics_f2), function(i) {
  data.frame(
    t     = c(0, 2),
    P     = c(0, pics_f2$pente0[i] * 2),
    label = pics_f2$label[i]
  )
}))

fig2 <- ggplot(courbes_f2, aes(x = t, y = P, color = label)) +
  geom_line(linewidth = 0.9) +
  geom_line(data = tangentes_f2, aes(x = t, y = P, color = label),
            linetype = "dotted", linewidth = 0.8) +
  geom_vline(data = pics_f2, aes(xintercept = tpic, color = label),
             linetype = "dashed", linewidth = 0.5, alpha = 0.7) +
  geom_point(data = pics_f2, aes(x = tpic, y = Ppic, color = label),
             size = 3, show.legend = FALSE) + 
  scale_y_continuous(limits = c(-30, 10)) +
  scale_color_manual(values = c("#3266ad","#c85a30","#1d9e75")) +
  labs(title = "Effet de b et d sur la dynamique temporelle",
       subtitle = "Pointillés : tangente en t=0 (pente ≈ ab) ; verticales : position du pic",
       x = "t", y = "P(t)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# ── Figure 3 (remplacée) ── t* en fonction de chaque paramètre ────────────────

install.packages("patchwork")
library(patchwork)

# Valeurs de référence
a0 <- 6; b0 <- 0.5; c0 <- 0.5; d0 <- 0.2

tpic_calc <- function(a, b, c, d) {
  R <- (a * b) / (c * d)
  if (R <= 1) return(NA)
  log(R) / (b + d)
}

# Grilles de variation pour chaque paramètre (les autres fixés à leur valeur ref)
grille_a <- data.frame(x = seq(1, 12, length.out = 200)) |>
  mutate(tpic = sapply(x, function(v) tpic_calc(v, b0, c0, d0)),
         param = "a  (b, c, d fixés)")

grille_b <- data.frame(x = seq(0.05, 1.5, length.out = 200)) |>
  mutate(tpic = sapply(x, function(v) tpic_calc(a0, v, c0, d0)),
         param = "b  (a, c, d fixés)")

grille_c <- data.frame(x = seq(0.05, 3, length.out = 200)) |>
  mutate(tpic = sapply(x, function(v) tpic_calc(a0, b0, v, d0)),
         param = "c  (a, b, d fixés)")

grille_d <- data.frame(x = seq(0.02, 1, length.out = 200)) |>
  mutate(tpic = sapply(x, function(v) tpic_calc(a0, b0, c0, v)),
         param = "d  (a, b, c fixés)")

# Points de référence
refs <- data.frame(
  x     = c(a0, b0, c0, d0),
  tpic  = c(tpic_calc(a0, b0, c0, d0),
            tpic_calc(a0, b0, c0, d0),
            tpic_calc(a0, b0, c0, d0),
            tpic_calc(a0, b0, c0, d0)),
  param = c("a  (b, c, d fixés)", "b  (a, c, d fixés)",
            "c  (a, b, d fixés)", "d  (a, b, c fixés)")
)

couleurs <- c(
  "a  (b, c, d fixés)" = "#3266ad",
  "b  (a, c, d fixés)" = "#c85a30",
  "c  (a, b, d fixés)" = "#1d9e75",
  "d  (a, b, c fixés)" = "#ba7517"
)

xlabs <- c(
  "a  (b, c, d fixés)" = "a",
  "b  (a, c, d fixés)" = "b",
  "c  (a, b, d fixés)" = "c",
  "d  (a, b, c fixés)" = "d"
)

make_panel <- function(grille, ref, param_name) {
  ggplot(grille, aes(x = x, y = tpic)) +
    geom_line(color = couleurs[param_name], linewidth = 1) +
    geom_point(data = ref, aes(x = x, y = tpic),
               color = "grey30", size = 3) +
    geom_vline(data = ref, aes(xintercept = x),
               linetype = "dashed", color = "grey60", linewidth = 0.5) +
    geom_hline(data = ref, aes(yintercept = tpic),
               linetype = "dashed", color = "grey60", linewidth = 0.5) +
    labs(title = paste("t* en fonction de", xlabs[param_name]),
         x = xlabs[param_name], y = "t*") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 11))
}

panel_a <- make_panel(grille_a, refs[refs$param == "a  (b, c, d fixés)", ], "a  (b, c, d fixés)")
panel_b <- make_panel(grille_b, refs[refs$param == "b  (a, c, d fixés)", ], "b  (a, c, d fixés)")
panel_c <- make_panel(grille_c, refs[refs$param == "c  (a, b, d fixés)", ], "c  (a, b, d fixés)")
panel_d <- make_panel(grille_d, refs[refs$param == "d  (a, b, c fixés)", ], "d  (a, b, c fixés)")

fig3 <- (panel_a | panel_b) / (panel_c | panel_d) +
  plot_annotation(
    title    = "Effet de chaque paramètre sur la position du pic t*",
    subtitle = "Le point gris indique la valeur de référence (a=6, b=0.5, c=0.5, d=0.2)"
  )


# ── Affichage ─────────────────────────────────────────────────────────────────

fig1
fig2
fig3


ggsave("methodo/a_et_c.png", plot = fig1, width = 12, height = 9)
ggsave("methodo/b_et_d.png", plot = fig2, width = 12, height = 9)
ggsave("methodo/effets_params_t_pic.png", plot = fig3, width = 12, height = 9)





