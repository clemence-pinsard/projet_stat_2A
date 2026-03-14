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


# ── Figure 3 ── Relation linéaire entre ln(R) et t* ──────────────────────────

lnR_f3 <- seq(-0.5, 3, length.out = 300)

scenarios_f3 <- list(
  list(bd = 0.3, label = "b+d = 0.3 (dynamique lente)"),
  list(bd = 0.7, label = "b+d = 0.7 (dynamique rapide)")
)

droites_f3 <- bind_rows(lapply(scenarios_f3, function(s) {
  data.frame(
    lnR   = lnR_f3,
    tpic  = lnR_f3 / s$bd,
    label = s$label
  )
})) |> filter(tpic >= 0)

limite_f3 <- data.frame(lnR = 0, tpic = 0)

fig3 <- ggplot(droites_f3, aes(x = lnR, y = tpic, color = label)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.6) +
  geom_point(data = limite_f3, aes(x = lnR, y = tpic),
             color = "#e24b4a", size = 4, inherit.aes = FALSE) +
  annotate("text", x = 0.08, y = 0.3,
           label = "R = 1\n(t* = 0)", size = 3, color = "#e24b4a", hjust = 0) +
  annotate("text", x = 2.5, y = 2.5,
           label = "pente = 1/(b+d)", size = 3, color = "#3266ad", hjust = 0) +
  scale_color_manual(values = c("#3266ad","#c85a30")) +
  labs(title = "Position du pic en fonction du rapport R = ab/cd",
       x = "ln(R) = ln(ab/cd)", y = "t*", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# ── Affichage ─────────────────────────────────────────────────────────────────

fig1
fig2
fig3
