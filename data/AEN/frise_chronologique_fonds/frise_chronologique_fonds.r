# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.


# Etape 1 : Import

# Répertoire de travail
setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/AEN/frise_chronologique_fonds")

# Librairies
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)

df <- read_excel("DF_inventaire_photos_AEN.xlsx")

# Etape 2 : Préparation des données

df_fonds_base <- df %>%
  mutate(
    fonds = stringr::str_squish(as.character(fonds)),
    date_min = suppressWarnings(as.numeric(date_min)),
    date_max = suppressWarnings(as.numeric(date_max))
  ) %>%
  mutate(
    date_min_eff = dplyr::coalesce(date_min, date_max),
    date_max_eff = dplyr::coalesce(date_max, date_min)
  ) %>%
  mutate(
    date_min_b = pmax(1850, pmin(2005, date_min_eff)),
    date_max_b = pmax(1850, pmin(2005, date_max_eff))
  )

# Agrégation par fonds
df_fonds_agg <- df_fonds_base %>%
  group_by(fonds) %>%
  summarise(
    start = suppressWarnings(min(date_min_b, na.rm = TRUE)),
    end = suppressWarnings(max(date_max_b, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    start = pmax(1850, pmin(2005, start)),
    end   = pmax(1850, pmin(2005, end))
  ) %>%
  filter(!is.infinite(start), !is.infinite(end), !is.na(start), !is.na(end), end >= start)

# Inclure tous les fonds et ordonner par ordre alphabétique
df_fonds_top <- df_fonds_agg %>%
  arrange(fonds) %>%
  mutate(
    fonds_ord = factor(fonds, levels = rev(sort(unique(fonds))))
  )

# Etape 3 : Visualization

p_frise_fonds_simple <- ggplot(df_fonds_top) +
  geom_segment(aes(x = start, xend = end, y = fonds_ord, yend = fonds_ord),
    linewidth = 2.4, color = "#413f3f"
  ) +
  geom_point(
    data = dplyr::filter(df_fonds_top, start == end), aes(x = start, y = fonds_ord),
    size = 2.4, color = "#413f3f"
  ) +
  scale_x_continuous(limits = c(1850, 2005), breaks = seq(1850, 2005, by = 10)) +
  labs(
    title = "Répartition dans le temps des fonds photographiques",
    subtitle = "Chaque segment représente l'intervalle de dates couvert par un fonds.",
    x = "Année",
    y = "Fonds",
    caption = "Données : Inventaire des fonds photographiques - Archives de l'État de Neuchâtel"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, colour = "grey30"),
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 2)),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    panel.grid.major = element_line(colour = "#E0E0E0"), # ~ grey88
    panel.grid.minor = element_line(colour = "#EBEBEB") # ~ grey92
  ) +
  coord_cartesian(xlim = c(1850, 2005), clip = "off")

# SAVE JPEG
ggsave(file.path(base_dir, "frise_fonds_1850_2005_simple.jpeg"), p_frise_fonds_simple, width = 18, height = 10, dpi = 300, bg = "white")
# SAVE SVG
ggsave(file.path(base_dir, "frise_fonds_1850_2005_simple.svg"), p_frise_fonds_simple, width = 18, height = 10, dpi = 300, bg = "white")
