# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.


# Etape 1 : Import

# Répertoire de travail

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/AEN/projection_des_fonds_et_de_leur_taille")

# Librairies

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(packcircles)


df <- read_excel("DF_inventaire_photos_AEN.xlsx")

# Etape 2 : Préparation des données

# Agrégation par fonds
df_bubbles <- df %>%
  mutate(across(starts_with("nbre_"), ~ suppressWarnings(as.numeric(.x)))) %>%
  group_by(fonds) %>%
  summarise(
    vol_total = sum(c_across(starts_with("nbre_")), na.rm = TRUE),
    type_fonds = dplyr::first(na.omit(as.character(type_fonds))),
    .groups = "drop"
  ) %>%
  filter(!is.na(fonds), vol_total > 0) %>%
  mutate(
    type_fonds = factor(type_fonds, levels = c("privé", "public", "rassemblement thématique", "Privé", "Public", "Rassemblement thématique"))
  )


# Définition des seuils de taille pour les cercles
size_breaks <- c(20, 50, 100, 500, 1000, 2000, 5000, 10000, 15000)

# Création des classes de taille avec ajustement des tailles
df_bubbles <- df_bubbles %>%
  mutate(
    adjusted_size = case_when(
      vol_total > 5000 ~ vol_total * 0.9,
      vol_total < 100 ~ vol_total * 1.3,
      vol_total == 100 ~ vol_total * 1.25,
      vol_total == 500 ~ vol_total * 1.2,
      vol_total == 1000 ~ vol_total * 1.15,
      TRUE ~ vol_total * 1.1
    ),
    vol_size = findInterval(adjusted_size, size_breaks, left.open = TRUE) %>%
      {
        size_breaks[. + 1] %>% replace_na(last(size_breaks))
      }
  )

# Calcul du layout des cercles
layout_cp <- packcircles::circleProgressiveLayout(df_bubbles$vol_size, sizetype = "area")

# Préparation des données pour le tracé
df_bubbles <- bind_cols(
  df_bubbles,
  layout_cp %>% dplyr::rename(cx = x, cy = y, r = radius)
)

# Création des sommets pour les polygones des cercles
vertices <- packcircles::circleLayoutVertices(layout_cp, npoints = 60, idcol = NULL) %>%
  dplyr::mutate(id = as.integer(id))

# Joindre les attributs aux sommets
vertices_full <- vertices %>%
  left_join(df_bubbles %>% mutate(id = row_number()), by = "id")

pal3 <- RColorBrewer::brewer.pal(3, "Set2")
pal_fonds <- c(
  "privé" = pal3[1], "Privé" = pal3[1],
  "public" = pal3[2], "Public" = pal3[2],
  "rassemblement thématique" = pal3[3], "Rassemblement thématique" = pal3[3]
)

# Nouveaux seuils pour les tailles de cercles
size_breaks <- c(10, 20, 50, 100, 500, 1000, 3000, 5000, 10000)

df_size_leg <- tibble::tibble(size_val = size_breaks)

# Etape 3 : Visualisation

p_circlepack <- ggplot(vertices_full) +
  geom_polygon(aes(x, y, group = id, fill = type_fonds), colour = "white", linewidth = 0.3) +
  geom_point(
    data = df_size_leg,
    aes(x = 0, y = 0, size = size_val),
    inherit.aes = FALSE,
    alpha = 0
  ) +
  coord_equal() +
  scale_fill_manual(
    values = pal_fonds,
    na.value = "#B0B0B0",
    name = "Type de fonds"
  ) +
  scale_size_area(
    name = "Taille des fonds",
    breaks = size_breaks,
    labels = scales::comma(size_breaks),
    max_size = 20,
    guide = guide_legend(
      title.position = "top",
      title.theme = element_text(face = "bold", size = 12),
      override.aes = list(alpha = 1, colour = "#555555")
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    plot.margin = margin(15, 15, 15, 25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 5, l = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 10, l = 5)),
    plot.caption = element_text(size = 9, colour = "#555555"),

    # Légende
    legend.position = "right",
    legend.justification = "left",
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key.size = unit(0.7, "lines"),
    legend.spacing.y = unit(5, "pt"),
    legend.title = element_text(size = 10, face = "bold", hjust = 0, margin = margin(b = 5, l = 0)),
    legend.text = element_text(size = 9, hjust = 0),
    legend.box = "vertical"
  ) +
  labs(
    title = "Projection des fonds photographiques selon leur volume et leur statut",
    subtitle = "Chaque cercle représente le volume photographique conservé dans un fonds.",
    caption = "Données : Inventaire des fonds photographiques — Archives de l'État de Neuchâtel"
  ) +
  guides(
    fill = guide_legend(order = 1, title.theme = element_text(face = "bold")),
    size = guide_legend(
      order = 2, title.theme = element_text(face = "bold"),
      override.aes = list(alpha = 1, colour = "#555555")
    )
  )

# Libellés
df_labels <- df_bubbles %>%
  mutate(
    size_mapped = as.numeric(as.character(cut(vol_total,
      breaks = c(0, size_breaks[-1], Inf),
      labels = size_breaks,
      right = TRUE
    ))),
    r = sqrt(size_mapped / max(size_breaks, na.rm = TRUE)) * 1.2,
    label = fonds,
    size_pt = pmin(8.0, pmax(3.6, 3.0 + 6.0 * (r / max(r, na.rm = TRUE))))
  ) %>%
  filter(vol_total >= 80)

p_circlepack <- p_circlepack +
  geom_text(
    data = df_labels,
    aes(x = cx, y = cy, label = label),
    inherit.aes = FALSE,
    size = df_labels$size_pt / ggplot2::.pt,
    colour = "#0b0b0b",
    fontface = "bold",
    lineheight = 0.95
  )

# Sauvegarde SVG
ggsave("Projection_fonds_circle_packing.svg",
  plot = p_circlepack,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# Sauvegarde JPEG
ggsave("Projection_fonds_circle_packing.jpeg",
  plot = p_circlepack,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
