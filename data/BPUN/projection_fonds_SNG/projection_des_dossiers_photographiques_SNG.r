# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

# Répertoire de travail
setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/BPUN/projection_fonds_SNG")

# Librairies
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(RColorBrewer)
library(packcircles)


excel_file <- "df_fonds_sng.xlsx"
df <- read_excel(excel_file)

# Etape 2 : Préparation des données

# Agrégation et nettoyage des données par dossier

df_bubbles <- df %>%
  mutate(volume_photographie = suppressWarnings(as.numeric(volume_photographie))) %>%
  group_by(nom_dossier) %>%
  summarise(total_photos = sum(volume_photographie, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(total_photos) & total_photos > 0) %>%
  mutate(
    type_volume = case_when(
      total_photos >= 100 ~ "Important (>= 100)",
      total_photos >= 10 ~ "Modéré (10 - 99)",
      TRUE ~ "Faible (< 10)"
    ),
    type_volume = factor(type_volume, levels = c("Important (>= 100)", "Modéré (10 - 99)", "Faible (< 10)"))
  )

# Définition des tailles pour les cercles
size_breaks <- c(1, 5, 10, 20, 50, 100, 200, 500)

df_bubbles <- df_bubbles %>%
  mutate(
    adjusted_size = total_photos^0.5
  )

# Calcul du layout des cercles
layout_cp <- packcircles::circleProgressiveLayout(df_bubbles$adjusted_size, sizetype = "area")


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
pal_volumes <- c(
  "Important (>= 100)" = pal3[1],
  "Modéré (10 - 99)"   = pal3[2],
  "Faible (< 10)"      = pal3[3]
)

# Etape 3 : Visualisation

# Rendu de la visualisation
p_circlepack <- ggplot(vertices_full) +
  geom_polygon(aes(x, y, group = id, fill = type_volume), colour = "white", linewidth = 0.3) +
  geom_point(
    data = tibble::tibble(size_val = size_breaks),
    aes(x = 0, y = 0, size = size_val^0.5),
    inherit.aes = FALSE,
    alpha = 0
  ) +
  coord_equal() +
  scale_fill_manual(
    values = pal_volumes,
    na.value = "#B0B0B0",
    name = "Catégorie de volume"
  ) +
  scale_size_area(
    name = "Volume de photos",
    breaks = size_breaks^0.5,
    labels = scales::comma(size_breaks),
    max_size = 20,
    guide = guide_legend(
      title.position = "top",
      title.theme = element_text(face = "bold", size = 12),
      override.aes = list(alpha = 1, colour = "#475569")
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    plot.margin = margin(15, 15, 15, 25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 5, l = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 10, l = 5)),
    plot.caption = element_text(size = 9, colour = "#475569"),

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
    title = "Projection des dossiers photographiques selon leur volume - Fonds SNG",
    subtitle = "Chaque cercle représente le volume de photographies conservé dans un dossier.",
    caption = "Données : Recensement des fonds contenant des objets photographiques - Base de donnée Flora\nBibliothèque publique et universitaire de Neuchâtel"
  ) +
  guides(
    fill = guide_legend(order = 1, title.theme = element_text(face = "bold")),
    size = guide_legend(
      order = 2, title.theme = element_text(face = "bold"),
      override.aes = list(alpha = 1, colour = "#475569")
    )
  )

# Libellés intégrés dans les cercles
df_labels <- df_bubbles %>%
  mutate(
    r_relative = r / max(r, na.rm = TRUE),
    label = stringr::str_wrap(nom_dossier, width = 12),
    size_pt = pmin(9.5, pmax(3.6, 2.8 + 6.7 * r_relative))
  )

# Ajout des étiquettes textuelles
p_circlepack <- p_circlepack +
  geom_text(
    data = df_labels,
    aes(x = cx, y = cy, label = label),
    inherit.aes = FALSE,
    size = df_labels$size_pt / ggplot2::.pt,
    colour = "#1e293b",
    fontface = "bold",
    lineheight = 0.88
  )

# Sauvegarde au format JPEG
ggsave("projection_des_fonds_par_photographe_SNG.jpeg",
  plot = p_circlepack,
  width = 13,
  height = 10.4,
  dpi = 300,
  bg = "white"
)
# Sauvegarde au format SVG
ggsave("projection_des_fonds_par_photographe_SNG.svg",
  plot = p_circlepack,
  width = 13,
  height = 10.4,
  dpi = 300,
  bg = "white"
)
