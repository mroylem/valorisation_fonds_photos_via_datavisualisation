# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/LAT/treemap_hierarchie_latenium")

library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(treemapify)
library(tidyr)


df <- read_excel("df_inventaire_photos_lat.xlsx")

# Etape 2 : Préparation des données

df_clean <- df %>%
  rename(code_brut = code_designation) %>%
  tidyr::extract(
    code_brut,
    into = c("commune", "site", "theme_principal"),
    regex = "^([^-]+)-([^-]+)-(\\d{3})",
    remove = FALSE
  ) %>%
  filter(!is.na(commune) & !is.na(site) & !is.na(theme_principal)) %>%
  mutate(across(c(commune, site, theme_principal), str_trim)) %>%
  mutate(commune = if_else(commune %in% c("EXP", "MUS", "EXPO"), "LAT", commune)) %>%
  filter(commune != "" & site != "" & theme_principal != "") %>%
  filter(commune != "NA" & site != "NA")

# Agrégation

df_plot <- df_clean %>%
  mutate(diapositives = suppressWarnings(as.numeric(diapositives))) %>%
  group_by(commune, site) %>%
  summarise(diapos_site = sum(diapositives, na.rm = TRUE), .groups = "drop") %>%
  filter(diapos_site >= 50) %>%
  mutate(categorie_diapos = cut(
    diapos_site,
    breaks = c(49, 200, 400, 800, 1000, 5000, 8000, 10000, Inf),
    labels = c("50 à 200", "201 à 400", "401 à 800", "801 à 1 000", "1 001 à 5 000", "5 001 à 8 000", "8 001 à 10 000", "> 10 000"),
    right = TRUE
  )) %>%
  mutate(text_color = if_else(
    categorie_diapos %in% c("50 à 200", "201 à 400", "401 à 800", "801 à 1 000"),
    "grey20", "white"
  )) %>%
  arrange(commune, desc(diapos_site))

# Etape 3 : Visualisation

p_treemap <- ggplot(df_plot, aes(
  area = diapos_site,
  fill = categorie_diapos,
  subgroup = commune,
  label = site
)) +
  geom_treemap(color = "white", size = 1.2, alpha = 0.85) +
  geom_treemap_subgroup_border(colour = "black", size = 2.5) +
  geom_treemap_subgroup_text(
    place = "centre", grow = TRUE, alpha = 0.3,
    colour = "black", fontface = "bold"
  ) +
  geom_treemap_text(
    aes(colour = text_color),
    place = "topleft", alpha = 1,
    size = 11, fontface = "bold.italic",
    reflow = TRUE,
    min.size = 0
  ) +
  scale_colour_identity() +
  scale_fill_viridis_d(
    option = "viridis",
    direction = -1,
    name = "Poids documentaire par\nsite archéologique",
    drop = FALSE
  ) +
  labs(
    title = "Hiérarchie spatiale des fonds de la Diathèque : des communes aux sites de fouilles.",
    subtitle = "Distribution du volume de diapositives par unité topographique (Total : 170 502 unités).",
    caption = "Données : Inventaire de la Diathèque - Laténium Parc et Musée d'Archéologie.\nPériode : 1960 - 2005"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1, colour = "grey30"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Sauvegarde de l'image
ggsave("treemap_hierarchie_spaciale_latenium.png", plot = p_treemap, width = 12, height = 9, dpi = 300, bg = "white")
ggsave("treemap_hierarchie_spaciale_latenium.svg", plot = p_treemap, width = 12, height = 9, dpi = 300, bg = "white")
