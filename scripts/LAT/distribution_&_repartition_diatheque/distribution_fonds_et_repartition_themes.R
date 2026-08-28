# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import
setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/LAT/distribution_&_repartition_diatheque")

# Libraries

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readxl)
library(scales)


df <- read_excel("df_inventaire_photos_lat.xlsx")

# Etape 2 : Préparation des données

df_supports <- df %>%
  dplyr::select(theme, diapositives, plaque_de_verre, photographie, planche_contact) %>%
  mutate(across(
    c(diapositives, plaque_de_verre, photographie, planche_contact),
    ~ as.numeric(gsub(",", ".", ., fixed = TRUE))
  ))


df_supports_long <- df_supports %>%
  pivot_longer(
    cols = c(diapositives, plaque_de_verre, photographie, planche_contact),
    names_to = "type_support",
    values_to = "nombre"
  ) %>%
  filter(!is.na(nombre)) %>%
  mutate(type_support_renomme = case_when(
    type_support == "diapositives" ~ "Diapositives",
    type_support == "photographie" ~ "Photographies",
    type_support == "planche_contact" ~ "Planches-contact",
    type_support == "plaque_de_verre" ~ "Plaques de verre",
    TRUE ~ type_support
  ))

# Calcul des totaux par type de support
df_totaux <- df_supports_long %>%
  group_by(type_support_renomme) %>%
  summarise(total = sum(nombre, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total))

couleurs_supports <- c(
  "Diapositives" = "#44aac4",
  "Photographies" = "#ED7D31",
  "Planches-contact" = "#6ff03c",
  "Plaques de verre" = "#FFC000"
)

# Etape 3 : Visualisation numéro 1

p_distribution_supports <- ggplot(df_totaux, aes(x = type_support_renomme, y = total, fill = type_support_renomme)) +
  geom_col(width = 0.9, color = "white", linewidth = 0.2) +
  geom_text(aes(label = scales::label_number(big.mark = " ")(total)), vjust = -0.5, size = 4) +
  scale_fill_manual(
    name = "Légende",
    values = couleurs_supports,
    breaks = names(couleurs_supports)
  ) +
  scale_y_continuous(
    breaks = c(20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000),
    labels = scales::label_number(big.mark = " "),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    title = "Distribution du fonds par type de support",
    x = "Type de support",
    y = "Volume (nombre d'unités)",
    caption = "Données : Inventaire de la Diathèque - Laténium Parc et Musée d'Archéologie\nPériode : 1960 - 2005"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 7, hjust = 1, colour = "grey30", lineheight = 0.95, margin = margin(t = 4)),
    plot.caption.position = "plot",
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.8, "lines"),
    axis.title.x = element_text(margin = margin(t = 12, r = 20)),
    axis.title.y = element_text(margin = margin(r = 12, t = 20))
  )

# Sauvegarde
ggsave("distribution_fonds_type_support.png", plot = p_distribution_supports, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("distribution_fonds_type_support.svg", plot = p_distribution_supports, width = 10, height = 6, dpi = 300, bg = "white")

# Etape 4 : Visualisation numéro 2

## Préparation des données avec regroupement thématique
df_themes_grouped <- df_supports %>%
  mutate(
    theme_regroupe = case_when(
      str_detect(theme, regex("^Technique :", ignore_case = TRUE)) ~ "Technique",
      str_detect(theme, regex("^Archéologie expérimentale :", ignore_case = TRUE)) ~ "Archéologie expérimentale",
      str_detect(theme, regex("^Environnement et paléo-environnement :", ignore_case = TRUE)) ~ "Environnement et paléo-environnement",
      theme == "Plans et relevés" ~ "Autres thèmes",
      theme == "Surveillances de chantiers, sans substance archéologique" ~ "Autres thèmes",
      theme == "Diapositives illisibles" ~ "Autres thèmes",
      is.na(theme) | theme == "NA" | is.na(diapositives) ~ "Autres thèmes",
      TRUE ~ theme
    )
  ) %>%
  group_by(theme_regroupe) %>%
  summarise(total_diapositives = sum(diapositives, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_diapositives))

## Création du df avec thèmes

df_themes <- df_themes_grouped %>%
  arrange(desc(total_diapositives)) %>%
  mutate(theme_regroupe = factor(theme_regroupe, levels = unique(theme_regroupe)))

## Palette de couleurs

themes_ordre <- df_themes$theme_regroupe

nombre_themes <- length(themes_ordre)

palette_aen_base <- c(
  "#95f36a", "#d64444", "#ffd670", "#70d6ff", "#f3722c",
  "#9d4edd", "#dd1dd3", "#B0B0B0"
)

palette_complementaires <- c(
  "#44aac4", "#ED7D31", "#6ff03c", "#FFC000", "#bf4ad6",
  "#15776c", "#54eff5", "#95f36a", "#d64444"
)

if (nombre_themes <= 8) {
  palette_aen_complete <- palette_aen_base[1:nombre_themes]
} else {
  palette_aen_complete <- c(palette_aen_base, palette_complementaires)[1:nombre_themes]
}

couleurs_themes_nommees <- setNames(palette_aen_complete, themes_ordre)

## Visualisation

p_themes_horizontaux <- ggplot(df_themes, aes(x = total_diapositives, y = reorder(theme_regroupe, total_diapositives), fill = theme_regroupe)) +
  geom_col(color = "white", linewidth = 0.2) +
  geom_text(aes(label = scales::label_number(big.mark = " ")(total_diapositives)),
    hjust = -0.2, size = 3
  ) +
  scale_fill_manual(values = couleurs_themes_nommees) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = " "),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Répartition thématique de la Diathèque",
    subtitle = "Analyse globale du fonds par sujets (170 502 unités)",
    x = "Nombre de diapositives",
    y = "Thèmes",
    caption = "Données : Inventaire de la Diathèque - Laténium Parc et Musée d'Archéologie\nPériode : 1960 - 2005"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, hjust = 0, colour = "grey30", lineheight = 0.95, margin = margin(t = 4)),
    plot.caption.position = "plot",
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(l = 20, r = 20, t = 20, b = 20)
  )

# Sauvegarde du graphique des thèmes
ggsave("repartition_themes_horizontaux.png", plot = p_themes_horizontaux, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("repartition_themes_horizontaux.svg", plot = p_themes_horizontaux, width = 10, height = 8, dpi = 300, bg = "white")
