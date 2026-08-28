# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/BPUN/timeline_chart_SNG")

# Librairies
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

# Etape 2 : Prépration des données

excel_file <- "df_fonds_sng.xlsx"
df <- read_excel(excel_file)

# Nettoyage et création des colonnes numériques 'dates_min' et 'dates_max'
df_clean <- df %>%
  mutate(
    pays_clean = str_trim(pays),
    pays_clean = gsub("\u00A0", "", pays_clean),
    pays_clean = str_trim(pays_clean),
    volume_photographie = suppressWarnings(as.numeric(volume_photographie))
  ) %>%
  mutate(
    dates_min = suppressWarnings(as.numeric(substr(dates, 1, 4))),
    dates_max = suppressWarnings(as.numeric(substr(dates, 6, 9)))
  ) %>%
  mutate(
    volume_photographie = ifelse(is.na(volume_photographie), 1, volume_photographie),
    nom_dossier_clean = gsub("\r\n", " ", nom_dossier),
    nom_dossier_clean = gsub("\n", " ", nom_dossier_clean),
    nom_dossier_clean = str_squish(nom_dossier_clean)
  ) %>%
  filter(!is.na(dates_min) & !is.na(dates_max))

# Etape 3 : Correspondance entre pays et régions

country_to_region <- c(
  "Afrique australe"   = "Afrique australe",
  "Amérique du Nord"   = "Amérique du Nord",
  "Amérique du Sud"    = "Amérique du Sud",
  "Argentine"          = "Amérique du Sud",
  "Australie"          = "Océanie",
  "Bénin"              = "Afrique de l'Ouest",
  "Bosnie-Herzégovine" = "Europe de l'Est",
  "Brésil"             = "Amérique du Sud",
  "Canada"             = "Amérique du Nord",
  "Costa Rica"         = "Amérique centrale",
  "Crimée"             = "Europe de l'Est",
  "Croatie"            = "Europe de l'Est",
  "Egypte"             = "Afrique du Nord",
  "Espagne"            = "Europe de l'Ouest",
  "Europe du Nord"     = "Europe du Nord",
  "France"             = "Europe de l'Ouest",
  "Géorgie"            = "Asie de l'Ouest",
  "Ghana"              = "Afrique de l'Ouest",
  "Grèce"              = "Europe de l'Est",
  "Inde"               = "Asie du Sud",
  "Indonésie"          = "Asie du Sud-Est",
  "Italie"             = "Europe de l'Ouest",
  "Japon"              = "Asie de l'Est",
  "Lesotho"            = "Afrique australe",
  "Madagascar"         = "Afrique de l'Est",
  "Mexique"            = "Amérique du Nord",
  "Monaco"             = "Europe de l'Ouest",
  "Mongolie"           = "Asie de l'Est",
  "Mozambique"         = "Afrique de l'Est",
  "Neuchâtel"          = "Europe de l'Ouest",
  "Nouvelle-Calédonie" = "Océanie",
  "Ouzbékistan"        = "Asie centrale",
  "Paraguay"           = "Amérique du Sud",
  "Pérou"              = "Amérique du Sud",
  "Pologne"            = "Europe de l'Est",
  "Portugal"           = "Europe de l'Ouest",
  "Roumanie"           = "Europe de l'Est",
  "Royaume-Uni"        = "Europe de l'Ouest",
  "Russie"             = "Europe de l'Est",
  "Suisse"             = "Europe de l'Ouest",
  "Surinam"            = "Amérique du Sud",
  "Tibet"              = "Asie de l'Est",
  "Turkménistan"       = "Asie centrale",
  "Turquie"            = "Asie de l'Ouest",
  "Uruguay"            = "Amérique du Sud",
  "USA"                = "Amérique du Nord"
)

df_clean <- df_clean %>%
  mutate(
    region_precise = ifelse(is.na(pays_clean) | pays_clean == "", "Inconnu", country_to_region[pays_clean]),
    region_precise = ifelse(is.na(region_precise), "Inconnu", region_precise)
  )

region_to_continent <- c(
  "Afrique du Nord"    = "Afrique",
  "Afrique de l'Ouest" = "Afrique",
  "Afrique de l'Est"   = "Afrique",
  "Afrique australe"   = "Afrique",
  "Amérique du Nord"   = "Amérique",
  "Amérique centrale"  = "Amérique",
  "Amérique du Sud"    = "Amérique",
  "Europe du Nord"     = "Europe",
  "Europe de l'Ouest"  = "Europe",
  "Europe de l'Est"    = "Europe",
  "Asie centrale"      = "Asie",
  "Asie de l'Est"      = "Asie",
  "Asie du Sud"        = "Asie",
  "Asie du Sud-Est"    = "Asie",
  "Asie de l'Ouest"    = "Asie",
  "Océanie"            = "Océanie",
  "Inconnu"            = "Inconnu"
)

# Etape 4 : Regroupement par dossier & région et calcul de la date médiane

df_dossiers_region <- df_clean %>%
  group_by(region_precise, nom_dossier_clean) %>%
  summarise(
    dossier_min = min(dates_min, na.rm = TRUE),
    dossier_max = max(dates_max, na.rm = TRUE),
    dossier_volume = sum(volume_photographie, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dossier_median = (dossier_min + dossier_max) / 2
  ) %>%
  mutate(continent = region_to_continent[region_precise])

# Etape 5 : Ordonnancement géographique de l'axe Y

region_order <- c(
  "Amérique du Nord", "Amérique centrale", "Amérique du Sud",
  "Europe du Nord", "Europe de l'Ouest", "Europe de l'Est",
  "Asie de l'Est", "Asie du Sud-Est", "Asie du Sud", "Asie centrale", "Asie de l'Ouest",
  "Afrique du Nord", "Afrique de l'Ouest", "Afrique de l'Est", "Afrique australe",
  "Océanie",
  "Inconnu"
)

ord_levels <- rev(region_order)
df_dossiers_region$region_precise <- factor(df_dossiers_region$region_precise, levels = ord_levels)

regions_range <- df_dossiers_region %>%
  group_by(region_precise) %>%
  summarise(
    date_min_global = min(dossier_min),
    date_max_global = max(dossier_max),
    .groups = "drop"
  )
regions_range$region_precise <- factor(regions_range$region_precise, levels = ord_levels)

continent_levels <- c("Amérique", "Europe", "Asie", "Afrique", "Océanie", "Inconnu")
df_dossiers_region$continent <- factor(df_dossiers_region$continent, levels = continent_levels)

# Etape 6 : Visualisation

continent_colors <- c(
  "Amérique" = "#EF4444",
  "Europe"   = "#3B82F6",
  "Asie"     = "#F59E0B",
  "Afrique"  = "#10B981",
  "Océanie"  = "#8B5CF6",
  "Inconnu"  = "#94A3B8"
)

p_timeline <- ggplot() +
  geom_segment(
    data = regions_range,
    aes(x = date_min_global, xend = date_max_global, y = region_precise, yend = region_precise),
    color = "#e2e8f0",
    linewidth = 2.0,
    lineend = "round",
    alpha = 0.85
  ) +
  geom_point(
    data = df_dossiers_region,
    aes(x = dossier_median, y = region_precise, fill = continent, size = dossier_volume),
    color = "#ffffff",
    stroke = 0.4,
    shape = 21,
    alpha = 0.75
  ) +
  scale_x_continuous(
    breaks = seq(1860, 1940, by = 5),
    limits = c(1868, 1932),
    name = "Années (Chronologie)"
  ) +
  scale_y_discrete(
    name = "Régions géographiques"
  ) +
  scale_fill_manual(
    values = continent_colors,
    name = "Continent parent",
    guide = guide_legend(
      override.aes = list(size = 4, alpha = 0.9),
      order = 1
    )
  ) +
  scale_size(
    range = c(3.5, 11),
    name = "Volume de photos",
    breaks = c(1, 5, 20, 50, 100, 200, 500),
    labels = c("1", "5", "20", "50", "100", "200", "500"),
    guide = guide_legend(
      override.aes = list(fill = "#64748b", color = "white", shape = 21),
      order = 2
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "sans"),
    plot.background = element_rect(fill = "#f8fafc", color = NA),
    panel.background = element_rect(fill = "#f8fafc", color = NA),
    panel.grid.major.x = element_line(color = "#e2e8f0", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(face = "bold", color = "#1e293b", size = 9),
    axis.text.x = element_text(color = "#475569", size = 9),
    axis.title.x = element_text(face = "bold", color = "#0f172a", margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", color = "#0f172a", margin = margin(r = 12)),
    plot.title = element_text(face = "bold", size = 14, color = "#0f172a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 18)),
    plot.caption = element_text(size = 7.5, color = "#64748b", margin = margin(t = 15), hjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.background = element_rect(fill = "#ffffff", color = "#e2e8f0", linewidth = 0.4),
    legend.margin = margin(5, 12, 5, 12),
    legend.spacing.x = unit(6, "pt"),
    legend.title = element_text(face = "bold", size = 9, color = "#1e293b"),
    legend.text = element_text(size = 8, color = "#334155"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "Répartition chronologique des volumes photographiques par régions du monde - Fonds SNG",
    subtitle = "Les lignes horizontales grises représentent les dates extrêmes d'activité (min et max) de l'ensemble des fonds de chaque région.\nChaque bulle représente le volume total de photos d'un dossier positionné à la date médiane de son activité.",
    caption = "Données : Recensement des objets photographiques (Base de données Flora)\nBibliothèque publique et universitaire de Neuchâtel"
  )

# Etape 7 : Sauvegarde

ggsave(
  filename = "periode_activite_photographes_SNG.jpeg",
  plot = p_timeline,
  width = 12.5,
  height = 9.0,
  dpi = 300,
  bg = "#f8fafc"
)

ggsave(
  filename = "periode_activite_photographes_SNG.svg",
  plot = p_timeline,
  width = 12.5,
  height = 9.0,
  dpi = 300,
  bg = "#f8fafc"
)
