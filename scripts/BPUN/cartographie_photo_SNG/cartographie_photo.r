# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

library(readxl)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(stringr)
library(RColorBrewer)

# Répertoire de travail

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/BPUN/cartographie_photo_SNG")

# Etape 2 : Préparation des données

excel_file <- "df_fonds_sng.xlsx"
df <- read_excel(excel_file)

# Nettoyage de la colonne 'pays'
df_clean <- df %>%
  mutate(
    pays_clean = str_trim(pays),
    pays_clean = gsub("\u00A0", "", pays_clean),
    pays_clean = str_trim(pays_clean),
    volume_photographie = suppressWarnings(as.numeric(volume_photographie))
  ) %>%
  filter(!is.na(pays_clean) & !is.na(volume_photographie) & volume_photographie > 0)

# Correspondance des noms de pays
country_mapping <- c(
  "Afrique australe"   = "Afrique du Sud",
  "Amérique du Nord"   = "États-Unis",
  "Amérique du Sud"    = "Brésil",
  "Argentine"          = "Argentine",
  "Australie"          = "Australie",
  "Bosnie-Herzégovine" = "Bosnie-Herzégovine",
  "Brésil"             = "Brésil",
  "Bénin"              = "Bénin",
  "Canada"             = "Canada",
  "Costa Rica"         = "Costa Rica",
  "Crimée"             = "Ukraine",
  "Croatie"            = "Croatie",
  "Egypte"             = "Égypte",
  "Espagne"            = "Espagne",
  "Europe du Nord"     = "Norvège",
  "France"             = "France",
  "Ghana"              = "Ghana",
  "Grèce"              = "Grèce",
  "Géorgie"            = "Géorgie",
  "Inde"               = "Inde",
  "Indonésie"          = "Indonésie",
  "Italie"             = "Italie",
  "Japon"              = "Japon",
  "Lesotho"            = "Lesotho",
  "Madagascar"         = "Madagascar",
  "Mexique"            = "Mexique",
  "Monaco"             = "Monaco",
  "Mongolie"           = "Mongolie",
  "Mozambique"         = "Mozambique",
  "Neuchâtel"          = "Suisse",
  "Nouvelle-Calédonie" = "Nouvelle-Calédonie",
  "Ouzbékistan"        = "Ouzbékistan",
  "Paraguay"           = "Paraguay",
  "Pologne"            = "Pologne",
  "Portugal"           = "Portugal",
  "Pérou"              = "Pérou",
  "Roumanie"           = "Roumanie",
  "Royaume-Uni"        = "Royaume-Uni",
  "Russie"             = "Russie",
  "Suisse"             = "Suisse",
  "Surinam"            = "Suriname",
  "Tibet"              = "République populaire de Chine",
  "Turkménistan"       = "Turkménistan",
  "Turquie"            = "Turquie",
  "USA"                = "États-Unis",
  "Uruguay"            = "Uruguay"
)

# Application de la correspondance et agrégation des volumes de photos par pays
df_agg <- df_clean %>%
  mutate(pays_map = country_mapping[pays_clean]) %>%
  filter(!is.na(pays_map)) %>%
  group_by(pays_map) %>%
  summarise(
    total_photos = sum(volume_photographie, na.rm = TRUE),
    .groups = "drop"
  )

# Etape 3 : Préparation du fonds de carte

# Importation des frontières mondiales
world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% filter(continent != "Antarctica")

world_mapped <- world %>%
  left_join(df_agg, by = c("name_fr" = "pays_map"))


centroids_sf <- world_mapped %>%
  st_drop_geometry() %>%
  filter(!is.na(total_photos)) %>%
  st_as_sf(coords = c("label_x", "label_y"), crs = 4326, remove = FALSE)

# Etape 4 : Projection

world_proj <- st_transform(world_mapped, crs = "+proj=robin")
centroids_proj <- st_transform(centroids_sf, crs = "+proj=robin")
coords <- st_coordinates(centroids_proj)
centroids_proj$X <- coords[, 1]
centroids_proj$Y <- coords[, 2]

# Etape 5 : Visualisation

p_map <- ggplot(data = world_proj) +
  geom_sf(fill = "#f1f5f9", colour = "#cbd5e1", linewidth = 0.2) +
  geom_sf(
    data = world_proj %>% filter(!is.na(total_photos)),
    aes(fill = total_photos),
    colour = "#94a3b8",
    linewidth = 0.2
  ) +
  scale_fill_gradientn(
    colors = c("#dbeafe", "#60a5fa", "#1d4ed8", "#1e3a8a"),
    trans = "log10",
    breaks = c(1, 5, 10, 50, 100, 500),
    labels = c("1", "5", "10", "50", "100", "500"),
    name = "Volume de photos",
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      title.position = "top",
      title.theme = element_text(face = "bold", size = 10, colour = "#1e293b"),
      label.theme = element_text(size = 8, colour = "#475569")
    )
  ) +
  geom_point(
    data = centroids_proj,
    aes(x = X, y = Y),
    color = "#e11d48",
    size = 1,
    alpha = 0.85
  ) +
  geom_label_repel(
    data = centroids_proj,
    aes(x = X, y = Y, label = total_photos),
    fill = "#1e293b",
    color = "white",
    fontface = "bold",
    size = 2.2,
    label.padding = unit(0.12, "lines"),
    box.padding = unit(0.2, "lines"),
    point.padding = unit(0.15, "lines"),
    segment.color = "black",
    segment.size = 0.3,
    min.segment.length = 0,
    force = 1.2,
    max.overlaps = 50
  ) +
  coord_sf(
    xlim = c(-12500000, 16000000),
    ylim = c(-6200000, 8300000),
    expand = FALSE
  ) +
  theme_void(base_size = 12) +
  theme(
    text = element_text(family = "sans"),
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 15, colour = "#0f172a", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10.5, colour = "#475569", margin = margin(b = 10)),
    plot.caption = element_text(size = 8, colour = "#64748b", margin = margin(t = 10), hjust = 0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(t = 5)
  ) +

  # Légendes
  labs(
    title = "Répartition géographique des dossiers photographiques - Fonds SNG (1869-1931)",
    subtitle = "Chaque pays mentionné est coloré selon son volume de photographies. Les points rouges marquent leur position\net les badges indiquent le nombre exact d'images concernées.",
    caption = "Données : Recensement des objets photographiques (Base de données Flora)\nBibliothèque publique et universitaire de Neuchâtel\nNote : Les zones régionales historiques ont été rattachées aux pays modernes correspondants (ex. Tibet -> Chine, Crimée -> Ukraine)."
  )


output_filename <- "cartographie_fonds_SNG.jpeg"
ggsave(output_filename,
  plot = p_map,
  width = 8.8,
  height = 6.0,
  dpi = 300,
  bg = "white"
)
output_filename <- "cartographie_fonds_SNG.svg"
ggsave(output_filename,
  plot = p_map,
  width = 8.8,
  height = 6.0,
  dpi = 300,
  bg = "white"
)
