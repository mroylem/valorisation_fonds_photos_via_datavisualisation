# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/LAT/cartographie_sites_de_fouilles")


# Librairies

library(sf)
library(tidyverse)
library(readxl)
library(ggrepel)
library(ggspatial)

# Etape 2 : Charger les fonds de carte


canton_cible <- st_read("canton_neuchatel/canton_ne_districts.shp", quiet = TRUE)
cantons_voisins <- st_read("canton_neuchatel/cantons_voisins.shp", quiet = TRUE)
doubs <- st_read("canton_neuchatel/doubs.shp", quiet = TRUE)
lac_neuchatel <- st_read("canton_neuchatel/lac_neuchatel.shp", quiet = TRUE)

# Calcul du cadrage
bbox <- st_bbox(canton_cible)
x_margin <- (bbox["xmax"] - bbox["xmin"]) * 0.18
y_margin <- (bbox["ymax"] - bbox["ymin"]) * 0.18
xlim_zoom <- c(bbox["xmin"] - x_margin, bbox["xmax"] + x_margin)
ylim_zoom <- c(bbox["ymin"] - y_margin, bbox["ymax"] + y_margin)

# Découpage des zones limitrophes afin de ne pas les avoir afficher dans leur intégralité.
boite_bbox <- st_bbox(c(
  xmin = unname(xlim_zoom[1]), xmax = unname(xlim_zoom[2]),
  ymin = unname(ylim_zoom[1]), ymax = unname(ylim_zoom[2])
), crs = st_crs(canton_cible))

suppressWarnings({
  cantons_voisins <- st_crop(cantons_voisins, boite_bbox)
  doubs <- st_crop(doubs, boite_bbox)
})

# Etape 3 : Préparation des données

# Chargement du fichier de données avec les coordonnées des communes.
df_brut <- read_excel("df_inventaire_communes_sites_lat.xlsx")

# Filtrage des lignes ayant NA en communes et sélection des colonnes
df_clean <- df_brut %>%
  filter(!is.na(commune) & commune != "NA") %>%
  dplyr::select(commune, lat, long, district, diapositives)

# Regroupement par commune
df_communes <- df_clean %>%
  mutate(diapositives = as.numeric(diapositives)) %>%
  group_by(commune, lat, long, district) %>%
  summarise(total_diapos = sum(diapositives, na.rm = TRUE), .groups = "drop") %>%
  filter(total_diapos > 0) %>%
  filter(!is.na(lat) & !is.na(long))

# Conversion des données en objet géographique
df_points <- st_as_sf(df_communes, coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Création des catégories pour l'échelle
df_points <- df_points %>%
  mutate(categorie_diapos = cut(
    total_diapos,
    breaks = c(-1, 499, 1000, 5000, 10000, 50000, Inf),
    labels = c("< 500", "500 à 1 000", "1 001 à 5 000", "5 001 à 10 000", "10 001 à 50 000", "> 50 000")
  ))

# Etape 4 : Visualisation


carte_finale <- ggplot() +
  geom_sf(data = cantons_voisins, fill = "#E5E5E5", color = "white", linewidth = 0.3) +
  geom_sf(data = doubs, fill = "#E5E5E5", color = "white", linewidth = 0.3) +
  geom_sf(data = canton_cible, fill = "#D4D4D4", color = "white", linewidth = 0.6) +
  geom_sf(data = lac_neuchatel, fill = "#C4E4FF", color = "#82C2FF", linewidth = 0.4) +
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom, expand = FALSE) +
  geom_sf(
    data = df_points,
    aes(size = categorie_diapos, color = categorie_diapos),
    alpha = 0.9
  ) +
  # Ajout des labels repoussés
  geom_text_repel(
    data = df_points,
    aes(label = commune, geometry = geometry),
    stat = "sf_coordinates",
    max.overlaps = 50, size = 2.5, color = "black",
    bg.color = "white", bg.r = 0.15,
    box.padding = 0.8, point.padding = 1.2, force = 3,
    segment.color = "grey50", segment.linetype = "dotted", min.segment.length = 0
  ) +
  annotation_scale(location = "bl", width_hint = 0.2, bar_cols = c("black", "white")) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(1.5, "cm"), pad_y = unit(1.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  scale_size_manual(
    values = c(
      "< 500" = 2,
      "500 à 1 000" = 4,
      "1 001 à 5 000" = 6,
      "5 001 à 10 000" = 8,
      "10 001 à 50 000" = 11,
      "> 50 000" = 16
    ),
    name = "Nombre de diapositives",
    drop = FALSE
  ) +
  scale_color_manual(
    values = c(
      "< 500" = "#0000FF",
      "500 à 1 000" = "#00BFFF",
      "1 001 à 5 000" = "#8DB600",
      "5 001 à 10 000" = "#800080",
      "10 001 à 50 000" = "#FFD700",
      "> 50 000" = "#FF0000"
    ),
    name = "Nombre de diapositives",
    drop = FALSE
  ) +
  # Mise en forme
  labs(
    title = "Répartition des diapositives par commune",
    subtitle = "Canton de Neuchâtel - Période 1960-2005",
    size = "Nombre de diapositives",
    color = "Nombre de diapositives",
    x = NULL, y = NULL,
    caption = "Données : Inventaire de la Diathèque - Laténium Parc et Musée d'Archéologie"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(size = 8, hjust = 1, colour = "grey30", lineheight = 0.95, margin = margin(t = 4)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(0.5, 0.1, 0.5, 0.1, "cm"),
    legend.box.margin = margin(t = 20, r = 10, b = 0, l = -15)
  )

# Affichage de la carte
print(carte_finale)

# Sauvegarde de l'image
ggsave("Répartition des diapositives par commune.png",
  plot = carte_finale,
  width = 12, height = 9, dpi = 300, bg = "white"
)
ggsave("Répartition des diapositives par commune.svg",
  plot = carte_finale,
  width = 12, height = 9, dpi = 300, bg = "white"
)
