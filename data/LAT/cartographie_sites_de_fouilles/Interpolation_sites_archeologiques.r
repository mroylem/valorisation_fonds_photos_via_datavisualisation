# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/LAT/cartographie_sites_de_fouilles")

# 1. Charger les bibliothèques
library(sf)
library(tidyverse)
library(readxl)
library(ggrepel)
library(ggspatial)
library(viridis)
library(gstat)
library(raster)
library(sp)

# 2. Charger les fonds de carte

canton_cible <- st_read("canton_neuchatel/canton_ne_districts.shp", quiet = TRUE)
cantons_voisins <- st_read("canton_neuchatel/cantons_voisins.shp", quiet = TRUE)
doubs <- st_read("canton_neuchatel/doubs.shp", quiet = TRUE)
lac_neuchatel <- st_read("canton_neuchatel/lac_neuchatel.shp", quiet = TRUE)

# 3. Charger et préparer les données

df <- read_excel("df_inventaire_communes_sites_lat.xlsx")
df_clean <- df %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  mutate(diapositives = as.numeric(diapositives))

# Calcul du total global
total_diapos_canton <- sum(df_clean$diapositives, na.rm = TRUE)

# Calcul par commune et pourcentage
df_communes <- df_clean %>%
  group_by(commune, lat, long) %>%
  summarise(total_diapos = sum(diapositives, na.rm = TRUE), .groups = "drop") %>%
  mutate(pourcentage = (total_diapos / total_diapos_canton) * 100)

# Pour le lissage, s'assurer qu'il n'y a pas de doublons parfaits de coordonnées
df_points <- df_communes %>%
  group_by(long, lat) %>%
  summarise(pourcentage = sum(pourcentage), commune = first(commune), total_diapos = sum(total_diapos), .groups = "drop")

# Convertir en sf
sf_points <- st_as_sf(df_points, coords = c("long", "lat"), crs = 4326)
sf_points <- st_transform(sf_points, crs = st_crs(canton_cible))

# Extraction des villes repères pour l'affichage
villes_repere <- sf_points %>%
  filter(commune %in% c("Bevaix", "Neuchâtel", "La Chaux-de-Fonds"))

# 4. Préparation de la grille d'interpolation (Lissage spatial)

# On utilise le package sp et gstat
sp_points <- as(sf_points, "Spatial")
sp_canton <- as(st_union(canton_cible), "Spatial")

# Création d'une grille englobant le canton de Neuchâtel
bbox_canton <- st_bbox(canton_cible)
grd <- expand.grid(
  X = seq(from = bbox_canton["xmin"], to = bbox_canton["xmax"], length.out = 300),
  Y = seq(from = bbox_canton["ymin"], to = bbox_canton["ymax"], length.out = 300)
)
coordinates(grd) <- ~ X + Y
gridded(grd) <- TRUE
proj4string(grd) <- proj4string(sp_points)

# Interpolation par Krigeage (rendu beaucoup plus lisse et naturel que IDW)

v_empirique <- variogram(pourcentage ~ 1, locations = sp_points)
# Modèle initial (portée estimée à 10km en Suisse)
v_model <- vgm(psill = var(sp_points$pourcentage), model = "Sph", range = 10000, nugget = 0)
fit_v <- fit.variogram(v_empirique, v_model)

# Krigeage
interpolation <- krige(formula = pourcentage ~ 1, locations = sp_points, newdata = grd, model = fit_v)

# Convertir en raster et masquer tout ce qui dépasse des frontières du canton
r <- raster(interpolation)
r_masked <- mask(r, sp_canton)

# Convertir en dataframe pour ggplot2
raster_pts <- rasterToPoints(r_masked, spatial = TRUE)
raster_df <- as.data.frame(raster_pts)
# var1.pred contient la valeur interpolée
names(raster_df)[1:3] <- c("pourcentage_pred", "x", "y")

# 5. Définition du cadrage zoomé (18% de marge autour du canton)
marge <- 0.18
x_range <- bbox_canton["xmax"] - bbox_canton["xmin"]
y_range <- bbox_canton["ymax"] - bbox_canton["ymin"]
xlim_zoom <- c(bbox_canton["xmin"] - marge * x_range, bbox_canton["xmax"] + marge * x_range)
ylim_zoom <- c(bbox_canton["ymin"] - marge * y_range, bbox_canton["ymax"] + marge * y_range)

# 6. Création de la carte avec ggplot2
carte_interpolation <- ggplot() +
  # Couches de fond (grises)
  geom_sf(data = cantons_voisins, fill = "#E5E5E5", color = "white", linewidth = 0.3) +
  geom_sf(data = doubs, fill = "#E5E5E5", color = "white", linewidth = 0.3) +

  # Base du canton de Neuchâtel
  geom_sf(data = canton_cible, fill = "white", color = "black", linewidth = 0.5) +

  # Couche Raster : l'interpolation des pourcentages
  geom_raster(data = raster_df, aes(x = x, y = y, fill = pourcentage_pred), alpha = 0.8) +

  # Ajout du Lac de Neuchâtel
  geom_sf(data = lac_neuchatel, fill = "#C4E4FF", color = "#82C2FF", linewidth = 0.4) +

  # Ajout des villes de repère
  geom_sf(data = villes_repere, color = "black", size = 1.5) +
  geom_text_repel(
    data = villes_repere,
    aes(label = commune, geometry = geometry),
    stat = "sf_coordinates",
    size = 4, color = "black",
    bg.color = "white", bg.r = 0.15,
    box.padding = 1.2, point.padding = 0.5, force = 5,
    segment.color = NA # Pas de segments
  ) +

  # Cadrage
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom, expand = FALSE) +

  # Palette de couleurs experte : inferno (adaptée aux valeurs lissées du raster)
  scale_fill_viridis(
    option = "inferno",
    direction = 1,
    name = "Concentration des unités\n(en %)"
  ) +

  # Ajout de l'échelle et de la flèche du nord
  annotation_scale(location = "bl", width_hint = 0.2, bar_cols = c("black", "white")) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(1.5, "cm"), pad_y = unit(1.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +

  # Textes et thème
  labs(
    title = "Carte de chaleur de l'activité archéologique dans le canton de Neuchâtel",
    subtitle = "Les valeurs expriment le pourcentage du total des diapositives de la Diathèque par zone géographique\nPériode 1960-2005",
    x = NULL, y = NULL,
    caption = "Données : Inventaire de la Diathèque - Laténium\nMéthode : Interpolation Spatiale (Krigeage)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(size = 8, hjust = 1, colour = "grey30", lineheight = 0.95),
    plot.margin = margin(0.5, 0.1, 0.5, 0.1, "cm"),
    legend.box.margin = margin(t = 20, r = 10, b = 0, l = -15)
  )

# 7. Affichage et sauvegarde
print(carte_interpolation)

ggsave("Interpolation_raster_sites.png",
  plot = carte_interpolation,
  width = 12, height = 9, dpi = 300, bg = "white"
)
ggsave("Interpolation_raster_sites.svg",
  plot = carte_interpolation,
  width = 12, height = 9, dpi = 300, bg = "white"
)
S
