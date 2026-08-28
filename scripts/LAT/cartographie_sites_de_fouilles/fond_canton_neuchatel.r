# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

library(sf)
library(dplyr)

# Etape 2 : Extraction du Canton de Neuchâtel (Districts)
suisse_districts <- st_read("gadm41_CHE_shp/gadm41_CHE_2.shp", quiet = TRUE)
canton_cible <- suisse_districts %>%
  filter(NAME_1 == "Neuchâtel")
st_write(canton_cible, "canton_neuchatel/canton_ne_districts.shp", delete_layer = TRUE, quiet = TRUE)

# Etape 3 : Extraction des cantons voisins (Berne, Vaud, Fribourg, Jura)

suisse_cantons <- st_read("romandie/gadm41_CHE_shp/gadm41_CHE_1.shp", quiet = TRUE)
cantons_voisins <- suisse_cantons %>%
  filter(NAME_1 %in% c("Bern", "Vaud", "Fribourg", "Jura")) %>%
  st_transform(crs = st_crs(canton_cible))
st_write(cantons_voisins, "canton_neuchatel/cantons_voisins.shp", delete_layer = TRUE, quiet = TRUE)

# Etape 4 : Extraction du département du Doubs (France)

france_dep <- st_read("romandie/gadm41_FRA_shp/gadm41_FRA_2.shp", quiet = TRUE)
doubs <- france_dep %>%
  filter(NAME_2 == "Doubs") %>%
  st_transform(crs = st_crs(canton_cible))
st_write(doubs, "canton_neuchatel/doubs.shp", delete_layer = TRUE, quiet = TRUE)

# 5. Extraction du Lac de Neuchâtel

lacs <- st_read("romandie/swissnames3d/swissNAMES3D_PLY.shp", quiet = TRUE)
lac_neuchatel <- lacs %>%
  filter(NAME %in% c("Lac de Neuchâtel", "Neuenburgersee")) %>%
  select(NAME) %>%
  st_transform(crs = st_crs(canton_cible)) %>%
  st_zm(drop = TRUE)
st_write(lac_neuchatel, "canton_neuchatel/lac_neuchatel.shp", delete_layer = TRUE, quiet = TRUE)
