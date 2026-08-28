# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/BPUN/projection_fonds_SNG")

install.packages("tidyverse")
install.packages("rvest")

# Librairies

library(rvest)
library(tidyverse)

# URL de base du fonds
url_fonds <- "https://floraweb.ne.ch/flora/ark:/37964/001247074"

# Lecture de la page principale du fonds
page <- read_html(url_fonds)

# Etape 2 : Extraction des informations

cote_fonds <- page %>%
    html_element(".cote-selector") %>%
    html_text(trim = TRUE)

# Gestion de l'arborescence
liens_sous_dossiers <- page %>%
    html_elements("a[href*='ark:/37964/']") %>%
    html_attr("href") %>%
    unique()

# Etape 3 : Fonction pour extraire les sous-niveaux
extraire_notice <- function(url_notice) {
    html <- read_html(url_notice)


    cote <- html %>%
        html_element(".cote") %>%
        html_text(trim = TRUE)
    intitule <- html %>%
        html_element(".intitule") %>%
        html_text(trim = TRUE)
    dates <- html %>%
        html_element(".dates") %>%
        html_text(trim = TRUE)
    niveau <- html %>%
        html_element(".niveau-description") %>%
        html_text(trim = TRUE)


    tibble(
        cote = cote,
        intitule = intitule,
        dates = dates,
        type_niveau = niveau,
        url = url_notice
    )
}


safe_read_notice <- possibly(extraire_notice, otherwise = NULL)


donnees_extraites <- map_df(liens_sous_dossiers, safe_read_notice)
