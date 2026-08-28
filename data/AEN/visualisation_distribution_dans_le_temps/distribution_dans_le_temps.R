# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/AEN/visualisation_distribution_dans_le_temps")

library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(grid)
library(stringi)

# Etape 2 : Prépartion des données

df <- read_excel("DF_inventaire_photos_AEN.xlsx")

# Calcul dates médianes des fonds
df_mid <- df %>%
  mutate(
    date_min = as.numeric(date_min),
    date_max = as.numeric(date_max),
    year     = floor((date_min + date_max) / 2)
  ) %>%
  filter(!is.na(year), year >= 1880, year <= 1995)


y_breaks_fun <- scales::breaks_extended(n = 6)
y_labels_fun <- scales::label_number(
  accuracy = 1,
  big.mark = " "
)

breaks_10 <- seq(1880, 2000, by = 10)
labels_10 <- paste0(breaks_10[-length(breaks_10)], "–", breaks_10[-1] - 1)

df_supports_10 <- df_mid %>%
  mutate(bin_10y = cut(year, breaks = breaks_10, right = FALSE, labels = labels_10)) %>%
  mutate(
    nbre_photographie      = suppressWarnings(as.numeric(nbre_photographie)),
    nbre_carte_postale     = suppressWarnings(as.numeric(nbre_carte_postale)),
    nbre_negatif           = suppressWarnings(as.numeric(nbre_negatif)),
    nbre_planches_contact  = suppressWarnings(as.numeric(nbre_planches_contact)),
    nbre_diapositive       = suppressWarnings(as.numeric(nbre_diapositive)),
    nbre_plaque_de_verre   = suppressWarnings(as.numeric(nbre_plaque_de_verre)),
    nbre_daguerrotype      = suppressWarnings(as.numeric(nbre_daguerrotype))
  ) %>%
  select(
    bin_10y, nbre_photographie, nbre_carte_postale, nbre_negatif,
    nbre_planches_contact, nbre_diapositive, nbre_plaque_de_verre, nbre_daguerrotype
  ) %>%
  pivot_longer(cols = starts_with("nbre_"), names_to = "support_col", values_to = "n") %>%
  mutate(n = replace_na(n, 0)) %>%
  mutate(Support = case_when(
    support_col == "nbre_photographie" ~ "Tirage positif sur papier",
    support_col == "nbre_carte_postale" ~ "Carte postale photographique",
    support_col == "nbre_negatif" ~ "Négatif souple (film)",
    support_col == "nbre_planches_contact" ~ "Planche-contact",
    support_col == "nbre_diapositive" ~ "Diapositive",
    support_col == "nbre_plaque_de_verre" ~ "Négatif sur plaque de verre",
    support_col == "nbre_daguerrotype" ~ "Daguerréotype",
    TRUE ~ NA_character_
  )) %>%
  mutate(Support = factor(
    Support,
    levels = c(
      "Tirage positif sur papier", "Carte postale photographique",
      "Planche-contact", "Négatif souple (film)", "Négatif sur plaque de verre", "Daguerréotype", "Diapositive"
    )
  )) %>%
  filter(!is.na(bin_10y), !is.na(Support)) %>%
  group_by(bin_10y, Support) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(bin_10y = factor(bin_10y, levels = labels_10)) %>%
  group_by(bin_10y) %>%
  mutate(
    total = sum(n, na.rm = TRUE),
    prop = ifelse(total > 0, n / total, 0),
    label_ok = prop >= 0.20,
    label_txt = scales::label_number(accuracy = 1, big.mark = " ")(n)
  ) %>%
  ungroup()


y_max_10 <- df_supports_10 %>%
  group_by(bin_10y) %>%
  summarise(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
  summarise(max_total = max(total, na.rm = TRUE)) %>%
  pull(max_total)
y_breaks_10 <- y_breaks_fun(c(0, y_max_10))
if (length(y_breaks_10) == 0 || max(y_breaks_10) < y_max_10) {
  y_breaks_10 <- sort(unique(c(y_breaks_10, y_max_10)))
}


df_totals_10 <- df_supports_10 %>%
  group_by(bin_10y) %>%
  summarise(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(label_total = scales::label_number(accuracy = 1, big.mark = " ")(total))


pal_set2 <- RColorBrewer::brewer.pal(7, "Set2")
pal_support <- setNames(pal_set2[seq_along(cats_support)], cats_support)

pc_col <- pal_support[["Planche-contact"]]
neg_col <- pal_support[["Négatif souple (film)"]]
pal_support[["Planche-contact"]] <- neg_col
pal_support[["Négatif souple (film)"]] <- pc_col

# Etape 3 : Visualisation
p_support_bar_10_mid <- ggplot(df_supports_10, aes(x = bin_10y, y = n, fill = Support)) +
  geom_col(width = 0.9, color = "white", linewidth = 0.2) +
  geom_text(
    data = df_totals_10,
    aes(x = bin_10y, y = total, label = label_total),
    inherit.aes = FALSE,
    vjust = -0.4,
    size = 3.2
  ) +
  scale_fill_manual(
    name = "Support",
    values = pal_support,
    breaks = cats_support
  ) +
  scale_y_continuous(
    breaks = y_breaks_10,
    labels = y_labels_fun,
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    title    = "Types de support dans les fonds photographiques (1880–1995)",
    subtitle = "Point médian par fonds, regroupé par tranche décennales (10 ans). Chaque fonds est positionné sur sa date médiane de production, faute de dates précises pour chaque image.",
    x        = "Période (tranches de 10 ans)",
    y        = "Nombre de photos par support",
    caption  = "Données : Inventaire des fonds photographiques - Archives de l'État de Neuchâtel"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, hjust = 1, colour = "grey30", lineheight = 0.95, margin = margin(t = 4)),
    plot.caption.position = "plot",
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 12))
  )

# SAVE JPEG
ggsave("distribution_types_supports_1880_1995_10ans_median.jpeg",
  p_support_bar_10_mid,
  width = 14, height = 7, dpi = 300, bg = "white"
)

# SAVE SVG
ggsave("distribution_types_supports_1880_1995_10ans_median.svg",
  p_support_bar_10_mid,
  width = 14, height = 7, dpi = 300, bg = "white"
)
