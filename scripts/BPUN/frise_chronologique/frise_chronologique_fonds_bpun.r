# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

# Répertoire de travail
setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/BPUN/frise_chronologique")

# Librairies

library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)


df <- read_excel("df_inventaire_bpun.xlsx")

# Mise en page

export_width <- 25.7
export_height <- 17.0
export_units <- "cm"

font_title <- 11.5
font_subtitle <- 8.5
font_caption <- 7
font_axis_y <- 6.5
font_axis_x <- 8
line_width_bar <- 1.6

# Etape 2 : Préparation des données

df_fonds_base <- df %>%
  mutate(
    fonds = stringr::str_squish(as.character(fonds)),
    date_min = suppressWarnings(as.numeric(dates_min)),
    date_max = suppressWarnings(as.numeric(dates_max))
  ) %>%
  mutate(
    date_min_eff = dplyr::coalesce(date_min, date_max),
    date_max_eff = dplyr::coalesce(date_max, date_min)
  )

# Agrégation par fonds
df_fonds_agg <- df_fonds_base %>%
  group_by(fonds) %>%
  summarise(
    start = suppressWarnings(min(date_min_eff, na.rm = TRUE)),
    end = suppressWarnings(max(date_max_eff, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  filter(!is.infinite(start), !is.infinite(end), !is.na(start), !is.na(end), end >= start) %>%
  filter(start >= 1850, start <= 2025) %>%
  mutate(
    end = pmin(2025, end)
  )

unique_fonds <- sort(unique(df_fonds_agg$fonds))

# Séparation en 2 parties
half_n <- ceiling(length(unique_fonds) / 2)
fonds_group1 <- unique_fonds[1:half_n]
fonds_group2 <- unique_fonds[(half_n + 1):length(unique_fonds)]

# Sous-ensemble 1
df_group1 <- df_fonds_agg %>%
  filter(fonds %in% fonds_group1) %>%
  mutate(
    fonds_ord = factor(fonds, levels = rev(fonds_group1))
  )

# Sous-ensemble 2
df_group2 <- df_fonds_agg %>%
  filter(fonds %in% fonds_group2) %>%
  mutate(
    fonds_ord = factor(fonds, levels = rev(fonds_group2))
  )

subtitle_text <- "Chaque segment représente les dates extrêmes des fonds contenant des objets photographiques."

# Etape 3 : Visualisation de la partie 1

p_frise_group1 <- ggplot(df_group1) +
  geom_segment(aes(x = start, xend = end, y = fonds_ord, yend = fonds_ord),
    linewidth = line_width_bar, color = "#413f3f"
  ) +
  geom_point(
    data = dplyr::filter(df_group1, start == end), aes(x = start, y = fonds_ord),
    size = line_width_bar, color = "#413f3f"
  ) +
  scale_x_continuous(limits = c(1850, 2025), breaks = seq(1850, 2025, by = 25)) +
  labs(
    title = "Amplitude chronologique des fonds contenant des objets photographiques - Partie 1",
    subtitle = subtitle_text,
    x = "Année",
    y = "Fonds",
    caption = "Données : Recensement des fonds contenant des objets photographiques - Base de donnée Flora\nBibliothèque publique et universitaire de Neuchâtel"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = font_title, hjust = 0),
    plot.subtitle = element_text(size = font_subtitle, lineheight = 1.1),
    plot.caption = element_text(size = font_caption, colour = "grey30", hjust = 0),
    plot.caption.position = "plot",
    legend.position = "none",
    axis.text.y = element_text(size = font_axis_y),
    axis.text.x = element_text(size = font_axis_x),
    axis.title.x = element_text(size = font_axis_x + 0.5, margin = margin(t = 8)),
    axis.title.y = element_text(size = font_axis_x + 0.5, margin = margin(r = 2)),
    plot.margin = margin(t = 8, r = 15, b = 8, l = 8),
    panel.grid.major = element_line(colour = "#E0E0E0"),
    panel.grid.minor = element_line(colour = "#EBEBEB")
  ) +
  coord_cartesian(xlim = c(1850, 2025), clip = "off")

# Sauvegarde
ggsave("frise_fonds_bpun_A_F_simple.jpeg", p_frise_group1, width = export_width, height = export_height, units = export_units, dpi = 300, bg = "white")
ggsave("frise_fonds_bpun_A_F_simple.svg", p_frise_group1, width = export_width, height = export_height, units = export_units, dpi = 300, bg = "white")

# Etape 4 : Visualisation de la partie 2

p_frise_group2 <- ggplot(df_group2) +
  geom_segment(aes(x = start, xend = end, y = fonds_ord, yend = fonds_ord),
    linewidth = line_width_bar, color = "#413f3f"
  ) +
  geom_point(
    data = dplyr::filter(df_group2, start == end), aes(x = start, y = fonds_ord),
    size = line_width_bar, color = "#413f3f"
  ) +
  scale_x_continuous(limits = c(1850, 2025), breaks = seq(1850, 2025, by = 25)) +
  labs(
    title = "Amplitude chronologique des fonds contenant des objets photographiques - Partie 2",
    subtitle = subtitle_text,
    x = "Année",
    y = "Fonds",
    caption = "Données : Recensement des fonds contenant des objets photographiques - Base de donnée Flora\nBibliothèque publique et universitaire de Neuchâtel"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = font_title, hjust = 0),
    plot.subtitle = element_text(size = font_subtitle, lineheight = 1.1),
    plot.caption = element_text(size = font_caption, colour = "grey30", hjust = 0),
    plot.caption.position = "plot",
    legend.position = "none",
    axis.text.y = element_text(size = font_axis_y),
    axis.text.x = element_text(size = font_axis_x),
    axis.title.x = element_text(size = font_axis_x + 0.5, margin = margin(t = 8)),
    axis.title.y = element_text(size = font_axis_x + 0.5, margin = margin(r = 2)),
    plot.margin = margin(t = 8, r = 15, b = 8, l = 8),
    panel.grid.major = element_line(colour = "#E0E0E0"),
    panel.grid.minor = element_line(colour = "#EBEBEB")
  ) +
  coord_cartesian(xlim = c(1850, 2025), clip = "off")

# Sauvegarde
ggsave("frise_fonds_bpun_G_Z_simple.jpeg", p_frise_group2, width = export_width, height = export_height, units = export_units, dpi = 300, bg = "white")
ggsave("frise_fonds_bpun_G_Z_simple.svg", p_frise_group2, width = export_width, height = export_height, units = export_units, dpi = 300, bg = "white")
