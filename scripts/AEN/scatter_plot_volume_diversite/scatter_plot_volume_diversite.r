# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import

setwd("C:/Users/raiku/Documents/Memoire_RM/dataviz/AEN/scatter_plot_volume_diversite")
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ggrepel)


df <- read_excel("DF_inventaire_photos_AEN.xlsx")

# Etape 2 : Préparation et nettoyage des données
df_clean <- df %>%
    filter(!is.na(fonds)) %>%
    mutate(
        fonds = stringr::str_squish(as.character(fonds)),
        date_min = suppressWarnings(as.numeric(date_min)),
        date_max = suppressWarnings(as.numeric(date_max))
    ) %>%
    mutate(
        date_min_eff = dplyr::coalesce(date_min, date_max),
        date_max_eff = dplyr::coalesce(date_max, date_min)
    ) %>%
    mutate(
        year_mid = floor((date_min_eff + date_max_eff) / 2)
    ) %>%
    mutate(across(starts_with("nbre_"), ~ replace_na(suppressWarnings(as.numeric(.x)), 0)))

# Agrégation par fonds
df_fonds_agg <- df_clean %>%
    group_by(fonds) %>%
    summarise(
        year_mid = suppressWarnings(floor(median(year_mid, na.rm = TRUE))),
        across(starts_with("nbre_"), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
        vol_total = sum(c_across(starts_with("nbre_")), na.rm = TRUE),
        diversity = sum(c_across(starts_with("nbre_")) > 0, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(vol_total > 0) %>%
    mutate(vol_plot = pmax(50, pmin(8000, vol_total))) %>%
    arrange(diversity, vol_total) %>%
    group_by(diversity) %>%
    mutate(
        rank_in_div = row_number(),
        alt_offset = case_when(
            rank_in_div %% 5 == 1 ~ -0.25,
            rank_in_div %% 5 == 2 ~ 0.25,
            rank_in_div %% 5 == 3 ~ -0.12,
            rank_in_div %% 5 == 4 ~ 0.12,
            rank_in_div %% 5 == 0 ~ 0.00
        )
    ) %>%
    ungroup() %>%
    group_by(vol_plot, diversity) %>%
    mutate(
        n_points = n(),
        idx = row_number(),
        y_offset = if_else(
            n_points > 1,
            -0.38 + (idx - 1) * (0.76 / (n_points - 1)),
            alt_offset
        ),
        y_base = case_when(
            diversity == 1 ~ 1.0,
            diversity == 2 ~ 2.6,
            diversity == 3 ~ 3.8,
            diversity == 4 ~ 4.7,
            diversity == 5 ~ 5.5
        ),
        y_plot = y_base + y_offset
    ) %>%
    ungroup() %>%
    mutate(
        nudge_x = case_when(
            fonds == "Zofingue" ~ -0.08,
            fonds == "Fondation suisse Téléthèses" ~ -0.05,
            TRUE ~ 0
        ),
        nudge_y = case_when(
            fonds == "Zofingue" ~ -0.4,
            fonds == "Fondation suisse Téléthèses" ~ -0.4,
            TRUE ~ 0
        )
    )

# Etape 3 : Visualisation

p_scatter <- ggplot(df_fonds_agg, aes(x = vol_plot, y = y_plot)) +
    geom_rect(
        data = data.frame(1), aes(xmin = 35, xmax = 1000, ymin = 0.4, ymax = 3.2),
        fill = "#f5f5f5", alpha = 0.8, inherit.aes = FALSE
    ) +
    geom_rect(
        data = data.frame(1), aes(xmin = 1000, xmax = 11000, ymin = 0.4, ymax = 3.2),
        fill = "#e8f5e9", alpha = 0.6, inherit.aes = FALSE
    ) +
    geom_rect(
        data = data.frame(1), aes(xmin = 35, xmax = 1000, ymin = 3.2, ymax = 5.9),
        fill = "#ffebee", alpha = 0.6, inherit.aes = FALSE
    ) +
    geom_rect(
        data = data.frame(1), aes(xmin = 1000, xmax = 11000, ymin = 3.2, ymax = 5.9),
        fill = "#fbe9e7", alpha = 0.6, inherit.aes = FALSE
    ) +
    geom_vline(xintercept = 1000, linetype = "dashed", color = "#7f8c8d", linewidth = 0.6) +
    geom_hline(yintercept = 3.2, linetype = "dashed", color = "#7f8c8d", linewidth = 0.6) +
    geom_point(aes(fill = year_mid),
        shape = 21, color = "white", stroke = 0.8,
        size = 4.5
    ) +
    scale_x_log10(
        limits = c(35, 11000),
        breaks = c(50, 100, 300, 600, 1000, 2000, 4000, 8000),
        labels = c("Moins de 50", "100", "300", "600", "1 000", "2 000", "4 000", "Plus de 8 000")
    ) +
    scale_y_continuous(
        limits = c(0.4, 5.9),
        breaks = c(1.0, 2.6, 3.8, 4.7, 5.5),
        labels = c("1 (Simple)", "2", "3", "4", "5 (Très complexe)")
    ) +
    # Palette de couleur
    scale_fill_gradientn(
        colors = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c"),
        name = "Date médiane\ndu fonds",
        na.value = "#B0B0B0",
        breaks = seq(1860, 2000, by = 20),
        guide = guide_colorbar(
            title.position = "top",
            title.theme = element_text(face = "bold", size = 10),
            barwidth = unit(0.7, "lines"),
            barheight = unit(14, "lines"),
            frame.colour = "#b2bec3",
            ticks.colour = "white"
        )
    ) +
    # Étiquettes textuelles
    geom_text_repel(
        data = df_fonds_agg,
        aes(label = fonds),
        size = 3.0,
        color = "#2d3436",
        fontface = "bold",
        bg.color = "white",
        bg.r = 0.12,
        max.overlaps = 50,
        box.padding = 0.25,
        point.padding = 0.2,
        min.segment.length = 0,
        segment.color = "#636e72",
        segment.size = 0.4,
        segment.alpha = 0.8,
        ylim = c(0.42, 5.7),
        nudge_x = df_fonds_agg$nudge_x,
        nudge_y = df_fonds_agg$nudge_y,
        force = 4,
        force_pull = 0.5,
        max.iter = 10000
    ) +
    # Annotations textuelles
    annotate("text",
        x = 40, y = 3.1, label = "Fonds homogènes (Faible volume,\nFaible diversité)",
        hjust = 0, vjust = 1.0, size = 3.4, color = "#27ae60", fontface = "bold"
    ) +
    annotate("text",
        x = 9500, y = 3.1, label = "Fonds homogènes (Volume important,\nFaible diversité)",
        hjust = 1, vjust = 1.0, size = 3.4, color = "#27ae60", fontface = "bold"
    ) +
    annotate("text",
        x = 40, y = 5.8, label = "Fonds hétérogènes (Faible volume, Haute diversité)",
        hjust = 0, vjust = 0.5, size = 3.4, color = "#c0392b", fontface = "bold"
    ) +
    annotate("text",
        x = 9500, y = 5.8, label = "Fonds hétérogènes (Volume important,\nHaute diversité)",
        hjust = 1, vjust = 0.5, size = 3.4, color = "#c0392b", fontface = "bold"
    ) +
    # Titres, légendes et thèmes
    labs(
        title = "Complexité matérielle des fonds photographiques",
        subtitle = "Analyse combinée de la masse globale et de la diversité des objets photographiques.\nLa couleur du point représente la date médiane de production du fonds.",
        x = "Volume global d'objets photographiques (échelle log)",
        y = "Diversité des supports physiques",
        caption = "Données : Inventaire des fonds photographiques - Archives de l'État de Neuchâtel"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        text = element_text(family = "sans"),
        plot.title = element_text(face = "bold", size = 15, colour = "#2d3436", margin = margin(b = 6)),
        plot.subtitle = element_text(size = 11, colour = "#4b5563", lineheight = 1.2, margin = margin(b = 20)),
        plot.caption = element_text(size = 8.5, colour = "#6b7280", margin = margin(t = 14), hjust = 0),
        plot.caption.position = "plot",
        axis.title.x = element_text(face = "bold", size = 11, colour = "#2d3436", margin = margin(t = 12)),
        axis.title.y = element_text(face = "bold", size = 11, colour = "#2d3436", margin = margin(r = 12)),
        axis.text = element_text(size = 9.5, colour = "#2d3436"),
        panel.grid.major = element_line(colour = "#dfe6e9", linewidth = 0.45),
        panel.grid.minor = element_blank(),
        legend.position = "left",
        legend.box = "vertical",
        legend.margin = margin(r = 15),
        legend.title = element_text(size = 9.5, face = "bold", colour = "#2d3436"),
        legend.text = element_text(size = 9, colour = "#2d3436"),
        plot.margin = margin(20, 20, 20, 20)
    )

# Sauvegarde des graphiques

ggsave("scatter_plot_volume_diversite.svg",
    plot = p_scatter,
    width = 13.5,
    height = 9.5,
    dpi = 300,
    bg = "white"
)

ggsave("scatter_plot_volume_diversite.jpeg",
    plot = p_scatter,
    width = 13.5,
    height = 9.5,
    dpi = 300,
    bg = "white"
)
