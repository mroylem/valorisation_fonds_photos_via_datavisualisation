# Romain Margaron
# Dans le cadre du Master Patrimoine régional et humanités numériques, Université de Neuchâtel
# Pour le travail de mémoire intitulé :
# Valoriser les archives photographiques par la datavisualisation : potentiel d’un outil de médiation dans trois institutions patrimoniales neuchâteloises.
# Réalisé entre avril et août 2026.

# Etape 1 : Import
setwd("c:/Users/raiku/Documents/Memoire_RM/dataviz/LAT/dendrogramme_numero_inventaire")

library(ggplot2)
library(dplyr)
library(readxl)
library(ggraph)
library(igraph)

# Dataframe
df <- read_excel("df_numero_inventaire.xlsx")

# Etape 2 : Préparation des données

df <- df %>% arrange(`1ère donnée : Commune`, `2ème donnée : Site de fouille`, `3ème donnée : Thème principal`)

df <- df %>%
    mutate(
        id_1 = `1ère donnée : Commune`,
        id_2 = paste(id_1, `2ème donnée : Site de fouille`, sep = "_"),
        id_3 = paste(id_2, `3ème donnée : Thème principal`, sep = "_"),
        id_4 = paste(id_3, `4ème donnée : Thème secondaire`, sep = "_"),
        id_5 = paste(id_4, `5ème donnée : Classement des conteneurs à images`, sep = "_")
    )

# Création de la liste des liens
edges <- bind_rows(
    df %>% dplyr::select(from = id_1, to = id_2) %>% distinct(),
    df %>% dplyr::select(from = id_2, to = id_3) %>% distinct(),
    df %>% dplyr::select(from = id_3, to = id_4) %>% distinct(),
    df %>% dplyr::select(from = id_4, to = id_5) %>% distinct()
) %>% filter(!is.na(to))

# Fonction pour ajouter des sauts de ligne
wrap_text <- function(x, n = 100) {
    sapply(x, function(y) paste(strwrap(y, width = n), collapse = "\n"))
}

# Création des informations pour chaque noeud
vertices <- bind_rows(
    df %>% dplyr::select(name = id_1, label = `1ère donnée : Commune`) %>% mutate(depth = 1) %>% distinct(),
    df %>% dplyr::select(name = id_2, label = `2ème donnée : Site de fouille`) %>% mutate(depth = 2) %>% distinct(),
    df %>% dplyr::select(name = id_3, label = `3ème donnée : Thème principal`) %>% mutate(depth = 3) %>% distinct(),
    df %>% dplyr::select(name = id_4, label = `4ème donnée : Thème secondaire`) %>% mutate(depth = 4) %>% distinct(),
    df %>% dplyr::select(name = id_5, label = `5ème donnée : Classement des conteneurs à images`) %>% mutate(depth = 5) %>% distinct()
) %>%
    filter(!is.na(name)) %>%
    mutate(label_wrap = wrap_text(label)) %>%
    mutate(text_color = if_else(depth %in% c(1, 5), "white", "#000000"))

# Construction du graphe
graph <- graph_from_data_frame(edges, vertices = vertices, directed = TRUE)

# Etape 3 : Visualisation

p_tree <- ggraph(graph, layout = "dendrogram", circular = FALSE) +
    geom_hline(yintercept = 0:4, linetype = "dashed", color = "gray80") +
    geom_edge_diagonal(color = "gray50", edge_width = 1) +
    geom_node_label(aes(label = label_wrap, fill = as.factor(depth), color = text_color),
        fontface = "bold", size = 4, label.padding = unit(0.4, "lines")
    ) +
    coord_flip(clip = "off") +
    scale_x_reverse() +
    scale_y_reverse(
        breaks = 4:0,
        labels = c("1ère donnée\n(Commune)", "2ème donnée\n(Site)", "3ème donnée\n(Thème principal)", "4ème donnée\n(Thème secondaire)", "5ème donnée\n(Classement)"),
        position = "right",
        expand = expansion(mult = c(0.05, 0.05))
    ) +
    scale_fill_viridis_d(option = "turbo", direction = -1, guide = "none") +
    scale_color_identity() +
    theme_minimal() +
    labs(
        title = "Structure hiérarchique du numéro d'identification",
        subtitle = "Ce diagramme illustre la structure logique des six segments d'identification. L'arborescence est présentée ici à titre d'exemple et ne contient pas l'intégralité des données de l'inventaire.",
        caption = "Données : Inventaire de la Diathèque - Laténium Parc et Musée d'Archéologie."
    ) +
    theme(
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", linetype = "dashed", linewidth = 0.8),
        axis.text.x.top = element_text(size = 13, face = "bold", color = "gray30", vjust = 0),
        plot.title = element_text(face = "bold", size = 24, hjust = 0.5, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 20)),
        plot.caption = element_text(size = 11, hjust = 0, colour = "grey30"),
        plot.margin = margin(1, 4, 1, 1, "cm")
    )

# Sauvegarde en image PNG et SVG
ggsave("structure_hierarchique_numero_identification.png", plot = p_tree, width = 20, height = 14, dpi = 300, bg = "white")
ggsave("structure_hierarchique_numero_identification.svg", plot = p_tree, width = 20, height = 14, dpi = 300, bg = "white")
