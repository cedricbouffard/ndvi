#!/usr/bin/env Rscript
# Script R pour créer une belle carte NDVI (version GitHub Actions)
# Packages requis : terra, ggplot2, ggspatial, sf

# Installation automatique des packages manquants
options(repos = c(CRAN = "https://cloud.r-project.org"))
packages <- c("terra", "ggplot2", "ggspatial", "sf")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installation du package %s...\n", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 1. Trouver le fichier NDVI le plus récent
cat("Recherche du fichier NDVI le plus récent...\n")
ndvi_files <- list.files(pattern = "ndvi_sentinel2_\\d{8}\\.tif")

if (length(ndvi_files) == 0) {
  stop("Aucun fichier NDVI trouvé. Veuillez d'abord exécuter check_and_extract_ndvi.R")
}

# Trier par date (le nom contient la date au format YYYYMMDD)
ndvi_files <- sort(ndvi_files, decreasing = TRUE)
latest_ndvi_file <- ndvi_files[1]

# Extraire la date du nom de fichier
date_match <- regmatches(latest_ndvi_file, regexpr("\\d{8}", latest_ndvi_file))
if (length(date_match) > 0) {
  ndvi_date <- as.Date(date_match, format = "%Y%m%d")
  ndvi_date_str <- format(ndvi_date, "%d %B %Y")
} else {
  ndvi_date_str <- "Date inconnue"
}

cat(sprintf("Fichier NDVI utilisé : %s (Date : %s)\n", latest_ndvi_file, ndvi_date_str))

# 2. Charger le raster NDVI et le polygone
cat("Chargement du raster NDVI et du polygone...\n")

ndvi_rast <- rast(latest_ndvi_file)
polygone <- st_read("champ.shp", quiet = TRUE)

# Convertir le raster en dataframe pour ggplot
ndvi_df <- as.data.frame(ndvi_rast, xy = TRUE)
names(ndvi_df)[3] <- "ndvi"

# Obtenir les statistiques pour ajuster l'échelle
ndvi_min <- min(ndvi_df$ndvi, na.rm = TRUE)
ndvi_max <- max(ndvi_df$ndvi, na.rm = TRUE)
ndvi_mean <- mean(ndvi_df$ndvi, na.rm = TRUE)

cat(sprintf("Valeurs NDVI - Min: %.4f, Max: %.4f, Moyenne: %.4f\n", ndvi_min, ndvi_max, ndvi_mean))

# 3. Créer la carte
cat("Création de la carte...\n")

# Palette de couleurs NDVI adaptée aux valeurs réelles
ndvi_colors <- c(
  "#8B0000",  # Rouge foncé (très bas)
  "#CD5C5C",  # Rouge indien
  "#F4A460",  # Brun sable
  "#DEB887",  # Beige foncé
  "#F0E68C",  # Kaki
  "#9ACD32",  # Jaune-vert
  "#7CFC00",  # Vert prairie
  "#32CD32",  # Vert lime
  "#228B22",  # Vert forêt
  "#006400"   # Vert foncé
)

carte_ndvi <- ggplot() +
  # Couche NDVI avec échelle ajustée aux valeurs réelles
  geom_raster(data = ndvi_df, aes(x = x, y = y, fill = ndvi)) +
  scale_fill_gradientn(
    colors = ndvi_colors,
    limits = c(0, 0.4),
    breaks = c(0, 0.1, 0.2, 0.3, 0.4),
    labels = c("0.0", "0.1", "0.2", "0.3", "0.4"),
    na.value = "transparent",
    name = "NDVI",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 1.2,
      barheight = 12,
      ticks.colour = "black",
      frame.colour = "black"
    )
  ) +
  
  # Contour du polygone
  geom_sf(data = polygone, fill = NA, color = "white", linewidth = 2.5) +
  geom_sf(data = polygone, fill = NA, color = "black", linewidth = 0.8) +
  
  # Configuration de la carte
  coord_sf(crs = crs(ndvi_rast)) +
  
  # Thème
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#2c3e50", margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "#555555", margin = margin(b = 20)),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0.5, margin = margin(t = 15)),
    legend.position = "right",
    legend.title = element_text(size = 13, face = "bold", margin = margin(b = 10)),
    legend.text = element_text(size = 11),
    legend.margin = margin(l = 10),
    panel.grid = element_line(color = "gray90", linewidth = 0.3),
    panel.background = element_rect(fill = "gray95", color = NA),
    axis.text = element_text(size = 10, color = "gray40"),
    axis.title = element_blank()
  ) +
  
  # Titre avec statistiques
  labs(
    title = "Carte NDVI - Sentinel-2",
    subtitle = sprintf("Date : %s | Résolution : 10m | NDVI moyen : %.3f (min: %.3f, max: %.3f)", 
                       ndvi_date_str, ndvi_mean, ndvi_min, ndvi_max),
    caption = "Source : ESA Sentinel-2 L2A via Microsoft Planetary Computer | Mise à jour automatique quotidienne"
  ) +
  
  # Flèche du Nord
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm"),
    style = north_arrow_fancy_orienteering(
      fill = c("white", "black"),
      line_col = "black",
      line_width = 0.5
    )
  ) +
  
  # Échelle
  annotation_scale(
    location = "br",
    width_hint = 0.3,
    bar_cols = c("black", "white"),
    text_col = "black",
    line_width = 1,
    height = unit(0.3, "cm"),
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")
  )

# 4. Sauvegarder la carte
cat("Sauvegarde de la carte...\n")

ggsave(
  filename = "carte_ndvi_sentinel2.png",
  plot = carte_ndvi,
  width = 13,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("Carte sauvegardée : carte_ndvi_sentinel2.png\n")

# Afficher la carte
print(carte_ndvi)

cat("\nScript terminé avec succès !\n")
