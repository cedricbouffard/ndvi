#!/usr/bin/env Rscript
# Script pour vérifier et extraire le NDVI le plus récent si nécessaire
# Utilisé par GitHub Actions

library(rstac)
library(terra)
library(sf)

# Fichier pour stocker la date de la dernière image traitée
LAST_DATE_FILE <- "last_ndvi_date.txt"

# Fonction pour lire la dernière date traitée
read_last_date <- function() {
  if (file.exists(LAST_DATE_FILE)) {
    date_str <- trimws(readLines(LAST_DATE_FILE))
    if (length(date_str) > 0 && nchar(date_str) > 0) {
      return(as.Date(date_str))
    }
  }
  return(NULL)
}

# Fonction pour sauvegarder la date
save_last_date <- function(date) {
  writeLines(as.character(date), LAST_DATE_FILE)
}

# Fonction principale
cat("=== Vérification du NDVI Sentinel-2 ===\n\n")

# 1. Charger le shapefile
cat("Chargement du shapefile...\n")
polygone <- st_read("champ.shp", quiet = TRUE)

# S'assurer que le polygone est en WGS84
if (st_crs(polygone)$epsg != 4326) {
  polygone <- st_transform(polygone, 4326)
}

bbox <- st_bbox(polygone)

# 2. Lire la dernière date traitée
last_date <- read_last_date()
if (is.null(last_date)) {
  cat("Aucune date précédente trouvée. Première exécution.\n")
  last_date <- as.Date("2000-01-01")  # Date ancienne pour tout récupérer
} else {
  cat(sprintf("Dernière date traitée : %s\n", last_date))
}

# 3. Connexion au catalogue STAC
cat("Connexion au catalogue STAC...\n")
s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

# 4. Recherche des images Sentinel-2
cat("Recherche des images Sentinel-2...\n")

end_date <- format(Sys.Date(), "%Y-%m-%d")
start_date <- format(Sys.Date() - 60, "%Y-%m-%d")  # 60 jours en arrière

items <- s_obj %>%
  stac_search(
    collections = "sentinel-2-l2a",
    bbox = c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]),
    datetime = paste(start_date, end_date, sep = "/"),
    limit = 20
  ) %>%
  get_request()

# Vérifier si des résultats ont été trouvés
if (length(items$features) == 0) {
  cat("Aucune image Sentinel-2 trouvée.\n")
  quit(status = 0)  # Sortie normale, pas d'erreur
}

# 5. Trouver l'image la plus récente
dates <- sapply(items$features, function(x) x$properties$datetime)
dates <- as.Date(dates)
recent_idx <- which.max(dates)
recent_date <- dates[recent_idx]
recent_item <- items$features[[recent_idx]]

cat(sprintf("Image la plus récente trouvée : %s\n", recent_date))

# 6. Comparer avec la dernière date traitée
if (recent_date <= last_date) {
  cat(sprintf("Pas de nouvelle image depuis le %s. Arrêt.\n", last_date))
  quit(status = 0)  # Pas de nouvelle image, sortie normale
}

cat(sprintf("Nouvelle image détectée ! Date : %s\n", recent_date))

# 7. Extraire le NDVI
cat("\n=== Extraction du NDVI ===\n")

# Signer les assets
items_signed <- items_sign(items, sign_fn = sign_planetary_computer())
recent_item_signed <- items_signed$features[[recent_idx]]

# URLs des bandes
red_url <- recent_item_signed$assets$B04$href
nir_url <- recent_item_signed$assets$B08$href

if (is.null(red_url) || is.null(nir_url)) {
  stop("Impossible d'obtenir les URLs des bandes")
}

# Lire les bandes via VSI
cat("Lecture des bandes...\n")
red_vsi <- paste0("/vsicurl/", red_url)
nir_vsi <- paste0("/vsicurl/", nir_url)

red_rast <- rast(red_vsi)
nir_rast <- rast(nir_vsi)

# Découper selon le polygone
cat("Découpage selon le polygone...\n")
polygone_vect <- vect(polygone)
if (crs(polygone_vect) != crs(red_rast)) {
  polygone_vect <- project(polygone_vect, crs(red_rast))
}

red_crop <- crop(red_rast, polygone_vect, mask = TRUE)
nir_crop <- crop(nir_rast, polygone_vect, mask = TRUE)

# Calcul du NDVI
cat("Calcul du NDVI...\n")
ndvi <- (nir_crop - red_crop) / (nir_crop + red_crop)
ndvi <- clamp(ndvi, -1, 1)

# Sauvegarder le raster
output_file <- sprintf("ndvi_sentinel2_%s.tif", format(recent_date, "%Y%m%d"))
writeRaster(ndvi, output_file, overwrite = TRUE, datatype = "FLT4S")
cat(sprintf("NDVI sauvegardé : %s\n", output_file))

# Supprimer l'ancien fichier NDVI s'il existe (garde seulement le plus récent)
old_files <- list.files(pattern = "ndvi_sentinel2_\\d{8}\\.tif")
for (f in old_files) {
  if (f != output_file) {
    file.remove(f)
    cat(sprintf("Ancien fichier supprimé : %s\n", f))
  }
}

# Sauvegarder la date
save_last_date(recent_date)
cat(sprintf("Date sauvegardée : %s\n", recent_date))

# Statistiques
cat("\n=== Statistiques NDVI ===\n")
cat(sprintf("Date : %s\n", recent_date))
cat(sprintf("Valeur moyenne : %.4f\n", global(ndvi, fun = "mean", na.rm = TRUE)[1,1]))
cat(sprintf("Valeur min : %.4f\n", minmax(ndvi)[1]))
cat(sprintf("Valeur max : %.4f\n", minmax(ndvi)[2]))

cat("\nExtraction terminée avec succès !\n")

# Sortie avec code 0 pour indiquer le succès
quit(status = 0)
