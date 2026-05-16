# Script para regenerar carbon_models.rda desde inst/carbon_values.xlsx

library(readxl)
library(dplyr)

# Leer el xlsx
carbon_raw <- read_excel("inst/carbon_values.xlsx")

# Limpiar y convertir tipos
carbon_models <- as_tibble(carbon_raw) %>%
  mutate(
    article_id        = as.character(article_id),
    carbon_percentage = suppressWarnings(as.numeric(carbon_percentage)),  # "-" -> NA
    r2                = suppressWarnings(as.numeric(r2)),
    rmse              = suppressWarnings(as.numeric(rmse))
  )

# Mostrar resumen de validación
cat("=== VALIDACIÓN DEL DATASET ===\n")
cat("Dimensiones:", nrow(carbon_models), "x", ncol(carbon_models), "\n")
cat("Columnas:", paste(names(carbon_models), collapse = ", "), "\n\n")

cat("Resumen por artículo:\n")
for (id in unique(carbon_models$article_id)) {
  x       <- subset(carbon_models, article_id == id)
  na_pct  <- sum(is.na(x$carbon_percentage))
  cat(sprintf("  %s: %d rows | carbon_percentage NA: %d\n", id, nrow(x), na_pct))
}

cat("\nEspecies únicas:", length(unique(carbon_models$species)), "\n")
cat("Componentes únicos:", paste(sort(unique(carbon_models$tree_component)), collapse = ", "), "\n")

na_total <- sum(is.na(carbon_models$carbon_percentage))
cat("\nTOTAL carbon_percentage NA:", na_total, "(filas con '-' en xlsx)\n")

if (na_total > 0) {
  cat("Filas con NA en carbon_percentage:\n")
  print(
    carbon_models %>%
      filter(is.na(carbon_percentage)) %>%
      select(article_id, species, tree_component)
  )
}

# Guardar el dataset
cat("\nGuardando carbon_models.rda...\n")
usethis::use_data(carbon_models, overwrite = TRUE)
cat("Hecho!\n")
