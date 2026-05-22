# Script para regenerar biomass_models.rda desde inst/biomass_equations.xlsx

library(readxl)
library(dplyr)

# Leer el xlsx
biomass_equations <- read_excel("inst/biomass_equations.xlsx")

# Convertir a tibble y asegurar tipos
biomass_models <- as_tibble(biomass_equations) %>%
  mutate(
    article_id = as.character(article_id),
    r2 = as.numeric(r2),
    rmse = as.numeric(rmse)
  )

# Mostrar resumen de validación
cat("=== VALIDACIÓN DEL DATASET ===\n")
cat("Dimensiones:", nrow(biomass_models), "x", ncol(biomass_models), "\n")
cat("Columnas:", paste(names(biomass_models), collapse=", "), "\n\n")

# Resumen por artículo
cat("Resumen por artículo:\n")
for (id in unique(biomass_models$article_id)) {
  x <- subset(biomass_models, article_id == id)
  dash_count <- sum(trimws(x$expression) == "-", na.rm = TRUE)
  expr_valid <- !is.na(x$expression)
  malformed_count <- sum(grepl("^exp\\([^()]*(\\*\\*2|\\^2)\\) / 2\\)", x$expression[expr_valid]))
  cat(sprintf("  %s: %d rows | dashes: %d | malformed: %d\n", 
              id, nrow(x), dash_count, malformed_count))
}

# Verificar que no haya dashes ni malformaciones
dashes <- sum(trimws(biomass_models$expression) == "-", na.rm = TRUE)
expr_valid <- !is.na(biomass_models$expression)
malformed <- sum(grepl("^exp\\([^()]*(\\*\\*2|\\^2)\\) / 2\\)", biomass_models$expression[expr_valid]))

cat("\nTOTAL - dashes:", dashes, "| malformed:", malformed, "\n")

if (dashes == 0 && malformed == 0) {
  cat("✓ Dataset limpio - listo para guardar\n\n")
} else {
  cat("✗ Aún hay problemas - revisar manualmente\n")
  if (dashes > 0) {
    cat("  Filas con dashes:\n")
    print(biomass_models %>% filter(trimws(expression) == "-") %>% select(article_id, species, tree_component, expression))
  }
  if (malformed > 0) {
    cat("  Filas con expresiones malformadas:\n")
    print(biomass_models %>% filter(grepl("^exp\\([^()]*(\\*\\*2|\\^2)\\) / 2\\)", expression)) %>% select(article_id, species, tree_component, expression) %>% head(5))
  }
}

# Guardar el dataset
cat("\nGuardando biomass_models.rda...\n")
usethis::use_data(biomass_models, overwrite = TRUE)
cat("✓ Hecho!\n")
