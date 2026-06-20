# Script para regenerar snfi3_volume_coefficients y snfi4_volume_coefficients
# desde los CSVs de inst/volume/

library(dplyr)

# 1. Cargar catálogo de provincias INE para mapeo de nombres de provincias
ine_provs <- read.csv("inst/volume/INE_provinces/ine_provinces.csv", stringsAsFactors = FALSE) %>%
  select(Codigo_provincia = province_id, Nombre_provincia = province_name) %>%
  distinct()

# 2. Compilar Coeficientes SNFI4
cat("Compilando coeficientes SNFI4...\n")
snfi4_files <- list.files("inst/volume/SNFI4_volume_coefficients", full.names = TRUE)
snfi4_list <- lapply(snfi4_files, function(f) {
  read.csv(f, colClasses = "character", stringsAsFactors = FALSE)
})
snfi4_raw <- do.call(rbind, snfi4_list)

snfi4_volume_coefficients <- snfi4_raw %>%
  mutate(
    Codigo_provincia = as.integer(Codigo_provincia),
    Codigo_especie   = as.integer(Codigo_especie),
    F.c.             = as.integer(F.c.),
    Modelo           = as.integer(Modelo),
    a                = suppressWarnings(as.numeric(a)),
    b                = suppressWarnings(as.numeric(b)),
    c                = suppressWarnings(as.numeric(c)),
    d                = suppressWarnings(as.numeric(d)),
    p                = suppressWarnings(as.numeric(p)),
    q                = suppressWarnings(as.numeric(q)),
    r                = suppressWarnings(as.numeric(r))
  ) %>%
  as_tibble()

# 3. Compilar Coeficientes SNFI3 (con missing species)
cat("Compilando coeficientes SNFI3...\n")
snfi3_files <- list.files("inst/volume/SNFI3_volume_coefficients", full.names = TRUE)
snfi3_files_clean <- snfi3_files[!grepl("missing", snfi3_files)]
snfi3_list <- lapply(snfi3_files_clean, function(f) {
  read.csv(f, colClasses = "character", stringsAsFactors = FALSE)
})
snfi3_raw <- do.call(rbind, snfi3_list)

# Cargar y armonizar missing species
missing_sp <- read.csv(
  "inst/volume/SNFI3_volume_coefficients/SNFI3_missing_species_volume_coefficients.csv",
  colClasses = "character",
  stringsAsFactors = FALSE
)

missing_clean <- missing_sp %>%
  rename(Codigo_provincia = Provincia) %>%
  mutate(Codigo_provincia = as.integer(Codigo_provincia)) %>%
  left_join(ine_provs, by = "Codigo_provincia") %>%
  select(all_of(names(snfi3_raw)))

snfi3_combined <- rbind(snfi3_raw, missing_clean)

snfi3_volume_coefficients <- snfi3_combined %>%
  mutate(
    Codigo_provincia = as.integer(Codigo_provincia),
    Codigo_especie   = as.integer(Codigo_especie),
    F.c.             = as.integer(F.c.),
    Modelo           = as.integer(Modelo),
    a                = suppressWarnings(as.numeric(a)),
    b                = suppressWarnings(as.numeric(b)),
    c                = suppressWarnings(as.numeric(c)),
    d                = suppressWarnings(as.numeric(d)),
    p                = suppressWarnings(as.numeric(p)),
    q                = suppressWarnings(as.numeric(q)),
    r                = suppressWarnings(as.numeric(r)),
    R2               = suppressWarnings(as.numeric(R2))
  ) %>%
  as_tibble()

# 4. Mostrar Validación Básica
cat("=== VALIDACIÓN DE DATASETS ===\n")
cat("SNFI4: ", nrow(snfi4_volume_coefficients), " filas | ", length(unique(snfi4_volume_coefficients$Codigo_provincia)), " provincias\n")
cat("SNFI3: ", nrow(snfi3_volume_coefficients), " filas | ", length(unique(snfi3_volume_coefficients$Codigo_provincia)), " provincias\n")

# 5. Guardar datasets
cat("\nGuardando datasets compilados en data/...\n")
usethis::use_data(snfi3_volume_coefficients, overwrite = TRUE)
usethis::use_data(snfi4_volume_coefficients, overwrite = TRUE)
cat("Hecho!\n")
