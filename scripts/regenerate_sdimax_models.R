# Script to regenerate sdimax_models.rda from inst/2020_RodriguezdePrado_sdi_models_spain.csv

library(dplyr)
library(readr)

# Read the raw CSV
raw_data <- read_csv("inst/2020_RodriguezdePrado_sdi_models_spain.csv", show_col_types = FALSE)

# Helper function to clean numeric fields (e.g. replace en-dashes with standard minus signs)
clean_num <- function(x) {
  if (is.character(x)) {
    x <- gsub("–", "-", x)
    x <- gsub("—", "-", x)
  }
  suppressWarnings(as.numeric(x))
}

# Clean, format, and add metadata
sdimax_models <- raw_data %>%
  transmute(
    article_id   = "rodriguez-prado-2020",
    title        = "Potential climatic inﬂuence on maximum stand carrying capacity for 15 Mediterranean coniferous and broadleaf species",
    doi_url      = "https://doi.org/10.1016/j.foreco.2019.117824",
    country      = "Spain",
    species      = as.character(Species),
    model_name   = as.character(Model),
    a0           = clean_num(a0),
    a1           = clean_num(a1),
    b0           = clean_num(b0),
    b1           = clean_num(b1),
    aic          = clean_num(AIC),
    pseudo_r2    = clean_num(pseudoR2),
    q_index      = clean_num(Q_index)
  ) %>%
  mutate(
    # Fill NAs in climate slope/interaction terms with 0
    a1 = ifelse(is.na(a1), 0, a1),
    b1 = ifelse(is.na(b1), 0, b1)
  ) %>%
  as_tibble()

# Validation checks
cat("=== VALIDATING sdimax_models ===\n")
cat("Dimensions:", nrow(sdimax_models), "x", ncol(sdimax_models), "\n")
cat("Columns:", paste(names(sdimax_models), collapse = ", "), "\n")
cat("Unique species count:", length(unique(sdimax_models$species)), "\n")
cat("Checking for NAs in critical columns:\n")
cat("  - a0 NAs:", sum(is.na(sdimax_models$a0)), "\n")
cat("  - b0 NAs:", sum(is.na(sdimax_models$b0)), "\n")
cat("  - a1 NAs:", sum(is.na(sdimax_models$a1)), "\n")
cat("  - b1 NAs:", sum(is.na(sdimax_models$b1)), "\n")

# Save dataset to data/
cat("\nSaving sdimax_models.rda...\n")
usethis::use_data(sdimax_models, overwrite = TRUE)
cat("Done!\n")
