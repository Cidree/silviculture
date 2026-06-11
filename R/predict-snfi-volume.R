
# Internal equation functions ---------------------------------------------------
# All functions expect dbh in mm, h in m, and return dm^3.
# Coefficients that are "-" in SNFI4 CSVs are read as NA after .snfi_coef().

# Helper: convert a coefficient that may be "-" to numeric
.snfi_coef <- function(x) {
  x
}

# VCC: Volumen maderable con corteza (dm^3)
.snfi_vcc <- function(dbh_mm, h_m, pars) {
  if (nrow(pars) == 0L) return(NA_real_)
  modelo <- pars$Modelo

  if (modelo == 1L) {
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b)
    a + b * dbh_mm^2 * h_m
  } else if (modelo == 11L) {
    p <- .snfi_coef(pars$p); q <- .snfi_coef(pars$q); r <- .snfi_coef(pars$r)
    p * dbh_mm^q * h_m^r
  } else {
    cli::cli_warn("VCC: model {modelo} not recognized. Returning NA.")
    NA_real_
  }
}

# VSC: Volumen maderable sin corteza (dm^3)
.snfi_vsc <- function(vcc, pars) {
  if (nrow(pars) == 0L || is.na(vcc)) return(NA_real_)
  modelo <- pars$Modelo

  if (modelo == 7L) {
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b); c <- .snfi_coef(pars$c)
    a + b * vcc + c * vcc^2
  } else {
    cli::cli_warn("VSC: model {modelo} not recognized. Returning NA.")
    NA_real_
  }
}

# IAVC: Incremento anual de volumen con corteza (dm^3)
.snfi_iavc <- function(dbh_mm, dnm_mm, h_m, vcc, pars) {
  if (nrow(pars) == 0L) return(NA_real_)
  modelo <- pars$Modelo

  if (modelo == 8L) {
    if (is.na(vcc)) { cli::cli_warn("IAVC model 8 requires vcc."); return(NA_real_) }
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b); c <- .snfi_coef(pars$c)
    a + b * vcc + c * vcc^2
  } else if (modelo == 13L) {
    if (is.na(dbh_mm) || is.na(dnm_mm)) { cli::cli_warn("IAVC model 13 requires dbh and dnm."); return(NA_real_) }
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b)
    a + b * (dbh_mm + dnm_mm)
  } else if (modelo == 14L) {
    p <- .snfi_coef(pars$p); q <- .snfi_coef(pars$q)
    p * dbh_mm^q
  } else if (modelo == 16L) {
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b)
    a + b * dbh_mm^2
  } else if (modelo == 17L) {
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b); c <- .snfi_coef(pars$c)
    a + b * dbh_mm + c * dbh_mm^2
  } else if (modelo == 18L) {
    p <- .snfi_coef(pars$p); q <- .snfi_coef(pars$q)
    p * exp(q * dbh_mm)
  } else if (modelo == 19L) {
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b)
    c <- .snfi_coef(pars$c); d <- .snfi_coef(pars$d)
    a + b * dbh_mm + c * dbh_mm^2 + d * dbh_mm^3
  } else if (modelo == 20L) {
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b); d <- .snfi_coef(pars$d)
    a + b * dbh_mm + d * dbh_mm^3
  } else if (modelo == 21L) {
    c <- .snfi_coef(pars$c); d <- .snfi_coef(pars$d)
    c * dbh_mm^2 + d * dbh_mm^3
  } else if (modelo == 25L) {
    if (is.na(h_m)) { cli::cli_warn("IAVC model 25 requires height."); return(NA_real_) }
    p <- .snfi_coef(pars$p); q <- .snfi_coef(pars$q); r <- .snfi_coef(pars$r)
    p * dbh_mm^q * h_m^r
  } else if (modelo == 12L) {
    # Power model (same formula as VLE model 12, applied to IAVC component)
    p <- .snfi_coef(pars$p); q <- .snfi_coef(pars$q)
    p * dbh_mm^q
  } else {
    cli::cli_warn("IAVC: model {modelo} not recognized. Returning NA.")
    NA_real_
  }
}

# VLE: Volumen de lenas gruesas (dm^3)
.snfi_vle <- function(dbh_mm, vcc, pars) {
  if (nrow(pars) == 0L) return(NA_real_)
  modelo <- pars$Modelo

  if (modelo == 10L) {
    if (is.na(vcc)) { cli::cli_warn("VLE model 10 requires vcc."); return(NA_real_) }
    a <- .snfi_coef(pars$a); b <- .snfi_coef(pars$b); c <- .snfi_coef(pars$c)
    a + b * vcc + c * vcc^2
  } else if (modelo == 12L) {
    p <- .snfi_coef(pars$p); q <- .snfi_coef(pars$q)
    p * dbh_mm^q
  } else if (modelo == 21L) {
    # Cubic polynomial (same formula as IAVC model 21, applied to VLE component)
    c <- .snfi_coef(pars$c); d <- .snfi_coef(pars$d)
    c * dbh_mm^2 + d * dbh_mm^3
  } else {
    cli::cli_warn("VLE: model {modelo} not recognized. Returning NA.")
    NA_real_
  }
}


# Support functions -------------------------------------------------------------

# Resolve a province name or numeric code to its integer ID.
# Returns integer or stops with a clear error.
.snfi_province_id <- function(province) {
  prov_num <- suppressWarnings(as.integer(province))
  if (!is.na(prov_num)) return(prov_num)

  prov_file <- system.file("volume", "INE_provinces", "ine_provinces.csv",
                           package = "silviculture")
  prov_df <- utils::read.csv(prov_file, stringsAsFactors = FALSE)
  match_row <- prov_df[tolower(prov_df$province_name) == tolower(province), ]
  if (nrow(match_row) == 0L)
    cli::cli_abort("Province {.val {province}} not found. Check {.fn silv_snfi_provinces}.")
  match_row$province_id[1L]
}

# Load the coefficient table for one province (SNFI4 first, fallback to SNFI3).
# Returns a data.frame with a `snfi_version` attribute ("SNFI4" or "SNFI3").
.snfi_load_pars <- function(province_id) {
  # Check in SNFI4 first
  has_snfi4 <- any(silviculture::snfi4_volume_coefficients$Codigo_provincia == province_id)

  if (has_snfi4) {
    pars <- silviculture::snfi4_volume_coefficients[silviculture::snfi4_volume_coefficients$Codigo_provincia == province_id, ]
    attr(pars, "snfi_version") <- "SNFI4"
  } else {
    has_snfi3 <- any(silviculture::snfi3_volume_coefficients$Codigo_provincia == province_id)
    if (has_snfi3) {
      pars <- silviculture::snfi3_volume_coefficients[silviculture::snfi3_volume_coefficients$Codigo_provincia == province_id, ]
      attr(pars, "snfi_version") <- "SNFI3"
    } else {
      cli::cli_abort(
        "No coefficient data found for province ID {province_id}.
         Check {.fn silv_snfi_provinces} for supported provinces."
      )
    }
  }
  pars
}

# Filter a coefficient table to one species and one quality class.
.snfi_filter_pars <- function(pars, species, quality) {
  # species: numeric code or character name
  if (!is.na(suppressWarnings(as.numeric(species)))) {
    sub <- pars[pars$Codigo_especie == as.integer(species), ]
  } else {
    sub <- pars[tolower(pars$Especie) == tolower(species), ]
  }

  if (nrow(sub) == 0L) {
    cli::cli_warn("No parameters found for species {.val {species}}.")
    return(sub)
  }

  # quality class
  if (identical(quality, "default")) {
    for (q in 1:6) {
      q_sub <- sub[sub[["F.c."]] == q, ]
      if (nrow(q_sub) > 0L) { sub <- q_sub; break }
    }
  } else {
    q_val <- suppressWarnings(as.integer(quality))
    if (is.na(q_val) || !q_val %in% 1:6)
      cli::cli_abort("`quality` must be \"default\" or an integer between 1 and 6.")
    sub <- sub[sub[["F.c."]] == q_val, ]
    if (nrow(sub) == 0L)
      cli::cli_warn("No parameters found for quality {.val {quality}} and species {.val {species}}.")
  }
  sub
}


# Main exported function --------------------------------------------------------

#' Predict tree volume using Spanish National Forest Inventory equations
#'
#' Computes four volume metrics (in dm^3) for one or more trees using the
#' allometric equations from the 3rd and 4th Spanish National Forest Inventory
#' (SNFI3/SNFI4), as documented by MITECO (Anexo 19).
#'
#' @param province A character or numeric vector. Province name (e.g.
#'   `"Cantabria"`) or province code (e.g. `39`). See [silv_snfi_provinces()].
#' @param species A character or numeric vector. Species scientific name (e.g.
#'   `"Pinus radiata"`) or SNFI species code (e.g. `28`). See
#'   [silv_snfi_species()].
#' @param dbh A numeric vector of diameter at breast height in **cm**.
#' @param h A numeric vector of tree heights in **m**. Optional for some
#'   models (e.g. SNFI3 model 14), but required by most VCC equations.
#' @param dnm A numeric vector of mean plot diameter in **cm**. Required only
#'   when the IAVC equation uses model 13.
#' @param quality A character or numeric vector. Quality/shape class (`1`-`6`).
#'   Use `"default"` (the default) to select the first available class.
#' @param quiet A logical value. If `TRUE`, suppresses citation messages.
#'   Defaults to `FALSE`.
#'
#' @return A `data.frame` with one row per input tree and five columns:
#' \describe{
#'   \item{`vcc`}{Merchantable volume **with** bark (dm^3).}
#'   \item{`vsc`}{Merchantable volume **without** bark (dm^3).}
#'   \item{`iavc`}{Annual volume increment with bark (dm^3).}
#'   \item{`vle`}{Coarse firewood volume (dm^3).}
#'   \item{`snfi_version`}{Dataset used: `"SNFI4"` or `"SNFI3"`.}
#' }
#' Any component for which parameters are missing or a required input is
#' `NA` returns `NA` for that column, without stopping execution.
#'
#' @details
#' ## Model selection
#'
#' For each province, SNFI4 data are preferred. When a province is not covered
#' by SNFI4 (11 provinces), the function falls back to SNFI3 automatically.
#' SNFI3 also provides a supplementary file for species not present in the
#' standard provincial tables.
#'
#' ## Equation models
#'
#' | Component | Models used |
#' |---|---|
#' | VCC | 1, 11 |
#' | VSC | 7 |
#' | IAVC | 8, 13, 14, 16, 17, 18, 19, 20, 21, 25 |
#' | VLE | 10, 12 |
#'
#' Full model equations are available in Anexo 19 of the MITECO SIG
#' documentation (see References).
#'
#' ## Units
#'
#' Inputs: `dbh` and `dnm` in **cm** (converted to mm internally),
#' `h` in **m**. All outputs in **dm^3**.
#'
#' @references
#' MITECO. 4th Spanish National Forest Inventory - SIG database codes.
#' \url{https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf}
#'
#' MITECO. 3rd Spanish National Forest Inventory - SIG database codes.
#' \url{https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf}
#'
#' @seealso [silv_snfi_provinces()], [silv_snfi_species()], [silv_tree_volume()]
#'
#' @export
#'
#' @examples
#' # Single tree: Pinus radiata in Alava (province 1), code 28
#' silv_predict_snfi_volume(
#'   province = 1,
#'   species  = 28,
#'   dbh      = 20,
#'   h        = 15,
#'   dnm      = 23
#' )
#'
#' # Mixed inputs: province and species by name
#' silv_predict_snfi_volume(
#'   province = c(1, 39),
#'   species  = c("Pinus radiata", "Pinus radiata"),
#'   dbh      = c(20, 25),
#'   h        = c(15, 18),
#'   dnm      = c(23, 26)
#' )
silv_predict_snfi_volume <- function(
    province,
    species,
    dbh     = NULL,
    h       = NULL,
    dnm     = NULL,
    quality = "default",
    quiet   = FALSE) {

  # 0. Validate and recycle inputs -----------------------------------------------
  n <- max(length(province), length(species))
  if (length(province) == 1L) province <- rep(province, n)
  if (length(species)  == 1L) species  <- rep(species,  n)
  if (length(quality)  == 1L) quality  <- rep(quality,  n)

  if (is.null(dbh)) dbh <- rep(NA_real_, n)
  if (is.null(h))   h   <- rep(NA_real_, n)
  if (is.null(dnm)) dnm <- rep(NA_real_, n)
  if (length(dbh) == 1L) dbh <- rep(dbh, n)
  if (length(h)   == 1L) h   <- rep(h,   n)
  if (length(dnm) == 1L) dnm <- rep(dnm, n)

  if (!all(lengths(list(province, species, dbh, h, dnm, quality)) == n))
    cli::cli_abort("All inputs must have the same length or be scalar.")

  # 1. Per-row calculation -------------------------------------------------------
  results <- vector("list", n)

  for (i in seq_len(n)) {
    prov_id  <- .snfi_province_id(province[i])
    all_pars <- .snfi_load_pars(prov_id)
    version  <- attr(all_pars, "snfi_version")

    pars_sp <- .snfi_filter_pars(all_pars, species[i], quality[i])

    dbh_mm <- dbh[i] * 10   # cm -> mm
    dnm_mm <- dnm[i] * 10   # cm -> mm

    p_vcc  <- pars_sp[!is.na(pars_sp$Parametro) & pars_sp$Parametro == "VCC",  ]
    p_vsc  <- pars_sp[!is.na(pars_sp$Parametro) & pars_sp$Parametro == "VSC",  ]
    p_iavc <- pars_sp[!is.na(pars_sp$Parametro) & pars_sp$Parametro == "IAVC", ]
    p_vle  <- pars_sp[!is.na(pars_sp$Parametro) & pars_sp$Parametro == "VLE",  ]

    vcc  <- .snfi_vcc(dbh_mm, h[i], p_vcc)
    vsc  <- .snfi_vsc(vcc, p_vsc)
    iavc <- .snfi_iavc(dbh_mm, dnm_mm, h[i], vcc, p_iavc)
    vle  <- .snfi_vle(dbh_mm, vcc, p_vle)

    results[[i]] <- data.frame(
      vcc          = vcc,
      vsc          = vsc,
      iavc         = iavc,
      vle          = vle,
      snfi_version = version,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, results)

  # 2. Citation message ----------------------------------------------------------
  if (!quiet) {
    versions_used <- unique(out$snfi_version)
    if ("SNFI4" %in% versions_used)
      cli::cli_alert_info(
        "SNFI4 data used. Cite: MITECO SNFI4 SIG codes - {.url https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf}"
      )
    if ("SNFI3" %in% versions_used)
      cli::cli_alert_info(
        "SNFI3 data used. Cite: MITECO SNFI3 SIG codes - {.url https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf}"
      )
  }

  out
}


#' List provinces supported by SNFI volume equations
#'
#' Returns a data frame with province codes, names, and which SNFI dataset
#' (SNFI4 or SNFI3 fallback) is used for each.
#'
#' @return A `data.frame` with columns `province_id`, `province_name`, and
#'   `snfi_version`.
#' @export
#'
#' @examples
#' silv_snfi_provinces()
silv_snfi_provinces <- function() {
  prov_file <- system.file("volume", "INE_provinces", "ine_provinces.csv",
                           package = "silviculture")
  prov_df <- utils::read.csv(prov_file, stringsAsFactors = FALSE)
  prov_df <- unique(prov_df[, c("province_id", "province_name")])

  snfi4_ids <- unique(silviculture::snfi4_volume_coefficients$Codigo_provincia)

  prov_df$snfi_version <- ifelse(
    prov_df$province_id %in% snfi4_ids, "SNFI4", "SNFI3"
  )
  prov_df[, c("province_id", "province_name", "snfi_version")]
}


#' List species supported by SNFI volume equations
#'
#' Returns a data frame of species codes and names from the SNFI4 species
#' catalogue.
#'
#' @param snfi_version Character. Either `"SNFI4"` (default) or `"SNFI3"`.
#'
#' @return A `data.frame` with columns `species_code` and `species_name`.
#' @export
#'
#' @examples
#' head(silv_snfi_species())
#' head(silv_snfi_species("SNFI3"))
silv_snfi_species <- function(snfi_version = "SNFI4") {
  snfi_version <- match.arg(snfi_version, c("SNFI4", "SNFI3"))

  if (snfi_version == "SNFI4") {
    f <- system.file("volume", "SNFI4_species", "SNFI4_species_codes.csv",
                     package = "silviculture")
    df <- utils::read.csv(f, stringsAsFactors = FALSE)
    data.frame(
      species_code = df$Codigo_IFN,
      species_name = df$Nombre_Especie,
      stringsAsFactors = FALSE
    )
  } else {
    f <- system.file("volume", "SNFI3_species_codes.csv",
                     package = "silviculture")
    df <- utils::read.csv(f, stringsAsFactors = FALSE)
    data.frame(
      species_code = df$Codigo_IFN,
      species_name = df$Nombre_Especie,
      stringsAsFactors = FALSE
    )
  }
}
