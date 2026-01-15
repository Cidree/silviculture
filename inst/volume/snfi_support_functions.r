
silv_predict_snfi_volume <- function(province, species, dbh = NULL, h = NULL, dnm = NULL, quality = "default") {
  
  output_list <- vector("list", length(province))
  
  for(i in seq_along(province)) {
    cat("Calculating volumes for province", province[i], "species", species[i], "\n")
    
    # get and filter parameters
    pars <- get_snfi_pars(province = province[i])
    pars <- filter_pars(pars = pars,
                        species = species[i],
                        quality = quality[i])
    
    # at this point, pars should have only one row per variable: VCC, VSC, VLE, IAVC
    vcc <- get_snfi_vcc(dbh_mm = dbh[i] * 10,  # BE CAREFUL WITH THE UNITS!
                        h_m = h[i], 
                        pars = pars[pars$Parametro == "VCC", ])
    vsc <- get_snfi_vsc(vcc = vcc[i],
                        pars = pars[pars$Parametro == "VSC", ])
    iavc <- get_snfi_iavc(dbh_mm = dbh[i] * 10,  # BE CAREFUL WITH THE UNITS!
                          dnm_mm = dnm[i],   # BE CAREFUL WITH THE UNITS!
                          h_t = h[i], 
                          vcc = vcc[i],
                          pars = pars[pars$Parametro == "IAVC", ])
    vle <- get_snfi_vle(dbh_mm = dbh[i] * 10,  # BE CAREFUL WITH THE UNITS!
                        vcc = vcc[i],
                        pars = pars[pars$Parametro == "VLE", ])
    
    # store results
    output_list[[i]] <- data.frame(
      vcc = vcc,
      vsc = vsc,
      iavc = iavc,
      vle = vle
    )
  }
  
  # results into a single data frame
  output_df <- do.call(rbind, output_list)
  
  # reccomendation message
  cat("\n")
  cat("Volume estimates have been calculated. \n")
  cat("Note: You can cite this models as follows: \n")
  cat("MITECO, n.d. CUARTO INVENTARIO FORESTAL NACIONAL. Descripción de los códigos de la base de datos Sig. URL: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf \n")
  cat("MITECO, n.d. 3er INVENTARIO FORESTAL NACIONAL. DESCRIPCIÓN DE LOS CÓDIGOS DE LA BASE DE DATOS DE SIG. URL: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf \n")
  
  return(output_df)
}


get_snfi_pars <- function(province) {
  
  # check if province is numeric or character
  is_num <- !is.na(suppressWarnings(as.numeric(province)))
  if(!is_num){
    df_provinces <- read.csv("ine_regions_provinces.csv") 
    df_provinces <- df_provinces[df_provinces$province_name == province, ]
    province <- df_provinces$province_id  # get province code
  }
    
  # choose parameters file
  pars_file_snfi4 <- paste("snfi4/SNFI4_all_volume_coefficients_", province, ".csv", sep = '')
  pars_file_snfi3 <- paste("snfi3/SNFI3_all_volume_coefficients_", province, ".csv", sep = '')
  
  if (file.exists(pars_file_snfi4)) {
    pars <- read.csv(pars_file_snfi4)
  } else if (file.exists(pars_file_snfi3)) {
    pars <- read.csv(pars_file_snfi3)
  } else {
    stop("Parameters file not found.")
  }

  return(pars)
}


filter_pars <- function(pars, species, quality) {
  
  # filter data
  if(!is.na(suppressWarnings(as.numeric(species)))){
    pars <- pars[pars$Codigo_especie == species, ]
  } else {
    pars <- pars[pars$Especie == species, ]
  }
  if(nrow(pars) == 0){
    stop(paste("No parameters found for the specified species:", species))
  }
  
  # shape/quality
  # pars <- pars[pars$F.c. == quality, ]
  
  if (quality == "default") {
    quality <- 1:6
    
    for (q in quality) {
      pars_q <- pars[pars[["F.c."]] == q, ]
      
      if (nrow(pars_q) > 0) {
        quality <- q
        pars <- pars_q
        break
      }
    }
  } else if(quality %in% 1:6) {
    pars <- pars[pars[["F.c."]] == quality, ]
  } else {
    stop("Quality must be 'default' or a numeric value between 1 to 6.")
  }
  if(nrow(pars) == 0){
    stop(paste("No parameters found for the specified quality:", quality))
  }
  
  return(pars)
}
