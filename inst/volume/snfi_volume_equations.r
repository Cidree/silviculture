# considering parameter using the same name as in the documentation

# documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf
# see anexo 19, page 66

# dbh_mm is the tree dbh in mm
# dnm_mm is the mean plot dbh in mm
# h_m is the tree height in m


# VCC: Volumen maderable con corteza, en dm3

get_snfi_vcc <- function(dbh_mm, h_m, pars) {
  
  # check if pars available
  if(nrow(pars) == 0){
    return(NA)
  }
  
  if(pars$Modelo == 1){
    # model 1
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    vcc <- a + b*dbh_mm^2*h_m
  } else if(pars$Modelo == 11){
    # model 11
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    r <- as.numeric(pars$r)
    vcc <- p * dbh_mm^q * h_m^r
  } else {
    print("Model not recognized for VCC.")
    vcc <- NA
  }

  return(vcc)
}


# VSC: Volumen maderable sin corteza, en dm3

get_snfi_vsc <- function(vcc, pars) {
  
  # check if pars available
  if(nrow(pars) == 0){
    return(NA)
  }
  
  if(pars$Modelo == 7){
    # model 7
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    vsc <- a + b*vcc + c*vcc^2
  } else {
    print("Model not recognized for VSC.")
    vsc <- NA
  }

  return(vsc)
}


# IAVC: Incremento anual de volumen con corteza, en dm3

get_snfi_iavc <- function(dbh_mm = NULL, dnm_mm = NULL, h_t = NULL, vcc = NULL, pars) {

  # check if pars available
  if(nrow(pars) == 0){
    return(NA)
  }
  
  if(pars$Modelo == 8){
    # check vcc is provided
    if(is.null(vcc)){
      print("vcc must be provided for model 8.")
      return(NA)
    }
    # model 8
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    iavc <- a + b*vcc + c*vcc^2    
  } else if(pars$Modelo == 13){
    # check dbh_mm and dnm_mm are provided
    if(is.null(dbh_mm) || is.null(dnm_mm)){
      print("dbh_mm and dnm_mm must be provided for model 13.")
      return(NA)
    }
    # model 13
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    iavc <- a + b*(dbh_mm + dnm_mm)
  } else if(pars$Modelo == 14){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 14.")
      return(NA)
    }
    # model 14
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    iavc <- p*dbh_mm^q
  } else if(pars$Modelo == 16){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 16.")
      return(NA)
    }
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    # model 16
    iavc <- a + b*dbh_mm^2
  } else if(pars$Modelo == 17){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 17.")
      return(NA)
    }
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    # model 17
    iavc <- a + b*dbh_mm + c*dbh_mm^2
  } else if(pars$Modelo == 18){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 18.")
      return(NA)
    }
    # model 18 (IFN3)
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    iavc <- p * exp(q*dbh_mm)
  } else if(pars$Modelo == 19){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 19.")
      return(NA)
    }
    # model 19
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    d <- as.numeric(pars$d)
    iavc <- a + b*dbh_mm + c*dbh_mm^2 + d*dbh_mm^3
  } else if(pars$Modelo == 20){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 20.")
      return(NA)
    }
    # model 20
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    d <- as.numeric(pars$d)
    iavc <- a + b*dbh_mm + d*dbh_mm^3
  } else if(pars$Modelo == 21){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 21.")
      return(NA)
    }
    # model 21
    c <- as.numeric(pars$c)
    d <- as.numeric(pars$d)
    iavc <- c*dbh_mm^2 + d*dbh_mm^3
  } else if(pars$Modelo == 25){
    # check dbh_mm and h_t are provided
    if(is.null(dbh_mm) || is.null(h_t)){
      print("dbh_mm and h_t must be provided for model 25.")
      return(NA)
    }
    # model 25
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    r <- as.numeric(pars$r)
    iavc <- p*dbh_mm^q*h_t^r
  } else {
    print("Model not recognized for IAVC.")
    iavc <- NA
  }

  # model 15 (IFN3)
  # iavc <- a + b*(cd - cdm)

  return(iavc)
}


# VLE: Volumen de leñas gruesas, en dm3

get_snfi_vle <- function(dbh_mm = NULL, vcc = NULL, pars) {
  
  # check if pars available
  if(nrow(pars) == 0){
    return(NA)
  }
  
  if(pars$Modelo == 10){
    # check vcc is provided
    if(is.null(vcc)){
      print("vcc must be provided for model 10.")
      return(NA)
    }
    # model 10
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    vle <- a + b*vcc + c*vcc^2
  } else if(pars$Modelo == 12){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 12.")
      return(NA)
    }
    # model 12
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    vle <- p*dbh_mm^q
  } else {
    print("Model not recognized for VLE.")
    vle <- NA
  }

  return(vle)
}
