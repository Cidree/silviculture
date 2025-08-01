.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}

utils::globalVariables(
  c(
    ":=",
    "biomass_models",
    "cumulative_before",
    "d",
    "dclass",
    "dg",
    "g_ha",
    "h",
    "h0",
    "n",
    "nt",
    "ntrees",
    "ntrees_ha",
    "remaining_to_extract",
    "weighted.mean",
    ".cumtrees",
    ".data",
    ".do",
    ".nmax"
  )
)