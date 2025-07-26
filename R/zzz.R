.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}

utils::globalVariables(
  c(
    "biomass_models",
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
    "weighted.mean",
    ".cumtrees",
    ".do",
    ".nmax"
  )
)