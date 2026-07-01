#' Forest inventory samples
#'
#' Inventory data from Spanish National Forest Inventory
#'
#' @format A `tibble`
"inventory_samples"


#' Biomass models
#'
#' Biomass models available in \code{silviculture} package. If you would like
#' to suggest new models, please open a new issue.
#'
#' @format A `tibble`
"biomass_models"


#' Carbon content models
#'
#' Carbon content percentages per biomass component available in the
#' \code{silviculture} package, compiled from Díeguez-Aranda et al. (2009)
#' and Montero et al. (2005). If you would like to suggest new models, please
#' open a new issue.
#'
#' @format A `tibble` with 264 rows and 13 variables:
#' \describe{
#'   \item{article_id}{Character. Short identifier of the source article
#'     (e.g. \code{"montero-2005"}, \code{"dieguez-aranda-2009"}).}
#'   \item{title}{Character. Full title of the source article.}
#'   \item{doi_url}{Character. DOI URL of the source article.}
#'   \item{country}{Character. Country where the study was conducted.}
#'   \item{region}{Character. Region within the country.}
#'   \item{species}{Character. Scientific name of the tree species.}
#'   \item{biomass_group}{Character. Biomass group (\code{"AGB"} or
#'     \code{"BGB"}).}
#'   \item{tree_group}{Character. Sub-group within the biomass group.}
#'   \item{tree_component}{Character. Tree component (e.g. \code{"stem"},
#'     \code{"bark"}, \code{"thick branches"}, \code{"small branches"},
#'     \code{"twigs"}, \code{"leaves"}, \code{"needles"}, \code{"roots"}).}
#'   \item{carbon_percentage}{Numeric. Carbon content as a percentage (0-100).
#'     Multiply by the component biomass and divide by 100 to obtain carbon
#'     mass in the same units. \code{NA} where not reported in the source.}
#'   \item{r2}{Numeric. Coefficient of determination of the original model,
#'     if reported.}
#'   \item{rmse}{Numeric. Root mean square error of the original model, if
#'     reported.}
#'   \item{obs}{Character. Additional observations from the source article.}
#' }
#'
#' @references
#' Díeguez-Aranda, U., Rojo Alboreca, A., Castedo-Dorado, F., Álvarez
#' González, J.G., Barrio-Anta, M., Crecente-Campo, F., González González,
#' J.M., Pérez-Cruzado, C., Rodríguez Soalleiro, R., López-Sánchez, C.A.,
#' Balboa-Murias, M.A., Gorgoso Varela, J.J., Sánchez Rodríguez, F. (2009).
#' \emph{Herramientas selvícolas para la gestión forestal sostenible en
#' Galicia}. Xunta de Galicia.
#'
#' Montero, G., Ruiz-Peinado, R., Muñoz, M. (2005).
#' \emph{Producción de biomasa y fijación de CO2 por los bosques españoles}.
#' Monografías INIA: Serie Forestal n.º 13. Instituto Nacional de
#' Investigación y Tecnología Agraria y Alimentaria, Madrid.
"carbon_models"


#' SNFI3 tree volume coefficients
#'
#' Allometric equation coefficients for tree volume and increment from the 3rd
#' Spanish National Forest Inventory (SNFI3/IFN3), as documented by MITECO.
#'
#' @format A `tibble` with 37,730 rows and 16 variables:
#' \describe{
#'   \item{Codigo_provincia}{Integer. Province numeric code.}
#'   \item{Nombre_provincia}{Character. Province name.}
#'   \item{Codigo_especie}{Integer. Species numeric code.}
#'   \item{Especie}{Character. Species scientific name.}
#'   \item{Parametro}{Character. Volume component: VCC (merchantable volume with bark), VSC (merchantable volume without bark), IAVC (annual volume increment with bark), or VLE (coarse firewood volume).}
#'   \item{F.c.}{Integer. Tree quality class (calidad del árbol), from 1 (healthy and straight tree) to 6 (dead tree).}
#'   \item{Par_esp}{Character. Special parameter metadata.}
#'   \item{Modelo}{Integer. Equation model index.}
#'   \item{a, b, c, d, p, q, r}{Numeric. Equation coefficients (NA where not applicable).}
#'   \item{R2}{Numeric. Coefficient of determination of the model.}
#' }
#' @references
#' MITECO. 3rd Spanish National Forest Inventory - SIG database codes.
#' \url{https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf}
"snfi3_volume_coefficients"


#' SNFI4 tree volume coefficients
#'
#' Allometric equation coefficients for tree volume and increment from the 4th
#' Spanish National Forest Inventory (SNFI4/IFN4), as documented by MITECO.
#'
#' @format A `tibble` with 20,925 rows and 15 variables:
#' \describe{
#'   \item{Codigo_provincia}{Integer. Province numeric code.}
#'   \item{Nombre_provincia}{Character. Province name.}
#'   \item{Codigo_especie}{Integer. Species numeric code.}
#'   \item{Especie}{Character. Species scientific name.}
#'   \item{Parametro}{Character. Volume component: VCC (merchantable volume with bark), VSC (merchantable volume without bark), IAVC (annual volume increment with bark), or VLE (coarse firewood volume).}
#'   \item{F.c.}{Integer. Tree quality class (calidad del árbol), from 1 (healthy and straight tree) to 6 (dead tree).}
#'   \item{Modelo}{Integer. Equation model index.}
#'   \item{a, b, c, d, p, q, r}{Numeric. Equation coefficients (NA where not applicable).}
#'   \item{D.n.m.}{Character. Mean plot diameter metadata (diámetro normal medio), required for IAVC equation model 13.}
#' }
#' @references
#' MITECO. 4th Spanish National Forest Inventory - SIG database codes.
#' \url{https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf}
"snfi4_volume_coefficients"


#' SDI beta coefficients
#'
#' Specific beta coefficients for Reineke's Stand Density Index (SDI) per
#' species, country, and region.
#'
#' @format A `tibble`
#' \describe{
#'   \item{article_id}{Character. Short identifier of the source article.}
#'   \item{title}{Character. Full title of the source article.}
#'   \item{doi_url}{Character. DOI URL of the source article.}
#'   \item{country}{Character. Country where the study was conducted.}
#'   \item{region}{Character. Region within the country.}
#'   \item{species}{Character. Scientific name of the tree species.}
#'   \item{beta}{Numeric. Beta coefficient for SDI calculation.}
#' }
"sdi_coefficients"


#' Maximum stand density index (SDImax) models
#'
#' Coefficients for calculating maximum stand density index (SDImax) from
#' Rodríguez de Prado (2020).
#'
#' @format A `tibble` with 88 rows and 13 variables:
#' \describe{
#'   \item{article_id}{Character. Identifier of the article.}
#'   \item{title}{Character. Title of the article.}
#'   \item{doi_url}{Character. DOI URL of the article.}
#'   \item{country}{Character. Country where the study was conducted.}
#'   \item{species}{Character. Tree species scientific name.}
#'   \item{model_name}{Character. Name of the model/equation variant (e.g. "basic", "P1", "MXT3").}
#'   \item{a0}{Numeric. Coeffient a0.}
#'   \item{a1}{Numeric. Coeffient a1 (0 if not used/applicable).}
#'   \item{b0}{Numeric. Coeffient b0.}
#'   \item{b1}{Numeric. Coeffient b1 (0 if not used/applicable).}
#'   \item{aic}{Numeric. Akaike Information Criterion.}
#'   \item{pseudo_r2}{Numeric. Pseudo R-squared value.}
#'   \item{q_index}{Numeric. Q index value.}
#' }
#' @references
#' Rodríguez-de-Prado, M., et al. (2020). Potential climatic influence on maximum stand carrying capacity for 15 Mediterranean coniferous and broadleaf species. Forest Ecology and Management, 458, 117824.
"sdimax_models"
