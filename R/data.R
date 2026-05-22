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
