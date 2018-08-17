#' Create county map
#'
#' @param df The data.frame with the values to map. The data must be in long-form
#' and contain only the values of a single point in time.
#' @param megyeid The name of the Hungarian counties, defaults to \code{megye}.
#' @param values  The name of numeric values, defaults to \code{values}.
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate left_join filter
#' @importFrom ggplot2 ggplot aes geom_sf theme theme_light scale_fill_gradient2
#' @importFrom ggplot2 element_blank
#' @keywords hungary, mapping, county, megye, choropleth
#' @examples
#'\dontrun{
#' create_megye_map (df)
#' }
#' @export

create_megye_map <- function ( df, megyeid = "name",
                              values_var = "values") {

  megye_col  <- which ( names(df) == megyeid )
  values_col <- which ( names(df) == values_var )

  if ( length ( megye_col) == 0) stop  ( "No megye column found.")
  if ( length ( values_col) == 0) stop ( "No values column found.")

  map_data <- data.frame (
    name =   as.character(df[ , megye_col]),
    values = as.numeric(df[, values_col])
  )
  megye_map <- dplyr::mutate ( megye_map, name = as.character (name) )
  map_data <- dplyr::left_join ( map_data, megye_map, by = "name" )

  ggplot2::ggplot ( map_data ) +
    ggplot2::geom_sf ( ggplot2::aes ( fill = values ),
                       show.legend = FALSE,
                       lwd = 0,
                       color="white" ) +
    ggplot2::theme ( legend.position = "bottom",
                     axis.text = ggplot2::element_blank() ) +
    ggplot2::scale_fill_gradient2(low = "grey80",
                                  midpoint = median(map_data$values, na.rm=TRUE),
                                  high = '#DB001C')
}




