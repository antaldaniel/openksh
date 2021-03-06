#' Average monthly net earnings of full-time employees by the geographic area of Hungary
#'
#' 6.2.1.16. Average monthly net earnings of full-time employees by the geographic area of the local unit (2000–)
#' @param directory Defaults to \code{NULL}.
#' @param region_level Any of \code{megye}, \code{county},
#' \code{régió}, \code{region}, \code{nagyrégió}
#' or \code{NULL} for all data units.
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @importFrom dplyr mutate filter select left_join
#' @importFrom tidyr gather spread fill
#' @keywords ksh, hungary, opengov, openstatistics
#' @source \url{https://www.ksh.hu/docs/eng/xstadat/xstadat_annual/i_qli050b.html}
#' @examples
#'\dontrun{
#' get_regional_net_earnings (region_level = "region")
#' }
#' @export

get_regional_net_earnings <- function(
                              directory = NULL,
                              region_level = NULL) {
  . <- NULL

  message ("Unit: natural unit, square meter, for non-residential floor space thousand square meters")
  stadat_name <- "6_2_1_16i"; filename <- paste0(stadat_name, ".xls")
  message ("avg_net_earnings unit: HUF, avg_net_earnings_index y/y percent")

  if (! is.null(directory) ) {
    if ( check_directory (directory) ) {
      filename <- file.path( directory, filename )
    } else {
      stop ( "The specified directory does not exist.")
    }
  } else ( directory == "" )

  if (! file.exists(filename )) {
    download_stadat_file(stadat_name, directory)
    message ( filename, " downloaded and saved.")
  }

  tmp <- readxl::read_excel(filename,
                            sheet = 1,
                            skip = 1) %>%
    tidyr::gather ( years, values, !!2:ncol(.) ) %>%
    dplyr::mutate ( years = as.numeric(substr (as.character(years), 1,4))) %>%
    purrr::set_names (x =.,  nm = c( "name", "years", "values") ) %>%
    dplyr::filter (name != "neve") %>%
    dplyr::mutate ( var= dplyr::case_when(
      grepl("\\$Nettó átlagkereset", name) ~ "avg_net_earnings",
      grepl(" év = 100,0%", name) ~ "avg_net_earnings_index"
         )) %>%
    tidyr::fill (var) %>%
    dplyr::filter (!is.na(values)) %>%
    dplyr::mutate ( values = gsub("\\.\\.", NA, values)) %>%
    dplyr::mutate ( values = as.numeric(as.character(values)))  %>%
    tidyr::spread ( var, values )

  if (is.null(region_level)) return (tmp)

  if ( region_level %in% c("régió", "region")) {
    tmp <- dplyr::filter ( tmp, grepl("régió", level )) %>%
      dplyr::filter ( level != "nagyrégió")
  }

  if ( region_level == "nagyrégió") {
    tmp <- dplyr::filter ( tmp, grepl("nagyrégió", level ))
  }

  if ( region_level %in% c("megye", "county")) {
    megye <- dplyr::select ( openksh:::megye_map, name, isocode )
    gyms  <- megye$name[which ( megye$isocode == "HU-GS") ]

    tmp$name <- ifelse ( grepl ( "Moson-", tmp$name ), gyms, tmp$name )

    tmp <- dplyr::left_join( megye, tmp, by = "name")

  }

  tmp
}
