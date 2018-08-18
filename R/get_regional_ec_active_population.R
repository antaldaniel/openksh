#' Get economically activity population for Hungarian regions and counties
#'
#' 6.2.1.7. Economically active population (2000–)
#' @param directory Defaults to \code{NULL}.
#' @param region_level Any of \code{megye}, \code{county},
#' \code{régió}, \code{region}, \code{nagyrégió} or \code{NULL} for all
#' data units.
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @importFrom dplyr mutate filter select left_join
#' @importFrom tidyr gather spread fill
#' @keywords ksh, hungary, opengov, openstatistics
#' @source \url{https://www.ksh.hu/docs/eng/xstadat/xstadat_annual/i_qlf023b.html}
#' @examples
#'\dontrun{
#' get_regional_ec_active_population (region_level = "county")
#' }
#' @export

get_regional_ec_active_population <- function( directory = NULL,
                              region_level = NULL) {
  . <- NULL

  message ("active_population unit: ezer f\u0151 - thousand")
  stadat_name <- "6_2_1_7i"; filename <- paste0(stadat_name, ".xls")

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
    tidyr::gather ( years, values, !!3:ncol(.) ) %>%
    purrr::set_names (., c( "name", "level", "years", "active_population")) %>%
    dplyr::mutate ( years = as.numeric(substr (as.character(years), 1,4))) %>%
    dplyr::filter (name != "neve")

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
