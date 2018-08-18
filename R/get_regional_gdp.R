#' Get regional GDP data for Hungary
#'
#' 6.3.1.1. Gross domestic product (GDP) (2000–)
#' @param directory Defaults to \code{NULL}.
#' @param region_level Either \code{megye} or default \code{NULL} for all
#' data units.
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @importFrom stats ts
#' @keywords ksh, hungary, opengov, openstatistics
#' @source \url{https://www.ksh.hu/docs/eng/xstadat/xstadat_annual/i_qpt012b.html}
#' @examples
#'\dontrun{
#' ksh_real_income <- get_real_income ()
#' }
#' @export

get_regional_gdp <- function( directory = NULL,
                              region_level = NULL) {
  . <- NULL

  stadat_name <- "6_3_1_1i"; filename <- paste0(stadat_name, ".xls")
  message ("gdp unit: at purchasers'prices, million HUF")

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
    purrr::set_names (., c( "name", "years", "gdp"))

  if (is.null(region_level)) return(tmp)

  if ( region_level %in% c("megye", "county")) {
    megye <- dplyr::select ( openksh:::megye_map, name, isocode )
    gyms  <- megye$name[which ( megye$isocode == "HU-GS") ]

    tmp$name <- ifelse ( grepl ( "Moson-", tmp$name ), gyms, tmp$name )

    tmp <- dplyr::left_join( megye, tmp, by = "name")

  }

  tmp
}
