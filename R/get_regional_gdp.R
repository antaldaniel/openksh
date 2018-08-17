#' Get regional GDP data for Hungary
#'
#' Get the Stadat 6.3.1.1i statistics
#' @param directory Defaults to \code{NULL}.
#' @param class Defaults to \code{NULL}. If \code{ts} is specified, returns a
#'  time series object.
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @importFrom stats ts
#' @keywords ksh, hungary, opengov, openstatistics
#' @examples
#'\dontrun{
#' ksh_real_income <- get_real_income ()
#' }
#' @export

get_regional_gdp <- function( directory = NULL,
                              region_level = NULL,
                              class = NULL) {
  . <- NULL

  stadat_name <- "6_3_1_1i"; filename <- paste0(stadat_name, ".xls")

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
    purrr::set_names (., c( "name", "years", "values"))

  megye <- dplyr::select ( openksh:::megye_map, name, isocode )
  gyms  <- megye$name[which ( megye$isocode == "HU-GS") ]

  tmp$name <- ifelse ( grepl ( "Moson-", tmp$name ), gyms, tmp$name )

  tmp <- dplyr::left_join( megye, tmp, by = "name")

  tmp
}
