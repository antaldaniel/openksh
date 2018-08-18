#' Get theatre and cinema admissions for Hungarian regions and counties
#'
#' 6.2.6.2. Number of theatre and cinema admissions (2000–)
#' @param directory Defaults to \code{NULL}.
#' @param region_level Any of \code{megye}, \code{county},
#' \code{régió}, \code{region}, \code{nagyrégió}, \code{large_region},
#' or \code{NULL} for all data units.
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @importFrom dplyr mutate filter select left_join
#' @importFrom tidyr gather spread fill
#' @keywords ksh, hungary, opengov, openstatistics
#' @source \url{https://www.ksh.hu/docs/eng/xstadat/xstadat_annual/i_zkk004b.html}
#' @examples
#'\dontrun{
#' get_regional_theatre_cinema_admissions (region_level = "county")
#' }
#' @export

get_regional_theatre_cinema_admissions <- function( directory = NULL,
                                        region_level = NULL) {
  . <- isocode <- level <- values <- NULL

  message ("Unit: ezer / thousand")
  stadat_name <- "6_2_6_2i"; filename <- paste0(stadat_name, ".xls")

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
    purrr::set_names (., c( "name", "years", "values")) %>%
    dplyr::mutate ( years = as.numeric(substr (as.character(years), 1,4))) %>%
    dplyr::filter (name != "neve") %>%
    dplyr::mutate ( var= dplyr::case_when(
      grepl("\\$Sz", name) ~ "theatre_admissions",
      grepl("Mozi", name) ~ "cinema_admissions")) %>%
    tidyr::fill (var) %>%
    dplyr::filter (!is.na(values)) %>%
    dplyr::mutate ( values = gsub("\\.\\.", NA, values)) %>%
    dplyr::mutate ( values = gsub("\\–", "0", values)) %>%
    dplyr::mutate ( values = as.numeric(as.character(values)))  %>%
    tidyr::spread ( var, values )

  if (is.null(region_level)) return (tmp)

  if ( region_level %in% c("régió", "region")) {
    tmp <- dplyr::filter ( tmp, grepl("régió", level )) %>%
      dplyr::filter ( level != "nagyrégió")
  }

  if ( region_level %in% c("nagyrégió", "large_region")) {
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
