#' Get resident population by age-group, for Hungarian regions and counties
#'
#' 6.1.2. Resident population by age-group, 1 January (2001–)
#' @param directory Defaults to \code{NULL}.
#' @param region_level Any of \code{megye}, \code{county},
#' \code{régió}, \code{region}, \code{nagyrégió}, \code{large_region},
#' \code{national}, \code{ország}, or \code{NULL} for all data units.
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @importFrom dplyr mutate filter select left_join
#' @importFrom tidyr gather spread fill
#' @keywords ksh, hungary, opengov, openstatistics
#' @source \url{https://www.ksh.hu/docs/eng/xstadat/xstadat_annual/i_wdsd004b.html}
#' @examples
#'\dontrun{
#' get_regional_demography_age_group(region_level = "county")
#' }
#' @export

get_regional_demography_age_group <- function( directory = NULL,
                                        region_level = NULL) {
  . <- NULL

  message ("population_0_14, etc unit: natural units, population_rate_0_14 etc percent.")
  stadat_name <- "6_1_2i"; filename <- paste0(stadat_name, ".xls")

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
    purrr::set_names (., c( "name", "level", "years", "values")) %>%
    dplyr::mutate ( years = as.numeric(substr (as.character(years), 1,4))) %>%
    dplyr::filter (name != "neve") %>%
    dplyr::mutate ( var= dplyr::case_when(
      grepl("0–14", name) ~ "population_0_14",
      grepl("15–64", name) ~ "population_15_64",
      grepl("65–X", name) ~ "population_65p")) %>%
    tidyr::fill (var) %>%
    dplyr::filter (!is.na(values)) %>%
    dplyr::mutate ( values = gsub("\\.\\.", NA, values)) %>%
    dplyr::mutate ( values = gsub("\\–", "0", values)) %>%
    dplyr::mutate ( values = as.numeric(as.character(values)))  %>%
    tidyr::spread ( var, values ) %>%
    dplyr::mutate ( population = population_0_14 + population_15_64 + population_65p) %>%
    dplyr::mutate ( population_rate_0_14 = population_0_14 / population,
                    population_rate_15_64 = population_15_64 / population,
                    population_rate_65p = population_65p / population )

  if (is.null(region_level)) return (tmp)

  if ( region_level %in% c("régió", "region")) {
    tmp <- dplyr::filter ( tmp, grepl("régió", level )) %>%
      dplyr::filter ( level != "nagyrégió")
  }

  if ( region_level %in% c("nagyrégió", "large_region")) {
    tmp <- dplyr::filter ( tmp, grepl("nagyrégió", level ))
  }

  if ( region_level %in% c("national", "ország")) {
    tmp <- dplyr::filter ( tmp, grepl("orsz", level ))
  }

  if ( region_level %in% c("megye", "county")) {
    megye <- dplyr::select ( openksh:::megye_map, name, isocode )
    gyms  <- megye$name[which ( megye$isocode == "HU-GS") ]

    tmp$name <- ifelse ( grepl ( "Moson-", tmp$name ), gyms, tmp$name )

    tmp <- dplyr::left_join( megye, tmp, by = "name")

  }

  tmp
}
