#' New construction permits for Hungarian regions and counties
#'
#' 6.2.2.3. Issued new construction permits (2003–)
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
#' @source \url{https://www.ksh.hu/docs/eng/xstadat/xstadat_annual/i_zrp002b.html}
#' @examples
#'\dontrun{
#' get_regional_building_permits (region_level = "county")
#' }
#' @export

get_regional_building_permits <- function(
                              directory = NULL,
                              region_level = NULL) {
  . <- NULL

  message ("Units: natural unit, square meter, for non-residential floor space thousand square meters")
  stadat_name <- "6_2_2_3i"; filename <- paste0(stadat_name, ".xls")

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
    dplyr::mutate ( years = as.numeric(substr (as.character(years), 1,4))) %>%
    purrr::set_names (x =.,  nm = c( "name", "level", "years", "values") ) %>%
    dplyr::filter (name != "neve") %>%
    dplyr::mutate ( var= dplyr::case_when(
      grepl("\\$A lakóépületek száma", name) ~ "residential",
      grepl("nem lakóépületek száma", name) ~ "non_residential",
      grepl("nem lakóépületek alapterülete, ezer m2", name) ~ "non_residential_floor_space",
      grepl("l az egylakásos", name ) ~ "one_unit_building",
      grepl("egylakásos lakóépületek alapterülete", name ) ~ "avg_single_unit_floor_space",
      grepl("A lakóépületek alapterülete", name ) ~ "avg_residential_floor_space"
      )) %>%
    tidyr::fill (var) %>%
    dplyr::filter (!is.na(values)) %>%
    dplyr::mutate ( values = gsub("\\–", "0", values)) %>%
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
