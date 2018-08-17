#' Get Real Income (2.1.53i)
#' Get the Stadat 2.1.53 statistics
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

get_real_income <- function( directory = NULL,
                             class = NULL) {
  . <- NULL

  stadat_name <- "2_1_53i"; filename <- paste0(stadat_name, ".xls")

  if (! is.null(directory) ) {
    if ( check_directory (directory) ) {
      filename <- file.path( directory, filename )
    } else {
      stop ( "The specified directory does not exist.")
    }
  } else ( directory == "" )

  if (! file.exists(filename )) {
    download_stadat_file("2_1_53i", directory)
    message ( filename, " downloaded and saved.")
  }

  tmp <- readxl::read_excel(filename,
                            sheet = 1,
                            skip = 3) %>%
    purrr::set_names (., c( "years", "gross", "net", "gross_index", "net_index", "cpi", "real_gross_index",
                            "real_net_index" )
    )

  if ( class == "ts")   return(ts ( tmp[,-1], frequency = 1, start = 1992))

  tmp
}
