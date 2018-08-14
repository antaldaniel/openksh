#' Get Real Income (2.1.53i)
#' Get the Stadat 2.1.53 statistics
#' @param directory Defaults to \code{NULL}.
#' @param filename \code{2_1_53i.xls}.
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
                             filename = "2_1_53i.xls") {
  . <- NULL

  if (! is.null(directory) )  filename <- file.path( directory, filename )

  if (! file.exists(filename )) {
    download_stadat_file("2_1_53i")
    filename <- file.path("ksh_data", filename)
  }

  tmp <- readxl::read_excel(filename,
                            sheet = 1,
                            skip = 3) %>%
    purrr::set_names (., c( "years", "gross", "net", "gross_index", "net_index", "cpi", "real_gross_index",
                            "real_net_index" )
    )
  ts ( tmp[,-1], frequency = 1, start = 1992)
}
