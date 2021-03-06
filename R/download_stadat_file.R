#' Download a Stadat file
#'
#' @param directory Defaults to \code{NULL}. In this case the \code{ksh_data}
#' directory is used, if exists, or it is created.
#' @param filename Currently defaults to \code{2_1_53i}.
#' @importFrom utils download.file
#' @keywords ksh, hungary, opengov, openstatistics
#' @examples
#'\dontrun{
#' download_stadat_file ( "2_1_53i" )
#' }
#' @export

download_stadat_file  <- function ( filename = "2_1_53i",
                                    directory = NULL ) {

  if ( is.null (directory)) {
    destfile <-  paste0(filename, ".xls")
  } else {
    destfile <- file.path ( directory, paste0(filename, ".xls"))
  }
  url <- paste0("http://www.ksh.hu/docs/hun/xstadat/xstadat_eves/xls/", filename, ".xls")
  openksh:::check_directory( directory )



  utils::download.file(url, destfile, method = "auto",
                quiet = FALSE, mode = "wb",
                cacheOK = TRUE)

}
