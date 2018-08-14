#' Check if there is a directory to use
#'
#' @param directory Defaults to \code{NULL}. In this case the \code{ksh_data}
#' directory is used, if exists, or it is created.
#' @examples
#'\dontrun{
#' check_directory ( directory = NULL )
#' }

check_directory <- function (directory = NULL) {

  if ( is.null (directory)) {
    directory <- "ksh_data"
  }
  if (! file.exists(directory)){
    dir.create(file.path(getwd(), directory))
  }
  message ( file.path(getwd(), directory), " created.")
}
