% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_regional_building_permits.R
\name{get_regional_building_permits}
\alias{get_regional_building_permits}
\title{New construction permits for Hungarian regions and counties}
\source{
\url{https://www.ksh.hu/docs/eng/xstadat/xstadat_annual/i_zrp002b.html}
}
\usage{
get_regional_building_permits(directory = NULL, region_level = NULL)
}
\arguments{
\item{directory}{Defaults to \code{NULL}.}

\item{region_level}{Any of \code{megye}, \code{county},
\code{régió}, \code{region}, \code{nagyrégió}
or \code{NULL} for all data units.}
}
\description{
6.2.2.3. Issued new construction permits (2003–)
}
\examples{
\dontrun{
get_regional_building_permits (region_level = "county")
}
}
\keyword{hungary,}
\keyword{ksh,}
\keyword{opengov,}
\keyword{openstatistics}
