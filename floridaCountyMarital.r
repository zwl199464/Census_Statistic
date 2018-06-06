#' Large SpatialPolygonDataFrame with marital data on all the counties in Florida
#'
#' An acs object  It can be downloaded using the ACS package.  After getting an
#' API key run this line \code{
#'acs.fetch(endyear = 2016,
#'          geography = geo.make(state = "FL", county = "*"),
#'          variable = acs.lookup(endyear = 2015,
#'                                table.number = "B12002",
#'                                dataset = "acs"),
#'          dataset = "acs",
#'          col.names = "pretty")}.
#'
#' @usage data(floridaCountyMarital)
#'
#' @format Large acs:
#' \describe{
#'   \item{@@endyear}{2016}
#'   \item{@@span}{5}
#'   \item{@@geography}{data.frame with 67 observations:}
#'     \itemize{
#'       \item{NAME: }{}
#'       \item{state: }{12}
#'       \item{county: }{three character county code}
#'     }
#'   \item{@@acs.colnames}{}
#'   \item{@@modified}{}
#'   \item{@@acs.units}{}
#'   \item{@@currency.year}{}
#'   \item{@@estimate}{list with two items:}
#'     \itemize{
#'     \item{1}{string holding county names}
#'     \item{2}{three strings holding variable names}
#'     }
#'  \item{@@standard.error}{Full Full Geocode for tracts}
#'     \itemize{
#'     \item{1}{string holding county names}
#'     \item{2}{three strings holding variable names}
#'     }
#'   }
#'
#' @source {Data downloaded ACS package}
"floridaCountyMarital"
