#' Place names and locations in Australia
#'
#' For convenience, does not include Lord Howe Island and external territories.
#'
#' @format An sf data frame with 4 variables:
#' * `fclass`: Classification of place.
#' * `name`: Name of place.
#' * `state`: Name of the nearest state.
#' * `geometry`: sf geometry with coordinates.
#' 
#' @source OpenStreetMap via Geofabrik \url{https://download.geofabrik.de/}
#' @name places
NULL

#' @rdname places
"places_in_australia"
