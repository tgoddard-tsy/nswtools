#' Common magnitude constants for scaling
#'
#' These constants are provided to improve readability and reduce errors when working with large numeric values,
#' especially in financial, economic, or demographic data. They are intended to be used in calculations, plots,
#' and summaries where scaling by powers of ten is common.
#'
#' @format Numeric constants:
#' \describe{
#'   \item{`thousand`}{Equivalent to 1,000}
#'   \item{`million`}{Equivalent to 1,000,000}
#'   \item{`billion`}{Equivalent to 1,000,000,000}
#'   \item{`trillion`}{Equivalent to 1,000,000,000,000}
#' }
#'
#' @examples
#' # Write 100 billion as numeric:
#' 100 * billion
#' 
#' # Scale a value to millions
#' value <- 2500000
#' value_mil <- value / million
#'
#' # Use in ggplot axis labels
#' scale_y_continuous(labels = ~ . / million)
#'
#' @keywords constants, scaling, readability
#' @name constants
NULL

#' @rdname constants
#' @export
thousand <- 10^3

#' @rdname constants
#' @export
million <- 10^6

#' @rdname constants
#' @export
billion <- 10^9

#' @rdname constants
#' @export
trillion <- 10^12
