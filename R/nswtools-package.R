#' @section Theme functions:
#' \strong{Charts} - On loading the package, the default 'ggplot2' theme and colour scales are updated to \emph{NSW Treasury} defaults. These can also be summoned manually with \code{\link{theme_tsy}()} and \code{\link{scale_colour}_tsy()} functions.
#' 
#' \strong{Tables} - On loading the package, the default 'flextable' theme is updated to a \emph{NSW Treasury} default. This can also be summoned manually with \code{\link{theme_tsy_flextable}()}.
#' 
#' @section Data transformations:
#' Functions for time series transformations to "long" format dataframes (i.e. dataframes with columns "date", "variable" and "value").
#' 
#' Rolling statistics are available such as moving averages and sum - these functions are prefixed by \code{moving_}. There are several growth rate calculations available which are prefixed by \code{growth_}. Finally, there is an indexing/rebasing function, \code{\link{index}()}, which is flexible to different benchmark date periods and parity levels.
#' @keywords internal
"_PACKAGE"