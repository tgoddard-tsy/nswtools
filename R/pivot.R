#' Wrapper functions for pivoting data
#' 
#' Pivot between "long" and "wide" data. These wrapper functions provide more convenient defaults to the \code{tidyr} functions.
#' @param cols Columns to pivot into longer format. Defaults to pivot all columns except the first.
#' @param id_cols Columns that uniquely identify each observation to pivot into wider format. Defaults to the first column.
#' @param names_to,names_from A character vector specifying the new column name (\code{_longer}) or the column to collect names from (\code{_wider}).
#' @param ... Other arguments passed on to \code{\link[tidyr]{pivot_longer}()} or \code{\link[tidyr]{pivot_wider}()}.
#' @examples 
#' pivot_wider_nsw(economics_long)
#' 
#' pivot_longer_nsw(economics)
#' @name pivot
NULL

#' @rdname pivot
#' @export
pivot_longer_nsw <- function(data, cols = -1, names_to = "series", ...){
  if (cols < 0) {
    cols <- names(data)[-(-cols)]
  }
  tidyr::pivot_longer(data, cols = cols, names_to = names_to, ...)
}

#' @rdname pivot
#' @export
pivot_wider_nsw <- function(data, id_cols = 1, names_from = "series", ...){
  tidyr::pivot_wider(data, id_cols = id_cols, names_from = names_from, ...)
}
