#' Index or rebase data series
#' 
#' Index/re-index series to a base date (or an unweighted average across a date span).
#' \cr\cr
#' For indexing each series based on their own values, use \code{index()} -- i.e all series will be parity in the base period. 
#' For indexing each series relative to one base series, use \code{index_target()} -- i.e only the target series will be parity in the base period (relativity with other series will be preserved after indexing). 
#' For example, if indexing \emph{Actual GDP} and \emph{Potential GDP}, we would need to preserve the relativity so that the differences continue to reflect the output gap.
#' @param df A dataframe containing columns named "date", "series" and "value".
#' @param base one of:
#' \itemize{
#'   \item A vector of integers (for calendar/financial years basing). 
#'   \item A vector of dates (for any other basing such as single dates, or any other manual date span). Can be a date object or a character string compatible with \code{\link[base]{as.Date}()}, e.g. '2011-02-01'.}
#' @param FY A logical determining whether to base to financial years (\code{TRUE}), or calendar years (\code{FALSE}; default). Only used when \code{base} is of class \code{numeric}.
#' @param par The par value to index to. Defaults to 100.
#' @param series_to_transform An object of class \code{character}, containing the names of series you wish to transform. Defaults to all series/variables.
#' @param target An object of class \code{character}, containing the name of the target series to index based on. Only used in \code{index_target()}.
#' @examples 
#' # For a base of 1-Feb-2011:
#' index(df, base = '2011-02-01')
#' 
#' # For a base of calendar year 2011:
#' index(df, base = 2011)
#' 
#' # For a base of financial year 2010-11:
#' index(df, base = 2011, FY = TRUE)
#' # or equivalently;
#' index(df, base = c('2010-07-01', '2011-06-30'))
#' 
#' # For a base of the decade-average in the 2000s:
#' index(df, base = 2000:2009)
#' # or equivalently;
#' index(df, base = c(2000, 2009))
#' # or equivalently;
#' index(df, base = c('2000-01-01', '2009-12-31'))
#' 
#' # For indexing with one series as the target base:
#' index_target(df, base = 2019, target = "Real GDP")
#' @name index
NULL

#' @rdname index
#' @export
index <- function(df, base, FY = FALSE, par = 100, series_to_transform = unique(df$series)){
  if (is.numeric(base)){
    if ( FY ){
      min_date <- lubridate::make_date(year = min(base) - 1, month = 7, day = 1)
      max_date <- lubridate::make_date(year = max(base), month = 6, day = 30)
      temp_df <- df |> 
        dplyr::filter(series %in% series_to_transform,
                      date >= min_date,
                      date <= max_date) |> 
        dplyr::group_by(series) |> 
        dplyr::summarise(value = mean(value, na.rm = TRUE)) |> 
        dplyr::ungroup()
    } else{
      temp_df <- df |> 
        dplyr::filter(series %in% series_to_transform,
                      lubridate::year(date) >= min(base),
                      lubridate::year(date) <= max(base)) |> 
        dplyr::group_by(series) |> 
        dplyr::summarise(value = mean(value, na.rm = TRUE)) |> 
        dplyr::ungroup()
    }
  } else{
    temp_df <- df |> 
      dplyr::filter(series %in% series_to_transform,
                    date >= min(as.Date(base)),
                    date <= max(as.Date(base))) |> 
      dplyr::group_by(series) |> 
      dplyr::summarise(value = mean(value, na.rm = TRUE)) |> 
      dplyr::ungroup()
  }
  for (i in series_to_transform){
    df$value[df$series == i] <- df$value[df$series == i] / temp_df$value[temp_df$series == i] * par
  }
  return(df)
}

#' @rdname index
#' @export
index_target <- function(df, base, target, FY = FALSE, par = 100, series_to_transform = unique(df$series)){
  if (is.numeric(base)){
    if ( FY ){
      min_date <- lubridate::make_date(year = min(base) - 1, month = 7, day = 1)
      max_date <- lubridate::make_date(year = max(base), month = 6, day = 30)
      temp_df <- df |> 
        dplyr::filter(series == target,
                      date >= min_date,
                      date <= max_date) |> 
        dplyr::summarise(value = mean(value, na.rm = TRUE))
    } else{
      temp_df <- df |> 
        dplyr::filter(series == target,
                      lubridate::year(date) >= min(base),
                      lubridate::year(date) <= max(base)) |> 
        dplyr::summarise(value = mean(value, na.rm = TRUE))
    }
  } else{
    temp_df <- df |> 
      dplyr::filter(series == target,
                    date >= min(as.Date(base)),
                    date <= max(as.Date(base))) |> 
      dplyr::summarise(value = mean(value, na.rm = TRUE))
  }
  for (i in series_to_transform){
    df$value[df$series == i] <- df$value[df$series == i] / temp_df$value * par
  }
  return(df)
}
