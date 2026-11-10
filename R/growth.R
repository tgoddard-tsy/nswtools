#' @import mathjaxr
NULL

#' Growth rates
#' 
#' Calculate growth rates in a dataframe. Can be used to calculate "through-the-year" (equiv. "year-ended") growth.
#' Defaults to period-on-period growth (\code{n = 1}).
#' 
#' \loadmathjax
#' \mjdeqn{y_t = \frac{x_t - x_{t-n}}{x_{t-n}} \cdot 100}{Yt = ( Xt - Xt-n ) / Xt-n  * 100}
#' @param df A (long) dataframe containing columns named "date", "series" and "value".
#' @param n Number of periods to span.
#' @param series_to_transform An object of class \code{character}, containing the names of series you wish to transform. Defaults to all series/variables.
#' @seealso \code{\link{growth_avg}()}, \code{\link{growth_contrib}()}, \code{\link{growth_cagr}()}, \code{\link{difference}()}
#' @examples 
#' library(tidyverse)
#' 
#' df <- filter(economics_long, variable == 'pce') |> 
#'   rename(series = variable) |> 
#'   growth()
#' @export
growth <- function(df, n = 1, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = (value / dplyr::lag(value, n = n) - 1) * 100) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Differences
#' 
#' Calculate differences in a dataframe. Defaults to period-on-period difference (\code{n = 1}).
#' 
#' \loadmathjax
#' \mjdeqn{y_t = x_t - x_{t-n}}{Yt = Xt - Xt-n}
#' @inheritParams growth
#' @seealso \code{\link{growth}()}, \code{\link{growth_avg}()}, \code{\link{growth_contrib}()}, \code{\link{growth_cagr}()}
#' @export
difference <- function(df, n = 1, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = value - dplyr::lag(value, n = n)) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Average growth rates
#' 
#' Calculate (average) growth rates in a dataframe. Can be used to calculate "year-average" growth. It calculates the growth of the moving average level.
#' Defaults to year-average growth for quarterly data (\code{n = 4}).
#' 
#' \loadmathjax
#' \mjdeqn{\begin{aligned} \bar x_t &= \frac{1}{n} \sum_{i=0}^n x_{t-i} \\\\\[1.5ex]
#'   &= \frac{x_{t} + x_{t-1}+ \cdots + x_{t-n}}{n} \\\\\[3ex] 
#'   y_t &= \frac{\bar x_t - \bar x_{t-n}}{\bar x_{t-n}} \cdot 100
#'   \end{aligned}}{XBARt = ( Xt-n + ... + Xt ) / n}
#' \mjdeqn{}{Yt = ( XBARt - XBARt-n ) / XBARt-n  * 100}
#' @inheritParams growth
#' @seealso \code{\link{growth}()}, \code{\link{growth_contrib}()}, \code{\link{growth_cagr}()}, \code{\link{difference}()}
#' @export
growth_avg <- function(df, n = 4, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = zoo::rollmean(value, k = n, fill = NA, align = 'right')) |> 
    dplyr::mutate(value = (value / dplyr::lag(value, n = n) - 1) * 100) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Contributions to growth by component
#' 
#' Calculates the contributions to growth in a dataframe. Defaults to period-on-period growth (\code{n = 1}). The values returned for the nominated \code{total} series will be simple growth.
#' 
#' \loadmathjax
#' \mjdeqn{y_t = \frac{x_t - x_{t-n}}{total_{t-n}} \cdot 100}{Yt = ( Xt - Xt-n ) / TOTALt-n * 100}
#' \cr
#' Note: if any component series are missing from the input \code{df}, the contributions may not sum to the \code{total}.
#' @inheritParams growth
#' @param total An object of class \code{character}, nominating the series to be used as the total. If not defined, the function will calculate the total by summing all `series_to_transform` together.
#' @seealso \code{\link{growth}()}, \code{\link{growth_avg}()}, \code{\link{growth_cagr}()}, \code{\link{difference}()}, \code{\link{share_of_total}()}
#' @export
growth_contrib <- function(df, n = 1, total, series_to_transform = unique(df$series) ){
  if(!missing(total)){
    total_df <- df |>
      dplyr::filter(series == total)
    } else{
    total_df <- df |>
      dplyr::filter(series %in% series_to_transform) |>
      dplyr::group_by(date) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
      dplyr::ungroup()
  }
  
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = (value - dplyr::lag(value, n = n)) / dplyr::lag(total_df$value, n = n) * 100) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Share of total by component
#' 
#' Calculates the percentage shares of components in a dataframe. The nominated \code{total} series will be dropped after transformation since it will just be 100s.
#' 
#' \loadmathjax
#' \mjdeqn{y_t = \frac{x_t}{total_{t}} \cdot 100}{Yt =  Xt / TOTALt * 100}
#' @inheritParams growth_contrib
#' @seealso \code{\link{growth_contrib}()}
#' @export
share_of_total <- function(df, total, series_to_transform = unique(df$series) ){
  if(!missing(total)){
    total_df <- df |>
      dplyr::filter(series == total)}
  else{
    total_df <- df |>
      dplyr::filter(series %in% series_to_transform) |>
      dplyr::group_by(date) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
      dplyr::ungroup()
  }
  
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = value / total_df$value * 100) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(temp_df)
}

#' Moving CAGR
#' 
#' Calculates the compound annual growth rate (CAGR) in a dataframe. Defaults to five-year CAGR for annual data (\code{n = 5}).
#' 
#' \loadmathjax
#' \mjdeqn{y_t = \left( \left( \frac{x_t}{x_{t-n}} \right)^{\frac{1}{\text{no. years}}} - 1 \right) \cdot 100}{}
#' \cr
#' Note: \code{n} is the number of \emph{periods}; so if dealing with a higher frequency than annual data, you must account for this in \code{n} (e.g. 10-year CAGR with monthly data: \code{n = 10 * 12}). This is a moving function, so if you're only interested in some values, then filter the results as needed.
#' @inheritParams growth
#' @export
growth_cagr <- function(df, n = 5, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = ((value / dplyr::lag(value, n = n))^ ( 1/ (lubridate::interval(dplyr::lag(date, n = n), date)/ years(1)) ) -1) * 100) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Moving Sum
#' 
#' Calculates moving sums/totals in a dataframe. Can be used to to calculate the moving annual total (MAT).
#' 
#' \loadmathjax
#' \mjdeqn{\begin{aligned} y_t &= \sum_{i=0}^n x_{t-i} \\\\\[2ex] 
#'   &= x_{t} + x_{t-1}+ \cdots + x_{t-n} \end{aligned}}{Yt = Xt-n + ... + Xt}
#' @inheritParams growth
#' @export
moving_sum <- function(df, n = 4, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = zoo::rollsum(value, k = n, fill = NA, align = 'right')) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Moving Average
#' 
#' Calculates moving averages in a dataframe. Can be used to to calculate the moving annual average (MAA).
#' 
#' \loadmathjax
#' \mjdeqn{\begin{aligned} y_t &= \frac{1}{n} \sum_{i=0}^n x_{t-i} \\\\\[1.5ex] 
#'   &= \frac{x_{t} + x_{t-1}+ \cdots + x_{t-n}}{n} \end{aligned}}{Yt = ( Xt-n + ... + Xt ) / n}
#' @inheritParams growth
#' @export
moving_avg <- function(df, n = 4, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = zoo::rollmean(value, k = n, fill = NA, align = 'right')) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Moving Maximum
#' 
#' Calculates moving maximum values in a dataframe.
#' 
#' \loadmathjax
#' \mjdeqn{y_t = \text{max}\left\\\\{x_t, x_{t-1}, \cdots, x_{t-n} \right\\\\}}{Yt = max{Xt, Xt-1, ... , Xt-n}}
#' @inheritParams growth
#' @export
moving_max <- function(df, n = 4, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = zoo::rollmax(value, k = n, fill = NA, align = 'right')) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Moving Minimum
#' 
#' Calculates moving minimum values in a dataframe.
#' 
#' \loadmathjax
#' \mjdeqn{y_t = \text{min}\left\\\\{x_t, x_{t-1}, \cdots, x_{t-n} \right\\\\}}{Yt = min{Xt, Xt-1, ... , Xt-n}}
#' @inheritParams growth
#' @export
moving_min <- function(df, n = 4, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = zoo::rollapply(value, FUN = min, width = n, fill = NA, align = 'right')) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}

#' Moving Median
#' 
#' Calculates moving median values in a dataframe.
#' 
#' \loadmathjax
#' \mjdeqn{y_t = \begin{cases}
#' X_{t\left[\frac{n+1}{2} \right]},  & \text{if $n$ is odd} \\\\\[2ex]
#' \frac{X_{t \left[ \frac{n}{2} \right]} + X_{t \left[ \frac{n}{2} + 1 \right]}}{2}, & \text{if $n$ is even} \end{cases}}{}
#' \mjdeqn{\text{where $X_t$ is an ordered set of size $n$}}{}
#' @inheritParams growth
#' @export
moving_median <- function(df, n = 4, series_to_transform = unique(df$series)){
  temp_df <- df |> 
    dplyr::filter(series %in% series_to_transform) |> 
    dplyr::group_by(series) |> 
    dplyr::mutate(value = zoo::rollapply(value, FUN = median, width = n, fill = NA, align = 'right')) |> 
    dplyr::ungroup()
  df[df$series %in% series_to_transform, ] <- temp_df
  return(df)
}
