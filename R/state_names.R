#' Convert Australian states and territories to abbreviations
#'
#' Converts Australian state and territory names, abbreviations, or numeric
#' codes to standard abbreviations.
#'
#' Numeric codes follow the Australian Bureau of Statistics ordering:
#' New South Wales, Victoria, Queensland, South Australia, Western Australia,
#' Tasmania, Northern Territory, and Australian Capital Territory.
#'
#' Unrecognised values are retained unchanged and produce a warning. If
#' `relevel = TRUE`, unrecognised values are placed after the standard state
#' and territory levels.
#'
#' @param x A character vector, factor, or numeric vector. Values may be full
#'   state or territory names, abbreviations, or numeric codes from 1 to 8.
#'   Matching of character values is case-insensitive and ignores leading and
#'   trailing whitespace.
#' @param as_factor Logical. Should the result be returned as a factor?
#'   Defaults to `TRUE` when `x` is a factor and `FALSE` otherwise.
#' @param relevel Logical. If `TRUE`, return a factor with recognised states
#'   and territories in the usual order, followed by any unrecognised values.
#'   This overrides `as_factor`.
#'
#' @return A character vector or factor containing state and territory
#'   abbreviations, with unrecognised values retained unchanged.
#'
#' @examples
#' state_abbr(c("New South Wales", "Victoria", "Queensland"))
#' state_abbr(c("nsw", "VIC", "Queensland"))
#' state_abbr(1:8)
#'
#' state_abbr(c("NSW", "Victoria", "Other"))
#'
#' state_abbr(
#'   c("Western Australia", "New South Wales"),
#'   as_factor = TRUE
#' )
#'
#' state_abbr(
#'   factor(c("Victoria", "New South Wales")),
#'   relevel = TRUE
#' )
#'
#' @export
state_abbr <- function(
    x,
    as_factor = is.factor(x),
    relevel = TRUE
) {
  convert_state(
    x = x,
    output = "abbreviation",
    as_factor = as_factor,
    relevel = relevel
  )
}


#' Convert Australian states and territories to full names
#'
#' Converts Australian state and territory names, abbreviations, or numeric
#' codes to full names.
#'
#' Numeric codes follow the Australian Bureau of Statistics ordering:
#' New South Wales, Victoria, Queensland, South Australia, Western Australia,
#' Tasmania, Northern Territory, and Australian Capital Territory.
#'
#' Unrecognised values are retained unchanged and produce a warning. If
#' `relevel = TRUE`, unrecognised values are placed after the standard state
#' and territory levels.
#'
#' @inheritParams state_abbr
#'
#' @return A character vector or factor containing full state and territory
#'   names, with unrecognised values retained unchanged.
#'
#' @examples
#' state_name(c("NSW", "Vic", "qld"))
#' state_name(1:8)
#'
#' state_name(c("NSW", "Vic", "Other"))
#'
#' state_name(
#'   factor(c("WA", "NSW"), levels = c("WA", "NSW")),
#'   relevel = FALSE
#' )
#'
#' state_name(c("WA", "NSW"), relevel = TRUE)
#'
#' @export
state_name <- function(
    x,
    as_factor = is.factor(x),
    relevel = TRUE
) {
  convert_state(
    x = x,
    output = "name",
    as_factor = as_factor,
    relevel = relevel
  )
}


#' Convert Australian state and territory values
#'
#' @param x A character vector, factor, or numeric vector.
#' @param output Either `"name"` or `"abbreviation"`.
#' @param as_factor Logical. Should the result be a factor?
#' @param relevel Logical. Should standard factor ordering be applied?
#'
#' @return A character vector or factor.
#'
#' @keywords internal
#' @noRd
convert_state <- function(
    x,
    output = c("name", "abbreviation"),
    as_factor = is.factor(x),
    relevel = TRUE
) {
  output <- match.arg(output)
  
  if (!is.character(x) && !is.factor(x) && !is.numeric(x)) {
    stop(
      "`x` must be a character vector, factor, or numeric vector.",
      call. = FALSE
    )
  }
  
  if (!is.logical(as_factor) ||
      length(as_factor) != 1L ||
      is.na(as_factor)) {
    stop(
      "`as_factor` must be `TRUE` or `FALSE`.",
      call. = FALSE
    )
  }
  
  if (!is.logical(relevel) ||
      length(relevel) != 1L ||
      is.na(relevel)) {
    stop(
      "`relevel` must be `TRUE` or `FALSE`.",
      call. = FALSE
    )
  }
  
  state_names <- c(
    "New South Wales",
    "Victoria",
    "Queensland",
    "South Australia",
    "Western Australia",
    "Tasmania",
    "Northern Territory",
    "Australian Capital Territory",
    "Australia"
  )
  
  state_abbreviations <- c(
    "NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT", "Aus"
  )
  
  state_codes <- c(
    "1", "2", "3", "4", "5", "6", "7", "8", "AUS"
  )
  
  input_values <- as.character(x)
  input_keys <- toupper(trimws(input_values))
  
  lookup_keys <- c(
    toupper(state_names),
    toupper(state_abbreviations),
    state_codes
  )
  
  output_values <- switch(
    output,
    name = rep(state_names, 3L),
    abbreviation = rep(state_abbreviations, 3L)
  )
  
  lookup <- stats::setNames(output_values, lookup_keys)
  result <- unname(lookup[input_keys])
  
  missing_input <- is.na(x)
  unrecognised <- !missing_input & is.na(result)
  
  if (any(unrecognised)) {
    unrecognised_values <- unique(input_values[unrecognised])
    
    warning(
      paste0(
        "Unrecognised state or territory value",
        if (length(unrecognised_values) > 1L) "s" else "",
        ": ",
        paste(shQuote(unrecognised_values), collapse = ", "),
        ". Values retained unchanged."
      ),
      call. = FALSE
    )
    
    result[unrecognised] <- input_values[unrecognised]
  }
  
  if (!as_factor && !relevel) {
    return(result)
  }
  
  standard_levels <- if (output == "name") {
    state_names
  } else {
    state_abbreviations
  }
  
  if (relevel) {
    additional_levels <- unique(
      result[
        !is.na(result) &
          !result %in% standard_levels
      ]
    )
    
    return(
      factor(
        result,
        levels = c(standard_levels, additional_levels)
      )
    )
  }
  
  if (is.factor(x)) {
    original_levels <- levels(x)
    original_level_keys <- toupper(trimws(original_levels))
    mapped_levels <- unname(lookup[original_level_keys])
    
    unrecognised_levels <- is.na(mapped_levels)
    mapped_levels[unrecognised_levels] <-
      original_levels[unrecognised_levels]
    
    mapped_levels <- unique(mapped_levels)
    
    return(
      factor(
        result,
        levels = mapped_levels
      )
    )
  }
  
  factor(
    result,
    levels = unique(result[!is.na(result)])
  )
}
