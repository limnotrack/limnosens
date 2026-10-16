#' Detect Shelves from a Data Frame Column
#'
#' Wrapper around [find_shelves()] that extracts a temperature column from
#' a data.frame, optionally filtered by date range.
#'
#' @param df Data.frame containing temperature and datetime data.
#' @param src Name of the source temperature column. Default \code{"TmpWtr"}.
#' @param datetime Name of the datetime column. Default \code{"DateTime"}.
#' @param date_range Length-2 vector or list \code{c(start, end)} /
#'   \code{list(start, end)}. Values can be character
#'   (\code{"YYYY-MM-DD"}), POSIXct, or \code{NA} for open-ended.
#'   Use a list when mixing NA with POSIXct to avoid coercion.
#'   Default \code{c(NA, NA)} (all data).
#' @param ... Additional arguments passed to [find_shelves()].
#'
#' @return A data.frame of shelves (from [find_shelves()]).
#'
#' @export
detect_shelves_df <- function(df, src = "TmpWtr", datetime = "DateTime",
                              date_range = c(NA, NA), ...) {

  idx <- .date_filter(df[[datetime]], date_range)
  tmp <- df[[src]][idx]
  find_shelves(tmp, ...)
}


#' Apply Shelf Correction to a Data Frame Column
#'
#' Wrapper around [correct_shelves()] that reads from a source column,
#' writes to a destination column, optionally filtered by date range.
#' The destination column is created if it doesn't exist.
#'
#' @param df Data.frame containing temperature data.
#' @param shelves Data.frame of shelves (centre/width format).
#' @param src Name of the source temperature column. Default \code{"TmpWtr"}.
#' @param dest Name of the destination column for corrected values.
#'   Default \code{"TmpWtr_corr"}. Created if it doesn't exist.
#' @param datetime Name of the datetime column. Default \code{"DateTime"}.
#' @param date_range Length-2 vector or list \code{c(start, end)} /
#'   \code{list(start, end)}. Values can be character
#'   (\code{"YYYY-MM-DD"}), POSIXct, or \code{NA} for open-ended.
#'   Use a list when mixing NA with POSIXct to avoid coercion.
#'   Default \code{c(NA, NA)} (all data).
#' @param slope Sawtooth slope. Default 0.1.
#' @param ... Additional arguments passed to [correct_shelves()].
#'
#' @return The input data.frame with the destination column added/updated.
#'   Only rows within the date range are corrected; rows outside are
#'   copied from source unchanged (or kept as-is if dest already exists).
#'
#' @export
correct_shelves_df <- function(df, shelves, src = "TmpWtr",
                               dest = "TmpWtr_corr",
                               datetime = "DateTime",
                               date_range = c(NA, NA),
                               slope = 0.1, ...) {

  idx <- .date_filter(df[[datetime]], date_range)

  # Initialise dest column if needed
  if (!dest %in% names(df)) {
    df[[dest]] <- df[[src]]
  }

  # Correct only the date-filtered subset
  r <- correct_shelves(
    datetime = df[[datetime]][idx],
    depth = if ("DptSns" %in% names(df)) df$DptSns[idx] else rep(0, sum(idx)),
    tmp = df[[src]][idx],
    shelves = shelves,
    slope = slope,
    ...
  )

  df[[dest]][idx] <- r$tmp
  df
}


#' @keywords internal
.date_filter <- function(dt, date_range) {

  tz <- if (inherits(dt, "POSIXct")) {
    attr(dt, "tzone") %||% ""
  } else ""

  .to_posix <- function(x, default, end_of_day = FALSE) {
    if (is.null(x) || (length(x) == 1 && is.na(x))) return(default)
    if (inherits(x, "POSIXct")) return(x)
    if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = tz))
    # Character
    x <- as.character(x)
    if (end_of_day && !grepl(":", x)) {
      x <- paste(x, "23:59:59")
    }
    as.POSIXct(x, tz = tz)
  }

  # Use list access to avoid c() coercing POSIXct to numeric
  if (is.list(date_range)) {
    start_val <- date_range[[1]]
    end_val   <- date_range[[2]]
  } else {
    start_val <- date_range[1]
    end_val   <- date_range[2]
  }

  start <- .to_posix(start_val, as.POSIXct("1900-01-01", tz = "UTC"))
  end   <- .to_posix(end_val, as.POSIXct("2099-12-31 23:59:59", tz = "UTC"),
                     end_of_day = TRUE)

  !is.na(dt) & dt >= start & dt <= end
}
