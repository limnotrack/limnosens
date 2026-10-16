#' Quick Temperature Shelf Correction
#'
#' Convenience wrapper around [correct_shelves()] that returns just the
#' corrected temperature vector. Applies a continuous piecewise-linear
#' sawtooth correction — no data is deleted.
#'
#' @param tmp Numeric vector of temperature values.
#' @param shelves Data.frame of shelves with columns \code{centre} and
#'   \code{width}. Also accepts legacy \code{shelf_temp}/\code{band_lo}/
#'   \code{band_hi} format.
#' @param slope Sawtooth slope. Default 0.1.
#'
#' @return Numeric vector of corrected temperatures (same length as input).
#'
#' @examples
#' \dontrun{
#' shelves <- data.frame(
#'   centre = c(8.725, 13.025, 16.925, 20.575),
#'   width  = c(0.10, 0.10, 0.10, 0.10)
#' )
#' pro$TmpWtr_corrected <- correct_tmp(pro$TmpWtr, shelves)
#' }
#'
#' @export
correct_tmp <- function(tmp, shelves, slope = 0.1) {
  correct_shelves(
    datetime = NULL, depth = NULL, tmp = tmp,
    shelves = shelves, slope = slope
  )$tmp
}
