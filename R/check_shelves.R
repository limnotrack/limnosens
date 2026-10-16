#' Check Shelf Spacing for Consistency
#'
#' Validates that detected shelves are approximately 4 deg C apart.
#'
#' @param shelves Data.frame with columns \code{centre} and \code{width}.
#' @param expected_spacing Expected spacing in deg C. Default 4.
#' @param tolerance Allowed deviation in deg C. Default 0.5.
#'
#' @return The input data.frame with an additional \code{spacing} column.
#'   Returned invisibly.
#'
#' @export
check_shelves <- function(shelves,
                          expected_spacing = 4,
                          tolerance = 0.5) {

  if (nrow(shelves) == 0L) {
    message("No shelves to check.")
    return(invisible(shelves))
  }

  shelves <- shelves[order(shelves$centre), ]
  rownames(shelves) <- NULL

  n <- nrow(shelves)
  spacing <- rep(NA_real_, n)
  if (n > 1L) {
    spacing[seq_len(n - 1L)] <- diff(shelves$centre)
  }
  shelves$spacing <- round(spacing, 2)

  cat("\n  Shelf centres:\n")
  for (i in seq_len(n)) {
    cat(sprintf("    %d. %.2f\u00b0C  (width=%.2f)",
      i, shelves$centre[i], shelves$width[i]))
    if (!is.na(spacing[i])) {
      gap <- spacing[i]
      n_intervals <- round(gap / expected_spacing)
      if (abs(gap - expected_spacing) <= tolerance) {
        cat(sprintf("  \u2192 gap: %.2f\u00b0C \u2713", gap))
      } else if (n_intervals >= 2 &&
                 abs(gap - n_intervals * expected_spacing) <=
                 tolerance * n_intervals) {
        cat(sprintf(
          "  \u2192 gap: %.2f\u00b0C \u2014 %d missing shelf(s)?",
          gap, n_intervals - 1L))
      } else {
        cat(sprintf("  \u2192 gap: %.2f\u00b0C \u2717 (expected ~%.0f)",
          gap, expected_spacing))
      }
    }
    cat("\n")
  }

  if (n == 1L) cat("  Only one shelf \u2014 cannot check spacing.\n")
  cat("\n")
  invisible(shelves)
}
