#' Find Shelf Temperatures in a Vector of Temperature Readings
#'
#' Identifies temperature values that are over-represented relative to a loess
#' smooth of the histogram, indicating sensor quantisation artifacts ("shelves").
#' A shelf is detected where any bin count exceeds the loess baseline by at
#' least \code{min_ratio} times. The shelf band extends to include all
#' contiguous bins that exceed the baseline.
#'
#' @param tmp Numeric vector of temperature values.
#' @param bin_width Bin width in degrees C for histogram. Default 0.05.
#' @param loess_span Span parameter for loess smooth. If NULL (default),
#'   auto-scaled so the smoothing window is approximately 1 deg C.
#' @param min_ratio Minimum ratio of bin count to loess baseline to trigger
#'   shelf detection. Default 2.5.
#' @param max_shelf_band Maximum allowed shelf half-width in degrees C.
#'   Wider detections are rejected as loess tracking artifacts on steep
#'   distribution slopes. Default 0.2.
#'
#' @return A data.frame with columns: shelf_temp, peak_count, baseline_count,
#'   ratio, excess, band_lo, band_hi. band_lo and band_hi are the actual
#'   temperature edges of the contiguous region exceeding the baseline.
#'   Returns empty data.frame if no shelves.
#'
#' @export
find_shelves <- function(tmp,
                         bin_width = 0.05,
                         loess_span = NULL,
                         min_ratio = 2.5,
                         max_shelf_band = 0.2) {

  empty <- data.frame(
    shelf_temp = numeric(), peak_count = integer(),
    baseline_count = numeric(), ratio = numeric(),
    excess = numeric(), band_lo = numeric(), band_hi = numeric()
  )

  tmp <- tmp[!is.na(tmp)]
  if (length(tmp) < 100) return(empty)

  # Exclude extreme values before fitting smoother
  tmp <- tmp[tmp >= 4 & tmp <= 28]
  if (length(tmp) < 100) return(empty)

  breaks <- seq(
    floor(min(tmp) / bin_width) * bin_width,
    ceiling(max(tmp) / bin_width) * bin_width,
    by = bin_width
  )
  h <- graphics::hist(tmp, breaks = breaks, plot = FALSE)
  temps <- h$mids
  counts <- h$counts
  n <- length(counts)

  # Loess baseline — scale span so smoothing window is ~1 deg C
  # regardless of bin width
  if (is.null(loess_span)) {
    loess_span <- max(1.0 / (max(temps) - min(temps)), 0.02)
  }
  fit <- stats::loess(counts ~ temps, span = loess_span,
                      degree = 2, family = "symmetric")
  baseline <- pmax(stats::predict(fit, temps), 0)

  # Find bins exceeding min_ratio * baseline
  ratio <- counts / pmax(baseline, 1)
  triggered <- which(ratio >= min_ratio)

  shelves <- empty
  if (length(triggered) > 0L) {
    # Group triggered bins
    groups <- cumsum(c(1L, diff(triggered) > 1L))
    rows <- list()

    for (g in unique(groups)) {
      trigger_idx <- triggered[groups == g]
      peak_i <- trigger_idx[which.max(counts[trigger_idx])]

      # Expand outward from trigger group to include all contiguous bins
      # that exceed the baseline
      band_lo_i <- min(trigger_idx)
      while (band_lo_i > 1L && counts[band_lo_i - 1L] > baseline[band_lo_i - 1L] * 1.2) {
        band_lo_i <- band_lo_i - 1L
      }
      band_hi_i <- max(trigger_idx)
      while (band_hi_i < n && counts[band_hi_i + 1L] > baseline[band_hi_i + 1L] * 1.2) {
        band_hi_i <- band_hi_i + 1L
      }

      # Actual band edges: left edge of lowest bin to right edge of highest bin
      band_lo <- temps[band_lo_i] - bin_width / 2
      band_hi <- temps[band_hi_i] + bin_width / 2

      # Reject the first (coldest) bin — edge artifact from loess undershoot
      if (band_lo_i == 1L) next

      # Reject bands wider than max — typically loess tracking artifacts
      # on steep distribution slopes
      if ((band_hi - band_lo) / 2 > max_shelf_band) next

      rows[[length(rows) + 1L]] <- data.frame(
        shelf_temp = temps[peak_i],
        peak_count = counts[peak_i],
        baseline_count = round(baseline[peak_i]),
        ratio = round(ratio[peak_i], 1),
        excess = round(counts[peak_i] - baseline[peak_i]),
        band_lo = band_lo,
        band_hi = band_hi
      )
    }

    if (length(rows) > 0L) shelves <- do.call(rbind, rows)
  }

  # Merge shelves closer than 2 * max_shelf_band — these are fragments
  # of the same calibration anchor. Keep the one with higher excess count
  # and widen the band to span both.
  if (nrow(shelves) > 1L) {
    shelves <- shelves[order(shelves$shelf_temp), ]
    merged <- shelves[1L, , drop = FALSE]
    for (i in 2:nrow(shelves)) {
      prev <- merged[nrow(merged), ]
      curr <- shelves[i, ]
      if (curr$shelf_temp - prev$shelf_temp < 2 * max_shelf_band) {
        # Merge: keep the one with higher excess, widen band
        if (curr$excess > prev$excess) {
          merged[nrow(merged), ] <- curr
        }
        merged$band_lo[nrow(merged)] <- min(prev$band_lo, curr$band_lo)
        merged$band_hi[nrow(merged)] <- max(prev$band_hi, curr$band_hi)
      } else {
        merged <- rbind(merged, curr)
      }
    }
    shelves <- merged
    rownames(shelves) <- NULL
  }

  shelves
}
