#' Correct Temperature Shelf Artifacts
#'
#' Applies a continuous piecewise-linear sawtooth bias correction in the
#' temperature domain. No values are deleted — every reading gets corrected.
#'
#' The bias function has two phases per shelf cycle:
#' \enumerate{
#'   \item \strong{Clean segment} (between shelf bands): bias drifts linearly
#'     from zero at the segment midpoint to a peak at each shelf edge.
#'   \item \strong{Shelf band} (through the shelf): bias transitions linearly
#'     from the peak of the segment below to the (negative) peak of the
#'     segment above, forming the reset phase of the sawtooth.
#' }
#'
#' @param datetime POSIXct vector of timestamps (unused but kept for API
#'   compatibility with plotting functions).
#' @param depth Numeric vector of depth values (unused).
#' @param tmp Numeric vector of temperature values.
#' @param shelves Data.frame of shelves with columns \code{centre} and
#'   \code{width}.
#' @param profile_id Unused (kept for API compatibility).
#' @param slope Sawtooth drift rate (bias per degree C). Default 0.1.
#' @param profile_gap_hours Unused.
#'
#' @return A list with:
#'   \describe{
#'     \item{tmp}{Numeric vector of corrected temperatures.}
#'     \item{shelves}{Data.frame of shelves used.}
#'     \item{bias_fn}{The continuous sawtooth bias function.}
#'   }
#'
#' @export
correct_shelves <- function(datetime,
                            depth,
                            tmp,
                            shelves,
                            profile_id = NULL,
                            slope = 0.1,
                            profile_gap_hours = 2) {

  # Accept legacy format
  shelves <- .normalise_shelves(shelves)

  result <- tmp
  bias_fn <- NULL

  if (nrow(shelves) < 2L || is.null(slope) || slope == 0) {
    return(list(tmp = result, shelves = shelves, bias_fn = NULL))
  }

  bias_fn <- .build_sawtooth(shelves, slope)

  valid <- !is.na(tmp)
  result[valid] <- tmp[valid] - bias_fn(tmp[valid])

  list(
    tmp = result,
    shelves = shelves,
    bias_fn = bias_fn
  )
}


#' Build a continuous sawtooth bias function
#'
#' The bias is piecewise-linear and continuous everywhere:
#' - In clean segments (between shelf bands): \code{bias = slope * (T - midpoint)}
#' - Through shelf bands: linear transition connecting the bias at the
#'   upper edge of the segment below to the bias at the lower edge of the
#'   segment above.
#' - Beyond the outermost shelves: virtual segments mirror the nearest
#'   real segment width.
#'
#' @keywords internal
.build_sawtooth <- function(shelves, slope) {

  shelves <- shelves[order(shelves$centre), ]
  n_shelves <- nrow(shelves)

  if (n_shelves < 2L) {
    return(function(t) rep(0, length(t)))
  }

  # Derive band edges from centre + width
  band_lo <- shelves$centre - shelves$width / 2
  band_hi <- shelves$centre + shelves$width / 2

  # Clean segments: band_hi[i] to band_lo[i+1]
  seg_lo <- band_hi[-n_shelves]
  seg_hi <- band_lo[-1L]
  seg_mid <- (seg_lo + seg_hi) / 2
  n_seg <- length(seg_lo)

  # Virtual segments beyond the detected range
  gap_below <- seg_hi[1L] - seg_lo[1L]
  gap_above <- seg_hi[n_seg] - seg_lo[n_seg]

  virt_lo_hi <- band_lo[1L]
  virt_lo_lo <- virt_lo_hi - gap_below
  virt_hi_lo <- band_hi[n_shelves]
  virt_hi_hi <- virt_hi_lo + gap_above

  # All segments: virtual_below, real segments, virtual_above
  all_lo  <- c(virt_lo_lo, seg_lo, virt_hi_lo)
  all_hi  <- c(virt_lo_hi, seg_hi, virt_hi_hi)
  all_mid <- (all_lo + all_hi) / 2
  n_all <- length(all_lo)

  # Pre-compute bias at each segment edge (for shelf band transitions)
  # bias at upper edge of segment j = slope * (all_hi[j] - all_mid[j])
  # bias at lower edge of segment j = slope * (all_lo[j] - all_mid[j])
  bias_at_hi <- slope * (all_hi - all_mid)  # bias approaching shelf from below
  bias_at_lo <- slope * (all_lo - all_mid)  # bias approaching shelf from above

  # Shelf band transitions: from bias_at_hi[j] to bias_at_lo[j+1]
  # where j is the segment below the shelf and j+1 is the segment above
  # Segment indices: virtual_below=1, real segments=2..(n_seg+1), virtual_above=n_seg+2
  # Shelf i sits between segment (i) and segment (i+1) in the all_* arrays
  # (shelf 1 is between segment 1 and 2, etc.)
  shelf_band_lo <- band_lo
  shelf_band_hi <- band_hi
  # bias entering shelf from below = bias at top of segment below
  # shelf i is above segment i in the all_* arrays
  bias_enter_lo <- bias_at_hi[seq_len(n_shelves)]      # from segment below
  bias_enter_hi <- bias_at_lo[seq_len(n_shelves) + 1L] # from segment above

  function(t) {
    bias <- numeric(length(t))
    na_mask <- is.na(t)

    # Clean segments
    for (j in seq_len(n_all)) {
      in_seg <- !na_mask & t >= all_lo[j] & t < all_hi[j]
      bias[in_seg] <- slope * (t[in_seg] - all_mid[j])
    }

    # Shelf band transitions: smoothstep (cosine) from bias_enter_lo to bias_enter_hi
    # Cosine interpolation rounds the corners where segment slope meets band
    # transition, reducing visual voids in the corrected distribution.
    for (i in seq_len(n_shelves)) {
      in_band <- !na_mask & t >= shelf_band_lo[i] & t < shelf_band_hi[i]
      if (any(in_band)) {
        frac <- (t[in_band] - shelf_band_lo[i]) /
                (shelf_band_hi[i] - shelf_band_lo[i])
        # Cosine smoothstep: 0→1 with zero derivative at both ends
        smooth_frac <- (1 - cos(frac * pi)) / 2
        bias[in_band] <- bias_enter_lo[i] * (1 - smooth_frac) +
                         bias_enter_hi[i] * smooth_frac
      }
    }

    # Beyond virtual segments
    below <- !na_mask & t < all_lo[1L]
    bias[below] <- slope * (t[below] - all_mid[1L])

    above <- !na_mask & t >= all_hi[n_all]
    bias[above] <- slope * (t[above] - all_mid[n_all])

    bias[na_mask] <- NA_real_
    bias
  }
}


#' Convert legacy shelf format to centre/width
#' @keywords internal
.normalise_shelves <- function(shelves) {
  if ("shelf_temp" %in% names(shelves) && !"centre" %in% names(shelves)) {
    shelves$centre <- shelves$shelf_temp
    shelves$width <- shelves$band_hi - shelves$band_lo
    shelves <- shelves[, c("centre", "width")]
  }
  shelves
}


#' @keywords internal
.detect_profiles <- function(datetime, depth, gap_hours) {
  n <- length(datetime)
  if (n == 0L) return(integer(0))

  dt_num <- as.numeric(datetime)
  ord <- order(dt_num)
  dt_sorted <- dt_num[ord]
  dp_sorted <- depth[ord]

  gap_secs <- gap_hours * 3600

  dt_diff <- diff(dt_sorted)
  dp_diff <- diff(dp_sorted)

  time_gap <- is.na(dt_diff) | dt_diff > gap_secs
  depth_reset <- !is.na(dp_diff) & dp_diff < -5

  pid_sorted <- cumsum(c(1L, time_gap | depth_reset))

  pid <- integer(n)
  pid[ord] <- pid_sorted
  pid
}
