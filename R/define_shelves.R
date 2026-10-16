#' Interactively Define Shelf Temperatures (Two-Stage)
#'
#' Stage 1: Identify shelf centres from a broad histogram with loess baseline.
#' Auto-detected candidates are shown; user can accept, remove, or add centres.
#'
#' Stage 2: For each confirmed centre, a fine histogram (±1 deg C, bin_width
#' = 0.01) is shown to precisely determine the band edges. Default band width
#' is 0.1 deg C centred on the peak.
#'
#' @param tmp Numeric vector of temperature values.
#' @param bin_width Bin width for stage 1 detection. Default 0.05.
#' @param loess_span Span parameter for loess smooth.
#' @param min_ratio Minimum ratio for shelf detection. Default 2.5.
#' @param max_shelf_band Maximum shelf half-width for detection. Default 0.2.
#'
#' @return A data.frame with columns: centre, width.
#'
#' @export
define_shelves <- function(tmp,
                           bin_width = 0.05,
                           loess_span = NULL,
                           min_ratio = 2.5,
                           max_shelf_band = 0.2) {

  if (!interactive()) stop("define_shelves() requires an interactive session.")

  # ==========================================================
  # Stage 1: Identify shelf centres
  # ==========================================================

  shelves <- find_shelves(tmp, bin_width = bin_width, loess_span = loess_span,
                          min_ratio = min_ratio, max_shelf_band = max_shelf_band)

  # Extract just the centres for stage 1
  centres <- shelves$shelf_temp

  # Build base histogram for stage 1
  tmp_clean <- tmp[!is.na(tmp) & tmp >= 4 & tmp <= 28]
  breaks <- seq(
    floor(min(tmp_clean) / bin_width) * bin_width,
    ceiling(max(tmp_clean) / bin_width) * bin_width,
    by = bin_width
  )
  h <- graphics::hist(tmp_clean, breaks = breaks, plot = FALSE)
  temps <- h$mids
  counts <- h$counts

  # Loess baseline
  if (is.null(loess_span)) {
    loess_span <- max(1.0 / (max(temps) - min(temps)), 0.02)
  }
  fit <- stats::loess(counts ~ temps, span = loess_span,
                      degree = 2, family = "symmetric")
  baseline <- pmax(stats::predict(fit, temps), 0)

  hdf <- data.frame(temp = temps, count = counts, baseline = baseline)

  repeat {
    p <- ggplot2::ggplot(hdf, ggplot2::aes(x = .data$temp, y = .data$count)) +
      ggplot2::geom_col(width = bin_width, fill = "steelblue", alpha = 0.8) +
      ggplot2::geom_line(ggplot2::aes(y = .data$baseline), colour = "orange",
                         linewidth = 0.7) +
      ggplot2::labs(title = "Stage 1: Identify shelf centres",
                    x = "Temperature (\u00b0C)", y = "Count") +
      ggplot2::theme_minimal(base_size = 11)
    if (length(centres) > 0L) {
      centre_df <- data.frame(x = centres,
                               label = sprintf("%.2f", centres))
      p <- p +
        ggplot2::geom_vline(xintercept = centres, colour = "red",
                            linewidth = 0.6) +
        ggplot2::geom_text(data = centre_df,
          ggplot2::aes(x = .data$x, y = Inf, label = .data$label),
          inherit.aes = FALSE, vjust = 1.5, size = 3, colour = "red")
    }
    print(p)

    if (length(centres) == 0L) {
      cat("\n  No shelves detected.\n")
    } else {
      cat(sprintf("\n  Stage 1: %d shelf centre(s):\n", length(centres)))
      for (i in seq_along(centres)) {
        cat(sprintf("    %d. %.2f\u00b0C\n", i, centres[i]))
      }
    }

    cat("\n  [a] Accept  [r] Remove  [d] Add  [t] Re-detect\n")
    choice <- tolower(trimws(readline("  Choice: ")))

    if (choice == "a") break
    else if (choice == "r" && length(centres) > 0L) {
      idx <- .ask_index("  Remove which? ", length(centres))
      if (!is.na(idx)) centres <- centres[-idx]
    } else if (choice == "d") {
      temp <- .ask_numeric("  Centre temperature: ")
      if (!is.na(temp)) centres <- sort(c(centres, temp))
    } else if (choice == "t") {
      new_ratio <- .ask_numeric(sprintf("  min_ratio [%.1f]: ", min_ratio))
      if (!is.na(new_ratio)) min_ratio <- new_ratio
      shelves_det <- find_shelves(tmp, bin_width = bin_width,
        loess_span = loess_span, min_ratio = min_ratio,
        max_shelf_band = max_shelf_band)
      centres <- shelves_det$shelf_temp
    }
  }

  if (length(centres) == 0L) {
    return(data.frame(centre = numeric(), width = numeric()))
  }

  # Build shelves data.frame with default 0.1°C width
  shelves <- data.frame(
    centre = centres,
    width = rep(0.1, length(centres))
  )

  if (nrow(shelves) == 0L) {
    shelves <- check_shelves(shelves)
    return(shelves)
  }

  # ==========================================================
  # Stage 2: Refine band edges for each shelf
  # ==========================================================

  cat("\n  Stage 2: Refining band edges (centre + width)...\n")
  tmp_clean <- tmp[!is.na(tmp)]

  for (i in seq_len(nrow(shelves))) {
    centre <- shelves$centre[i]
    width <- shelves$width[i]

    repeat {
      shelves$centre[i] <- centre
      shelves$width[i] <- width
      band_lo <- centre - width / 2
      band_hi <- centre + width / 2

      # Fine histogram ±1°C around centre
      fine_bw <- 0.01
      zoom_vals <- tmp_clean[tmp_clean >= centre - 1 & tmp_clean <= centre + 1]

      if (length(zoom_vals) < 50) {
        cat(sprintf("    Shelf %.2f: too few values for zoom, keeping defaults.\n",
          centre))
        break
      }

      breaks <- seq(centre - 1, centre + 1, by = fine_bw)
      h <- graphics::hist(zoom_vals, breaks = breaks, plot = FALSE)

      hdf <- data.frame(temp = h$mids, count = h$counts)
      p <- ggplot2::ggplot(hdf, ggplot2::aes(x = .data$temp, y = .data$count)) +
        ggplot2::geom_col(width = fine_bw, fill = "steelblue", alpha = 0.8) +
        ggplot2::geom_vline(xintercept = centre, colour = "red",
                            linewidth = 0.5, linetype = "dashed") +
        ggplot2::geom_vline(
          xintercept = c(band_lo, band_hi),
          colour = "orange", linewidth = 0.8) +
        ggplot2::labs(
          title = sprintf("Shelf %d/%d: centre=%.3f  width=%.3f  [%.3f, %.3f]",
            i, nrow(shelves), centre, width, band_lo, band_hi),
          subtitle = "Red = centre, Orange = band edges",
          x = "Temperature (\u00b0C)", y = "Count") +
        ggplot2::theme_minimal(base_size = 11)
      print(p)

      cat(sprintf("\n    Shelf %d: centre=%.3f  width=%.3f\n", i, centre, width))
      cat("    [a] Accept  [c] Change centre  [w] Change width\n")

      choice <- tolower(trimws(readline("    Choice: ")))

      if (choice == "a") {
        break
      } else if (choice == "c") {
        new_c <- .ask_numeric(sprintf("    Centre [%.3f]: ", centre))
        if (!is.na(new_c)) centre <- new_c
      } else if (choice == "w") {
        new_w <- .ask_numeric(sprintf("    Width [%.3f]: ", width))
        if (!is.na(new_w)) width <- new_w
      }
    }
  }

  shelves <- check_shelves(shelves)
  shelves
}


# --- internal helpers ---

#' @keywords internal
.ask_numeric <- function(prompt) {
  val <- readline(prompt)
  val <- trimws(val)
  if (val == "") return(NA_real_)
  suppressWarnings({ num <- as.numeric(val) })
  if (is.na(num)) {
    message("  Invalid number, skipping.")
    return(NA_real_)
  }
  num
}

#' @keywords internal
.ask_index <- function(prompt, max_idx) {
  val <- readline(prompt)
  val <- trimws(val)
  suppressWarnings({ idx <- as.integer(val) })
  if (is.na(idx) || idx < 1L || idx > max_idx) {
    message("  Invalid index, skipping.")
    return(NA_integer_)
  }
  idx
}
