#' Plot Shelf Detection Diagnostics
#'
#' Displays a histogram of temperature readings with a loess-smoothed baseline
#' overlay. Detected shelf bands are highlighted. If shelves are found, zoom
#' panels are shown for each shelf region.
#'
#' @param tmp Numeric vector of temperature values.
#' @param shelves Optional data.frame of shelves (output of [find_shelves()] or
#'   [define_shelves()]). If NULL, [find_shelves()] is called automatically.
#' @param bin_width Bin width in degrees C for histogram. Default 0.05.
#' @param loess_span Span parameter for loess smooth. If NULL (default),
#'   auto-scaled so the smoothing window is approximately 1 deg C.
#' @param title Title for the plot.
#' @param save_path Optional file path to save the plot. If NULL (default),
#'   the plot is displayed but not saved.
#'
#' @return The ggplot object (invisibly).
#'
#' @export
plot_shelves <- function(tmp,
                         shelves = NULL,
                         bin_width = 0.05,
                         loess_span = NULL,
                         title = "Shelf detection",
                         save_path = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plot_shelves()")
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required for plot_shelves()")

  tmp <- tmp[!is.na(tmp)]
  tmp <- tmp[tmp >= 4 & tmp <= 28]

  if (length(tmp) < 100) {
    message("Too few valid temperature values to plot.")
    return(invisible(NULL))
  }

  # Build histogram data
  breaks <- seq(
    floor(min(tmp) / bin_width) * bin_width,
    ceiling(max(tmp) / bin_width) * bin_width,
    by = bin_width
  )
  h <- graphics::hist(tmp, breaks = breaks, plot = FALSE)
  temps <- h$mids
  counts <- h$counts

  # Loess baseline
  if (is.null(loess_span)) {
    loess_span <- max(1.0 / (max(temps) - min(temps)), 0.02)
  }
  fit <- stats::loess(counts ~ temps, span = loess_span,
                      degree = 2, family = "symmetric")
  baseline <- pmax(stats::predict(fit, temps), 0)

  if (is.null(shelves)) {
    shelves <- find_shelves(tmp, bin_width = bin_width, loess_span = loess_span)
  }

  hdf <- data.frame(temp = temps, count = counts, baseline = baseline)

  p_full <- ggplot2::ggplot(hdf, ggplot2::aes(x = .data$temp, y = .data$count)) +
    ggplot2::geom_col(width = bin_width, fill = "steelblue", alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = .data$baseline), colour = "orange",
                       linewidth = 0.7) +
    ggplot2::labs(title = paste0(title, " \u2014 full distribution"),
                  x = "Temperature (\u00b0C)", y = "Count") +
    ggplot2::theme_minimal()

  if (nrow(shelves) > 0) {
    shelf_rects <- data.frame(
      xmin = shelves$band_lo, xmax = shelves$band_hi,
      label = sprintf("%.2f", shelves$shelf_temp)
    )
    p_full <- p_full +
      ggplot2::geom_rect(data = shelf_rects,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                     ymin = 0, ymax = Inf),
        inherit.aes = FALSE, fill = "tomato", alpha = 0.2) +
      ggplot2::geom_vline(xintercept = shelves$shelf_temp,
        linetype = "dashed", colour = "red", linewidth = 0.4) +
      ggplot2::geom_text(data = shelf_rects,
        ggplot2::aes(x = (.data$xmin + .data$xmax) / 2, y = Inf,
                     label = .data$label),
        inherit.aes = FALSE, vjust = 1.5, size = 3, colour = "red")

    # Zoom panels for each shelf
    zoom_panels <- list()
    for (i in seq_len(nrow(shelves))) {
      st <- shelves$shelf_temp[i]
      margin <- 1.5
      zoom_panels[[i]] <- ggplot2::ggplot(hdf,
          ggplot2::aes(x = .data$temp, y = .data$count)) +
        ggplot2::annotate("rect",
          xmin = shelves$band_lo[i], xmax = shelves$band_hi[i],
          ymin = -Inf, ymax = Inf, fill = "tomato", alpha = 0.2) +
        ggplot2::geom_col(width = bin_width, fill = "steelblue",
                          alpha = 0.8) +
        ggplot2::geom_line(ggplot2::aes(y = .data$baseline),
                           colour = "orange", linewidth = 0.7) +
        ggplot2::geom_vline(xintercept = st, linetype = "dashed",
          colour = "red", linewidth = 0.4) +
        ggplot2::geom_vline(
          xintercept = c(shelves$band_lo[i], shelves$band_hi[i]),
          linetype = "dotted", colour = "tomato", linewidth = 0.5) +
        ggplot2::coord_cartesian(xlim = c(st - margin, st + margin)) +
        ggplot2::labs(
          title = sprintf("Shelf %.2f\u00b0C (ratio %.1f)",
                          st, shelves$ratio[i]),
          subtitle = sprintf("Band [%.2f, %.2f]",
            shelves$band_lo[i], shelves$band_hi[i]),
          x = "Temperature (\u00b0C)", y = "Count") +
        ggplot2::theme_minimal()
    }

    n_shelves <- nrow(shelves)
    zoom_nrow <- if (n_shelves > 3) 2 else 1
    combined <- p_full / patchwork::wrap_plots(zoom_panels, nrow = zoom_nrow)
    height <- 4 + 4 * zoom_nrow
  } else {
    combined <- p_full
    height <- 5
  }

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined, width = 14, height = height, dpi = 150)
  }

  print(combined)
  invisible(combined)
}
