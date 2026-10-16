#' Plot Before/After Correction Diagnostics
#'
#' Produces two diagnostic visualisations comparing temperature data before
#' and after shelf correction:
#' \enumerate{
#'   \item Depth-coloured scatter time series (before and after side by side).
#'   \item Cumulative frequency distributions (before and after overlaid),
#'     with shelf bands highlighted.
#' }
#'
#' Uses \pkg{scattermore} for fast rendering of large point clouds when
#' available; falls back to \code{geom_point()} otherwise.
#'
#' @param datetime POSIXct vector of timestamps.
#' @param depth Numeric vector of depth values.
#' @param tmp_before Numeric vector of uncorrected temperatures.
#' @param tmp_after Numeric vector of corrected temperatures.
#' @param shelves Data.frame of shelves (must have \code{band_lo},
#'   \code{band_hi} columns).
#' @param title Title prefix for the plots.
#' @param save_path Optional file path to save the combined plot. If NULL
#'   (default), the plot is displayed but not saved.
#'
#' @return The combined ggplot object (invisibly).
#'
#' @export
plot_correction <- function(datetime,
                            depth,
                            tmp_before,
                            tmp_after,
                            shelves,
                            title = "Shelf correction",
                            save_path = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plot_correction()")
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required for plot_correction()")

  use_scattermore <- requireNamespace("scattermore", quietly = TRUE)

  n_changed <- sum(!is.na(tmp_before) & !is.na(tmp_after) &
                   tmp_before != tmp_after)

  # --- Panel 1: Depth-coloured scatter time series ---
  df <- rbind(
    data.frame(datetime = datetime, depth = depth, tmp = tmp_before,
               panel = "Before"),
    data.frame(datetime = datetime, depth = depth, tmp = tmp_after,
               panel = sprintf("After (%d values corrected)", n_changed))
  )
  df$panel <- factor(df$panel, levels = unique(df$panel))

  p_scatter <- ggplot2::ggplot(df,
      ggplot2::aes(x = .data$datetime, y = .data$tmp,
                   colour = .data$depth))

  if (use_scattermore) {
    p_scatter <- p_scatter +
      scattermore::geom_scattermore(pointsize = 2, pixels = c(2000, 1000))
  } else {
    p_scatter <- p_scatter +
      ggplot2::geom_point(size = 0.3, alpha = 0.5)
  }

  # Mark shelf temperatures on a secondary y-axis
  shelf_breaks <- NULL
  shelf_labels <- NULL
  if (nrow(shelves) > 0L) {
    shelf_breaks <- if ("centre" %in% names(shelves)) shelves$centre else shelves$shelf_temp
    shelf_labels <- sprintf("%.1f", shelf_breaks)
  }

  # Data-driven axis limits
  t_range <- range(datetime, na.rm = TRUE)
  all_tmp <- c(tmp_before, tmp_after)
  tmp_range <- range(all_tmp, na.rm = TRUE)

  p_scatter <- p_scatter +
    ggplot2::scale_colour_viridis_c(option = "C", direction = -1,
                                    name = "Depth (m)") +
    ggplot2::facet_wrap(~ .data$panel, ncol = 1) +
    ggplot2::coord_cartesian(xlim = t_range, ylim = tmp_range) +
    ggplot2::labs(title = paste0(title, " \u2014 time series"),
                  x = NULL, y = "Temperature (\u00b0C)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "right")

  if (!is.null(shelf_breaks)) {
    p_scatter <- p_scatter +
      ggplot2::scale_y_continuous(
        sec.axis = ggplot2::dup_axis(
          breaks = shelf_breaks, labels = shelf_labels, name = NULL)) +
      ggplot2::theme(
        axis.ticks.y.right = ggplot2::element_line(colour = "red",
                                                   linewidth = 1),
        axis.text.y.right = ggplot2::element_text(colour = "red", size = 8))
  }

  # --- Panel 2: Mirror density ---
  p_cdf <- .cdf_plot(tmp_before, tmp_after, shelves,
                     title = paste0(title, " \u2014 density"))

  # Combine: time series on top, density on bottom
  combined <- p_scatter / p_cdf +
    patchwork::plot_layout(heights = c(2, 1))

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined, width = 14, height = 14, dpi = 150)
  }

  print(combined)
  invisible(combined)
}
