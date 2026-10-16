#' Build mirrored density histogram data
#'
#' Internal helper that creates the data.frame for a mirrored density plot.
#' Used by [plot_correction()], [plot_compare()], and [summary_correction()].
#'
#' @param tmp_before Numeric vector of original temperatures.
#' @param tmp_after Numeric vector of corrected temperatures.
#' @param bin_width Histogram bin width. Default 0.05.
#'
#' @return A data.frame with columns: temp, density (negative for Before),
#'   panel ("Before" or "After").
#'
#' @keywords internal
.mirror_hist_data <- function(tmp_before, tmp_after, bin_width = 0.05) {
  all_vals <- c(tmp_before[!is.na(tmp_before)], tmp_after[!is.na(tmp_after)])
  breaks <- seq(
    floor(min(all_vals) / bin_width) * bin_width,
    ceiling(max(all_vals) / bin_width) * bin_width,
    by = bin_width
  )

  h_before <- graphics::hist(tmp_before[!is.na(tmp_before)],
                              breaks = breaks, plot = FALSE)
  h_after <- graphics::hist(tmp_after[!is.na(tmp_after)],
                             breaks = breaks, plot = FALSE)

  data.frame(
    temp = rep(h_before$mids, 2),
    density = c(-h_before$counts, h_after$counts),
    panel = rep(c("Before", "After"), each = length(h_before$mids))
  )
}


#' Build a mirrored density comparison plot
#'
#' Internal helper that creates a complete ggplot from mirrored histogram data.
#' Before density points downward (grey), After density points upward (blue).
#'
#' @param tmp_before Numeric vector of original temperatures.
#' @param tmp_after Numeric vector of corrected temperatures.
#' @param shelves Data.frame of shelves (needs \code{shelf_temp}).
#' @param bin_width Histogram bin width. Default 0.05.
#' @param title Plot title.
#'
#' @return A ggplot object.
#'
#' @keywords internal
.cdf_plot <- function(tmp_before, tmp_after, shelves = NULL,
                      bin_width = 0.05, title = "Density comparison") {

  df <- .mirror_hist_data(tmp_before, tmp_after, bin_width)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$temp, y = .data$density,
                                         fill = .data$panel)) +
    ggplot2::geom_col(width = bin_width, colour = NA) +
    ggplot2::scale_fill_manual(
      values = c(Before = "grey70", After = "steelblue"),
      name = NULL) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::labs(title = title,
                  x = "Temperature (\u00b0C)", y = "Count") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom")

  data_range <- range(c(tmp_before[!is.na(tmp_before)],
                        tmp_after[!is.na(tmp_after)]))

  if (!is.null(shelves) && nrow(shelves) > 0L) {
    shelf_breaks <- if ("centre" %in% names(shelves)) shelves$centre else shelves$shelf_temp
    shelf_labels <- sprintf("%.1f", shelf_breaks)
    p <- p +
      ggplot2::scale_x_continuous(
        limits = data_range,
        sec.axis = ggplot2::dup_axis(
          breaks = shelf_breaks, labels = shelf_labels, name = NULL)) +
      ggplot2::theme(
        axis.ticks.x.top = ggplot2::element_line(colour = "red",
                                                  linewidth = 1),
        axis.text.x.top = ggplot2::element_text(colour = "red", size = 7))
  } else {
    p <- p + ggplot2::scale_x_continuous(limits = data_range)
  }

  p
}
