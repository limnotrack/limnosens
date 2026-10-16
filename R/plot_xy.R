#' Plot Raw vs Corrected Temperature Scatter
#'
#' Produces a scatterplot of raw temperature (x) against corrected
#' temperature (y) with a 1:1 reference line and shelf positions marked.
#' Uses \pkg{scattermore} when available.
#'
#' @param tmp_before Numeric vector of raw temperatures.
#' @param tmp_after Numeric vector of corrected temperatures.
#' @param shelves Data.frame of shelves (needs \code{shelf_temp}).
#' @param title Plot title.
#' @param save_path Optional file path to save the plot.
#'
#' @return The ggplot object (invisibly).
#'
#' @export
plot_xy <- function(tmp_before, tmp_after, shelves = NULL,
                    title = "Raw vs corrected",
                    save_path = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plot_xy()")

  use_scattermore <- requireNamespace("scattermore", quietly = TRUE)

  ok <- !is.na(tmp_before) & !is.na(tmp_after)
  df <- data.frame(raw = tmp_before[ok], corrected = tmp_after[ok])

  p <- ggplot2::ggplot(df,
      ggplot2::aes(x = .data$raw, y = .data$corrected))

  if (use_scattermore) {
    p <- p +
      scattermore::geom_scattermore(pointsize = 2, pixels = c(1500, 1500),
                                   color = "steelblue")
  } else {
    p <- p +
      ggplot2::geom_point(size = 0.2, alpha = 0.2, colour = "steelblue")
  }

  p <- p +
    ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.4,
                         colour = "grey40") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title,
                  x = "Raw (\u00b0C)", y = "Corrected (\u00b0C)") +
    ggplot2::theme_minimal(base_size = 12)

  if (!is.null(shelves) && nrow(shelves) > 0L) {
    p <- p +
      ggplot2::geom_vline(xintercept = if ("centre" %in% names(shelves)) shelves$centre else shelves$shelf_temp, colour = "red",
                          linewidth = 0.3, alpha = 0.5) +
      ggplot2::geom_hline(yintercept = if ("centre" %in% names(shelves)) shelves$centre else shelves$shelf_temp, colour = "red",
                          linewidth = 0.3, alpha = 0.3)
  }

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, p, width = 8, height = 8, dpi = 150)
  }

  print(p)
  invisible(p)
}
