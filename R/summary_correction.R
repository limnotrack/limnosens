#' Summarise Correction Results
#'
#' Prints a breakdown of what happened to each reading (slope-adjusted,
#' interpolated, extrapolated, deleted, kept) and optionally plots three
#' diagnostic panels: difference histogram, before/after CDF, and a
#' depth vs change scatter coloured by temperature.
#'
#' @param tmp_before Numeric vector of original temperatures.
#' @param tmp_after Numeric vector of corrected temperatures.
#' @param result Output list from [correct_shelves()].
#' @param depth Optional numeric vector of depth values. If provided, a
#'   scatter plot of change vs depth (coloured by temperature) is included.
#' @param plot Logical. If TRUE (default), show diagnostic plots.
#'
#' @return A data.frame of counts (invisibly).
#'
#' @export
summary_correction <- function(tmp_before, tmp_after, result,
                               depth = NULL, plot = TRUE) {

  n <- length(tmp_before)
  valid <- !is.na(tmp_before) & !is.na(tmp_after)
  diff <- tmp_after[valid] - tmp_before[valid]

  unchanged <- sum(abs(diff) < 1e-10)
  changed <- sum(abs(diff) >= 1e-10)
  became_na <- sum(!is.na(tmp_before) & is.na(tmp_after))

  cat(sprintf("\n  Correction summary (%d readings)\n", n))
  cat(sprintf("  %-30s %7d (%5.1f%%)\n", "Total valid",
    sum(!is.na(tmp_before)), 100 * sum(!is.na(tmp_before)) / n))
  n_on_shelf <- if (!is.null(result$n_on_shelf)) result$n_on_shelf else 0L
  n_corrected <- if (!is.null(result$n_corrected)) result$n_corrected else 0L
  n_failed <- if (!is.null(result$n_failed)) result$n_failed else 0L

  cat(sprintf("  %-30s %7d (%5.1f%%)\n", "In shelf bands",
    n_on_shelf, 100 * n_on_shelf / n))
  cat(sprintf("  %-30s %7d (%5.1f%%)\n", "Interpolated/extrapolated",
    n_corrected, 100 * n_corrected / n))
  cat(sprintf("  %-30s %7d (%5.1f%%)\n", "Failed max_diff (to NA)",
    n_failed, 100 * n_failed / n))
  cat(sprintf("  %-30s %7d (%5.1f%%)\n", "Deleted (set to NA)",
    became_na, 100 * became_na / n))

  slope_only <- changed - n_corrected
  if (!is.null(result$bias_fn)) {
    cat(sprintf("  %-30s %7d (%5.1f%%)\n", "Slope-adjusted only",
      slope_only, 100 * slope_only / n))
  }

  cat(sprintf("  %-30s %7d (%5.1f%%)\n", "Unchanged",
    unchanged, 100 * unchanged / n))

  cat(sprintf("\n  Temperature shift (after - before):\n"))
  cat(sprintf("    Mean:   %+.4f\u00b0C\n", mean(diff)))
  cat(sprintf("    Median: %+.4f\u00b0C\n", median(diff)))
  cat(sprintf("    SD:      %.4f\u00b0C\n", sd(diff)))
  cat(sprintf("    Range:  [%+.4f, %+.4f]\n\n", min(diff), max(diff)))

  if (plot && requireNamespace("ggplot2", quietly = TRUE) &&
      requireNamespace("patchwork", quietly = TRUE)) {

    use_scattermore <- requireNamespace("scattermore", quietly = TRUE)

    # Panel 1: histogram of differences
    df_diff <- data.frame(diff = diff[abs(diff) >= 1e-10])
    p_hist <- NULL
    if (nrow(df_diff) > 0) {
      p_hist <- ggplot2::ggplot(df_diff, ggplot2::aes(x = .data$diff)) +
        ggplot2::geom_histogram(bins = 100, fill = "steelblue", alpha = 0.8) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "red") +
        ggplot2::coord_cartesian(xlim = range(df_diff$diff)) +
        ggplot2::labs(title = "\u0394T distribution",
                      x = "\u0394T (\u00b0C)", y = "Count") +
        ggplot2::theme_minimal(base_size = 10)
    }

    # Panel 2: mirror density
    p_cdf <- .cdf_plot(tmp_before, tmp_after, result$shelves)

    # Panel 3: change vs depth, coloured by temperature
    p_scatter <- NULL
    if (!is.null(depth)) {
      change_all <- rep(NA_real_, n)
      change_all[valid] <- diff
      ok <- valid & abs(change_all) >= 1e-10
      df_scat <- data.frame(
        change = change_all[ok],
        depth = depth[ok],
        tmp = tmp_before[ok]
      )

      p_scatter <- ggplot2::ggplot(df_scat,
          ggplot2::aes(x = .data$change, y = .data$depth,
                       colour = .data$tmp))

      if (use_scattermore) {
        p_scatter <- p_scatter +
          scattermore::geom_scattermore(pointsize = 2,
                                       pixels = c(1000, 800))
      } else {
        p_scatter <- p_scatter +
          ggplot2::geom_point(size = 0.3, alpha = 0.3)
      }

      p_scatter <- p_scatter +
        ggplot2::scale_y_reverse() +
        ggplot2::scale_x_continuous(limits = range(df_scat$change)) +
        ggplot2::scale_colour_viridis_c(option = "C", name = "T (\u00b0C)") +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            colour = "red", linewidth = 0.3) +
        ggplot2::labs(title = "\u0394T vs depth",
                      x = "\u0394T (\u00b0C)", y = "Depth (m)") +
        ggplot2::theme_minimal(base_size = 10)
    }

    # Layout: hist + scatter on top, CDF spanning bottom
    top_panels <- Filter(Negate(is.null), list(p_hist, p_scatter))
    if (length(top_panels) > 0) {
      top_row <- Reduce(`|`, top_panels)
      combined <- top_row / p_cdf + patchwork::plot_layout(heights = c(1, 1))
    } else {
      combined <- p_cdf
    }
    print(combined)
  }

  invisible(data.frame(
    n_total = n,
    n_valid = sum(!is.na(tmp_before)),
    n_on_shelf = n_on_shelf,
    n_corrected = n_corrected,
    n_failed = n_failed,
    n_deleted = became_na,
    n_slope_only = slope_only,
    n_unchanged = unchanged,
    mean_shift = mean(diff),
    median_shift = median(diff)
  ))
}
