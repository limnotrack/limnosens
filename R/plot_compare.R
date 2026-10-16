#' Compare Correction Settings
#'
#' Produces a faceted grid comparing correction results across combinations
#' of slope and band tolerance values. Slopes vary across columns, tolerances
#' across rows.
#'
#' @param datetime POSIXct vector of timestamps.
#' @param depth Numeric vector of depth values.
#' @param tmp Numeric vector of temperature values.
#' @param shelves Data.frame of shelves.
#' @param tolerances Numeric vector of band_tolerance values to compare.
#'   Default \code{c(0, 0.5, 1, 2)}.
#' @param slopes Numeric vector of slope values to compare.
#'   Default \code{-0.07}.
#' @param type Plot type: \code{"density"} for mirrored density histograms,
#'   \code{"xy"} for raw vs corrected scatter. Default \code{"density"}.
#' @param extrapolate Extrapolation mode (passed to [correct_shelves()]).
#'   Default \code{"delete"}.
#' @param profile_id Optional profile ID vector.
#' @param profile_gap_hours Time gap for profile detection. Default 2.
#' @param max_shift Maximum allowable shift (passed to [correct_shelves()]).
#'   Default 0.5.
#' @param bin_width Histogram bin width (for \code{type = "density"}).
#'   Default 0.05.
#' @param title Plot title.
#' @param save_path Optional file path to save the plot.
#'
#' @return The ggplot object (invisibly).
#'
#' @export
plot_compare <- function(datetime,
                         depth,
                         tmp,
                         shelves,
                         tolerances = c(0, 0.5, 1, 2),
                         slopes = -0.07,
                         type = c("density", "xy"),
                         extrapolate = "delete",
                         profile_id = NULL,
                         profile_gap_hours = 2,
                         max_shift = 0.5,
                         bin_width = 0.05,
                         title = NULL,
                         save_path = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plot_compare()")

  type <- match.arg(type)
  slopes <- as.numeric(slopes)
  use_scattermore <- requireNamespace("scattermore", quietly = TRUE)

  if (is.null(profile_id)) {
    profile_id <- .detect_profiles(datetime, depth, profile_gap_hours)
  }

  data_list <- list()
  k <- 0L

  for (sl in slopes) {
    for (bt in tolerances) {
      r <- correct_shelves(
        datetime = datetime, depth = depth, tmp = tmp, shelves = shelves,
        profile_id = profile_id, slope = sl, extrapolate = extrapolate,
        band_tolerance = bt, max_shift = max_shift,
        profile_gap_hours = profile_gap_hours
      )

      k <- k + 1L
      sl_label <- sprintf("slope = %.3f", sl)
      bt_label <- sprintf("tol = %.1f", bt)
      n_label <- sprintf("corrected: %d", r$n_corrected)

      if (type == "density") {
        df <- .mirror_hist_data(tmp, r$tmp, bin_width)
      } else {
        ok <- !is.na(tmp) & !is.na(r$tmp)
        df <- data.frame(raw = tmp[ok], corrected = r$tmp[ok])
      }

      df$slope <- sl_label
      df$tolerance <- bt_label
      df$label <- n_label
      data_list[[k]] <- df

      message(sprintf("  slope=%.3f tol=%.1f: %d corrected",
                       sl, bt, r$n_corrected))
    }
  }

  all_df <- do.call(rbind, data_list)
  all_df$slope <- factor(all_df$slope,
    levels = sprintf("slope = %.3f", slopes))
  all_df$tolerance <- factor(all_df$tolerance,
    levels = sprintf("tol = %.1f", tolerances))

  label_df <- unique(all_df[, c("slope", "tolerance", "label")])

  if (type == "density") {
    p <- ggplot2::ggplot(all_df,
        ggplot2::aes(x = .data$temp, y = .data$density,
                     fill = .data$panel)) +
      ggplot2::geom_col(width = bin_width, colour = NA) +
      ggplot2::scale_fill_manual(
        values = c(Before = "grey70", After = "steelblue"),
        name = NULL) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
      ggplot2::labs(x = "Temperature (\u00b0C)", y = "Count") +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(legend.position = "bottom")

    if (nrow(shelves) > 0L) {
      shelf_breaks <- shelves$shelf_temp
      shelf_labels <- sprintf("%.1f", shelf_breaks)
      p <- p +
        ggplot2::scale_x_continuous(
          sec.axis = ggplot2::dup_axis(
            breaks = shelf_breaks, labels = shelf_labels, name = NULL)) +
        ggplot2::theme(
          axis.ticks.x.top = ggplot2::element_line(colour = "red",
                                                    linewidth = 1),
          axis.text.x.top = ggplot2::element_text(colour = "red", size = 7))
    }
  } else {
    # xy scatter
    p <- ggplot2::ggplot(all_df,
        ggplot2::aes(x = .data$raw, y = .data$corrected))

    if (use_scattermore) {
      p <- p +
        scattermore::geom_scattermore(pointsize = 1, pixels = c(800, 800),
                                     color = "steelblue")
    } else {
      p <- p +
        ggplot2::geom_point(size = 0.1, alpha = 0.1, colour = "steelblue")
    }

    p <- p +
      ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.3,
                           colour = "grey40") +
      ggplot2::labs(x = "Raw (\u00b0C)", y = "Corrected (\u00b0C)") +
      ggplot2::theme_minimal(base_size = 10)

    if (nrow(shelves) > 0L) {
      p <- p +
        ggplot2::geom_vline(xintercept = shelves$shelf_temp, colour = "red",
                            linewidth = 0.3, alpha = 0.5) +
        ggplot2::geom_hline(yintercept = shelves$shelf_temp, colour = "red",
                            linewidth = 0.3, alpha = 0.3)
    }
  }

  # Labels
  p <- p +
    ggplot2::geom_text(data = label_df,
      ggplot2::aes(label = .data$label),
      x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
      size = 3, colour = "grey40", inherit.aes = FALSE)

  # Facet
  vary_slope <- length(slopes) > 1L
  vary_tol <- length(tolerances) > 1L

  if (vary_slope && vary_tol) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(.data$tolerance),
      cols = ggplot2::vars(.data$slope))
  } else if (vary_slope) {
    p <- p + ggplot2::facet_wrap(~ .data$slope, nrow = 1)
  } else if (vary_tol) {
    p <- p + ggplot2::facet_wrap(~ .data$tolerance, nrow = 1)
  }

  if (is.null(title)) {
    type_label <- if (type == "density") "Density" else "Raw vs corrected"
    if (vary_slope && vary_tol) {
      title <- paste0(type_label, " \u2014 slope \u00d7 tolerance")
    } else if (vary_slope) {
      title <- sprintf("%s \u2014 slope comparison (tol = %.1f)",
                       type_label, tolerances[1])
    } else {
      title <- sprintf("%s \u2014 tolerance comparison (slope = %.3f)",
                       type_label, slopes[1])
    }
  }
  p <- p + ggplot2::ggtitle(title)

  n_col <- max(length(slopes), 1L)
  n_row <- max(length(tolerances), 1L)
  panel_size <- if (type == "xy") 5 else 4

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, p,
                    width = panel_size * n_col + 1,
                    height = panel_size * n_row + 1, dpi = 150)
  }

  print(p)
  invisible(p)
}
