#' Detect Sensor Changes from Shelf Presence Over Time
#'
#' Scans a temperature time series in rolling windows to detect when
#' shelf artifacts appear or disappear, indicating a sensor swap.
#' Produces a time series plot with vertical lines at potential swap dates.
#'
#' @param datetime POSIXct vector of timestamps.
#' @param depth Numeric vector of depth values.
#' @param tmp Numeric vector of temperature values.
#' @param window_days Size of the rolling window in days. Default 30.
#' @param step_days Step size between windows in days. Default 7.
#' @param min_ratio Minimum ratio for shelf detection. Default 2.5.
#' @param title Plot title.
#' @param save_path Optional file path to save the plot.
#'
#' @return A data.frame with columns: window_start, window_end, n_shelves,
#'   shelf_temps (comma-separated). Also prints and optionally saves a
#'   diagnostic plot.
#'
#' @export
detect_sensor_change <- function(datetime, depth, tmp,
                                 window_days = 30,
                                 step_days = 7,
                                 min_ratio = 2.5,
                                 title = "Sensor change detection",
                                 save_path = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required")
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required")

  use_scattermore <- requireNamespace("scattermore", quietly = TRUE)

  dt_range <- range(datetime, na.rm = TRUE)
  window_secs <- window_days * 86400
  step_secs <- step_days * 86400

  starts <- seq(as.numeric(dt_range[1]),
                as.numeric(dt_range[2]) - window_secs,
                by = step_secs)

  results <- list()

  for (i in seq_along(starts)) {
    w_start <- as.POSIXct(starts[i], origin = "1970-01-01",
                          tz = attr(datetime, "tzone") %||% "")
    w_end <- w_start + window_secs

    in_window <- !is.na(datetime) & datetime >= w_start & datetime < w_end
    tmp_w <- tmp[in_window]
    tmp_w <- tmp_w[!is.na(tmp_w) & tmp_w > 5 & tmp_w < 30]

    if (length(tmp_w) < 200) {
      results[[i]] <- data.frame(
        window_start = w_start, window_end = w_end,
        n_shelves = NA_integer_, shelf_temps = ""
      )
      next
    }

    shelves <- tryCatch(
      find_shelves(tmp_w, min_ratio = min_ratio),
      error = function(e) data.frame(shelf_temp = numeric())
    )

    results[[i]] <- data.frame(
      window_start = w_start, window_end = w_end,
      n_shelves = nrow(shelves),
      shelf_temps = paste(sprintf("%.1f", shelves$shelf_temp), collapse = ",")
    )
  }

  res <- do.call(rbind, results)
  res$window_mid <- res$window_start + window_secs / 2

  # Detect transitions: where n_shelves changes significantly
  res$change <- FALSE
  if (nrow(res) > 1) {
    n_prev <- c(NA, res$n_shelves[-nrow(res)])
    res$change <- !is.na(res$n_shelves) & !is.na(n_prev) &
      abs(res$n_shelves - n_prev) >= 2
  }

  swap_dates <- res$window_mid[res$change]

  # Print summary
  cat(sprintf("\n  Scanned %d windows (%d-day, %d-day step)\n",
    nrow(res), window_days, step_days))
  if (length(swap_dates) > 0) {
    cat(sprintf("  Potential sensor changes:\n"))
    for (d in swap_dates) {
      cat(sprintf("    %s\n", format(as.POSIXct(d, origin = "1970-01-01"))))
    }
  } else {
    cat("  No sensor changes detected.\n")
  }

  # Plot
  # Top: shelf count over time
  p_count <- ggplot2::ggplot(res, ggplot2::aes(x = .data$window_mid,
                                                y = .data$n_shelves)) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.5) +
    ggplot2::geom_point(colour = "steelblue", size = 1) +
    ggplot2::labs(title = title, x = NULL, y = "Shelves detected") +
    ggplot2::theme_minimal(base_size = 10)

  if (length(swap_dates) > 0) {
    p_count <- p_count +
      ggplot2::geom_vline(xintercept = swap_dates, colour = "red",
                          linewidth = 0.5, alpha = 0.7)
  }

  # Bottom: temperature time series with swap lines
  df_ts <- data.frame(datetime = datetime, depth = depth, tmp = tmp)
  df_ts <- df_ts[!is.na(df_ts$tmp) & !is.na(df_ts$depth), ]

  p_ts <- ggplot2::ggplot(df_ts, ggplot2::aes(x = .data$datetime,
                                                y = .data$tmp,
                                                colour = .data$depth))

  if (use_scattermore) {
    p_ts <- p_ts +
      scattermore::geom_scattermore(pointsize = 2, pixels = c(2000, 800))
  } else {
    p_ts <- p_ts +
      ggplot2::geom_point(size = 0.3, alpha = 0.5)
  }

  p_ts <- p_ts +
    ggplot2::scale_colour_viridis_c(option = "C", direction = -1,
                                    name = "Depth (m)") +
    ggplot2::labs(x = NULL, y = "Temperature (\u00b0C)") +
    ggplot2::theme_minimal(base_size = 10)

  if (length(swap_dates) > 0) {
    p_ts <- p_ts +
      ggplot2::geom_vline(xintercept = swap_dates, colour = "red",
                          linewidth = 0.5, alpha = 0.7)
  }

  combined <- p_count / p_ts + patchwork::plot_layout(heights = c(1, 2))

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined, width = 16, height = 8, dpi = 150)
  }

  print(combined)
  invisible(res)
}
