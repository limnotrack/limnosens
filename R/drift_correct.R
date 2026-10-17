#' Piecewise-linear drift correction from reference measurements
#'
#' Builds a time-varying correction factor from sensor reference readings
#' (buffer checks, standard solutions, saturated water tests) and applies
#' it to a variable. Between reference points the correction is linearly
#' interpolated; before the first and after the last it is held constant
#' (no extrapolation).
#'
#' The correction model depends on the variable:
#' \describe{
#'   \item{Multiplicative (span drift)}{For DO (\code{c_dosat}, \code{c_do}):
#'     \code{corrected = measured / gain}, where
#'     \code{gain = value_measured / value_actual} at each reference point.}
#'   \item{Additive (offset drift)}{For pH, conductivity:
#'     \code{corrected = measured - offset}, where
#'     \code{offset = value_measured - value_actual}.}
#' }
#'
#' @param df Data frame with a datetime column and the variable to correct.
#' @param var Column name of the variable to correct.
#' @param site Site code (used to filter reference data).
#' @param device_id Device ID to filter references for (optional — if NULL,
#'   uses all references for the site + variable).
#' @param refs Data frame of sensor reference readings. If \code{NULL}, reads
#'   from \code{ltvocab::vocab_metadata("sensor_reference")}.
#' @param col_datetime Datetime column name (default \code{"datetime"}).
#' @param tz Timezone (default \code{"Etc/GMT-12"}).
#' @param model Correction model: \code{"auto"} (default, selects based on
#'   variable family), \code{"multiplicative"}, or \code{"additive"}.
#' @return \code{df} with the variable column corrected in place.
#'   Attribute \code{"drift_applied"} records the correction parameters.
#'
#' @export
apply_drift <- function(df, var, site, device_id = NULL, refs = NULL,
                        col_datetime = "datetime", tz = "Etc/GMT-12",
                        model = c("auto", "multiplicative", "additive")) {
  model <- match.arg(model)

  if (is.null(refs)) {
    if (!requireNamespace("ltvocab", quietly = TRUE))
      stop("ltvocab required to read sensor_reference")
    refs <- ltvocab::vocab_metadata("sensor_reference")
  }

  # Filter to site + variable
  r <- refs[refs$site == site, , drop = FALSE]
  # Match variable by prefix (e.g. "c_dosat" matches rows with variable "c_dosat")
  var_base <- sub("_[a-z]+$", "", var)  # strip unit suffix for matching
  r <- r[!is.na(r$variable) & startsWith(r$variable, var_base), , drop = FALSE]
  if (!is.null(device_id))
    r <- r[!is.na(r$device_id) & r$device_id == device_id, , drop = FALSE]

  # Need both actual and measured values
  r$value_actual   <- suppressWarnings(as.numeric(r$value_actual))
  r$value_measured <- suppressWarnings(as.numeric(r$value_measured))
  r <- r[!is.na(r$value_actual) & !is.na(r$value_measured), , drop = FALSE]
  if (nrow(r) == 0) {
    message("No matching reference data for ", var, " at ", site)
    return(df)
  }

  # Parse dates and sort
  r$dt <- as.POSIXct(r$date, tz = tz)
  r <- r[order(r$dt), , drop = FALSE]

  # Auto-select model
  if (model == "auto") {
    model <- if (grepl("^c_do", var_base)) "multiplicative" else "additive"
  }

  # Compute correction at each reference point
  if (model == "multiplicative") {
    r$correction <- r$value_measured / r$value_actual
    r$correction[!is.finite(r$correction)] <- 1
  } else {
    r$correction <- r$value_measured - r$value_actual
    r$correction[!is.finite(r$correction)] <- 0
  }

  # Interpolate correction to each data timestamp
  dt_num <- as.numeric(df[[col_datetime]])
  ref_num <- as.numeric(r$dt)

  corr <- stats::approx(ref_num, r$correction, xout = dt_num,
                         rule = 2)$y  # rule=2: constant extrapolation

  # Apply
  if (model == "multiplicative") {
    df[[var]] <- df[[var]] / corr
  } else {
    df[[var]] <- df[[var]] - corr
  }

  attr(df, "drift_applied") <- list(
    var = var, site = site, model = model,
    n_refs = nrow(r),
    ref_dates = r$dt,
    corrections = r$correction
  )
  df
}


#' Pressure-compensated DO calculation
#'
#' Converts dissolved oxygen between units with full pressure and temperature
#' compensation. Handles the common case where a sensor reports %sat at its
#' internal reference pressure (typically sea level) and you need mg/L at
#' actual site pressure.
#'
#' @param dosat Numeric vector of DO percent saturation.
#' @param temp Numeric vector of water temperature (deg C).
#' @param pressure_actual Actual barometric pressure at the site (hPa).
#'   Use [do_pressure_altitude()] for a fixed estimate, or a live
#'   \code{pr_baro_hpa} column for per-observation correction.
#' @param pressure_sensor Pressure assumed by the sensor firmware (hPa).
#'   Default 1013.25 (sea level). Only needed if the sensor applies its
#'   own pressure correction internally.
#' @param salinity Salinity in PSU (default 0).
#' @param output One of \code{"mgl"} (default), \code{"psat"} (re-referenced
#'   to actual pressure), or \code{"both"} (data frame).
#' @return Numeric vector (or data frame if \code{output = "both"}).
#' @export
compensate_do <- function(dosat, temp,
                          pressure_actual = 1013.25,
                          pressure_sensor = 1013.25,
                          salinity = 0,
                          output = c("mgl", "psat", "both")) {
  output <- match.arg(output)

  # True %sat referenced to actual pressure
  dosat_true <- dosat * pressure_actual / pressure_sensor

  # mg/L from true %sat
  do_sat_mg <- do_sat_conc(temp, pressure_actual, salinity)
  do_mgl <- dosat_true / 100 * do_sat_mg

  switch(output,
    mgl  = do_mgl,
    psat = dosat_true,
    both = data.frame(c_do_mgl = do_mgl, c_dosat_psat = dosat_true)
  )
}
