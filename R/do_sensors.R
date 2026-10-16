#' Ponsel OptoD sensor DO processing
#'
#' The Ponsel OptoD (Aqualabo) optical dissolved oxygen sensor outputs
#' DO in mg/L or %sat depending on configuration. This function handles
#' unit conversion and applies sensor-specific corrections.
#'
#' **Ponsel OptoD specifications:**
#' - Measurement principle: optical luminescence quenching
#' - Range: 0-20 mg/L (0-200% sat)
#' - Resolution: 0.01 mg/L / 0.1%
#' - Accuracy: ±0.1 mg/L (0-8 mg/L), ±0.2 mg/L (8-20 mg/L)
#' - Temperature compensation: internal NTC, automatic
#' - The sensor outputs mg/L calculated internally using its own T sensor
#'   and assumed pressure (factory default = 1013.25 hPa)
#'
#' If the sensor was deployed at altitude, the factory mg/L values are
#' biased high because the sensor assumes sea level. This function
#' recalculates using the correct pressure.
#'
#' @param do_mgl DO in mg/L as reported by the sensor
#' @param temp Water temperature in °C (from the DO sensor's thermistor)
#' @param pressure_actual Actual barometric pressure at the site (hPa).
#'   Use [do_pressure_altitude()] or [do_pressure_openmeteo()].
#' @param pressure_sensor Pressure assumed by the sensor firmware (hPa).
#'   Default 1013.25 (sea level — Ponsel factory default).
#' @param salinity Salinity in PSU (default 0)
#' @param output One of "mgl" (corrected mg/L), "psat" (percent saturation),
#'   or "both" (data frame with both)
#' @return Corrected DO values (numeric vector or data frame)
#' @export
#'
#' @examples
#' # Sensor at 300m altitude, reading 9.5 mg/L at 15°C
#' do_ponsel_optod(9.5, temp = 15,
#'                  pressure_actual = do_pressure_altitude(300))
do_ponsel_optod <- function(do_mgl, temp,
                             pressure_actual = 1013.25,
                             pressure_sensor = 1013.25,
                             salinity = 0,
                             output = "mgl") {

  # The sensor calculated mg/L using its assumed pressure.
  # Back-calculate %sat at sensor's assumed pressure, then
  # recalculate mg/L at actual pressure.
  sat_at_sensor_p <- do_sat_conc(temp, pressure_sensor, salinity)
  psat <- 100 * do_mgl / sat_at_sensor_p

  # Now calculate correct mg/L at actual pressure
  sat_at_actual_p <- do_sat_conc(temp, pressure_actual, salinity)
  mgl_corrected <- psat / 100 * sat_at_actual_p

  switch(output,
    mgl = mgl_corrected,
    psat = psat,
    both = data.frame(do_mgl = mgl_corrected, do_psat = psat),
    stop("output must be 'mgl', 'psat', or 'both'")
  )
}


#' In-Situ RDO Pro sensor DO processing
#'
#' The In-Situ RDO (Rugged Dissolved Oxygen) Pro sensor uses optical
#' luminescence quenching. It can output both mg/L and %sat.
#'
#' **In-Situ RDO Pro specifications:**
#' - Measurement principle: optical luminescence (RDO cap)
#' - Range: 0-50 mg/L
#' - Resolution: 0.01 mg/L
#' - Accuracy: ±0.1 mg/L (0-8 mg/L), ±2% of reading (8-50 mg/L)
#' - Temperature range: 0-50°C
#' - The sensor measures partial pressure of O2, converts internally
#'   to mg/L using its temperature sensor and barometric pressure setting
#' - Factory default pressure: 760 mmHg = 1013.25 hPa
#' - **Important:** The RDO measures O2 partial pressure, which is
#'   independent of altitude. %sat output is correct regardless of
#'   pressure setting. Only mg/L output needs pressure correction.
#'
#' @param do_value DO value from the sensor
#' @param temp Water temperature in °C
#' @param input_unit Input unit: "mgl" or "psat"
#' @param pressure_actual Actual barometric pressure at site (hPa)
#' @param pressure_sensor Pressure configured in the sensor (hPa, default 1013.25)
#' @param salinity Salinity in PSU (default 0)
#' @param output One of "mgl", "psat", or "both"
#' @return Corrected DO values
#' @export
#'
#' @examples
#' # RDO reading 95% sat at 12°C, site at 350m altitude
#' do_insitu_rdo(95, temp = 12, input_unit = "psat",
#'                pressure_actual = do_pressure_altitude(350))
do_insitu_rdo <- function(do_value, temp,
                           input_unit = "psat",
                           pressure_actual = 1013.25,
                           pressure_sensor = 1013.25,
                           salinity = 0,
                           output = "mgl") {

  if (input_unit == "psat") {
    # %sat from RDO is correct regardless of pressure setting
    # (sensor measures O2 partial pressure directly)
    psat <- do_value
  } else if (input_unit == "mgl") {
    # mg/L was calculated by sensor using its pressure setting
    # Back-calculate true %sat
    sat_at_sensor_p <- do_sat_conc(temp, pressure_sensor, salinity)
    psat <- 100 * do_value / sat_at_sensor_p
  } else {
    stop("input_unit must be 'mgl' or 'psat'")
  }

  # Calculate mg/L at actual pressure
  sat_at_actual_p <- do_sat_conc(temp, pressure_actual, salinity)
  mgl_corrected <- psat / 100 * sat_at_actual_p

  switch(output,
    mgl = mgl_corrected,
    psat = psat,
    both = data.frame(do_mgl = mgl_corrected, do_psat = psat),
    stop("output must be 'mgl', 'psat', or 'both'")
  )
}
