#' Estimate barometric pressure from altitude
#'
#' Uses the barometric formula (international standard atmosphere) to
#' estimate mean barometric pressure at a given altitude. Suitable as
#' a default when no measured pressure is available.
#'
#' @param altitude_m Altitude in metres above sea level
#' @param temp_c Optional mean air temperature in °C (default 15).
#'   Affects the pressure estimate slightly.
#' @return Estimated barometric pressure in hPa (mbar)
#' @export
#'
#' @examples
#' do_pressure_altitude(0)     # sea level: 1013.25 hPa
#' do_pressure_altitude(300)   # ~977 hPa
#' do_pressure_altitude(1000)  # ~899 hPa
do_pressure_altitude <- function(altitude_m, temp_c = 15) {
  # Barometric formula: P = P0 * (1 - L*h/T0)^(g*M/(R*L))
  # P0 = 1013.25 hPa, L = 0.0065 K/m, T0 = 288.15 K (15°C)
  # g = 9.80665 m/s², M = 0.0289644 kg/mol, R = 8.31447 J/(mol·K)
  T0 <- 273.15 + temp_c
  L <- 0.0065
  g <- 9.80665
  M <- 0.0289644
  R <- 8.31447

  1013.25 * (1 - L * altitude_m / T0) ^ (g * M / (R * L))
}


#' Fetch barometric pressure from Open-Meteo API
#'
#' Retrieves hourly surface pressure data from the Open-Meteo historical
#' or forecast API for a given location and time range. Useful for
#' accurate DO saturation calculations at inland/altitude sites.
#'
#' Requires the `httr2` and `jsonlite` packages.
#'
#' @param lat Latitude (decimal degrees)
#' @param lon Longitude (decimal degrees)
#' @param date_start Start date (Date or character "YYYY-MM-DD")
#' @param date_end End date (Date or character "YYYY-MM-DD")
#' @param hourly If TRUE (default), return hourly data. If FALSE, return
#'   daily mean pressure.
#' @param tz Timezone for output (default "Etc/GMT-12")
#' @return Data frame with columns `datetime` and `pressure_hpa`.
#'   Returns NULL with a warning if the API call fails.
#' @export
#'
#' @examples
#' \dontrun{
#' # Okataina (~311m altitude)
#' p <- do_pressure_openmeteo(lat = -38.13, lon = 176.40,
#'                             date_start = "2025-01-01",
#'                             date_end = "2025-01-07")
#' }
do_pressure_openmeteo <- function(lat, lon, date_start, date_end,
                                   hourly = TRUE, tz = "Etc/GMT-12") {

  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    warning("Packages 'httr2' and 'jsonlite' are required for Open-Meteo API access")
    return(NULL)
  }

  date_start <- as.character(as.Date(date_start))
  date_end   <- as.character(as.Date(date_end))

  # Use archive API for past dates, forecast for recent/future
  today <- Sys.Date()
  if (as.Date(date_end) < today - 5) {
    base_url <- "https://archive-api.open-meteo.com/v1/archive"
  } else {
    base_url <- "https://api.open-meteo.com/v1/forecast"
  }

  url <- paste0(base_url,
    "?latitude=", lat,
    "&longitude=", lon,
    "&start_date=", date_start,
    "&end_date=", date_end,
    "&hourly=surface_pressure",
    "&timezone=", gsub("/", "%2F", tz)
  )

  resp <- tryCatch({
    req <- httr2::request(url)
    httr2::req_perform(req)
  }, error = function(e) {
    warning("Open-Meteo API request failed: ", conditionMessage(e))
    return(NULL)
  })

  if (is.null(resp)) return(NULL)

  body <- httr2::resp_body_json(resp)

  if (is.null(body$hourly)) {
    warning("No hourly data returned from Open-Meteo")
    return(NULL)
  }

  datetimes <- as.POSIXct(unlist(body$hourly$time), tz = tz)
  pressures <- unlist(body$hourly$surface_pressure)

  df <- data.frame(datetime = datetimes, pressure_hpa = pressures)

  if (!hourly) {
    df$date <- as.Date(df$datetime)
    df <- stats::aggregate(pressure_hpa ~ date, data = df, FUN = mean)
    names(df) <- c("date", "pressure_hpa")
  }

  df
}
