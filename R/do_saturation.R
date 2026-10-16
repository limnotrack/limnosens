#' Calculate DO saturation concentration in mg/L
#'
#' Uses the Benson & Krause (1984) equations as implemented by USGS.
#' This is the standard method used by YSI, In-Situ, and most
#' limnological instruments.
#'
#' @param temp Water temperature in °C
#' @param pressure Barometric pressure in hPa (mbar). Default 1013.25 (sea level).
#'   Use [do_pressure_altitude()] or [do_pressure_openmeteo()] to get site-specific values.
#' @param salinity Salinity in PSU (default 0 for freshwater)
#' @return DO saturation concentration in mg/L
#' @export
#'
#' @examples
#' do_sat_conc(20)                                    # 9.09 mg/L
#' do_sat_conc(10)                                    # 11.29 mg/L
#' do_sat_conc(15, pressure = do_pressure_altitude(300))  # ~9.75 mg/L
do_sat_conc <- function(temp, pressure = 1013.25, salinity = 0) {

  P_atm <- pressure / 1013.25
  T_k <- temp + 273.15

  # Henry's law constant for O2 (USGS/Benson & Krause)
  rho <- exp(-0.589581 + (326.785 / T_k) - (45284.1 / T_k^2))
  # Water vapour pressure (atm)
  P_wv <- exp(11.8571 - (3840.7 / T_k) - (216961 / T_k^2))
  # O2 Henry's law constant
  k_o <- exp(3.71814 + (5596.17 / T_k) - (1049668 / T_k^2))
  M <- 18.0152  # molecular weight of water

  # Second virial coefficient for O2
  O_o <- 0.000975 - (1.426e-5 * temp) + (6.436e-8 * temp^2)

  # Salinity correction (Benson & Krause)
  T_sc <- log((298.15 - temp) / (273.15 + temp))
  B0 <- -0.00624609; B1 <- -0.007423444
  B2 <- -0.01048635; B3 <- -0.007987907
  C0 <- -4.679983e-7
  Sc <- exp(salinity * (B0 + B1 * T_sc + B2 * T_sc^2 + B3 * T_sc^3) +
              C0 * salinity^2)

  # Saturation concentration in mg/L
  # 31998800 is a unit conversion factor (µg to mg, mol to µmol, etc.)
  31998800 * (rho * (0.20946 * (P_atm - P_wv)) / (k_o * M) *
                (1 - O_o * P_atm) * Sc)
}


#' Convert DO percent saturation to mg/L
#'
#' @param psat DO in percent saturation
#' @param temp Water temperature in °C
#' @param pressure Barometric pressure in hPa (default 1013.25)
#' @param salinity Salinity in PSU (default 0)
#' @return DO concentration in mg/L
#' @export
#'
#' @examples
#' do_psat_to_mgl(100, temp = 20)  # ~9.09 mg/L
#' do_psat_to_mgl(85, temp = 10, pressure = 960)
do_psat_to_mgl <- function(psat, temp, pressure = 1013.25, salinity = 0) {
  psat / 100 * do_sat_conc(temp, pressure, salinity)
}


#' Convert DO mg/L to percent saturation
#'
#' @param mgl DO concentration in mg/L
#' @param temp Water temperature in °C
#' @param pressure Barometric pressure in hPa (default 1013.25)
#' @param salinity Salinity in PSU (default 0)
#' @return DO in percent saturation
#' @export
#'
#' @examples
#' do_mgl_to_psat(9.09, temp = 20)  # ~100%
do_mgl_to_psat <- function(mgl, temp, pressure = 1013.25, salinity = 0) {
  100 * mgl / do_sat_conc(temp, pressure, salinity)
}
