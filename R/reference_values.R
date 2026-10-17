#' pH of NIST standard buffer at a given temperature
#'
#' Returns the true pH of a NIST primary standard buffer solution at
#' a specified temperature. Buffer pH varies with temperature — a
#' buffer labelled "pH 7" is only exactly 7.000 at 25 deg C.
#'
#' Values are linearly interpolated from the NIST certification tables
#' (Buck et al. 2002, IUPAC Recommendations, Table 2). Valid 0–50 deg C.
#'
#' @param nominal Numeric vector. Labelled buffer value: one of
#'   4, 7, 9.18, or 10. Matched to nearest NIST standard.
#' @param temp_c Numeric vector. Temperature in degrees Celsius (0–50).
#' @return Numeric vector of true pH at the given temperature.
#'   Vectorised over both inputs (recycled to common length).
#'
#' @references
#' Buck, R.P. et al. (2002). Measurement of pH. Definition, standards,
#' and procedures (IUPAC Recommendations 2002). *Pure Appl. Chem.*,
#' 74(11), 2169--2200. Table 2.
#'
#' @export
#'
#' @examples
#' buffer_ph_at_temp(7, 25)    # 7.000
#' buffer_ph_at_temp(7, 15)    # 7.036
#' buffer_ph_at_temp(4, 25)    # 4.005
#' buffer_ph_at_temp(c(4, 7, 10), 15)
buffer_ph_at_temp <- function(nominal, temp_c) {
  n <- max(length(nominal), length(temp_c))
  nominal <- rep_len(nominal, n)
  temp_c  <- rep_len(temp_c, n)

  result <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    nom <- nominal[i]
    tc  <- temp_c[i]
    if (is.na(nom) || is.na(tc)) next
    result[i] <- .nist_buffer_ph(.match_buffer(nom), tc)
  }
  result
}


#' DO concentration at 100% air saturation (Garcia & Gordon 1992)
#'
#' Returns the dissolved oxygen solubility in mg/L at 100% air saturation
#' for freshwater, using the Garcia & Gordon (1992) equation with
#' coefficients from Benson & Krause (1984).
#'
#' Pressure correction uses the Mortimer (1981) method: solubility scales
#' with the ratio of actual to standard atmospheric pressure, corrected
#' for water vapour.
#'
#' @param temp_c Numeric vector. Water temperature in degrees Celsius.
#' @param pressure_hpa Numeric vector or scalar. Barometric pressure in hPa.
#'   If \code{NULL}, estimated from \code{elevation_m}.
#' @param elevation_m Numeric vector or scalar. Site elevation in metres
#'   above sea level. Used only if \code{pressure_hpa} is \code{NULL}.
#' @param salinity Numeric. Salinity in PSU (default 0).
#' @return Numeric vector. DO concentration in mg/L at 100% air saturation.
#'
#' @details
#' If both \code{pressure_hpa} and \code{elevation_m} are \code{NULL},
#' sea level pressure (1013.25 hPa) is assumed.
#'
#' @references
#' Garcia, H.E. and Gordon, L.I. (1992). Oxygen solubility in seawater:
#' Better fitting equations. *Limnol. Oceanogr.*, 37(6), 1307--1312.
#'
#' @export
#'
#' @examples
#' do_saturation_100(20)                        # ~9.09 mg/L at sea level
#' do_saturation_100(15, elevation_m = 329)     # Hayes at 329m
#' do_saturation_100(10, pressure_hpa = 975)
#' do_saturation_100(c(5, 10, 15, 20, 25))
do_saturation_100 <- function(temp_c, pressure_hpa = NULL,
                              elevation_m = NULL, salinity = 0) {

  if (is.null(pressure_hpa) && is.null(elevation_m)) {
    pressure_hpa <- 1013.25
  } else if (is.null(pressure_hpa)) {
    pressure_hpa <- 1013.25 * (1 - 2.25577e-5 * elevation_m)^5.25588
  }

  T_k <- 273.15 + temp_c
  Ts  <- log((298.15 - temp_c) / T_k)

  # Garcia & Gordon (1992) — Benson & Krause coefficients
  # ln(C*) in µmol/kg, Ts-polynomial form
  A0 <-  5.80871
  A1 <-  3.20291
  A2 <-  4.17887
  A3 <-  5.10006
  A4 <- -0.0986643
  A5 <-  3.80369

  ln_C <- A0 + A1 * Ts + A2 * Ts^2 + A3 * Ts^3 + A4 * Ts^4 + A5 * Ts^5

  # Salinity correction
  if (any(salinity > 0)) {
    B0 <- -0.00624523
    B1 <- -0.00737614
    B2 <- -0.01033410
    B3 <- -0.00817083
    C0 <- -4.88682e-7
    ln_C <- ln_C - salinity * (B0 + B1 * Ts + B2 * Ts^2 + B3 * Ts^3) -
            C0 * salinity^2
  }

  # µmol/kg → mg/L (O2 = 31.9988 g/mol, freshwater ≈ 1 kg/L)
  C_mgl <- exp(ln_C) * 31.9988 / 1000

  # Pressure correction (Mortimer 1981)
  P_wv  <- exp(11.8571 - 3840.7 / T_k - 216961 / T_k^2)
  theta <- 0.000975 - 1.426e-5 * temp_c + 6.436e-8 * temp_c^2
  P_atm <- pressure_hpa / 1013.25

  C_mgl * P_atm * ((1 - P_wv / P_atm) * (1 - theta * P_atm)) /
    ((1 - P_wv) * (1 - theta))
}


# -- internal helpers -------------------------------------------------------

#' Match nominal buffer value to nearest NIST standard
#' @noRd
.match_buffer <- function(nom) {
  stds <- c(4.005, 7.000, 9.180, 10.012)
  stds[which.min(abs(nom - stds))]
}

#' NIST buffer pH at temperature — interpolated from certification table
#'
#' Uses linear interpolation on the NIST/IUPAC tabulated values
#' (Buck et al. 2002, Table 2). More reliable than polynomial fits
#' across the full temperature range.
#' @noRd
.nist_buffer_ph <- function(buf, tc) {
  # NIST certification table values at 5°C intervals
  # Source: Buck et al. (2002) Table 2, IUPAC Recommendations
  temps <- c(0, 5, 10, 15, 20, 25, 30, 35, 37, 40, 45, 50)

  tbl <- switch(as.character(buf),
    # Phthalate (KHP) — SRM 185i
    "4.005" = c(4.003, 3.999, 3.998, 4.000, 4.002, 4.005,
                4.011, 4.018, 4.022, 4.027, 4.038, 4.050),
    # Phosphate — SRM 186II/186Ig
    "7" = c(7.118, 7.084, 7.058, 7.036, 7.021, 7.000,
            6.984, 6.971, 6.965, 6.960, 6.951, 6.944),
    # Borax — SRM 187e
    "9.18" = c(9.464, 9.392, 9.332, 9.276, 9.225, 9.180,
               9.139, 9.102, 9.088, 9.068, 9.038, 9.011),
    # Carbonate — SRM 191/192
    "10.012" = c(10.317, 10.245, 10.179, 10.118, 10.062, 10.012,
                 9.966, 9.926, 9.910, 9.889, 9.856, 9.828),
    NULL
  )

  if (is.null(tbl)) return(NA_real_)

  # Clamp to table range then interpolate
  tc <- max(0, min(50, tc))
  stats::approx(temps, tbl, xout = tc, rule = 2)$y
}
