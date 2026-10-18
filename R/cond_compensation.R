#' Convert between conductivity and specific conductance
#'
#' Converts raw conductivity (µS/cm at measurement temperature) to
#' specific conductance (µS/cm at 25°C reference), or the reverse.
#' Uses the linear temperature compensation model standard in
#' limnological instruments.
#'
#' The relationship is:
#' \deqn{SpCond = Cond / (1 + \alpha \cdot (T - 25))}
#' \deqn{Cond = SpCond \cdot (1 + \alpha \cdot (T - 25))}
#'
#' where \eqn{\alpha} is the temperature coefficient. Most natural
#' waters fall in the range 0.019–0.020 /°C. The Ponsel C4E uses
#' 0.020 internally.
#'
#' @param cond Numeric vector. Raw conductivity in µS/cm at measurement
#'   temperature.
#' @param temp Numeric vector. Water temperature in °C.
#' @param alpha Numeric. Temperature coefficient (per °C). Default 0.020,
#'   which matches the Ponsel C4E firmware default and is standard for
#'   freshwater in the 50–500 µS/cm range. Higher TDS waters may need
#'   0.019; very dilute waters may need 0.021.
#' @param t_ref Numeric. Reference temperature (°C). Default 25.
#' @return Numeric vector. Specific conductance in µS/cm at \code{t_ref}.
#'
#' @details
#' **When to use:** Sensors that report raw (uncompensated) conductivity
#' need this conversion to produce comparable values across temperatures.
#' The C4E applies this internally and reports SpCond directly — do NOT
#' double-compensate.
#'
#' **The linear model** is adequate for freshwater in the 5–30°C range
#' typical of NZ lakes. For extreme temperatures or high-salinity water,
#' use the non-linear (ISO 7888) model via [cond_to_spcond_iso()].
#'
#' @seealso [spcond_to_cond()] for the reverse conversion,
#'   [cond_to_spcond_iso()] for the non-linear ISO 7888 model.
#'
#' @export
#'
#' @examples
#' # 200 µS/cm at 15°C → SpCond at 25°C
#' cond_to_spcond(200, temp = 15)   # ~250 µS/cm
#'
#' # Vectorised across a profile
#' cond_to_spcond(c(150, 160, 170), temp = c(20, 15, 10))
cond_to_spcond <- function(cond, temp, alpha = 0.020, t_ref = 25) {
  cond / (1 + alpha * (temp - t_ref))
}


#' Convert specific conductance back to raw conductivity
#'
#' Reverse of [cond_to_spcond()] — takes specific conductance (at 25°C)
#' and returns what the raw conductivity would be at the measurement
#' temperature.
#'
#' @param spcond Numeric vector. Specific conductance in µS/cm at
#'   \code{t_ref}.
#' @param temp Numeric vector. Water temperature in °C.
#' @param alpha Numeric. Temperature coefficient (per °C). Default 0.020.
#' @param t_ref Numeric. Reference temperature (°C). Default 25.
#' @return Numeric vector. Raw conductivity in µS/cm at measurement
#'   temperature.
#'
#' @details
#' Useful for:
#' \itemize{
#'   \item Comparing a C4E (reports SpCond) against a sensor that reports
#'     raw Cond — convert the C4E value back to what raw Cond should be
#'     at the measurement temperature.
#'   \item Computing expected conductivity for a calibration check at a
#'     known temperature with a standard solution labelled in SpCond.
#' }
#'
#' @seealso [cond_to_spcond()]
#' @export
#'
#' @examples
#' # SpCond 250 µS/cm at 15°C → raw Cond
#' spcond_to_cond(250, temp = 15)   # ~200 µS/cm
spcond_to_cond <- function(spcond, temp, alpha = 0.020, t_ref = 25) {
  spcond * (1 + alpha * (temp - t_ref))
}


#' Non-linear temperature compensation (ISO 7888)
#'
#' Converts raw conductivity to specific conductance using the non-linear
#' model from ISO 7888:1985. More accurate than the linear model at
#' extreme temperatures or for high-TDS waters.
#'
#' The model uses a polynomial \eqn{f(T)} factor:
#' \deqn{f(T) = 1 + \alpha(T - 25) + \beta(T - 25)^2 + \gamma(T - 25)^3}
#' \deqn{SpCond = Cond / f(T)}
#'
#' Default coefficients are for a KCl-like freshwater. For specific water
#' types, determine \eqn{\alpha}, \eqn{\beta}, \eqn{\gamma} from
#' multi-temperature conductivity measurements.
#'
#' @param cond Numeric vector. Raw conductivity in µS/cm.
#' @param temp Numeric vector. Temperature in °C.
#' @param alpha Numeric. Linear coefficient. Default 0.0191.
#' @param beta Numeric. Quadratic coefficient. Default 1.288e-4.
#' @param gamma Numeric. Cubic coefficient. Default -2.422e-6.
#' @param t_ref Numeric. Reference temperature. Default 25.
#' @return Numeric vector. Specific conductance in µS/cm at \code{t_ref}.
#'
#' @seealso [cond_to_spcond()] for the simpler linear model.
#' @export
#'
#' @examples
#' # Compare linear vs non-linear at 5°C
#' cond_to_spcond(200, temp = 5)              # linear: ~333
#' cond_to_spcond_iso(200, temp = 5)          # non-linear: ~326
cond_to_spcond_iso <- function(cond, temp,
                               alpha = 0.0191, beta = 1.288e-4,
                               gamma = -2.422e-6, t_ref = 25) {
  dt <- temp - t_ref
  f  <- 1 + alpha * dt + beta * dt^2 + gamma * dt^3
  cond / f
}


#' Reverse non-linear compensation (ISO 7888)
#'
#' Converts specific conductance back to raw conductivity using the
#' ISO 7888 non-linear model.
#'
#' @inheritParams cond_to_spcond_iso
#' @param spcond Numeric vector. Specific conductance in µS/cm at
#'   \code{t_ref}.
#' @return Numeric vector. Raw conductivity in µS/cm.
#'
#' @seealso [cond_to_spcond_iso()]
#' @export
spcond_to_cond_iso <- function(spcond, temp,
                               alpha = 0.0191, beta = 1.288e-4,
                               gamma = -2.422e-6, t_ref = 25) {
  dt <- temp - t_ref
  f  <- 1 + alpha * dt + beta * dt^2 + gamma * dt^3
  spcond * f
}
