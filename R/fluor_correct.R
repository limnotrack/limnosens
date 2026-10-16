#' Fluorescence temperature correction
#'
#' Corrects fluorescence readings for thermal quenching by normalising to a
#' reference temperature. Fluorescence intensity decreases with increasing
#' water temperature — uncorrected readings underestimate concentration in
#' warm surface water and overestimate in cold deep water.
#'
#' The linear model (default, widely used in limnology) is:
#'
#' \deqn{F_{corr} = F_{meas} / (1 + \rho \cdot (T - T_{ref}))}
#'
#' The exponential model (Watras et al. 2011) is:
#'
#' \deqn{F_{corr} = F_{meas} \cdot \exp(-\rho \cdot (T - T_{ref}))}
#'
#' Both normalise to \code{t_ref}: a reading at \code{t_ref} is unchanged.
#'
#' **Typical \eqn{\rho} values (per degree C):**
#'
#' | Channel | Sensor | \eqn{\rho} | Source |
#' |---------|--------|------------|--------|
#' | Chl-a   | Turner Cyclops-7 | -0.010 to -0.017 | Ryder et al. 2012 |
#' | Chl-a   | Chelsea TriLux   | -0.012 to -0.015 | manufacturer |
#' | Phycocyanin | Turner Cyclops-7 | -0.014 to -0.028 | Hodges et al. 2018 |
#' | fDOM    | Turner Cyclops-7 | -0.008 to -0.012 | Watras et al. 2011 |
#' | Tryptophan | Turner Cyclops-7 | -0.008 to -0.012 | similar to fDOM |
#'
#' \eqn{\rho} is negative because fluorescence decreases with temperature.
#' The correction therefore increases warm-water readings and decreases
#' cold-water readings.
#'
#' \eqn{\rho} varies with sensor model, algal community composition,
#' and dissolved organic matter character. Ideally it should be determined
#' empirically from lab incubation of site water across a temperature range.
#' See [fluor_rho()] for literature defaults per variable and manufacturer.
#'
#' **Not for turbidity.** Nephelometric turbidity (FNU/NTU) measures light
#' scattering, not fluorescence. It is not subject to thermal quenching and
#' should not be temperature-corrected with this function.
#'
#' **Chelsea TriLux note:** Check whether firmware applies internal
#' temperature compensation before applying this correction. If so,
#' correcting again would double-correct. Turner Cyclops-7, C-FLUOR, and
#' C3 sensors output raw uncompensated fluorescence.
#'
#' @param fluor Numeric vector of fluorescence readings (RFU, QSU, or any
#'   linear fluorescence unit).
#' @param temp Numeric vector of water temperature (deg C), same length as
#'   \code{fluor}. Typically from the co-located temperature sensor.
#' @param rho Temperature coefficient (per deg C). Negative for the usual
#'   case where fluorescence decreases with temperature. No default —
#'   must be supplied explicitly to avoid silent use of a wrong coefficient.
#'   See [fluor_rho()] for lookup by variable and manufacturer.
#' @param t_ref Reference temperature (deg C) to normalise to. Default 20.
#' @param model Correction model: \code{"linear"} (default) or
#'   \code{"exponential"}.
#' @return Numeric vector of corrected fluorescence, same length as input.
#'   Returns \code{NA} where either input is \code{NA}.
#'
#' @seealso [fluor_rho()] for literature rho values by sensor and variable.
#'
#' @references
#' Watras, C.J., et al. (2011). A temperature compensation method for CDOM
#' fluorescence sensors in freshwater. *Limnol. Oceanogr.: Methods*, 9,
#' 296--301.
#'
#' Ryder, E., et al. (2012). Temperature quenching of chlorophyll-a
#' fluorescence in situ. *Freshwater Biology*, 57(8), 1638--1646.
#'
#' Hodges, C., et al. (2018). Sensitivity of phycocyanin fluorescence to
#' temperature. *Water Research*, 137, 410--419.
#'
#' @export
#'
#' @examples
#' # Chlorophyll at 25 deg C, rho = -0.012, normalise to 20 deg C
#' fluor_correct(10.0, temp = 25, rho = -0.012)
#' # => ~10.64 (warm water reading adjusted upward)
#'
#' # Vectorised across a profile
#' fluor_correct(c(5, 8, 12), temp = c(22, 15, 8), rho = -0.012)
#'
#' # Using fluor_rho() to look up a default
#' fluor_correct(10.0, temp = 25, rho = fluor_rho("f_chl", "turner"))
fluor_correct <- function(fluor, temp, rho, t_ref = 20,
                          model = c("linear", "exponential")) {
  model <- match.arg(model)

  if (length(fluor) != length(temp))
    stop("fluor and temp must be the same length", call. = FALSE)

  dt <- temp - t_ref

  switch(model,
    linear      = fluor / (1 + rho * dt),
    exponential = fluor * exp(-rho * dt)
  )
}


#' Look up default fluorescence temperature coefficient
#'
#' Returns a literature-derived \eqn{\rho} value for a given variable and
#' manufacturer combination. These are midpoint estimates from published
#' studies and manufacturer data — use as starting points. Site-specific
#' empirical determination is always preferable.
#'
#' @param var Variable code: \code{"f_chl"}, \code{"f_phyc"}, \code{"f_dom"},
#'   or \code{"f_tryp"}.
#' @param manufacturer Manufacturer name: \code{"turner"} or
#'   \code{"chelsea"}. Default \code{"turner"}.
#' @return Numeric scalar: rho in per degree C (negative).
#'
#' @details
#' Stored coefficients (midpoint of published ranges):
#'
#' | var | turner | chelsea |
#' |-----|--------|---------|
#' | f_chl | -0.014 | -0.013 |
#' | f_phyc | -0.021 | -0.018 |
#' | f_dom | -0.010 | -0.010 |
#' | f_tryp | -0.010 | -0.010 |
#'
#' @export
#'
#' @examples
#' fluor_rho("f_chl", "turner")    # -0.014
#' fluor_rho("f_phyc", "chelsea")  # -0.018
fluor_rho <- function(var, manufacturer = "turner") {
  var <- tolower(var)
  manufacturer <- tolower(manufacturer)

  rho_table <- list(
    f_chl  = c(turner = -0.014, chelsea = -0.013),
    f_phyc = c(turner = -0.021, chelsea = -0.018),
    f_dom  = c(turner = -0.010, chelsea = -0.010),
    f_tryp = c(turner = -0.010, chelsea = -0.010)
  )

  if (!(var %in% names(rho_table)))
    stop("Unknown variable: ", var,
         ". Use one of: ", paste(names(rho_table), collapse = ", "),
         call. = FALSE)

  rhos <- rho_table[[var]]
  if (!(manufacturer %in% names(rhos)))
    stop("Unknown manufacturer: ", manufacturer,
         ". Use one of: ", paste(names(rhos), collapse = ", "),
         call. = FALSE)

  unname(rhos[manufacturer])
}
