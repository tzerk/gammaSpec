#' Plot NaI and SiO2 Mass Attenuation Coefficients
#' 
#' This function produces a plot of the total mass attenuation coefficients for
#' NaI divided by the total mass attenuation coefficients for SiO2. Data taken 
#' from the NIST XCOM Photon Cross Section Database.
#' 
#' <placeholder>
#' 
#' @param data \code{\link{list}} \bold{required}: SiO2 and NaI total attenuation
#' coefficients included in this package (see examples). 
#' 
#' @param ... further parameters passed to \code{\link{plot}}
#'
#' @return
#' 
#' Returns a plot.
#' 
#' @export
#'
#' @examples
#' 
#' data("MassAttenuationCoefficients")
#' plot_MassAttenuationCoefficients(MassAttenuationCoefficients)
#' 
plot_MassAttenuationCoefficients <- function(data, ...) {
  
  settings <- list(x = data$NaI$energy_MeV,
                   y = data$NaI$attenuation_cm2g / data$SiO2$attenuation_cm2g,
                   log = "xy",
                   type = "l",
                   main = "NaI / SiO2",
                   xlab = "Energy (MeV)",
                   ylab = "Attenuation ratio (a.u.)")
  
  settings <- modifyList(settings, list(...))
  
  do.call(plot, settings)
  
}