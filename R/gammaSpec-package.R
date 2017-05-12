#' A Collection of Functions to Analyse Gamma Spectrums
#'
#' <placeholder>
#' 
#' @name gammaSpec-package
#' @aliases gammaSpec-package
#' @docType package
#' @author Christoph Burow (University of Cologne, Germany)
#' @keywords package
#' 
#' @importFrom graphics abline arrows lines mtext par legend plot points polygon text grconvertX grconvertY
#' @importFrom stats ecdf lm predict setNames coef
#' @importFrom utils modifyList
NULL


#' Photon Cross Section Data
#' 
#' Total attenuation (with coherent scatter) of photons for NaI and SiO2, given
#' in cm^2/g. Data taken from the NIST XCOM Photon Cross Section Database.
#' 
#' @name MassAttenuationCoefficients
#' @docType data
#' @format A \code{\link{list}} with two elements (\code{\link{data.frame}s})
#' with two columns (\code{energy_MeV, attenuation_cm2g}: \cr
#' \tabular{ll}{
#' $NaI \tab Total attenuation for sodium iodide \cr
#' $SiO2 \tab Total attenuation for silicon dioxide \cr
#' }
#' 
#' @source 
#' http://physics.nist.gov/PhysRefData/Xcom/html/xcom1-t.html
#' 
#' @keywords datasets
#' 
#' @examples 
#' data(MassAttenuationCoefficients, envir = environment())
#' plot(MassAttenuationCoefficients)
#' 
NULL