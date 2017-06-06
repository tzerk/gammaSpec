## REFERENCE SPECTRUM ----

# This is the spectrum used for calibration. While we could hardcode the values
# we rather provide the reference spectrum file, so it can be changed at any point
# in time without needing to update too much R code
#
# CURRENT CALIBRATION SPECTRUM:
# - original file name: 2017_04 Weimar bloc 10000s.Spe
# - date: 04/26/2017 12:04:52
# - place: Weimar
# - material: loess
# - dose rate: 1.315 +- 0.1(?)
# - measurement time: 10.000 s
# - contact: Tim Schüler
get_SpecCalib <- function() {
  fpath <- system.file("extdata", "calibration_spectrum.spe", package = "gammaSpec")
  read_SPE(fpath)
}


## THRESHOLD TECHNIQUE ----

# Define the energy (eV) from which on the counts are summed up
get_EnergyThreshold <- function() {
  500
}

## DOSE RATE CALIBRATION ----

# This is the dose rate of the reference spectrum used for calibration (Gy/ka)
# 2017_04 Weimar bloc 10000s.Spe
get_SpecDoseRate <- function() {
  c(1.36025, 0.0445)
}

## MISCALLENOUS ----

# A Vector of Keywords used as section headers in the SPE files and by which
# the text can be split into separate parts.
get_Keywords <- function() {
  c("SPEC_ID", "SPEC_REM", "DATE_MEA", "MEAS_TIM", "DATA", "ROI", 
    "PRESETS", "ENER_FIT", "MCA_CAL", "SHAPE_CAL")
}


## BACKGROUND MEASUREMENT ---

# CURRENT BACKGROUND SPECTRUM:
# - original file name: 2016-11_Labormessung_2_Burg_junior_72h.Spe
# - date: 11/28/2016 16:48:32
# - place: Cologne Luminescence Laboratory
# - material: measured in the lead casing of a HPGe gamma detector
# - measurement time: 259.200 s (72 h)
# - contact: Franz Hartung
get_SpecBackground <- function() {
  fpath <- system.file("extdata", "background_spectrum.spe", package = "gammaSpec")
  read_SPE(fpath)
}

## ERROR CALCULATION ----

# This function calculates the error on the dose rate by propagating the
# errors of the measured, calibration and background spectra through
# the linear fit (D = m * N + N0). dX denotes the error of the variable.
# See package vignette for detailed explanations.
calc_Error <- function(N, dN, N1, dN1, N2, dN2, D1, dD1, D2, dD2, m) {
  # slope error
  dm <- m * sqrt(
    (dD1 / (D1 - D2))^2 + (dD2 / (D1 - D2))^2 + (dN1 / (N1 - N2))^2 + (dN2 / (N1 - N2))^2
  )
  # intercept error
  dD0 <- sqrt(
    dD1^2 + (-N1 * dm)^2 + (-m * dN1)^2
  )
  # dose rate error
  dD <- sqrt(
    (N * dm)^2 + (m * dN)^2 + dD0^2
  )
  return(dD)
}