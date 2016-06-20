## REFERENCE SPECTRUM ----

# This is the spectrum used for calibration. While we could hardcode the values
# we rather provide the reference spectrum file, so it can be changed at any point
# in time without needing to update too much R code
get_SpecCalib <- function() {
  fpath <- system.file("extdata", "calibration_spectrum.spe", package="gammaSpec")
  read_SPE(fpath)
}


## THRESHOLD TECHNIQUE ----

# Define the energy (eV) from which on the counts are summed up
get_EnergyThreshold <- function() {
  500
}

## DOSE RATE CALIBRATION ----

# This is the dose rate of the reference spectrum used for calibration
get_SpecDoseRate <- function() {
  2.208
}

## MISCALLENOUS ----

# A Vector of Keywords used as section headers in the SPE files and by which
# the text can be split into separate parts.
get_Keywords <- function() {
  c("SPEC_ID", "SPEC_REM", "DATE_MEA", "MEAS_TIM", "DATA", "ROI", 
    "PRESETS", "ENER_FIT", "MCA_CAL", "SHAPE_CAL")
}
