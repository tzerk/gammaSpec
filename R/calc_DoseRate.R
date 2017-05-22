#' Estimate the in-situ wet gamma dose rate
#' 
#' This function estimates the in-situ gamma dose rate of the provided gamma spectrum
#' based on the recorded countrate and comparing the integrated sum to that
#' of an internal calibration spectrum.
#' 
#' @param data \bold{required}: 
#' An SPE file imported with \code{\link[gammaSpec]{read_SPE}}
#' 
#' @param energy.min \code{\link{numeric}} (default: \code{500}):
#' Lower integration limit of the photon energy (keV).
#' 
#' @param energy.max \code{\link{numeric}} (optional):
#' upper integration limit of the photon energy (keV).
#' 
#' @param background.correction \code{\link{logical}} (default: \code{TRUE}):
#' If \code{TRUE} a background signal (included in this package) is subtracted
#' from both the measured and the calibration spectrum.
#' 
#' @param plot \code{\link{logical}} (default: \code{TRUE}):
#' Show or hide the plot(s).
#' 
#' @param plot.combine \code{\link{logical}} (default: \code{TRUE}):
#' Combine all plots in a single plot device.
#' 
#' @param ... Additional arguments: \code{verbose}. Further arguments to be
#' passed to \code{\link{plot}} and related functions.
#'
#' @return
#' 
#' Returns terminal output, a plot and a \code{\link{list}}. 
#' 
#'
#' @examples
#' 
#' ## Load Example Data
#' file <- system.file("extdata", "Nievenheim_DORNIE_1.spe", package = "gammaSpec")
#' 
#' ## Import SPE files
#' spec <- read_SPE(file)
#' 
#' ## Estimate the in-situ gamma dose rate
#' res <- calc_DoseRate(data = spec,
#'                      energy.min = 1000,
#'                      energy.max = 2000,
#'                      background.correction = TRUE,
#'                      plot = TRUE,
#'                      plot.combine = TRUE,
#'                      verbose = TRUE)
#'                      
#' print(res$summary)
#' 
#' @export
calc_DoseRate <- function(data, 
                          energy.min = get_EnergyThreshold(),
                          energy.max = NULL,
                          background.correction = TRUE, 
                          plot = TRUE,
                          plot.combine = TRUE,
                          ...) {
  
  ## INPUT VERICIFATION ----
  if (!inherits(data, "SPE"))
    stop("Invalid input data. Please provide an object returned by 'gammaSpec::read_SPE()'.", 
         call. = FALSE)
  if (!is.null(energy.max) &&  energy.min >= energy.max)
    stop("'energy.min' must be lower than 'energy.max'.", call. = FALSE)
  if (energy.min < 0)
    stop("'energy.min' must not be negative.", call. = FALSE)
  if (!is.null(energy.max) && energy.max < 0)
    stop("'energy.max' must not be negative.", call. = FALSE)
  
  
  ## SETTINGS ----
  settings <- list(
    verbose = TRUE,
    cex = 1.0
  )
  settings <- modifyList(settings, list(...))
  
  ## FETCH DATA ----
  # reformat the measured data
  spec_measured <- data.frame(energy = data$DATA$energy,
                              counts_norm = data$DATA$counts_norm)
  
  # get the reference spectrum of which we know the dose rate of and from
  # which we will derive the dose rate for the measured spectrum
  data_calib <- get_SpecCalib()
  spec_calib <- data.frame(energy = data_calib$DATA$energy,
                           counts_norm = data_calib$DATA$counts_norm)
  
  # get the background spectrum
  data_bg <- get_SpecBackground()
  spec_bg <- data.frame(energy = data_bg$DATA$energy,
                        counts_norm = data_bg$DATA$counts_norm)
  
  
  # determine the highest common energy to cut of the data
  highest_common_energy <- min(c(spec_measured$energy[length(spec_measured$energy)], 
                                 spec_calib$energy[length(spec_calib$energy)],
                                 spec_bg$energy[length(spec_bg$energy)]))
  
  if (!is.null(energy.max))
    if (energy.max < highest_common_energy)
      highest_common_energy <- energy.max
  
  energyIndex_max_measured <- which.min(abs(spec_measured$energy - highest_common_energy))
  
  # determine the lowest common energy to cut of the data
  lowest_common_energy <- max(c(spec_measured$energy[1],
                                spec_calib$energy[1],
                                spec_bg$energy[1]))
  energyIndex_min_measured <- which.min(abs(spec_measured$energy - lowest_common_energy))
  
  sumCounts_measured_continous <- sapply(energyIndex_min_measured:energyIndex_max_measured, function(x) {
    sum(spec_measured$counts_norm[x:energyIndex_max_measured])
  })
  
  sumCounts_calib_continous <- sapply(spec_measured$energy[energyIndex_min_measured:energyIndex_max_measured], function(x) {
    energyIndex_min_calib <- which.min(abs(spec_calib$energy - x))
    energyIndex_max_calib <- which.min(abs(spec_calib$energy - highest_common_energy))
    sum(spec_calib$counts_norm[energyIndex_min_calib:energyIndex_max_calib])
  })
  
  sumCounts_bg_continous <- sapply(spec_measured$energy[energyIndex_min_measured:energyIndex_max_measured], function(x) {
    energyIndex_min_bg <- which.min(abs(spec_bg$energy - x))
    energyIndex_max_bg <- which.min(abs(spec_bg$energy - highest_common_energy))
    sum(spec_bg$counts_norm[energyIndex_min_bg:energyIndex_max_bg])
  })
  
  ## ERROR CALCULATION: SPECTRA ----
  sumCounts_calib_continous_error <- sqrt(sumCounts_calib_continous * data_calib$MEAS_TIM$live) / data_calib$MEAS_TIM$live
  sumCounts_measured_continous_error <- sqrt(sumCounts_measured_continous * data$MEAS_TIM$live) / data$MEAS_TIM$live
  sumCounts_bg_continous_error <- sqrt(sumCounts_bg_continous * data_bg$MEAS_TIM$live) / data_bg$MEAS_TIM$live
  
  
  ## BACKGROUND CORRECTION ----
  if (background.correction) {
    
    # subtract BG from MEAS and CALIB spectrum
    sumCounts_measured_continous <- sumCounts_measured_continous - sumCounts_bg_continous
    sumCounts_calib_continous <- sumCounts_calib_continous - sumCounts_bg_continous
    
    # update errors on CAL and MEAS spec
    sumCounts_calib_continous_error <- sqrt(sumCounts_calib_continous_error^2 + sumCounts_bg_continous_error^2)
    sumCounts_measured_continous_error <- sqrt(sumCounts_measured_continous_error^2 + sumCounts_bg_continous_error^2)
  }
  
  ## CALCULATE DOSE RATE ----
  # get dose rate of the reference calibration spectrum (Gy/ka)
  doseRate_calib <- get_SpecDoseRate()
  
  # linear fitting
  doseRate_continous <- do.call(rbind, Map(function(meas, meas_err, calib, calib_err) {
    xy_calib <- data.frame(x = c(0, calib), 
                           y = c(0, doseRate_calib[1]))
    lm <- lm(y ~ x, xy_calib)
    
    # derive the dose rate of the measured spectrum
    doseRate <- predict(lm, newdata = list(x = meas))
    
    doseRate_error <- calc_Error(N = meas, dN = meas_err,
                                 N1 = calib, dN1 = calib_err, 
                                 N2 = 0, dN2 = 0, 
                                 D1 = doseRate_calib[1], dD1 = doseRate_calib[2], 
                                 D2 = 0, dD2 = 0,
                                 m = coef(lm)[2])
    
    return(data.frame(doseRate = doseRate, doseRate_error = doseRate_error))
  }, sumCounts_measured_continous, sumCounts_measured_continous_error,
  sumCounts_calib_continous, sumCounts_calib_continous_error))
  
  # combine energy and dose rate values
  results <- data.frame(energy = spec_measured$energy[energyIndex_min_measured:energyIndex_max_measured],
                        doserate_continous = doseRate_continous$doseRate,
                        doserate_continous_error = doseRate_continous$doseRate_error,
                        countrate_measured = sumCounts_measured_continous,
                        countrate_measured_error = sumCounts_measured_continous_error,
                        countrate_calib = sumCounts_calib_continous,
                        countrate_calib_error = sumCounts_calib_continous_error,
                        countrate_bg = sumCounts_bg_continous,
                        countrate_bg_error = sumCounts_bg_continous_error)
  
  # get the dose rate derived from the desired energy threshold
  doseRate <- results$doserate_continous[which.min(abs(results$energy - energy.min))]
  doseRate_error <- results$doserate_continous_error[which.min(abs(results$energy - energy.min))]
  
  ## PLOTTING ----
  if (plot) {
    
    # save and restore plot parameters
    par.old <- par(no.readonly = TRUE)
    on.exit(par(par.old))
    
    # set graphical parameters (3x1 plot area)
    if (plot.combine) {
      par(mar = c(5, 14, 4, 14) + 0.1)
      par(mfrow = c(3, 1))
    }
    par(cex = settings$cex)
    
    ## Plot 1: Spectrum ----
    ## ---------------------------------------------------------------------- ##
    if (!plot.combine)
      par(mar = c(5, 4, 4, 12) + 0.1)
    
    plot(NA, NA, 
         ylim = c(0.0001, max(c(spec_bg$counts_norm,
                                spec_calib$counts_norm,
                                spec_measured$counts_norm))),
         xlim = c(0, max(spec_measured$energy)),
         main = "Gamma Spectrum",
         xlab = "Energy (keV)",
         ylab = "Count rate (1/s)",
         log = "y"
    )
    
    # add spectra
    lines(spec_bg[which(spec_bg$counts_norm > 0), ], col = "blue", type = "s")
    lines(spec_calib[which(spec_calib$counts_norm > 0), ], col = "red", type = "s")
    lines(spec_measured[which(spec_measured$counts_norm > 0), ], col = "black", type = "s")
    abline(v = c(energy.min, highest_common_energy), col = "grey", lty = 2)
    
    # add legend
    legend(par("usr")[2], par("usr")[4], xpd = NA,
           bty = "n",
           legend = c("Measured", "Calibration", "Background", "Energy thresholds"),
           lty = c(1, 1, 1, 2), lwd = 2,
           col = c("black", "red", "blue", "grey"))
    
    
    ## Plot 2: continous dose rate
    ## ---------------------------------------------------------------------- ##
    if (!plot.combine)
      par(mar = c(5, 4, 4, 2) + 0.1)
    
    plot(results$energy, 
         results$doserate_continous, 
         type = "s", main = "Continous dose rate estimation",
         xlab = "Lower threshold energy (keV)",
         ylab = "Dose rate (Gy/ka)",
         log = "")
    
    
    ## Plot 3: Dose rate fit
    ## ---------------------------------------------------------------------- ##
    if (!plot.combine)
      par(mar = c(5, 4, 4, 8) + 0.1)
    
    df <- data.frame(x = c(0, results$countrate_calib[which.min(abs(results$energy - energy.min))]), 
                     y = c(0, doseRate_calib[1]))
    
    plot(df, pch = 16, col = "red", 
         xlim = range(pretty(c(0, max(c(results$countrate_measured[which.min(abs(results$energy - energy.min))],
                                        results$countrate_calib[which.min(abs(results$energy - energy.min))])) * 1.2))),
         ylim = range(pretty(c(0, max(c(doseRate, doseRate_calib[1])) * 1.2))),
         ylab = "Fitted dose rate (Gy/ka)",
         xlab = "Count rate (1/s)",
         cex = 1.5,
         main = "Dose rate calibration line")
    
    abline(lm(y ~ x, df), lty = 1)
    
    points(results$countrate_measured[which.min(abs(results$energy - energy.min))],
           doseRate, 
           pch = 16, col = "darkgreen", cex = 1.5)
    
    lines(c(results$countrate_measured[which.min(abs(results$energy - energy.min))], 0),
          c(doseRate, doseRate),
          lty = 2)
    
    lines(c(results$countrate_measured[which.min(abs(results$energy - energy.min))],
            results$countrate_measured[which.min(abs(results$energy - energy.min))]),
          c(0, doseRate),
          lty = 2)
    
    mtext(paste("Fitted dose rate:", round(doseRate, 2), "\u00B1", round(doseRate_error, 2), "Gy/ka"), line = 0.3, cex = 0.8)
    
    legend(par("usr")[2], par("usr")[4], xpd = NA,
           legend = c("Calibration", "Measured"), pt.cex = 1.5,
           pch = c(16, 16), col = c("red", "darkgreen"),  bty = "n")
    
  }
  
  ## TERMINAL OUTPUT ----
  if (settings$verbose) {
    message("\n [calc_DoseRate()]\n\n",
            " Estimated external wet gamma dose rate (including cosmic dose rate):\n\n",
            " ", round(doseRate, 3), " \u00B1 ", round(doseRate_error, 3), "\n")
  }
  
  ## RETURN VALUE ----
  out <- list(
    summary = data.frame(
      doserate = doseRate,
      doseRate_error
    ),
    data = list(measured = data,
                calibration = get_SpecCalib(),
                background = get_SpecBackground()),
    table = results,
    call = sys.call(0),
    args = as.list(sys.call(0))[-1]
  )
  
  return(out)
}