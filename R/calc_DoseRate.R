#' Calculate the dose rate from a spectrum
#'
#' @param data input data
#' @param energy.min starting energy in eV
#' @param background.correction subtract background
#' @param plot plot
#' @param ... currently not used
#'
#' @return
#'
#' @examples
#' 
#' # none available yet
#' 
#' @export
calc_DoseRate <- function(data, 
                          energy.min = get_EnergyThreshold(), 
                          background.correction = TRUE, 
                          plot = TRUE, ...) {
  
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
  
  
  ## ERROR CALCULATION: SPECTRA ----
  sumCounts_calib_continous_error <- sqrt(sumCounts_calib_continous * data_calib$MEAS_TIM$live) / data_calib$MEAS_TIM$live
  sumCounts_measured_continous_error <- sqrt(sumCounts_measured_continous * data$MEAS_TIM$live) / data$MEAS_TIM$live
  
  ## BACKGROUND CORRECTION ----
  if (background.correction) {
    
    # sum counts (BG)
    sumCounts_bg_continous <- sapply(spec_measured$energy[energyIndex_min_measured:energyIndex_max_measured], function(x) {
      energyIndex_min_bg <- which.min(abs(spec_bg$energy - x))
      energyIndex_max_bg <- which.min(abs(spec_bg$energy - highest_common_energy))
      sum(spec_bg$counts_norm[energyIndex_min_bg:energyIndex_max_bg])
    })
    # sum counts error (BG)
    sumCounts_bg_continous_error <- sqrt(sumCounts_bg_continous * data_bg$MEAS_TIM$live) / data_bg$MEAS_TIM$live
    
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
                        doserate = doseRate_continous$doseRate,
                        doserate_error = doseRate_continous$doseRate_error,
                        counts_measured = sumCounts_measured_continous,
                        counts_measured_error = sumCounts_measured_continous_error,
                        counts_calib = sumCounts_calib_continous,
                        counts_calib_error = sumCounts_calib_continous_error,
                        counts_bg = sumCounts_bg_continous,
                        counts_bg_error = sumCounts_bg_continous_error)
  
  # get the dose rate derived from the desired energy threshold
  doseRate <- results$doserate[which.min(abs(results$energy - energy.min))]
  doseRate_error <- results$doserate_error[which.min(abs(results$energy - energy.min))]
  
  ## PLOTTING ----
  if (plot) {
    
    # set graphical parameters (2x2 plot area)
    par(mfrow = c(3, 2))
    
    # plot original spectrum
    plot_Spectrum(data, type = "line", info = FALSE, cex = 0.8,
                  main = "Gamma Spectrum")
    abline(v = c(energy.min, highest_common_energy), lty = 2)
    
    mtext("Measured", cex = 0.9, line = 0.3)
    
    # plot reference calibration spectrum
    plot_Spectrum(data_calib, type = "line", info = FALSE, cex = 0.8,
                  main = "Gamma Spectrum")
    abline(v = c(energy.min, highest_common_energy), lty = 2)
    
    mtext("Calibration", cex = 0.9, line = 0.3)
    
    # Empirical cumulative distribution
    plot(ecdf(spec_measured$counts_norm[which.min(abs(spec_measured$energy - energy.min)):
                                          which.min(abs(spec_measured$energy - highest_common_energy))]),
         verticals = TRUE, do.points = FALSE,
         main = "Empirical Cumulative Distribution")
    
    mtext("Measured", cex = 0.9, line = 0.3)
    
    plot(ecdf(spec_calib$counts_norm[which.min(abs(spec_calib$energy - energy.min)):
                                       which.min(abs(spec_calib$energy - highest_common_energy))]), 
         verticals = TRUE, do.points = FALSE,
         main = "Empirical Cumulative Distribution")
    
    mtext("Calibration", cex = 0.9, line = 0.3)
    
    # Linear fit
    df <- data.frame(x = c(0, results$counts_calib[which.min(abs(results$energy - energy.min))]), 
                           y = c(0, doseRate_calib[1]))
    
    plot(df, pch = 16, col = "red", 
         xlim = range(pretty(c(0, results$counts_measured[which.min(abs(results$energy - energy.min))]))),
         ylim = range(pretty(c(0, doseRate))),
         ylab = "Fitted dose rate (Gy/ka)",
         xlab = "Count rate (1/s)",
         cex = 1.5,
         main = "Dose rate calibration line")
    
    abline(lm(y ~ x, df), lty = 1)
    
    points(results$counts_measured[which.min(abs(results$energy - energy.min))],
           doseRate, 
           pch = 16, col = "darkgreen", cex = 1.5)
    
    lines(c(results$counts_measured[which.min(abs(results$energy - energy.min))], 0),
           c(doseRate, doseRate), 
          lty = 2)
    
    lines(c(results$counts_measured[which.min(abs(results$energy - energy.min))],
            results$counts_measured[which.min(abs(results$energy - energy.min))]),
          c(0, doseRate),
          lty = 2)
    
    mtext(paste("Fitted dose rate:", round(doseRate, 2), "\u00B1", round(doseRate_error, 2), "Gy/ka"), line = 0.3, cex = 0.8)
    
    legend("left", legend = c("Calibration", "Measured"), pt.cex = 1.5, y.intersp = 0.25, 
           pch = c(16, 16), col = c("red", "darkgreen"),  bty = "n")
    
    # continous dose rate plot
    plot(results$energy, 
         results$doserate, 
         type = "s", main = "Continous dose rate estimation",
         xlab = "Lower threshold energy (keV)",
         ylab = "Dose rate (Gy/ka)")
    
    arrows(x0 = results$energy[which.min(abs(results$energy - energy.min))],
           x1 = results$energy[which.min(abs(results$energy - energy.min))],
          y0 = 0, y1 = doseRate,
          lty = 1, col = "red", length = 0.15)
    
    
  }
}