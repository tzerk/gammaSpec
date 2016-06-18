#' Calculate the dose rate from a spectrum
#'
#' @param data input data
#' @param energy.min starting energy in eV
#' @param ... 
#'
#' @return
#'
#' @examples
#' 
#' # none available yet
#' 
#' @export
calc_DoseRate <- function(data, energy.min = get_EnergyThreshold(), plot = TRUE, ...) {
  
  ## FETCH DATA ----
  # reformat the measured data
  spec_measured <- data.frame(energy = data$DATA$energy,
                              counts_norm = data$DATA$counts_norm)
  
  # get the reference spectrum of which we know the dose rate of and from
  # which we will derive the dose rate for the measured spectrum
  data_calib <- get_SpecCalib()
  spec_calib <- data.frame(energy = data_calib$DATA$energy,
                           counts_norm = data_calib$DATA$counts_norm)
  
  # determine the highest common energy to cut of the data
  highest_common_energy <- min(c(spec_measured$energy[length(spec_measured$energy)], 
                                 spec_calib$energy[length(spec_calib$energy)]))
  energyIndex_max_measured <- which.min(abs(spec_measured$energy - highest_common_energy))
  
  # determine the lowest common energy to cut of the data
  lowest_common_energy <- max(c(spec_measured$energy[1],
                                spec_calib$energy[1]))
  energyIndex_min_measured <- which.min(abs(spec_measured$energy - lowest_common_energy))
  
  
  sumCounts_measured_continous <- sapply(energyIndex_min_measured:energyIndex_max_measured, function(x) {
    sum(spec_measured$counts_norm[x:energyIndex_max_measured])
  })
  
  sumCounts_calib_continous <- sapply(spec_measured$energy[energyIndex_min_measured:energyIndex_max_measured], function(x) {
    energyIndex_min_calib <- which.min(abs(spec_calib$energy - x))
    energyIndex_max_calib <- which.min(abs(spec_calib$energy - highest_common_energy))
    sum(spec_calib$counts_norm[energyIndex_min_calib:energyIndex_max_calib])
  })
  
  ## CALCULATE DOSE RATE ----
  
  # get dose rate of the reference calibration spectrum (Gy/ka)
  doseRate_calib <- get_SpecDoseRate()
  
  # linear fitting
  doseRate_continous <- as.numeric(mapply(function(meas, calib) {
    xy_calib <- data.frame(x = c(0, calib), 
                           y = c(0, doseRate_calib))
    lm <- lm(y ~ x, xy_calib)
    # derive the dose rate of the measured spectrum
    predict(lm, newdata = list(x = meas))
  }, sumCounts_measured_continous, sumCounts_calib_continous))
  
  # combine energy and dose rate values
  results <- data.frame(energy = spec_measured$energy[energyIndex_min_measured:energyIndex_max_measured],
                        doserate = doseRate_continous,
                        counts_measured = sumCounts_measured_continous,
                        counts_calib = sumCounts_calib_continous)
  
  # get the dose rate derived from the desired energy threshold
  doseRate <- results$doserate[which.min(abs(results$energy - energy.min))] 
  
  ## PLOTTING ----
  if (plot) {
    
    # set graphical parameters (2x2 plot area)
    par(mfrow = c(3, 2))
    
    # plot original spectrum
    plot_Spectrum(data, type = "line", info = FALSE, cex = 0.8,
                  main = "Measured Spectrum")
    abline(v = c(energy.min, highest_common_energy), lty = 2)
    
    # plot reference calibration spectrum
    plot_Spectrum(data_calib, type = "line", info = FALSE, cex = 0.8,
                  main = "Calibration Spectrum")
    abline(v = c(energy.min, highest_common_energy), lty = 2)
    
    # Empirical cumulative distribution
    plot(ecdf(spec_measured$counts_norm[which.min(abs(spec_measured$energy - energy.min)):
                                          which.min(abs(spec_measured$energy - highest_common_energy))]),
         verticals = TRUE, do.points = FALSE,
         main = "Empirical Cumulative Distribution")
    
    plot(ecdf(spec_calib$counts_norm[which.min(abs(spec_calib$energy - energy.min)):
                                       which.min(abs(spec_calib$energy - highest_common_energy))]), 
         verticals = TRUE, do.points = FALSE,
         main = "Empirical Cumulative Distribution")
    
    # Linear fit
    df <- data.frame(x = c(0, results$counts_calib[which.min(abs(results$energy - energy.min))]), 
                           y = c(0, doseRate_calib))
    plot(df, pch = 16, col = "red", 
         xlim = range(pretty(c(0, results$counts_measured[which.min(abs(results$energy - energy.min))]))),
         ylim = range(pretty(c(0, doseRate))),
         ylab = "Dose rate (Gy/s)",
         xlab = "Count rate (1/s)",
         cex = 1.5,
         main = "Dose rate calibration line")
    
    abline(lm(y ~ x, df), lty = 1)
    
    points(results$counts_measured[which.min(abs(results$energy - energy.min))],
           doseRate, 
           pch = 16, col = "darkgreen", cex = 1.5)
    
    arrows(x0 = results$counts_measured[which.min(abs(results$energy - energy.min))],
           x1 = 0,
           y0 = doseRate,
           y1 = doseRate, 
           length = 0.25, lty = 2)
    
    lines(c(results$counts_measured[which.min(abs(results$energy - energy.min))],
            results$counts_measured[which.min(abs(results$energy - energy.min))]),
          c(0, doseRate),
          lty = 2)
    
    mtext(paste("Dose rate:", round(doseRate, 2), "\u00B1", round(doseRate / 10, 2), "Gy/s"), cex = 0.9)
    
    # continous dose rate plot
    plot(results$energy, results$doserate, type = "s")
    
    lines(c(results$energy[which.min(abs(results$energy - energy.min))],
            results$energy[which.min(abs(results$energy - energy.min))]),
          c(0, doseRate),
          lty = 1, col = "red")
    
    
  }
  
  
  
}