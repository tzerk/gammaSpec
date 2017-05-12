####################################################################################################
# methods for generic: plot()
# ##################################################################################################
#' @rdname methods
#' @method plot SPE
#' @param type \code{\link{character}} (default: \code{"bar"}): 
#' Options: \code{"bar", "line", "point"}
#' @param xval \code{\link{character}} (default: '\code{"energy"}): 
#' Options: \code{"energy", "channel"}
#' @param yval \code{\link{character}} (default: \code{"rate"}): 
#' Options: \code{"rate", "count"}
#' @param info \code{\link{logical}} (default: \code{TRUE}): 
#' Show or hide info of the SPE file
#' @param fill \code{\link{logical}} (default: \code{FALSE}): 
#' Fill the area under the curve
#' @export 
plot.SPE <- function(x, y,
                     type = c("bar", "line", "point"),
                     xval = c("energy", "channel"), 
                     yval = c("rate", "count"),
                     info = TRUE,
                     fill = FALSE,
                     ...) {
  
  ## plot parameters
  settings <- list(x = switch(xval[1],
                              "energy" = x$DATA$energy,
                              "channel" = x$DATA$channel),
                   y = switch(yval[1],
                              "count" = x$DATA$counts,
                              "rate" = x$DATA$counts_norm),
                   ylab = switch(yval[1],
                                 "count" = "Count (a.u.)",
                                 "rate" = "Count rate (1/s)"),
                   xlab = switch(xval[1],
                                 "energy" = "Energy (keV)",
                                 "channel" = "Channel (a.u.)"),
                   type = switch(type[1],
                                 "bar" = "s",
                                 "line" = "l",
                                 "point" = "p"),
                   main = "Gamma Spectrum",
                   col = "grey50",
                   cex = 0.9,
                   bty = "o",
                   pch = 16,
                   log = "y")
  
  settings <- modifyList(settings, list(...))
  
  ## PLOT ----
  par(cex = settings$cex)
  do.call(plot, settings)
  
  # add legend
  if (info) {
    info <- x
    info$DATA <- NULL
    
    labels <- lapply(info, function(x) paste(x, collapse = ", "))
    labels <- paste0(paste0(names(labels), ": \n ", unlist(labels)), collapse = "\n")
    
    text(x = grconvertX(0.05, from = "npc"), y = grconvertY(0.3, from = "npc"), labels = labels, cex = 0.7 * settings$cex, adj = 0)
  }
  
  # add polygon fill
  if (fill)
    polygon(settings$x, pmin(settings$y), col = settings$col, border = NA)
  
}

#' @rdname methods
#' @method plot MAC
#' @export 
plot.MAC <- function(x, y, ...) {
  
  settings <- list(x = x$NaI$energy_MeV,
                   y = x$NaI$attenuation_cm2g / x$SiO2$attenuation_cm2g,
                   log = "xy",
                   type = "l",
                   main = "NaI / SiO2",
                   xlab = "Energy (MeV)",
                   ylab = "Attenuation ratio (a.u.)")
  
  settings <- modifyList(settings, list(...))
  
  do.call(plot, settings)
  
}