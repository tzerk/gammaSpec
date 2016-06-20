#' Plot gamma spectrum
#' 
#' <placeholder>
#' 
#' <placeholder>
#'
#' @param data \code{\link{list}} \bold{required}: data object produced by 
#' \code{\link{read_SPE}}.
#' 
#' @param type \code{\link{character}} (with default): 
#' 
#' @param xval \code{\link{character}} (with default):
#'  
#' @param yval \code{\link{character}} (with default):
#'  
#' @param info \code{\link{character}} (with default):
#'  
#' @param fill \code{\link{character}} (with default):
#'  
#' @param ... currently not used.
#'
#' @return
#'
#' @examples
#' 
#' # none available yet
#' 
#' @export 
plot_Spectrum <- function(data,
                          type = c("bar", "line", "point"),
                          xval = c("energy", "channel"), 
                          yval = c("rate", "count"),
                          info = TRUE,
                          fill = TRUE,
                          ...) {
  
  ## plot parameters
  settings <- list(x = switch(xval[1],
                              "energy" = data$DATA$energy,
                              "channel" = data$DATA$channel),
                   y = switch(yval[1],
                              "count" = data$DATA$counts,
                              "rate" = data$DATA$counts_norm),
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
                   pch = 16)
  
  settings <- modifyList(settings, list(...))
  
  ## PLOT ----
  par(cex = settings$cex)
  do.call(plot, settings)
  
  # add legend
  if (info) {
    info <- data
    info$DATA <- NULL
    
    labels <- lapply(info, function(x) paste(x, collapse = ", "))
    labels <- paste0(paste0(names(labels), ": \n ", unlist(labels)), collapse = "\n")
    
    text(x = mean(settings$x) * 0.33, y = max(settings$y) * 0.75, labels = labels, cex = 0.7 * settings$cex, adj = 0)
  }
  
  # add polygon fill
  if (fill)
    polygon(settings$x, pmin(settings$y), col = settings$col, border = NA)
  
}