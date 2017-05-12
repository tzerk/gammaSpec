#' Import SPE files to R
#' 
#' <placeholder>
#' 
#' <placeholder>
#'
#' @param file \code{\link{character}} \bold{required}: file path
#' @param ... currently not used.
#'
#' @return
#' 
#'
#' @examples
#' 
#' # none available yet
#' 
#' @export
read_SPE <- function(file, ...) {
  
  # Read file line by line
  doc <- readLines(file)
  
  # Sections in the SPE file start with these keywords, which we will use to 
  # split the text into chunks
  KEYWORDS <- get_Keywords()
  
  # Find positions
  key_pos_start <- sapply(KEYWORDS, function(key) grep(key, doc))
  key_pos_end <- key_pos_start + c(diff(key_pos_start), 
                                   length(doc) - key_pos_start[length(key_pos_start)] + 1) - 1
  
  # Split the text in separate sections
  doc_list <- Map(function(start, end) { doc[(start+1):end] },
                  key_pos_start, key_pos_end)
  
  # Energy Calibration
  doc_list$ENER_FIT <- setNames(as.list(as.numeric(strsplit(doc_list$ENER_FIT, " ")[[1]])),
                                c("intercept", "slope"))
  
  # Measurement time
  doc_list$MEAS_TIM <- setNames(as.list(as.integer(strsplit(doc_list$MEAS_TIM, " ")[[1]])),
                                c("live", "real"))
  
  # convert data to integer values. we start at index 2 as the first entry
  # is vector of length two indicating the min and max channel number; we know
  # this anyway, so it can be discarded.
  doc_list$DATA <- data.frame(channel = 1:(length(doc_list$DATA) - 1), 
                              energy = NA, 
                              counts = as.integer(doc_list$DATA[2:length(doc_list$DATA)]),
                              counts_norm = NA)
  
  # fill in the energy values
  doc_list$DATA$energy <- doc_list$ENER_FIT$slope * 1:nrow(doc_list$DATA) + doc_list$ENER_FIT$intercept
  
  # fill in the counts normalise by the live time
  doc_list$DATA$counts_norm <- doc_list$DATA$counts / doc_list$MEAS_TIM$live
  
  # add custom class to make use of S3 generics
  class(doc_list) <- c("SPE", class(doc_list))
  
  # return the formatted SPE text
  invisible(doc_list)
}