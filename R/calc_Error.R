calc_Error <- function(N, dN, N1, dN1, N2, dN2, D1, dD1, D2, dD2, m) {
  # slope error
  dm <- m * sqrt(
    (dD1 / (D1 - D2))^2 + (dD2 / (D1 - D2))^2 + (dN1 / (N1 - N2))^2 + (dN2 / (N1 - N2))^2
  )
  # intercept error
  dN0 <- sqrt(
    dD1^2 + (-N1 * dm)^2 + (-m * dN1)^2
  )
  # dose rate error
  dD <- sqrt(
    (N * dm)^2 + (m * dN)^2 + dN0^2
  )
  return(dD)
}