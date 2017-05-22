## ------------------------------------------------------------------------
library(gammaSpec)

## ---- eval=FALSE---------------------------------------------------------
#  spec <- read_SPE(file = "path/to/my/file.Spe")
#  str(spec, max.level = 2)

## ---- echo = FALSE-------------------------------------------------------
file <- system.file("extdata", "Nievenheim_DORNIE_1.spe", package = "gammaSpec")
spec <- read_SPE(file)
str(spec, max.level = 2)

## ---- fig.align='center', fig.keep='all', fig.show='hold', fig.width=7----
res <- calc_DoseRate(data = spec,
                     energy.min = 500,
                     energy.max = 2600,
                     background.correction = TRUE,
                     plot = TRUE,
                     plot.combine = FALSE,
                     app = FALSE,
                     cex = 0.8)

