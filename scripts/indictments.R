set.seed(21012022)

library(tidyverse)
library(haven)
library(foreign)
library(gsynth)
library(fect)

equation <- as.formula("casos ~ tratamiento + encounters_sedena + organized_crime_prosecutions")

indictments <- read_dta(file = "data/indictments.dta")

indictments_gsynth <- gsynth(formula = equation,
                             data = indictments, index = c("MUN", "periodo"),
                             seed = 21012022,
                             force = "two-way",
                             CV = TRUE,
                             estimator = "ife",
                             inference = "nonparametric",
                             parallel = TRUE,
                             nboots = 2000,
                             min.T0 = 12,
                             cores = 9,
                             se = TRUE)

save(indictments_gsynth, file = "models/indictments_gsynth.RData")
factors <- indictments_gsynth[["r.cv"]]
rm(indictments_gsynth)

set.seed(21012022)
indictments_placebo <- fect(formula = equation,
                            data = indictments,
                            seed = 21012022,
                            index = c("MUN", "periodo"),
                            method = "ife",
                            CV = FALSE,
                            force = "two-way",
                            se = TRUE,
                            nboots = 1500,
                            min.T0 = 12,
                            parallel = TRUE,
                            cores = 7,
                            placebo.period = c(-6,0),
                            r = factors,
                            placeboTest = TRUE)

save(indictments_placebo, file = "models/indictments_placebo.RData")
rm(indictments_placebo)

