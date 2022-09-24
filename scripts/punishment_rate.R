set.seed(21012022)

library(foreign)
library(haven)
library(tidyverse)
library(lubridate)
library(gsynth)
library(fect)

equation <- as.formula("procesados ~ treatment + encounters_sedena + organized_crime_prosecutions")
punishment_rate <- read_dta(file = "data/punishment_rate_panel.dta")
  
# Punishment rate by group
load(file = "data/district_groups.RData")
punishment_rate_gsynth <- list()

punishment_rate_gsynth[[1]] <- gsynth(formula = equation,
                                  data = punishment_rate %>% filter(distrito %in% grupo[[1]]),
                                  seed = 21012022,
                                  CV = TRUE,
                                  index=c("distrito","periodo"),
                                  force="two-way",
                                  estimator = "ife",
                                  inference = "parametric",
                                  se = T,
                                  nboots = 2000,
                                  parallel= TRUE,
                                  min.T0 = 7,
                                  cores = 9)

punishment_rate_gsynth[[2]] <- gsynth(formula = equation,
                                  data = punishment_rate %>% filter(distrito %in% grupo[[2]]),
                                  seed = 21012022,
                                  CV = TRUE,
                                  index=c("distrito","periodo"),
                                  force="two-way",
                                  estimator = "ife",
                                  inference = "parametric",
                                  se = T,
                                  nboots = 2000,
                                  parallel= TRUE,
                                  min.T0 = 7,
                                  cores = 9)


names(punishment_rate_gsynth)<- c("Indictments per homicide - cartels",
                             "Indictments per homicide - no cartels")

# Pruebas placebo-----
# Para grupos con procesados
set.seed(21012022)

punishment_rate_placebo <- list()
punishment_rate_placebo[[1]] <- fect(formula = equation,
                                 data = punishment_rate %>% filter(distrito %in% grupo[[1]]),
                                 seed = 21012022,
                                 CV = FALSE,
                                 index=c("distrito","periodo"),
                                 method = "ife",
                                 r = punishment_rate_gsynth[[1]]$r.cv,
                                 force = "two-way",
                                 se = TRUE,
                                 nboots = 2000,
                                 parallel = TRUE,
                                 cores = 7,
                                 min.T0 = 9,
                                 placebo.period = c(-3,0),
                                 placeboTest = TRUE)

punishment_rate_placebo[[2]] <- fect(formula = equation,
                                 data= punishment_rate %>% filter(distrito %in% grupo[[2]]),
                                 seed = 21012022,
                                 CV = FALSE,
                                 index=c("distrito","periodo"),
                                 method = "ife",
                                 r = punishment_rate_gsynth[[2]]$r.cv,
                                 force = "two-way",
                                 se = TRUE,
                                 nboots = 2000,
                                 parallel = TRUE,
                                 cores = 9,
                                 placebo.period = c(-3,0),
                                 placeboTest = TRUE)

names(punishment_rate_placebo)<- c("Indictments per homicide - cartels",
                                  "Indictments per homicide -  no cartels")

save(punishment_rate_gsynth, punishment_rate_placebo, file = "models/punishment_rate.RData")
rm(list = ls())
