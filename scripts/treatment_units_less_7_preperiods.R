library(tidyverse)

load("models/homicide_gsynth.RData")
removed_homicide <- homicide_gsynth[["removed.id"]]

load(file = "models/gsynth_pretrial.RData")
removed_pretrial <- pretrial_gsynth[[1]][["removed.id"]]

load(file = "models/gsynth_celerity.RData")
removed_celerity <- celerity_gsynth[[1]][["removed.id"]]

save(removed_homicide, removed_pretrial, removed_celerity, file = "data/less_7_preperiods.RData")
