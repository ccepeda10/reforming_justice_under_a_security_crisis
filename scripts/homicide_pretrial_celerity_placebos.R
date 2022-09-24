set.seed(21012022)
library(fect)
library(haven)
library(tidyverse)

# Homicide----
load("models/homicide_gsynth.RData")
homicide <- read_dta("data/panel_tasa_homicidios.dta")

placebo_homicide <- fect(homicide_gsynth[["formula"]],
                         Y = homicide_gsynth[["Y"]],
                         D = homicide_gsynth[["D"]],
                         X = homicide_gsynth[["X"]],
                         seed = 21012022,
                         data = homicide,
                         index = homicide_gsynth[["index"]],
                         method = "ife",
                         CV = FALSE,
                         r = homicide_gsynth[["r.cv"]],
                         force = homicide_gsynth[["call"]][["force"]],
                         se = TRUE,
                         nboots = 2000,
                         parallel = TRUE,
                         cores = 9,
                         placebo.period = c(-3,0),
                         placeboTest = TRUE)

save(placebo_homicide, file = "models/placebo_homicide.RData")

# (Homicide) groups with and without cartels-----
load("models/homicide_cartels_group.RData")
load("models/group_cartels.RData")
placebo_groups <- list()

for(i in seq_along(groups)){
  placebo_groups[[i]] <- fect(groups_gsynth[[i]][["formula"]],
                              data = homicide %>% filter(MUN %in% groups[[i]]),
                              index = groups_gsynth[[i]][["index"]],
                              seed = 21012022,
                              method = "ife",
                              CV = FALSE,
                              r = groups_gsynth[[i]][["r.cv"]],
                              force = groups_gsynth[[i]][["call"]][["force"]],
                              se = TRUE,
                              nboots = 2000,
                              parallel = TRUE,
                              cores = 9,
                              placebo.period = c(-3,0),
                              placeboTest = TRUE)
}
names(placebo_groups) <- c("Without cartels",
                           "One or more cartels")
save(placebo_groups, file = "models/placebo_cartels_group.RData")

# Pretrial detention----
load("models/gsynth_pretrial.RData")

delitos<- c("total","homicidio","secuestro","robo","violacion")
placebo_pretrial<- list()

for(i in 1:length(delitos)){
  pretrial<- read_dta(paste0("data/panel_prision_formal_",delitos[[i]],".dta"))
  
  placebo_pretrial[[i]] <- fect(Y = pretrial_gsynth[[i]]$Y,
                               D = pretrial_gsynth[[i]]$D,
                               X = pretrial_gsynth[[i]]$X,
                               seed = 21012022,
                               data = pretrial,
                               index = c("MUN","year"),
                               method = "ife",
                               CV = FALSE,
                               r = pretrial_gsynth[[i]]$r.cv %>% as.numeric(),
                               force = "two-way",
                               se = TRUE,
                               nboots = 2000,
                               parallel = TRUE,
                               cores = 9,
                               placebo.period = c(-3,0),
                               placeboTest = TRUE
  )  
}
names(placebo_pretrial) <- delitos
save(placebo_pretrial, file = "models/placebo_pretrial.RData")

# Tiempos sentencia----
load(file = "models/gsynth_celerity.RData")
placebo_celerity <- list()

for(i in seq_along(delitos)){
  celerity<- read_dta(paste0("data/panel_tiempos_sentencia_",delitos[[i]],".dta"))
  
  placebo_celerity[[i]] <- fect(Y = celerity_gsynth[[i]]$Y,
                                D = celerity_gsynth[[i]]$D,
                                X = celerity_gsynth[[i]]$X,
                                seed = 21012022,
                                data = celerity,
                                index = c("MUN","year"),
                                method = celerity_gsynth[[i]]$call[["estimator"]],
                                CV = FALSE,
                                lambda = celerity_gsynth[[i]]$lambda.cv,
                                r = celerity_gsynth[[i]]$r.cv,
                                force = "two-way",
                                se = TRUE,
                                nboots = 2000,
                                parallel = TRUE,
                                cores = 9,
                                placebo.period = c(-3,0),
                                min.T0 = 7,
                                placeboTest = TRUE
  )  
}

names(placebo_celerity) <- delitos
save(placebo_celerity, file = "models/placebo_celerity.RData")

rm(list = ls())