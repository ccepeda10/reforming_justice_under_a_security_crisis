set.seed(21012022)

## Packages -----
require(haven)
require(tidyverse)
require(gsynth)

#Equations -----
equations <- list()

equations[[1]] <- as.formula("tasa_homicidios ~ treatment + election_year + pobreza_alimentaria2 + gini2 + escolaridad_promedio2 + total_carteles2 + dist_cocaine + includes_pri + includes_pan + includes_prd + ejidos_pri + new_ejidos_pc + pc_ruralidad2 + includes_pri + includes_pan + includes_prd + pri_governor + pan_governor + prd_governor + same_party_governor + same_party_president + same_governor_election + same_president_election + total_seizures + immigrants + emigrants + encounters_sedena + organized_crime_prosecutions")
equations[[2]] <- as.formula("formal_prision ~ treatment + encounters_sedena + organized_crime_prosecutions")
equations[[3]] <- as.formula("tiempo_promedio ~ treatment + encounters_sedena + organized_crime_prosecutions")

# Homicide -----
homicide <- read_dta("data/panel_tasa_homicidios.dta")
homicide_gsynth<- gsynth(formula = equations[[1]],
                         data = homicide,
                         seed = 21012022,
                         index = c("MUN","year"),
                         CV = TRUE,
                         force = "two-way",
                         estimator = "ife",
                         inference = "nonparametric",
                         EM = TRUE,
                         se = TRUE,
                         nboots = 2000,
                         parallel =TRUE,
                         min.T0 = 7,
                         cores = 9)

save(homicide_gsynth, file = "models/homicide_gsynth.RData")
rm(homicide_gsynth)

# (Homicide) groups with and without cartels-----
group_without_cartels <- homicide %>%
  filter(year == 2006, total_carteles == 0) %>%
  select(MUN) %>%
  unique() %>%
  as_vector() %>%
  unname()

group_one_or_more <- homicide %>%
  filter(year == 2006, total_carteles >= 1) %>%
  select(MUN) %>%
  unique() %>%
  as_vector() %>%
  unname()

groups <- list(group_without_cartels, group_one_or_more)

save(groups,
     group_without_cartels,
     group_one_or_more,
     file = "models/group_cartels.RData")

groups_names <- c("No cartels", "One or more")

groups_gsynth <- list()

for(i in seq_along(groups)){
  groups_gsynth[[i]] <- gsynth(formula = equations[[1]],
                               data = homicide %>% filter(MUN %in% groups[[i]]),
                               seed = 21012022,
                               index = c("MUN","year"),
                               CV = TRUE,
                               force = "two-way",
                               estimator = "ife",
                               inference = "nonparametric",
                               EM = TRUE,
                               se = TRUE,
                               nboots = 2000,
                               parallel =TRUE,
                               min.T0 = 7,
                               cores = 9)
}
names(groups_gsynth) <- groups_names
save(groups_gsynth, file = "models/homicide_cartels_group.RData")
rm(groups_gsynth)

# Pretrial detention ----
crimes<- c("all_crime","homicide","kidnapping","property_crime","rape")
delitos<- c("total","homicidio","secuestro","robo","violacion")
pretrial_gsynth<-list()

for(i in seq_along(crimes)){
  pretrial<- read_dta(paste0("data/panel_prision_formal_",delitos[[i]],".dta"))
  pretrial_gsynth[[i]]<- gsynth(formula = equations[[2]],
                                data= pretrial,
                                seed = 21012022,
                                index = c("id_mun","year"),
                                CV = TRUE,
                                force = "two-way",
                                estimator = "ife",
                                inference = "nonparametric",
                                EM = TRUE,
                                se = TRUE,
                                nboots = 2000,
                                parallel =TRUE,
                                min.T0 = 7,
                                cores = 9)
}
names(pretrial_gsynth) <- crimes
save(pretrial_gsynth, file = "models/gsynth_pretrial.RData")

# Celerity ----
celerity_gsynth<-list()

for(i in seq_along(crimes)){
  celerity<- read_dta(paste0("data/panel_tiempos_sentencia_",delitos[[i]],".dta"))
  celerity_gsynth[[i]]<- gsynth(formula = equations[[3]],
                                data= celerity,
                                seed = 21012022,
                                index=c("id_mun","year"),
                                CV = TRUE,
                                force = "two-way",
                                estimator = "ife",
                                inference = if(i != 3) "nonparametric" else "parametric",
                                EM = TRUE,
                                se = TRUE,
                                nboots = 2000,
                                parallel =TRUE,
                                min.T0 = 7,
                                cores = 9)
}

names(celerity_gsynth)<- crimes
save(celerity_gsynth, file = "models/gsynth_celerity.RData")
rm(pretrial_gsynth, celerity_gsynth)

rm(list = ls())