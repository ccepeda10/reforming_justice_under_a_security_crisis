require(haven)
require(tidyverse)
library(xtable)

# We load the list of removed treatment units that had less than 7 preperiods
load("data/less_7_preperiods.RData")
`%notin%` <- Negate(`%in%`)

delitos<- c("total","homicidio","secuestro","robo","violacion")
english_names <- c( "all crime", "homicide", "kidnapping", "property crime", "rape")

for(i in seq_along(delitos)){
  pretrial<- read_dta(paste0("data/panel_prision_formal_",delitos[[i]],".dta"))
  
  treatment_group <- pretrial %>%
    group_by(MUN) %>%
    summarise(treatment_group = ifelse(sum(treatment) == 0, 0, 1))
  
  if(i == 1){
    controls_pretrial <- pretrial %>% 
      filter(MUN %notin% removed_pretrial) %>%
      left_join(treatment_group) %>%
      filter(year %in% 1997:2006) %>%
      group_by(treatment_group) %>%
      summarise(organized_crime_prosecutions = mean(organized_crime_prosecutions)) %>%
      pivot_longer(cols = -1, names_to = "variable", values_to = "Valores") %>%
      pivot_wider(names_from = treatment_group, values_from = Valores) %>%
      rename(control = 2, treatment = 3)
  }
  
  
  A <- pretrial %>% 
    filter(MUN %notin% removed_pretrial) %>%
    left_join(treatment_group) %>%
    filter(year %in% 1997:2006) %>%
    group_by(treatment_group) %>%
    summarise(mean = mean(formal_prision))
  
  
  if(i == 1){
    tabla <- tibble(variable = paste0("Pretrial detention (",english_names[i],")"), control = A[1,2] %>% unname() %>% as.numeric(), treatment = A[2,2] %>% unname() %>% as.numeric())
  } else{
    tabla <- rbind(tabla, tibble(variable = paste0("Pretrial detention (",english_names[i],")"), control = A[1,2] %>% unname() %>% as.numeric(), treatment = A[2,2] %>% unname() %>% as.numeric()))
  }
}

for(i in seq_along(delitos)){
  celerity<- read_dta(paste0("data/panel_tiempos_sentencia_",delitos[[i]],".dta"))
  
  treatment_group <- celerity %>%
    group_by(MUN) %>%
    summarise(treatment_group = ifelse(sum(treatment) == 0, 0, 1))
  
  if(i == 1){
    controls_celerity <- celerity %>% 
      filter(MUN %notin% removed_celerity) %>%
      left_join(treatment_group) %>%
      filter(year %in% 1997:2006) %>%
      group_by(treatment_group) %>%
      summarise(organized_crime_prosecutions = mean(organized_crime_prosecutions)) %>%
      pivot_longer(cols = -1, names_to = "variable", values_to = "Valores") %>%
      pivot_wider(names_from = treatment_group, values_from = Valores) %>%
      rename(control = 2, treatment = 3)
  }
  
  A <- celerity %>% 
    filter(MUN %notin% removed_celerity) %>%
    left_join(treatment_group) %>%
    filter(year %in% 1997:2006) %>%
    group_by(treatment_group) %>%
    summarise(mean = mean(tiempo_promedio))
  
  tabla <- rbind(tabla, tibble(variable = paste0("Celerity (",english_names[i],")"), control = A[1,2] %>% unname() %>% as.numeric(), treatment = A[2,2] %>% unname() %>% as.numeric()))

}

control_names <- c("Homicide rate",
                   "Poverty rate",
                   "Gini index",
                   "Number of cartels present",
                   "Distance to the border (log)",
                   "Years of schooling",
                   "New ejidos",
                   "Rural population",
                   "Municipal election years",
                   "Drug seizures in municipality",
                   "Immigrants",
                   "Emigrants",
                   "FLAOC prosecutions - Homicide rate")

homicide<- read_dta("data/panel_tasa_homicidios.dta")

tabla2 <- homicide %>% 
  filter(year %in% 1997:2006) %>%
  filter(MUN %notin% removed_homicide) %>%
  group_by(trat2) %>% 
  summarize(tasa_homicidios = mean(tasa_homicidios, na.rm = TRUE),
            pobreza_alimentaria2 = mean(pobreza_alimentaria2, na.rm = TRUE),
            gini2 = mean(gini2, na.rm = TRUE),
            total_carteles2 = mean(total_carteles2, na.rm = TRUE),
            log_distance = mean(log(dist_frontera)),
            escolaridad_promedio2 = mean(escolaridad_promedio2, na.rm = TRUE),
            new_ejidos_pc = mean(new_ejidos_pc, na.rm = TRUE),
            pc_ruralidad = mean(pc_ruralidad, na.rm = TRUE),
            election_year = mean(election_year, na.rm = TRUE),
            total_seizures = mean(total_seizures, na.rm = TRUE),
            immigrants = mean(immigrants, na.rm = TRUE),
            emigrants = mean(emigrants, na.rm = TRUE),
            organized_crime_prosecutions = mean(organized_crime_prosecutions, na.rm = TRUE)) %>%
  pivot_longer(cols = -1, names_to = "variable", values_to = "Valores") %>%
  pivot_wider(names_from = trat2, values_from = Valores) %>%
  rename(control = 2, treatment = 3)

tabla2$variable <- control_names
controls_pretrial$variable <- "FLAOC prosecutions - Pretrial (all crime)"
controls_celerity$variable <- "FLAOC prosecutions - Celerity (all crime)"

tabla <- bind_rows(tabla2[1,],
                   tabla ,
                   tabla2[-1,],
                   controls_pretrial,
                   controls_celerity)
names(tabla) <- c("Variable", "Mean, Control Group", "Mean, Treatment Group")

xtable(tabla,
       type = "latex",
       digits = 2,
       caption = "Means in Treatment and Control Groups (1997-2006)",
       label = "tab:desc_pre") %>%
  print(file = "tables/table_1.tex",
        caption.placement = "top",
        sanitize.text.function = identity,
        include.rownames = FALSE)

