set.seed(21012022)

# Packages
require(tidyverse)
require(gsynth)
require(xtable)
library(showtext)

source("scripts/tablasgsynth.R")

font_add_google("Open Sans", "Open Sans")
showtext_auto()

# Graph height and width
ancho = 12.35
alto = 9.56

# Strings ----

groups <- c("No cartels",
            "One or more")

columns_betas <- c("beta",
                   "SE",
                   "CI (lower)",
                   "CI (upper)",
                   "p-value")

control_homicide <- c("Municipal election year",
                      "Food poverty", 
                      "Gini coefficient",
                      "Mean years of schooling",
                      "Number of cartels present",
                      "log(distance) * cocaine seizures in Colombia", 
                      "PRI in municipal government", 
                      "PAN in municipal government", 
                      "PRD in municipal government", 
                      "New ejidos created under continuing PRI Rule", 
                      "Ejidos growth rate", 
                      "Rurality (\\% total population)", 
                      "PRI governor", 
                      "PAN governor", 
                      "PRD governor", 
                      "Same party governor", 
                      "Same party president", 
                      "Same party governor * municipal election", 
                      "Same party president * municipal election",
                      "Total drug seizures", 
                      "Immigrants", 
                      "Emigrants",
                      "Violent encounters involving military personnel",
                      "FLAOC prosecutions")

control_celerity_pretrial <- c("Violent encounters involving military personnel",
                               "FLAOC prosecutions")
# Homicide rate-----
load("models/homicide_gsynth.RData")

g <- plot(homicide_gsynth, type = "counterfactual", theme.bw = TRUE, shade.post = FALSE) +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

  ggsave(g, file = "plots/figure_3_a.pdf", device = "pdf", height = alto, width = ancho, units = "cm")
  
g <- plot(homicide_gsynth, theme.bw = TRUE, main = "") +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

  ggsave(g, file = "plots/figure_3_b.pdf", device = "pdf", height = alto, width = ancho, units = "cm")

if ( ifelse(length(homicide_gsynth$r.cv) == 0,0,homicide_gsynth$r.cv) != 0) {
  g <- plot(homicide_gsynth_mc, type = "factors", theme.bw = TRUE, main = "") +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = "plots/homicide_factors.pdf", device = "pdf", height = alto, width = ancho, units = "cm")
  
  g <- plot(homicide_gsynth, type = "loadings", theme.bw = TRUE, main = "") +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = "plots/homicide_loadings.pdf", device = "pdf", height = alto, width = ancho, units = "cm")
}

hacer_tabla_1(homicide_gsynth, y = "homicidios", z = "tables/table_2.tex")

if (is.null(homicide_gsynth$est.beta) == FALSE) {
  
  rownames(homicide_gsynth$est.beta) <- control_homicide
  colnames(homicide_gsynth$est.beta) <- columns_betas
  
  xtable(homicide_gsynth$est.beta, type = "latex",
         caption = "Estimated coefficientes",
         label = "tab:betas_homicide") %>%
    print(file = "tables/table_D_1.tex", 
          caption.placement = "top",
          sanitize.text.function = identity,
          include.rownames = TRUE)
}

# Groups with and without cartel presence -----
load("models/homicide_cartels_group.RData")
groups <- c("sin", "1mas")
groups_en <- c("No cartels", "One or more")

names(groups_gsynth) <- groups

for(i in seq_along(groups_gsynth)){
  g <- plot(groups_gsynth[[i]], type = "counterfactual", theme.bw = TRUE, shade.post = FALSE) +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = paste0("plots/figure_D_", i + 1, "_a.pdf"), height = alto, width = ancho, units = "cm")
  
  g <- plot(groups_gsynth[[i]], theme.bw = TRUE) +
    theme(text = element_text(family = "Open Sans")) +
    labs(subtitle = "Homicide rate")  +
    ggtitle(NULL)
  
    ggsave(g, file = paste0("plots/figure_D_", i + 1, "_b.pdf"), device = "pdf", height = alto, width = ancho, units = "cm")
  
  tables <- c("tables/table_D_2.tex", "tables/table_D_4.tex")
  hacer_tabla_1(x = groups_gsynth[[i]], y = paste0("homicidios_grupo_",groups[i]), z = tables[i])
  
  if (is.null(groups_gsynth[[i]]$est.beta) == FALSE) {
    rownames(groups_gsynth[[i]]$est.beta) <- control_homicide
    colnames(groups_gsynth[[i]]$est.beta) <- columns_betas
    
    tables <- c("tables/table_D_3.tex", "tables/table_D_5.tex")
    xtable(groups_gsynth[[i]]$est.beta, type = "latex",
           caption = "Estimated coefficientes",
           label = paste0("tab:betas_homicide_",groups[i])) %>%
      print(file = tables[i], 
            caption.placement = "top",
            sanitize.text.function = identity,
            include.rownames = TRUE)
  }
}

tabla_gsynth(groups_gsynth, header = "grupos", col.names = groups_en) %>% 
  xtable(type = "latex",
         caption = paste0("Estimated coefficientes"),
         label = "tab:avgatt_grupos") %>%
  print(file = "tables/table_3.tex", 
        caption.placement = "top",
        sanitize.text.function = identity,
        include.rownames = FALSE)


# Pretrial detention-----
load(file = "models/gsynth_pretrial.RData")
english_names <- c( "All crime", "Homicide", "Kidnapping", "Property crime", "Rape")
delitos <- c("total", "homicidio", "secuestro", "robo", "violacion")
names(pretrial_gsynth) <- english_names

figures_factors <- c("plots/figure_E_1.pdf",
                     "plots/figure_E_4.pdf",
                     "plots/kidnapping_factors.pdf",
                     "plots/figure_E_8.pdf",
                     "plots/figure_E_11.pdf")

figures_loadings <- c("plots/figure_E_2.pdf",
                      "plots/figure_E_5.pdf",
                      "plots/kidnapping_loadings.pdf",
                      "plots/figure_E_9.pdf",
                      "plots/figure_E_12.pdf")

appendix_tables <- c("tables/table_E_1.tex",
                     "tables/table_E_3.tex",
                     "tables/table_E_5.tex",
                     "tables/table_E_7.tex",
                     "tables/table_E_9.tex")

appendix_betas <- c("tables/table_E_2.tex",
                     "tables/table_E_4.tex",
                     "tables/table_E_6.tex",
                     "tables/table_E_8.tex",
                     "tables/table_E_10.tex")

for (i in seq_along(delitos)) {
  
  g <- plot(pretrial_gsynth[[i]], type = "counterfactual", theme.bw = TRUE, shade.post = FALSE) +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = paste0("plots/figure_",i+3,"_a.pdf"), device = "pdf", height = alto, width = ancho, units = "cm")
  
  g <- plot(pretrial_gsynth[[i]], theme.bw = TRUE) +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = paste0("plots/figure_",i+3,"_b.pdf"), device = "pdf", height = alto, width = ancho, units = "cm")
    
  
  if (ifelse(length(pretrial_gsynth[[i]]$r.cv) == 0,0,pretrial_gsynth[[i]]$r.cv) != 0) {
    g <- plot(pretrial_gsynth[[i]], type = "factors", theme.bw = TRUE, main = "") +
      theme(text = element_text(family = "Open Sans")) +
      xlab("Year") +
      ggtitle(NULL)
    
      ggsave(g, file = figures_factors[i] , device = "pdf", height = alto, width = ancho, units = "cm")
      
    
    plot(x = pretrial_gsynth[[i]], type = "loadings", theme.bw = TRUE, main = "") +
      theme(text = element_text(family = "Open Sans")) +
      ggtitle(NULL)
    
      ggsave(file = figures_loadings[i], device = "pdf", height = alto, width = ancho, units = "cm")
  }
    
  #Exportamos las tablas
 hacer_tabla_2(pretrial_gsynth,i, y = "formal_prision", z = appendix_tables[i])
 
  if (is.null(pretrial_gsynth[[i]]$est.beta) == FALSE) {
    rownames(pretrial_gsynth[[i]]$est.beta) <- control_celerity_pretrial
    colnames(pretrial_gsynth[[i]]$est.beta) <- columns_betas
    
    xtable(pretrial_gsynth[[i]]$est.beta, type = "latex",
           caption = "Estimated coefficientes",
           label = paste0("tab:betas_formal_prision_",delitos[i])) %>%
    print(file = appendix_betas[i], 
          caption.placement = "top",
          sanitize.text.function = identity,
          include.rownames = TRUE)}
}

tabla_gsynth(pretrial_gsynth) %>% 
  xtable(type = "latex",
         caption = "Estimated coefficientes",
         label = "tab:avgatt_prision") %>%
  print(file = "tables/table_5.tex", 
        caption.placement = "top",
        sanitize.text.function = identity,
        include.rownames = FALSE)

# Celerity -----
load(file = "models/gsynth_celerity.RData")
names(celerity_gsynth) <- english_names


figures_factors <- c("plots/figure_F_1.pdf",
                     "plots/homicide_factors.pdf",
                     "plots/kidnapping_factors.pdf",
                     "plots/figure_F_6.pdf",
                     "plots/rape_factors.pdf")

figures_loadings <- c("plots/figure_F_2.pdf",
                      "plots/homicide_loadings.pdf",
                      "plots/kidnapping_loadings.pdf",
                      "plots/figure_F_7.pdf",
                      "plots/rape_loadings.pdf")

appendix_tables <- c("tables/table_F_1.tex",
                     "tables/table_F_3.tex",
                     "tables/table_F_5.tex",
                     "tables/table_F_7.tex",
                     "tables/table_F_9.tex")

appendix_betas <- c("tables/table_F_2.tex",
                    "tables/table_F_4.tex",
                    "tables/table_F_6.tex",
                    "tables/table_F_8.tex",
                    "tables/table_F_10.tex")


for (i in seq_along(celerity_gsynth)) {
  g <- plot(celerity_gsynth[[i]], type = "counterfactual", theme.bw = TRUE, shade.post = FALSE) +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = paste0("plots/figure_",i + 8,"_a.pdf"), device = "pdf", height = alto, width = ancho, units = "cm")
  
  g <- plot(celerity_gsynth[[i]], theme.bw = TRUE) +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = paste0("plots/figure_",i + 8,"_b.pdf"), device = "pdf", height = alto, width = ancho, units = "cm")
  
  if (ifelse(length(celerity_gsynth[[i]]$r.cv) == 0,0,celerity_gsynth[[i]]$r.cv) != 0) {
    g <- plot(celerity_gsynth[[i]], type = "factors", theme.bw = TRUE) +
      theme(text = element_text(family = "Open Sans")) +
      xlab("Year") +
      ggtitle(NULL)
    
      ggsave(g, file = figures_factors[i], device = "pdf", height = alto, width = ancho, units = "cm")
    
      plot(x = celerity_gsynth[[i]], type = "loadings", theme.bw = TRUE, main = "") +
        theme(text = element_text(family = "Open Sans")) +
        ggtitle(NULL)
      
      ggsave(file = figures_loadings[i], device = "pdf", height = alto, width = ancho, units = "cm")
  }
  
  #Exportamos las tablas
  hacer_tabla_2(celerity_gsynth,i, "tiempos_sentencia", z = appendix_tables[i])  
                
  if (is.null(celerity_gsynth[[i]]$est.beta) == FALSE) {
    rownames(celerity_gsynth[[i]]$est.beta) <- control_celerity_pretrial
    colnames(celerity_gsynth[[i]]$est.beta) <- columns_betas
    
    xtable(celerity_gsynth[[i]]$est.beta, type = "latex",
           caption = "Estimated coefficientes",
           label = paste0("tab:betas_tiempos_sentencia_",delitos[i])) %>%
      print(file = appendix_betas[i], 
            caption.placement = "top",
            sanitize.text.function = identity,
            include.rownames = TRUE)
    }
  
}

tabla_gsynth(celerity_gsynth, r = 0) %>%
  xtable(type = "latex",
         caption = "Estimated coefficientes",
         label = "tab:avgatt_tiempos") %>%
  print(file = "tables/table_6.tex", 
        caption.placement = "top",
        sanitize.text.function = identity,
        include.rownames = FALSE)

# Indictments -----
load("models/indictments_gsynth.RData")

g <- plot(indictments_gsynth, theme.bw = TRUE, type = "counterfactual") +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

ggsave(g, file = "plots/figure_B_1_a.pdf", device = "pdf", height = alto, width = ancho, units = "cm") +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

g <- plot(indictments_gsynth, theme.bw = TRUE) +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

hacer_tabla_1(indictments_gsynth, "indictments", z = "tables/table_B_1.tex")

# Punishment rate ----
load(file = "models/punishment_rate.RData")
names(punishment_rate_gsynth) <- groups

for(i in 1:2){
  g <- plot(punishment_rate_gsynth[[i]], type = "counterfactual", theme.bw = TRUE, shade.post = FALSE) +
      theme(text = element_text(family = "Open Sans")) +
      ggtitle(NULL)
    
   ggsave(g, file = paste0("plots/figure_C_",i,"_a.pdf"), device = "pdf", height = alto, width = ancho, units = "cm")

   g <- plot(punishment_rate_gsynth[[i]], theme.bw = TRUE, shade.post = FALSE)+
      theme(text = element_text(family = "Open Sans")) +
      ggtitle(NULL)
    
   ggsave(g, file = paste0("plots/figure_C_",i,"_b.pdf"), device = "pdf", height = alto, width = ancho, units = "cm")
   
   if (is.null(punishment_rate_gsynth[[i]]$est.beta) == FALSE) {
     rownames(punishment_rate_gsynth[[i]]$est.beta) <- control_celerity_pretrial
     colnames(punishment_rate_gsynth[[i]]$est.beta) <- columns_betas
     
     xtable(punishment_rate_gsynth[[i]]$est.beta, type = "latex",
            caption = "Estimated coefficientes",
            label = paste0("tab:betas_punishment_",groups[i])) %>%
       print(file = paste0("tables/table_C_",i,".tex"), 
             caption.placement = "top",
             sanitize.text.function = identity,
             include.rownames = TRUE)
   }
}


tabla_gsynth(punishment_rate_gsynth) %>% 
  xtable(type = "latex",
         caption = "Estimated coefficientes",
         label = "tab:procesados_homicidio_att_grupos") %>%
  print(file = "tables/table_4.tex", 
        caption.placement = "top",
        sanitize.text.function = identity,
        include.rownames = FALSE)

rm(list = ls())
