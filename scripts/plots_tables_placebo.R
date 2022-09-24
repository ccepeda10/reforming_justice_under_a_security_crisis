# Set up----
library(fect)
library(xtable)
source("Scripts/tablasgsynth.R")

library(showtext)
font_add_google("Open Sans", "Open Sans")
showtext_auto()


#Plot dimensions
ancho = 12.35
alto = 9.56

# Homicide----
load("models/placebo_homicide.RData")
g <- plot(placebo_homicide,
          main = " ",
          stats = c("placebo.p","equiv.p"),
          ylab = "Effect on homicide rate",
          cex.text = 0.6, cex.axis = 0.6, 
          cex.lab = 0.8) +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

  ggsave(g, file = "plots/figure_D_1.pdf", height = alto, width = ancho, units = "cm")

# Grupos homicidio
load("models/placebo_cartels_group.RData")
grupos <- c("sin", "1mas")

for(i in seq_along(grupos)){
  g <- plot(placebo_groups[[i]], main = "", theme.bw = TRUE, shade.post = FALSE, stats = c("placebo.p","equiv.p"), ylab = "Effect on homicide rate", cex.text = 0.6, cex.axis = 0.6, cex.lab = 0.8) +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = paste0("plots/figure_D_", i + 3,".pdf"), height = alto, width = ancho, units = "cm")
}

# Pretrial detention----
load("models/placebo_pretrial.RData")
delitos<- c("total","homicidio","secuestro","robo","violacion")

figures <- c("plots/figure_E_3.pdf",
             "plots/figure_E_6.pdf",
             "plots/figure_E_7.pdf",
             "plots/figure_E_10.pdf",
             "plots/figure_E_13.pdf")

for(i in seq_along(delitos)){
  if(i %in% c(1,3,4)){
    g <- plot(placebo_pretrial[[i]], main = "", theme.bw = TRUE, shade.post = FALSE, stats = c("placebo.p","equiv.p"),
              ylab = "Effect on use of pre-trial detention", 
              cex.text = 0.6, 
              cex.axis = 0.6, 
              cex.lab = 0.8, 
              stats.pos = c(-13,-0.3)) +
      theme(text = element_text(family = "Open Sans")) +
      ggtitle(NULL)
    
  }  else {
    g <- plot(placebo_pretrial[[i]], main = "", theme.bw = TRUE, shade.post = FALSE, stats = c("placebo.p","equiv.p"), 
                    ylab = "Effect on use of pre-trial detention", 
                    cex.text = 0.6, 
                    cex.axis = 0.6, 
                    cex.lab = 0.8) +
      theme(text = element_text(family = "Open Sans")) +
      ggtitle(NULL)
  }
    ggsave(g, file = figures[i], height = alto, width = ancho, units = "cm")
}

# Celerity----
load("models/placebo_celerity.RData")
delitos<- c("total","homicidio","secuestro","robo","violacion")
figures <- c("plots/figure_F_3.pdf",
             "plots/figure_F_4.pdf",
             "plots/figure_F_5.pdf",
             "plots/figure_F_8.pdf",
             "plots/figure_F_9.pdf")

for(i in seq_along(delitos)){
  g <- plot(placebo_celerity[[i]], main = "", theme.bw = TRUE, shade.post = FALSE, stats = c("placebo.p","equiv.p"), ylab = "Effect on elapsed time until sentence", cex.text = 0.6, cex.axis = 0.6, cex.lab = 0.8, stats.pos = c(-10,-200)) +
    theme(text = element_text(family = "Open Sans")) +
    ggtitle(NULL)
  
    ggsave(g, file = figures[i], height = alto, width = ancho, units = "cm")
}

# Indictments ----
load("models/indictments_placebo.RData")

g <- plot(indictments_placebo, theme.bw = TRUE, main = "", shade.post = FALSE, stats = c("placebo.p","equiv.p"), ylab = "Effect on indictments", cex.text = 0.6, cex.axis = 0.6, cex.lab = 0.8, stats.pos = c(-130,-20)) +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

ggsave(g, file = "plots/figure_B_2.pdf", device = "pdf", height = alto, width = ancho, units = "cm")

# Punishment rate ----
load(file = "models/punishment_rate.RData")

g <- plot(punishment_rate_placebo[[1]], main = "", theme.bw = TRUE, shade.post = FALSE, stats = c("placebo.p","equiv.p"), ylab = "Effect on indictments", cex.text = 0.6, cex.axis = 0.6, cex.lab = 0.8) +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

  ggsave(g, file = "plots/figure_C_3.pdf", device = "pdf", height = alto, width = ancho, units = "cm")

g <- plot(punishment_rate_placebo[[2]], theme.bw = TRUE, shade.post = FALSE, stats = c("placebo.p","equiv.p"), ylab = "Effect on indictments", cex.text = 0.6, cex.axis = 0.6, cex.lab = 0.8) +
  theme(text = element_text(family = "Open Sans")) +
  ggtitle(NULL)

  ggsave(g, file = "plots/figure_C_4.pdf", device = "pdf", height = alto, width = ancho, units = "cm")

rm(list = ls())
