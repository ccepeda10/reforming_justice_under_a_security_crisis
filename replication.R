# Models and placebo tests
source("scripts/homicide_pretrial_celerity.R", encoding = "utf-8")
source("scripts/homicide_pretrial_celerity_placebos.R", encoding = "utf-8")
source("scripts/punishment_rate.R", encoding = "utf-8")
source("scripts/indictments.R", encoding = "utf-8")

# We create list of removed treatment units that had less than 7 pretreatment periods
source("scripts/treatment_units_less_7_preperiods.R", encoding = "utf-8")

# Plots and tables
source("scripts/descriptive_statistics_plots.R", encoding = "utf-8")
source("scripts/descriptive_statistics_table.R", encoding = "utf-8")
source("scripts/plots_tables_gsynth.R", encoding = "utf-8")
source("scripts/plots_tables_placebo.R", encoding = "utf-8")
