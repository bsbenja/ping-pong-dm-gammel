#' ---
#' title: Input for DM i Ping Pong
#' output: html_document
#' ---

#+ eval=F, warning=F, message=F
# Kopier/indsæt for offentliggørelse af hjemmeside via Terminal (Alt+Shift+M) =>
# quarto publish quarto-pub --no-prompt --no-browser

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
tbl0_input <- data.frame(
  k_status_1_2_3_4         = 3,      # 1/2/3/4 for tilmelding hhv. lukket, teaser, åben og endelig
  k_eventordre_T_F         = F,      # T/F for hentning af eventordre
  k_plakat_png_T_F         = F,      # T/F for plakater fra PDF til PNG
  k_webscraping_rating_T_F = F,      # T/F for webscraping af rating
  k_eventår                = "2023", # ÅÅÅÅ
  k_data                   = "Filer/Generelt/Deltagere og tilmeldinger til DM i Ping Pong.xlsx", # Sti til data
  check.names = T)
source(file = "_variables.R")
source(file = "bagvedliggende-kode.R")