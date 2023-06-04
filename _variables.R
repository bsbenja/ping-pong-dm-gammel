#' ---
#' title: Variable for DM i Ping Pong
#' output: html_document
#' ---

#' # Ikoner
# Ikoner ------------------------------------------------------------------
#+ eval=F, warning=F, message=F

var_ikon <- data.frame(
	k_bogmærke    = "<iconify-icon icon=mdi:bookmark-plus style=color:#F1C40F></iconify-icon>",  # var_ikon$k_bogmærke
	k_windows     = "<iconify-icon icon=mdi:microsoft-windows></iconify-icon>",                  # var_ikon$k_windows
	k_mac         = "<iconify-icon icon=mdi:apple></iconify-icon>",                              # var_ikon$k_mac
  k_ping_pong   = "<iconify-icon icon=mdi:table-tennis style=color:#398FCC></iconify-icon>",   # var_ikon$k_ping_pong
  k_fest        = "<iconify-icon icon=mdi:party-popper style=color:#F1C40F></iconify-icon>",   # var_ikon$k_fest
  k_spisning    = "<iconify-icon icon=mdi:food-drumstick style=color:#9B59B6></iconify-icon>", # var_ikon$k_spisning
  k_frokost     = "<iconify-icon icon=mdi:hamburger style=color:#1ABC9C></iconify-icon>",      # var_ikon$k_frokost
  k_herre       = "<iconify-icon icon=mdi:gender-male style=color:#009FB7></iconify-icon>",    # var_ikon$k_herre
  k_dame        = "<iconify-icon icon=mdi:gender-female style=color:#DA036B></iconify-icon>",  # var_ikon$k_dame
  k_tilmeldt    = "<iconify-icon icon=mdi:account-check style=color:#1ABC9C></iconify-icon>",  # var_ikon$k_tilmeldt
  k_drive_in    = "<iconify-icon icon=mdi:run-fast style=color:#F39C12></iconify-icon>",       # var_ikon$k_drive_in
  k_afbud       = "<iconify-icon icon=mdi:account-cancel style=color:#C0392B></iconify-icon>", # var_ikon$k_afbud
  k_klub        = "<iconify-icon icon=mdi:home></iconify-icon>",                               # var_ikon$k_klub
  k_ingen_klub  = "<iconify-icon icon=mdi:home-off></iconify-icon>",                           # var_ikon$k_ingen_klub
  k_person      = "<iconify-icon icon=mdi:account></iconify-icon>",                            # var_ikon$k_person
  k_født        = "<iconify-icon icon=mdi:cake-variant></iconify-icon>",                       # var_ikon$k_født
  k_debutant    = "<iconify-icon icon=mdi:numeric-1-circle></iconify-icon>",                   # var_ikon$k_debutant
  k_gentagelse  = "<iconify-icon icon=mdi:autorenew></iconify-icon>",                          # var_ikon$k_gentagelse
	k_nul         = "<iconify-icon icon=mdi:numeric-0-circle></iconify-icon>",                   # var_ikon$k_nul
  k_indbydelse  = "<iconify-icon icon=mdi:text-box-outline></iconify-icon>",                   # var_ikon$k_indbydelse
  k_timeglas    = "<iconify-icon icon=mdi:timer-sand></iconify-icon>",                         # var_ikon$k_timeglas
  k_regler      = "<iconify-icon icon=mdi:scale-balance></iconify-icon>",                      # var_ikon$k_regler
  k_wcpp        = "<iconify-icon icon=mdi:earth></iconify-icon>",                              # var_ikon$k_wcpp
  k_koncept     = "<iconify-icon icon=mdi:bullseye-arrow></iconify-icon>",                     # var_ikon$k_koncept
  k_link_ny     = "<iconify-icon icon=mdi:open-in-new></iconify-icon>",                        # var_ikon$k_link_ny
  k_penge       = "<iconify-icon icon=mdi:cash></iconify-icon>",                               # var_ikon$k_penge
  k_refusion    = "<iconify-icon icon=mdi:cash-refund></iconify-icon>",                        # var_ikon$k_refusion
  k_forplejning = "<iconify-icon icon=mdi:food-fork-drink></iconify-icon>",                    # var_ikon$k_forplejning
  k_parkering   = "<iconify-icon icon=mdi:parking></iconify-icon>",                            # var_ikon$k_parkering
  k_transport   = "<iconify-icon icon=mdi:train-car></iconify-icon>",                          # var_ikon$k_transport
  k_medbring    = "<iconify-icon icon=mdi:bag-personal></iconify-icon>",                       # var_ikon$k_medbring
  k_overnatning = "<iconify-icon icon=mdi:bed></iconify-icon>",                                # var_ikon$k_overnatning
  k_pokal       = "<iconify-icon icon=mdi:trophy></iconify-icon>",                             # var_ikon$k_pokal
  k_gave        = "<iconify-icon icon=mdi:gift></iconify-icon>",                               # var_ikon$k_gave
  k_hånd_ned    = "<iconify-icon icon=mdi:hand-pointing-down></iconify-icon>",                 # var_ikon$k_hånd_ned
  k_lokation    = "<iconify-icon icon=mdi:pin></iconify-icon>",                                # var_ikon$k_lokation
  k_mail        = "<iconify-icon icon=mdi:email></iconify-icon>",                              # var_ikon$k_mail
  k_telefon     = "<iconify-icon icon=mdi:phone></iconify-icon>",                              # var_ikon$k_telefon
  k_graf        = "<iconify-icon icon=mdi:chart-box></iconify-icon>",                          # var_ikon$k_graf
  k_facebook    = "<span class=bi-facebook style=color:#1877F2></span>",                       # var_ikon$k_facebook
  k_messenger   = "<span class=bi-messenger style=color:#0695FF></span>",                      # var_ikon$k_messenger
  k_github      = "<span class=bi-github style=color:#9F7BE1></span>",                         # var_ikon$k_github
  k_billet      = "<span class=bi-tags-fill></span>",                                          # var_ikon$k_billet
  check.names = T)