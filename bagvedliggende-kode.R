#' ---
#' title: Bagvedliggende R-kode for DM i Ping Pong
#' output:
#'    html_document:
#'      theme: united
#'      df_print: paged
#'      code_folding: show
#'      code_download: yes
#'      toc: true
#'      toc_float:
#'        smooth_scroll: yes
#' ---


# Opsætning ---------------------------------------------------------------
#+ eval=F, warning=F, message=F

suppressWarnings(suppressPackageStartupMessages(lapply(c(
  "readxl", "cellranger", "openxlsx", "writexl", "dplyr", "tidyr", "stringr", "formattable", "lubridate",
  "kableExtra", "ggplot2", "ggtext", "forcats", "plotDK", "httr", "rvest", "pdftools", "rmarkdown"),
  require, character.only = TRUE)))
Sys.setlocale("LC_ALL", "da-DK.UTF-8")
options(timeout = max(1000, getOption("timeout")))
options(OutDec= ",")
options(knitr.table.format = "html")
options(knitr.kable.NA = "")

#' # Data
# Data --------------------------------------------------------------------
#+ eval=F, warning=F, message=F

# Importer og manipuler data fra Excel
tbl0_join_alle <- read_excel(
	tbl0_input$k_data,
	col_names = c("k_deltager_id", "k_navn", "k_klub", "k_køn", "k_ordredato",
															"k_event_år_billettype", "k_status", "k_rang1", "k_rating2",
															"k_rang3", "k_slutspil", "k_placering", "k_præmiepenge"),
	range = cell_cols("A:M")) %>%
	slice_tail(n = -5) %>%
	
	# Left join Bordtennisklub_d1 fra Excel
	left_join(
		y = read_excel(
			tbl0_input$k_data, col_names = c("k_klub", "k_postnr", "k_by"),
			range = cell_cols("P:R")), by = "k_klub") %>%
	
	# Left join Event_år_billettype_d1 fra Excel
	left_join(
		y = read_excel(
			tbl0_input$k_data, col_names = c(
				"k_event_år", "k_billettype", "k_billettype_beskrivelse", "k_billettype_tilvalg", "k_billetpris",
				"k_arrangørpris", "k_billetantal_maks", "k_event_år_billettype"),
			range = cell_cols("V:AC")), by = "k_event_år_billettype") %>%
	
	# Left join Event_år_d2 fra Excel
	left_join(
		y = read_excel(
			tbl0_input$k_data, col_names = c(
				"k_eventnr", "k_event", "k_eventdato", "k_tilmeldingsfrist", "k_åbningsdato",
				"k_ratingopdatering", "k_puljeantal", "k_præmiepenge_sponseret", "k_eventsted",
				"k_eventadresse", "k_eventpostnr", "k_eventby", "k_eventsted_url", "k_uuid",
				"k_token", "k_farve_1", "k_farve_2", "k_event_år"),
			range = cell_cols("U:AL")), by = "k_event_år") %>%
	
	# k_deltager_id
	mutate(across("k_deltager_id", \(x) as.character(x))) %>%
	
	# k_navn
	mutate(across("k_navn", \(x) as.character(x))) %>%
	
	# k_klub
	mutate(across("k_klub", \(x) as.character(x))) %>%
	
	# k_køn
	mutate(across("k_køn", \(x) factor(x, levels = c(
		"♂️ Herre",
		"♀️ Dame"), ordered = T))) %>%
	
	# k_ordredato
	mutate(across("k_ordredato", \(x) convertToDateTime(x))) %>%
	
	# k_status
	mutate(k_status = case_when(
		grepl("Tilmeldt", k_status) ~ "✔️ Tilmeldt",
		grepl("Afbud",    k_status) ~ "❌ Afbud")) %>%
	mutate(across("k_status", \(x) factor(x, levels = c(
		"✔️ Tilmeldt",
		"❌ Afbud"), ordered = T))) %>%
	
	# k_rang1
	mutate(across("k_rang1", \(x) as.integer(x))) %>%
	
	# k_rating2
	mutate(across("k_rating2", \(x) as.integer(x))) %>%
	
	# k_rang3
	mutate(across("k_rang3", \(x) as.integer(x))) %>%
	
	# k_slutspil
	mutate(across("k_slutspil", \(x) factor(x, ordered = T))) %>%
	
	# k_placering
	mutate(across("k_placering", \(x) factor(x, ordered = T))) %>%
	
	# k_præmiepenge
	mutate(across("k_præmiepenge", \(x) as.numeric(x))) %>%
	
	# k_postnr
	mutate(across("k_postnr", \(x) as.character(x))) %>%
	
	# k_by
	mutate(across("k_by", \(x) as.character(x))) %>%
	
	# k_type_klub
	mutate(k_type_klub = case_when(
		grepl("Ingen klub", k_klub) ~ "Ingen klub",
		grepl("Udlandet", k_klub)   ~ "Udlandet",
		TRUE                        ~ "Klub")) %>%
	mutate(across("k_type_klub", \(x) factor(x, levels = c(
		"Ingen klub",
		"Udlandet",
		"Klub"), ordered = T))) %>%
	
	# k_samlet_postnr_by
	mutate(k_samlet_postnr_by = ifelse(
		grepl("Ingen klub|Udlandet", k_klub), k_klub, paste(k_postnr, k_by))) %>%
	mutate(across("k_samlet_postnr_by", \(x) as.character(x))) %>%
	
	# k_event_år
	mutate(across("k_event_år", \(x) factor(x, ordered = T))) %>%
	
	# k_billettype
	mutate(across("k_billettype", \(x) factor(x, levels = c(
		"🏓 DM i Ping Pong",
		"🥳 Fest om aftenen",
		"🎉 The Old Irish Pub",
		"🥪 Frokost"), ordered = T))) %>%
	
	# k_billettype_beskrivelse
	mutate(across("k_billettype_beskrivelse", \(x) as.character(x))) %>%
	
	# k_billettype_tilvalg
	mutate(across("k_billettype_tilvalg", \(x) factor(x, ordered = T))) %>%
	
	# k_billetpris
	mutate(across("k_billetpris", \(x) as.numeric(x))) %>%
	
	# k_arrangørpris
	mutate(across("k_arrangørpris", \(x) as.numeric(x))) %>%
	
	# k_billetantal_maks
	mutate(across("k_billetantal_maks", \(x) as.integer(x))) %>%
	
	# k_eventnr
	mutate(across("k_eventnr", \(x) as.integer(x))) %>%
	
	# k_event
	mutate(across("k_event", \(x) factor(x, ordered = T))) %>%
	
	# k_eventdato
	mutate(across("k_eventdato", \(x) convertToDateTime(x))) %>%
	
	# k_tilmeldingsfrist
	mutate(across("k_tilmeldingsfrist", \(x) convertToDateTime(x))) %>%
	
	# k_åbningsdato
	mutate(across("k_åbningsdato", \(x) convertToDateTime(x))) %>%
	
	# k_ratingopdatering
	mutate(across("k_ratingopdatering", \(x) convertToDate(x))) %>%
	
	# k_puljeantal
	mutate(across("k_puljeantal", \(x) as.integer(x))) %>%
	
	# k_præmiepenge_sponseret
	mutate(across("k_præmiepenge_sponseret", \(x) as.logical(x))) %>%
	
	# k_eventsted
	mutate(across("k_eventsted", \(x) factor(x, ordered = T))) %>%
	
	# k_eventadresse
	mutate(across("k_eventadresse", \(x) factor(x, ordered = T))) %>%
	
	# k_eventpostnr
	mutate(across("k_eventpostnr", \(x) as.integer(x))) %>%
	
	# k_eventby
	mutate(across("k_eventby", \(x) factor(x, ordered = T))) %>%
	
	# k_eventsted_url
	mutate(across("k_eventsted_url", \(x) factor(x, ordered = T))) %>%
	
	# k_uuid
	mutate(across("k_uuid", \(x) factor(x, ordered = T))) %>%
	
	# k_token
	mutate(across("k_token", \(x) factor(x, ordered = T))) %>%
	
	# k_farve_1
	mutate(across("k_farve_1", \(x) factor(x, ordered = T))) %>%
	
	# k_farve_2
	mutate(across("k_farve_2", \(x) factor(x, ordered = T))) %>%
	
	# k_eventår
	mutate(k_eventår = year(k_eventdato)) %>%
	mutate(across("k_eventår", \(x) as.integer(x))) %>%
	
	# k_event_ping_pong_år
	mutate(k_event_ping_pong_år = paste(k_event, "i Ping Pong", k_eventår)) %>%
	mutate(across("k_event_ping_pong_år", \(x) factor(x, ordered = T))) %>%
	
	# k_første_ordredato
	group_by(k_event_år, k_deltager_id) %>%
	mutate(k_første_ordredato = min(k_ordredato)) %>%
	ungroup() %>%
	
	# k_billetsalg_pr_tilmelding
	add_count(k_event_år, k_deltager_id, k_status, name = "k_billetsalg_pr_tilmelding") %>%
	mutate(k_billetsalg_pr_tilmelding = paste(k_billetsalg_pr_tilmelding, "stk. billetsalg")) %>%
	mutate(across("k_billetsalg_pr_tilmelding", \(x) factor(x, ordered = T))) %>%
	
	# k_klokkeslætsinterval
	mutate(k_klokkeslætsinterval = case_when(
		hour(k_første_ordredato) >= 18 ~ "[≥ 18] Aften",
		TRUE ~ "[≥ 00] Øvrig")) %>%
	mutate(across("k_klokkeslætsinterval", \(x) factor(x, levels = c(
		"[≥ 18] Aften",
		"[≥ 00] Øvrig"), ordered = T))) %>%
	
	# k_tilmeldingstype
	mutate(k_tilmeldingstype = case_when(
		grepl("Tilmeldt", k_status) & as_date(k_første_ordredato) <= k_tilmeldingsfrist ~ "🎫 Ordinær",
		grepl("Tilmeldt", k_status) & as_date(k_første_ordredato) >  k_tilmeldingsfrist ~ "🏃 Drive-in",
		grepl("Afbud",    k_status)                                                     ~ "❌ Afbud")) %>%
	mutate(across("k_tilmeldingstype", \(x) factor(x, levels = c(
		"🎫 Ordinær",
		"🏃 Drive-in",
		"❌ Afbud"), ordered = T))) %>%
	
	# Landsdel
	mutate(k_landsdel = case_when(
		suppressWarnings(as.integer(k_postnr)) <= 3699 ~ "Sjælland",
		suppressWarnings(as.integer(k_postnr)) <= 3799 ~ "Bornholm",
		suppressWarnings(as.integer(k_postnr)) <= 4999 ~ "Sjælland",
		suppressWarnings(as.integer(k_postnr)) <= 5999 ~ "Fyn",
		suppressWarnings(as.integer(k_postnr)) <= 9999 ~ "Jylland",
		TRUE ~ "(Ukendt landsdel)")) %>%
	mutate(across("k_landsdel", \(x) factor(x, levels = c(
		"Jylland",
		"Fyn",
		"Sjælland",
		"Bornholm",
		"(Ukendt landsdel)"), ordered = T))) %>%
	
	# Region
	mutate(k_region = case_when(
		suppressWarnings(as.integer(k_postnr)) <= 3799 ~ "Hovedstaden",
		suppressWarnings(as.integer(k_postnr)) <= 4999 ~ "Sjælland",
		suppressWarnings(as.integer(k_postnr)) <= 6899 ~ "Syddanmark",
		suppressWarnings(as.integer(k_postnr)) <= 6999 ~ "Midtjylland",
		suppressWarnings(as.integer(k_postnr)) <= 7199 ~ "Syddanmark",
		suppressWarnings(as.integer(k_postnr)) <= 7699 ~ "Midtjylland",
		suppressWarnings(as.integer(k_postnr)) <= 7799 ~ "Nordjylland",
		suppressWarnings(as.integer(k_postnr)) <= 8999 ~ "Midtjylland",
		suppressWarnings(as.integer(k_postnr)) <= 9999 ~ "Nordjylland",
		TRUE ~ "(Ukendt region)")) %>%
	mutate(across("k_region", \(x) factor(x, levels = c(
		"Nordjylland",
		"Midtjylland",
		"Syddanmark",
		"Sjælland",
		"Hovedstaden",
		"(Ukendt region)"), ordered = T))) %>%
	
	# k_2021_eller_senere
	mutate(k_2021_eller_senere = ifelse(k_eventår >= 2021, T, F)) %>%
	
	# k_antal_gentilmelding
	arrange(k_ordredato, k_billettype) %>%
	group_by(k_deltager_id, k_billettype, k_2021_eller_senere) %>%
	mutate(k_antal_gentilmelding = ifelse(grepl("Afbud", k_status), 0, 1)) %>%
	mutate(k_antal_gentilmelding = paste0(cumsum(k_antal_gentilmelding), " ", ifelse(
		cumsum(k_antal_gentilmelding) == 1, "gang", "gange"))) %>%
	ungroup() %>%
	mutate(across("k_antal_gentilmelding", \(x) factor(x, ordered = T))) %>%
	
	# k_gentilmelding
	mutate(k_gentilmelding = case_when(
		sub(" .*", "", k_antal_gentilmelding) == 1 ~ "Debutant",
		sub(" .*", "", k_antal_gentilmelding) >= 2 ~ "Gentilmelding",
		TRUE                                     ~ "Ikke hidtil")) %>%
	mutate(across("k_gentilmelding", \(x) factor(x, levels = c(
		"Debutant",
		"Gentilmelding",
		"Ikke hidtil"), ordered = T))) %>%
	
	# k_rating
	mutate(k_rating = case_when(
		!is.na(k_rang1)  ~ paste0("[", k_rang1, "] ", k_rating2),
		is.na(k_rating2) ~ "-",
		TRUE ~ as.character(k_rating2))) %>%
	mutate(across("k_rating", \(x) as.character(x))) %>%
	
	# k_ratinggruppe
	mutate(k_ratinggruppe = case_when(
		as.numeric(k_rating2) >= 2000 ~ "Elite",
		TRUE ~ "Amatør")) %>%
	mutate(across("k_ratinggruppe", \(x) factor(x, levels = c(
		"Elite",
		"Amatør"), ordered = T))) %>%
	
	# k_født
	mutate(k_født = if_else(
		substr(k_deltager_id, 5, 6) <= substr(year(k_eventdato), 3, 4),
		paste0(
			as.numeric(substr(year(k_eventdato), 1, 2)), substr(k_deltager_id, 5, 6), "-",
			substr(k_deltager_id, 3, 4), "-",
			substr(k_deltager_id, 1, 2)),
		paste0(
			as.numeric(substr(year(k_eventdato), 1, 2))-1, substr(k_deltager_id, 5, 6), "-",
			substr(k_deltager_id, 3, 4), "-",
			substr(k_deltager_id, 1, 2)))) %>%
	mutate(across("k_født", \(x) as_date(x))) %>%
	
	# k_alder
	mutate(k_alder = trunc((k_født %--% k_eventdato) / years(1))) %>%
	mutate(across("k_alder", \(x) as.integer(x))) %>%
	
	# k_aldersgruppe
	mutate(k_aldersgruppe = case_when(
		k_alder <= 17 ~ "Ungdom",
		k_alder <= 39 ~ "Senior",
		k_alder >= 40 ~ "Veteran")) %>%
	mutate(across("k_aldersgruppe", \(x) factor(x, levels = c(
		"Ungdom",
		"Senior",
		"Veteran"), ordered = T))) %>%
	
	# k_spillertype
	mutate(k_spillertype = case_when(
		suppressWarnings(as.integer(str_sub(
			k_deltager_id, -4))) >= 0 ~ "👤 Ikke-bordtennisspiller",
		TRUE ~ "🏓 Bordtennisspiller")) %>%
	mutate(across("k_spillertype", \(x) factor(x, levels = c(
		"🏓 Bordtennisspiller",
		"👤 Ikke-bordtennisspiller"), ordered = T))) %>%
	
	# k_billetantal_billettype_status
	add_count(k_event_år_billettype, k_billettype, k_status,
											name = "k_billetantal_billettype_status") %>%
	group_by(k_event_år_billettype, k_billettype, k_status) %>%
	mutate(k_billetantal_billettype_status = k_billetantal_billettype_status-sum(is.na(k_deltager_id))) %>%
	ungroup() %>%
	mutate(k_billetantal_billettype_status = case_when(
		!is.na(k_deltager_id) ~ k_billetantal_billettype_status)) %>%
	group_by(k_event_år_billettype) %>%
	fill(k_billetantal_billettype_status, .direction = "updown") %>%
	ungroup() %>%
	mutate(across("k_billetantal_billettype_status", \(x) as.integer(x))) %>%
	
	# k_navn_klub
	mutate(k_navn_klub = case_when(
		is.na(k_deltager_id) ~ NA_character_,
		grepl("Ingen klub|Udlandet", k_klub) ~ paste0(k_navn),
		TRUE ~ paste0(k_navn, ", <i>", k_klub, "</i>"))) %>%
	mutate(across("k_navn_klub", \(x) as.character(x))) %>%
	
	# k_ikon_billettype
	mutate(k_ikon_billettype = case_when(
		grepl("Ping Pong", k_billettype) ~ var_ikon$k_ping_pong,
		grepl("Fest om aftenen", k_billettype) ~ var_ikon$k_fest,
		grepl("The Old Irish Pub", k_billettype) ~ var_ikon$k_spisning,
		grepl("Frokost", k_billettype) ~ var_ikon$k_frokost)) %>%
	mutate(across("k_ikon_billettype", \(x) factor(x, levels = c(
		var_ikon$k_ping_pong,
		var_ikon$k_fest,
		var_ikon$k_spisning,
		var_ikon$k_frokost), ordered = T))) %>%
	
	# k_ikon_status
	mutate(k_ikon_status = case_when(
		grepl("Tilmeldt", k_status) ~ var_ikon$k_tilmeldt,
		grepl("Afbud", k_status) ~ var_ikon$k_afbud)) %>%
	mutate(across("k_ikon_status", \(x) factor(x, levels = c(
		var_ikon$k_tilmeldt,
		var_ikon$k_afbud), ordered = T))) %>%
	
	# k_ikon_tilmeldingstype
	mutate(k_ikon_tilmeldingstype = case_when(
		grepl("Ordinær", k_tilmeldingstype) ~ var_ikon$k_tilmeldt,
		grepl("Drive-in", k_tilmeldingstype) ~ var_ikon$k_drive_in,
		grepl("Afbud",    k_tilmeldingstype) ~ var_ikon$k_afbud)) %>%
	mutate(across("k_ikon_tilmeldingstype", \(x) factor(x, levels = c(
		var_ikon$k_tilmeldt,
		var_ikon$k_drive_in,
		var_ikon$k_afbud), ordered = T))) %>%
	
	# k_ikon_gentilmelding
	mutate(k_ikon_gentilmelding = case_when(
		grepl("Debutant", k_gentilmelding) ~ var_ikon$k_debutant,
		grepl("Gentilmelding", k_gentilmelding) ~ var_ikon$k_gentagelse,
		TRUE ~ var_ikon$k_nul)) %>%
	mutate(across("k_ikon_gentilmelding", \(x) factor(x, levels = c(
		var_ikon$k_debutant,
		var_ikon$k_gentagelse,
		var_ikon$k_nul), ordered = T))) %>%
	
	# k_ikon_køn
	mutate(k_ikon_køn = case_when(
		grepl("Herre", k_køn) ~ var_ikon$k_herre,
		grepl("Dame", k_køn) ~ var_ikon$k_dame)) %>%
	mutate(across("k_ikon_køn", \(x) factor(x, levels = c(
		var_ikon$k_herre,
		var_ikon$k_dame), ordered = T))) %>%
	
	# k_ikon_klub
	mutate(k_ikon_klub = case_when(
		grepl("Ingen klub", k_klub) ~ var_ikon$k_ingen_klub,
		grepl("Udlandet", k_klub) ~ var_ikon$k_wcpp,
		TRUE ~ var_ikon$k_klub)) %>%
	mutate(across("k_ikon_klub", \(x) factor(x, levels = c(
		var_ikon$k_klub,
		var_ikon$k_ingen_klub,
		var_ikon$k_wcpp), ordered = T))) %>%
	
	# k_logo_klub
	mutate(k_logo_klub = paste0(
		"<img src=Filer/Klublogoer/",
		gsub("\\(|\\)", "", gsub(" |/", "-", k_klub)), ".png width=15>")) %>%
	mutate(across("k_logo_klub", \(x) as.character(x))) %>%
	
	# k_navn_billettype
	group_by(k_event_år, k_deltager_id, k_status) %>%
	arrange(k_billettype, desc(k_første_ordredato)) %>%
	mutate(k_navn_billettype = case_when(
		is.na(k_deltager_id) ~ NA_character_,
		grepl("Ingen klub|Udlandet", k_klub) ~ paste0(k_navn, " (", k_alder, " år) ", str_c(
			k_ikon_billettype, collapse = "<wbr>")),
		TRUE ~ paste0(k_navn_klub, " (", k_alder, " år) ", str_c(
			k_ikon_billettype, collapse = "<wbr>")))) %>%
	ungroup() %>%
	mutate(across("k_navn_billettype", \(x) as.character(x))) %>%
	
	# Sorter efter (1) k_første_ordredato, (2) k_billettype
	arrange(desc(k_første_ordredato), k_billettype)

# Aktuelle tilmeldinger
tbl0_join_aktuel <- tbl0_join_alle %>% filter(
  k_eventår == tbl0_input$k_eventår)

# Unikke variable
tbl0_unik <- data.frame(
  k_eventår               = unique(tbl0_join_aktuel$k_eventår),
  k_event_ping_pong_år    = unique(as.character(tbl0_join_aktuel$k_event_ping_pong_år)),
  k_eventdato             = unique(tbl0_join_aktuel$k_eventdato),
  k_tilmeldingsfrist      = unique(tbl0_join_aktuel$k_tilmeldingsfrist),
  k_åbningsdato           = unique(tbl0_join_aktuel$k_åbningsdato),
  k_ratingopdatering      = unique(tbl0_join_aktuel$k_ratingopdatering),
  k_puljeantal            = unique(tbl0_join_aktuel$k_puljeantal),
  k_præmiepenge_sponseret = unique(tbl0_join_aktuel$k_præmiepenge_sponseret),
  k_eventsted             = unique(as.character(tbl0_join_aktuel$k_eventsted)),
  k_eventadresse          = unique(as.character(tbl0_join_aktuel$k_eventadresse)),
  k_eventpostnr           = unique(tbl0_join_aktuel$k_eventpostnr),
  k_eventby               = unique(as.character(tbl0_join_aktuel$k_eventby)),
  k_eventsted_url         = unique(as.character(tbl0_join_aktuel$k_eventsted_url)),
  k_uuid                  = unique(as.character(tbl0_join_aktuel$k_uuid)),
  k_token                 = unique(as.character(tbl0_join_aktuel$k_token)),
  k_farve_1               = unique(as.character(tbl0_join_aktuel$k_farve_1)),
  k_farve_2               = unique(as.character(tbl0_join_aktuel$k_farve_2)),
  k_2021_eller_senere     = unique(tbl0_join_aktuel$k_2021_eller_senere),
  check.names = F)

#' # Statistik
# Statistik ---------------------------------------------------------------
#+ eval=F, warning=F, message=F

tbl0_statistik <- data.frame(
  
  # Billetantal total
  k_billetantal_total = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id)) %>%
      count(k_tilmeldingstype, k_ikon_tilmeldingstype, sort = T) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(
        n, " ", sub(".*? ", "", k_tilmeldingstype), " (", pct, ") ", k_ikon_tilmeldingstype)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Billetantal billettype
  k_billetantal_billettype = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      count(k_billettype, k_ikon_billettype, k_billetantal_maks, sort = T) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(
        "<b>", n, " ",  ifelse(n == 1, "tilmelding", "tilmeldinger"), " til ",
        sub(".*? ", "", ifelse(
        	grepl("Ping Pong", k_billettype),
        	paste(k_billettype, tbl0_unik$k_eventår),
        	as.character(k_billettype))), " ", k_ikon_billettype, "</b>")) %>%
      summarise(label = str_c(label, collapse = "<br>"))),
  
  # Deltagerantal total
  k_deltagerantal_total = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id)) %>%
      distinct(k_deltager_id, k_status, .keep_all = T) %>%
      add_count(k_deltager_id) %>%
      filter(grepl("Tilmeldt", k_status) | n == 1) %>%
      count(k_status, k_ikon_status) %>%
      mutate(k_status = gsub("Afbud", '"Totalafbud"', k_status)) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(
        n, " ", sub(".*? ", "", k_status), " (", pct, ") ", k_ikon_status)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal køn
  k_deltagerantal_køn = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_køn, k_ikon_køn) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(n, " ", sub(".*? ", "", k_køn), " (", pct, ") ", k_ikon_køn)) %>%
      arrange(desc(n)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Gentilmeldinger
  k_deltagerantal_gentilmelding = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
    	group_by(k_deltager_id) %>%
    	slice(which.max(sub(" .*", "", k_antal_gentilmelding))) %>%
    	ungroup() %>%
      count(k_ikon_gentilmelding, k_gentilmelding) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(
        n, " ", sub(".*? ", "", k_gentilmelding), " (", pct, ") ", k_ikon_gentilmelding)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal aldersgruppe
  k_deltagerantal_aldersgruppe = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_aldersgruppe) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(
        n, " ", k_aldersgruppe, " (", pct, ") ", var_ikon$k_født)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Alder
  k_alder = paste0(
    "Yngst ",
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(min(k_alder)), " år ", var_ikon$k_født,
    " ∙ Gns. ",
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(round(mean(k_alder), 0)), " år ", var_ikon$k_født,
    " ∙ Ældst ",
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(max(k_alder)), " år ", var_ikon$k_født),
  
  # Deltagerantal_landsdel
  k_deltagerantal_landsdel = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & !grepl("Ingen klub|Udlandet", k_klub)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_ikon_klub, k_landsdel) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(n, " ", k_landsdel, " (", pct, ") ", k_ikon_klub)) %>%
      arrange(desc(n)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal region
  k_deltagerantal_region = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & !grepl("Ingen klub|Udlandet", k_klub)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_ikon_klub, k_region) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(n, " ", k_region, " (", pct, ") ", k_ikon_klub)) %>%
      arrange(desc(n)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal klub
  k_deltagerantal_klub = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      count(k_type_klub, k_ikon_klub) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(n, " ", k_type_klub, " (", pct, ") ", k_ikon_klub)) %>%
      arrange(desc(n)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Klubantal
  k_klubantal = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & !grepl("Ingen klub|Udlandet", k_klub)) %>%
      distinct(k_ikon_klub, k_klub) %>%
      summarise(k_ikon_klub = unique(k_ikon_klub), n = n()) %>%
      mutate(label = paste0(n, " ", ifelse(n == 1, "klub", "forskellige klubber"), " ", k_ikon_klub)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Deltagerantal ratinggruppe
  k_deltagerantal_ratinggruppe = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & grepl("Ping Pong", k_billettype)) %>%
      count(k_ratinggruppe) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(n, " ",  k_ratinggruppe, " (", pct, ") ", var_ikon$k_ping_pong)) %>%
      summarise(label = str_c(label, collapse = " ∙ "))),
  
  # Rating
  k_rating = paste0(
    "Min. ", tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      summarise(min(k_rating2, na.rm = T)), " rating ", var_ikon$k_ping_pong,
    " ∙ Gns. ", tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      summarise(round(mean(k_rating2, na.rm = T), 0)), " rating ", var_ikon$k_ping_pong,
    " ∙ Maks. ", tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) &
        grepl("Ping Pong", k_billettype)) %>%
      summarise(max(k_rating2, na.rm = T)), " rating ", var_ikon$k_ping_pong),
  
  # Antal forskudte tilmeldinger
  k_antal_forskudte_tilmeldinger = paste0(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      group_by(k_deltager_id) %>%
      filter(n() >= 2) %>%
      count(k_ordredato) %>%
      ungroup() %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(n = sum(n == 1)) %>%
      mutate(n = paste0(
        n, " ", ifelse(n == 1, "forskudt tilmelding", "forskudte tilmeldinger"),
        " ", var_ikon$k_billet))),
  
  # Billetantal gns.
  k_billetantal_gns = paste(
  	"Gns.", format(round(tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      summarise(n())/
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      summarise(n_distinct(k_deltager_id)), 1), nsmall = 1), "billetter pr. deltager", var_ikon$k_billet),
  
  # Økonomi
  k_økonomi = paste0(
    "Omsætning kr. ", tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      summarise(format(round(sum(k_billetpris, na.rm = T), 0), big.mark = ".")), " ",
    var_ikon$k_penge, " ∙ Arrangørpris kr. ", tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      summarise(format(round(sum(-k_arrangørpris, na.rm = T), 0), big.mark = ".")), " ",
    var_ikon$k_penge, " ∙ Over-/underskud arrangør kr. ", tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
      summarise(format(round(sum(k_billetpris, -k_arrangørpris, na.rm = T), 0), big.mark = ".")), " ",
    var_ikon$k_penge),
  
  # Billetantal Ping Pong (heltal)
  k_int_billetantal_ping_pong = as.integer(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & grepl("Ping Pong", k_billettype)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(n())),
  
  # Billetantal Ping Ping maks. (heltal)
  k_int_billetantal_ping_pong_maks = as.integer(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Ping Pong", k_billettype)) %>%
      summarise(max(k_billetantal_maks, na.rm = T))),
  
  # Aldersforskel yngste- og ældste Ping Pong deltager (heltal)
  k_int_aldersforskel = as.integer(
    as.integer(
      tbl0_join_aktuel %>%
        filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & grepl("Ping Pong", k_billettype)) %>%
        filter(k_født == min(k_født)) %>% select(k_alder))
    - as.integer(
        tbl0_join_aktuel %>%
          filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & grepl("Ping Pong", k_billettype)) %>%
          filter(k_født == max(k_født)) %>% select(k_alder))),
  
  # Antal puljer (heltal)
  k_int_antal_puljer = as.integer(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & grepl("Ping Pong", k_billettype)) %>%
      distinct(k_deltager_id, .keep_all = T) %>%
      summarise(ceiling(n()/tbl0_unik$k_puljeantal))),
  
  # Præmiepenge pr. deltager (numerisk)
  k_num_præmiepenge_pr_deltager = as.numeric(
    tbl0_join_aktuel %>%
      filter(!is.na(k_deltager_id) & grepl("Ping Pong", k_billettype)) %>%
      summarise(max(k_arrangørpris, na.rm = T))),
  
  check.names = F)

#' # Status
# Status ------------------------------------------------------------------
#+ eval=F, warning=F, message=F

tbl0_status <- data.frame(
  
  # Status CTA/plakat
  k_cta_plakat = if(tbl0_input$k_status_1_2_3_4 == 1) {
    "<img src=Filer/Generelt/Forside.jpg style=width:30em;max-width:100%;border-radius:20px>"
  } else if(tbl0_input$k_status_1_2_3_4 == 2) {
    paste0(
      "![](Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Teaserplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".png){width=30em}",
      "<br>",
      "<figcaption>",
      "[<i style=font-size:80%>[Klik her for teaserplakat som PDF til udskrift]</i>]",
      "(Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Teaserplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".pdf){target=_blank}",
      "</figcaption><p><p>")
  } else if(tbl0_input$k_status_1_2_3_4 == 3 | tbl0_input$k_status_1_2_3_4 == 4) {
    paste0(
    	knitr::raw_html(paste0(
    	  '<p style="text-align:center;width:50em;max-width:100%">
        <b style=font-size:120%>Nedtælling</b>
    	  <br>
    	  <b style=font-size:80%>', tbl0_unik$k_event_ping_pong_år, '</b>
        <br>
        <span id="nedtællingsur"></span>
        <br>
        <b style=font-size:80%>', format(tbl0_unik$k_eventdato, "%d.%m.%Y kl. %H:%M:%S"), '</b>
	
    		<script>
    		// Opdater nedtællingsur hvert sekund
    		var x = setInterval(function() {
    		  
    		  // Differencen mellem eventdatoen og dags dato
    		  var nedtællingsur_dif = new Date("', tbl0_unik$k_eventdato, '").getTime() - new Date().getTime();
    		  
  	      // Vis resultatet i elementet med id="nedtællingsur"
  	      document.getElementById("nedtællingsur").innerHTML =
		      
    		  "<b style=display:inline-block;border-style:solid;padding:5px;width:60px;text-align:center>" +
  	      Math.floor(nedtællingsur_dif / (1000 * 60 * 60 * 24)) +
  	      "<br><span style=font-size:80%>dage</span></b>&ensp;" +
  	      
    		  "<b style=display:inline-block;border-style:solid;padding:5px;width:60px;text-align:center>" +
  	      Math.floor((nedtællingsur_dif % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60)) +
  	      "<br><span style=font-size:80%>timer</span></b>&ensp;" +
  	      
    		  "<b style=display:inline-block;border-style:solid;padding:5px;width:60px;text-align:center>" +
  	      Math.floor((nedtællingsur_dif % (1000 * 60 * 60)) / (1000 * 60)) +
  	      "<br><span style=font-size:80%>min.</span></b>&ensp;" +
  	      
    		  "<b style=display:inline-block;border-style:solid;padding:5px;width:60px;text-align:center>" +
  	      Math.floor((nedtællingsur_dif % (1000 * 60)) / 1000) +
  	      "<br><span style=font-size:80%>sek.</span></b>";
		      
    		  // Hvis nedtællingsur er udløbet
  	      if (nedtællingsur_dif < 0) {
    	      clearInterval(x);
    	      document.getElementById("nedtællingsur").innerHTML = "<i>Eventet er udløbet</i>";
  	      }
	      }, 1000);
	      </script>')),
    	"<br><br>",
      "<a style=display:inline-block;background:#FF4A6E;color:#FFFFFF;",
      "border-radius:40px;padding-left:50px;padding-right:50px;padding-top:5px;padding-bottom:5px;",
      "text-decoration:none href=indbydelse-tilmelding.qmd#tilmelding>",
      "<b style=font-size:150%>", var_ikon$k_billet, " Tilmeld</b>",
    	"<br>",
      "<i style=font-size:90%>", tbl0_unik$k_event_ping_pong_år, "</i></a>",
      "<br><br>",
    	"<i style=font-size:80%>",
    	"Hurtigt overblik over eventet ses i indbydelsesplakaten ", var_ikon$k_hånd_ned, "</i>",
    	"<br>",
      "![](Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Indbydelsesplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".png)",
    	"<br>",
      "<span>",
      "[<i style=font-size:80%>",
    	"[Klik her for indbydelesplakat som PDF til udskrift]</i>]",
      "(Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Indbydelsesplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".pdf){target=_blank}",
      "</span>",
    	"</p>")
  },
  
  # Status forside DM
  k_forside_dm = if(tbl0_input$k_status_1_2_3_4 == 1) {
    paste(
      "<i>Nærmere information om DM i Ping Pong",
      unique(max(tbl0_join_alle$k_eventår)), "følger.</i>")
  } else if(tbl0_input$k_status_1_2_3_4 == 2) {
    paste0(
      "<i>", tbl0_unik$k_event_ping_pong_år, " afholdes ",
      trimws(format(tbl0_unik$k_eventdato, "%e. %B")), " i",
      "[",
      tbl0_unik$k_eventsted, "](", tbl0_unik$k_eventsted_url, "){target=_blank}. ",
      "Der åbnes for tilmelding", trimws(format(tbl0_unik$k_åbningsdato, "%e. %B")), 
      "hvor der vil komme en fane med hhv. <q>Indbydelse & tilmelding</q> samt ",
      "<q>Præmier & deltagere</q>, som vil blive opdateret løbende.</i>")
  } else if(tbl0_input$k_status_1_2_3_4 == 3 | tbl0_input$k_status_1_2_3_4 == 4) {
    paste0(
    	"<i style=font-size:100%>",
    	"<b>", str_to_sentence(trimws(format(tbl0_unik$k_eventdato, "%A %e. %B"))), " i ",
    	"[", tbl0_unik$k_eventsted, "](", tbl0_unik$k_eventsted_url, "){target=_blank}</b></i>",
    	"<br>",
    	"<i style=font-size:80%>Først til mølle-princip ∙ Tilmeldingsfrist ",
    	trimws(format(tbl0_unik$k_tilmeldingsfrist, "%e. %B")), "</i>",
      "<br><br>",
      "<ul>",
      "<li><p>", var_ikon$k_billet, " ", "[<b>Indbydelse & tilmelding</b>](indbydelse-tilmelding.qmd)",
      "<br>",
      "<i>Indbydelse, tidsplan, praktisk info samt tilmelding/betaling",
      "&nbsp;til ", tbl0_unik$k_event_ping_pong_år, ".</i></p></li>",
      "<li><p>", var_ikon$k_gentagelse, " ", "[<b>Præmier & deltagere</b>](praemier-deltagere.qmd)",
      "<br>",
      "<i>Præmier og deltagere opdateres løbende",
      "&nbsp;til ", tbl0_unik$k_event_ping_pong_år, ".</i></p>",
      "<i style=font-size:80%;display:inline-block;border:0.6px;border-style:solid;padding:5px>",
      tbl0_statistik$k_billetantal_billettype, "</i></li>",
      "</ul>")
  },
  
  # Status forside Facebook
  k_forside_facebook = if(tbl0_input$k_status_1_2_3_4 == 1) {
    paste0(
      "<i>Like og følg den officielle ",
      "[<b>Facebook-side <q>Ping Pong DK</q></b>]",
      "(https://www.facebook.com/{{< var var.facebook_side_id >}}){target=_blank}",
      "&nbsp;for at holde dig opdateret.</i>")
  } else if(tbl0_input$k_status_1_2_3_4 == 2 | tbl0_input$k_status_1_2_3_4 == 3 |
            tbl0_input$k_status_1_2_3_4 == 4) {
    paste0(
      "<i><p>Del gerne budskabet via ",
      "[<b>Facebook-begivenheden <q>", "DM I PING PONG {{< var var.event_år >}}", "</q></b>]",
      "({{< var var.facebook_event_url >}}){target=_blank}",
      "&nbsp;ved at trykke interesseret/deltager og inviter folk.</p>",
      "Like og følg",
      "&nbsp;[Facebook-siden <q>Ping Pong DK</q>]",
      "(https://www.facebook.com/{{< var var.facebook_side_id >}}){target=_blank}",
      "&nbsp;for at holde dig opdateret.</i>")
  },
  
  # k_tip_indbydelse_tilmelding
  k_tip_indbydelse_tilmelding = paste(
    var_ikon$k_billet, "Indbydelse, tidsplan og praktisk info til",
    tbl0_unik$k_event_ping_pong_år, "ses [<b>HER</b>](indbydelse-tilmelding.qmd).</i>"),
  
  # k_tip_præmier_deltagere
  k_tip_præmier_deltagere = paste0(
      "<p>",
      var_ikon$k_gentagelse, " Præmier og deltagere opdateres løbende til ",
      tbl0_unik$k_event_ping_pong_år, " [<b>HER</b>](praemier-deltagere.qmd).",
      "</p>",
      "<i style=font-size:80%;display:inline-block;border:0.6px;border-style:solid;padding:5px>",
      tbl0_statistik$k_billetantal_billettype, "</i>"),
  
  # k_tip_regler
  k_tip_regler = paste(
    var_ikon$k_regler, "I Ping Pong tages det bedste fra fortidens- og nutidens bordtennis og kan", 
    "sammenlignes med ordsproget <q>gammel vin på nye flasker</q>. Der er nogle få regler, der",
    "adskiller Ping Pong fra bordtennis, bl.a. spilles der til 15 point (14-14 er afgørende bold),",
    "alle spiller på lige vilkår med sandpapirsbat, hvor der byttes bat mellem hvert sæt, og der kan",
    "tages <q>dobbeltpoint</q>. Se mere [<b>HER</b>](regler.qmd)."),
  
  # k_tip_wcpp
  k_tip_wcpp = paste(
    var_ikon$k_wcpp, "World Championship of Ping Pong (WCPP) afholdes sædvanligvis",
    "i London med en præmiesum på $100.000 og eksponeres på bl.a. Viaplay Sport og Sky Sports.",
    "Se mere [<b>HER</b>](wcpp.qmd)."),
  
  check.names = F)

#' # Præmier
# Præmier -----------------------------------------------------------------

#' ## Pengepræmier
#+ eval=F, warning=F, message=F

tbl1_præmier_penge <- tbl0_join_alle %>%
  filter(grepl("Ping Pong", k_billettype) &
    !is.na(k_slutspil) & !is.na(k_placering) & !is.na(k_præmiepenge)) %>%
  select(
    k_deltager_id,
    k_event_år_billettype,
    k_eventår,
    k_slutspil,
    k_placering,
    k_præmiepenge,
    k_billetantal_maks,
    k_billetantal_billettype_status,
    k_billetpris) %>%
  
  group_by(k_event_år_billettype) %>%
  mutate(k_potentiel_præmiesum = sum(k_præmiepenge)) %>%
  mutate(k_præmiepenge_pct = k_præmiepenge/k_potentiel_præmiesum) %>%
  ungroup() %>%
  mutate(k_aktuel_præmiepenge = k_potentiel_præmiesum*k_billetantal_billettype_status/
           k_billetantal_maks*k_præmiepenge_pct) %>%
  mutate(k_spillergebyr_præmiepenge = k_potentiel_præmiesum/k_billetantal_maks) %>%
  mutate(k_spillergebyr_stævnearrangør = k_billetpris-(k_potentiel_præmiesum/k_billetantal_maks)) %>%
  
  mutate(across("k_eventår", \(x) factor(x, ordered = T))) %>%
  mutate(k_rank = "3") %>%
  group_by(k_event_år_billettype) %>%
  bind_rows(summarise(., across(where(is.numeric), \(x) sum(x)), .groups = "keep")) %>% ungroup() %>%
  mutate(k_rank = ifelse(!is.na(k_rank), k_rank, "1")) %>%
  
  group_by(k_event_år_billettype, k_slutspil) %>%
  bind_rows(summarise(., across(where(is.numeric), \(x) sum(x)), .groups = "keep")) %>% ungroup() %>%
  mutate(k_rank = ifelse(!is.na(k_rank), k_rank, "2")) %>%
  group_by(k_event_år_billettype) %>%
  fill(k_eventår, .direction = "updown") %>%
  ungroup() %>%
  
  mutate(across(c("k_slutspil", "k_placering"), \(x) as.character(x))) %>%
  mutate(across("k_rank", \(x) as.integer(x))) %>%
  mutate(k_slutspil = ifelse(!is.na(k_slutspil), k_slutspil, "1")) %>%
  arrange(desc(k_eventår), k_slutspil, k_rank, k_placering) %>%
  distinct(k_event_år_billettype, k_slutspil, k_placering, .keep_all = T) %>%
  mutate(k_placering = case_when(
    k_rank == "1" ~ "Præmiesum",
    k_rank == "2" ~ k_slutspil,
    k_rank == "3" ~ k_placering,
    TRUE ~ NA_character_)) %>%
  
  mutate(k_aktuel_præmiepenge = paste("kr.", format(round(
    0.001+k_aktuel_præmiepenge), big.mark = "."))) %>%
  mutate(k_præmiepenge = paste("kr.", format(round(
    0.001+k_præmiepenge), big.mark = "."))) %>%
  mutate(k_præmiepenge_pct = paste0(signif(
    0.001+100*k_præmiepenge_pct, 3), "%")) %>%
  mutate(k_billetpris = paste("kr.", format(round(
    0.001+k_billetpris), big.mark = "."))) %>%
  mutate(k_spillergebyr_præmiepenge = paste("kr.", format(round(
    0.001+k_spillergebyr_præmiepenge), big.mark = "."))) %>%
  mutate(k_spillergebyr_stævnearrangør = paste("kr.", format(round(
    0.001+k_spillergebyr_stævnearrangør), big.mark = ".")))

#' ## Gaver
#+ eval=F, warning=F, message=F

tbl1_præmier_yngst_ældst <- tbl0_join_aktuel %>%
  filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & grepl("Ping Pong", k_billettype)) %>%
    filter(k_født == max(k_født) | k_født == min(k_født)) %>%
  mutate(Født  = format(k_født, "%d.%m.%Y")) %>%
  mutate(k_yngst_ældst = case_when(
    k_alder == min(k_alder) ~ "Yngst",
    k_alder == max(k_alder) ~ "Ældst")) %>%
	arrange(desc(k_født), k_navn) %>%
  select(
    " "      = k_yngst_ældst,
    "&emsp;" = k_logo_klub,
    "Navn"   = k_navn_billettype,
    "Født"   = Født)

if(nrow(tbl1_præmier_yngst_ældst) == 0) {
  kbl1_præmier_yngst_ældst <- data.frame() %>% kbl()
} else {
kbl1_præmier_yngst_ældst <- tbl1_præmier_yngst_ældst %>%
  kbl(col.names = NA, align = "lclc", escape = F, table.attr = "data-quarto-disable-processing=true",
      caption = paste0(
      "<i style=font-size:90%> ", var_ikon$k_gave, " ",
      "Gave til <b>yngste</b>- og <b>ældste</b> Ping Pong deltager<br>",
      "for at hylde mangfoldigheden</i>")) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
  column_spec(1, bold = T) %>%
	column_spec(1:4, extra_css = "border-top:0.7px solid #999999") %>%
  footnote(ifelse(
    tbl0_statistik$k_int_aldersforskel == 0, "<i style=font-size:80%>Ingen aldersforskel.</i>",
    paste("<i style=font-size:80%>Aldersforskel", tbl0_statistik$k_int_aldersforskel, "år.</i>")),
    general_title = "", escape = F) %>%
  gsub("\\{\\{&lt;|&lt;", "{{<", .) %>% gsub("&gt;}}", ">}}", .)
}
kbl1_præmier_yngst_ældst

#' # Deltagere
# Deltagere ---------------------------------------------------------------

#' ## Foreløbige deltagere
#+ eval=F, warning=F, message=F

tbl2_deltagere_foreløbig <- tbl0_join_aktuel %>%
  filter(!is.na(k_deltager_id)) %>%
  distinct(k_deltager_id, k_status, .keep_all = T) %>%
  arrange(k_status, k_billettype, desc(k_født), k_navn) %>%
  group_by(k_status) %>%
  mutate(Nr. = row_number()) %>%
  ungroup() %>%
  select(
    Nr.,
    "&emsp;" = k_logo_klub,
    "Navn"   = k_navn_billettype,
    k_status)

if(nrow(tbl2_deltagere_foreløbig) == 0) {
  kbl2_deltagere_foreløbig <- data.frame() %>% kbl()
} else {
kbl2_deltagere_foreløbig <- tbl2_deltagere_foreløbig %>%
  kbl(col.names = NA, align = "ccl", escape = F, table.attr = "data-quarto-disable-processing=true",
      caption = paste0(
        var_ikon$k_person, " <b>Foreløbige deltagere</b>",
        "<br>",
        "<i style=font-size:80%>", tbl0_unik$k_puljeantal, "-mandspuljer ",
        "inddelt efter snake-system vises efter tilmeldingsfrist ",
        trimws(format(tbl0_unik$k_tilmeldingsfrist, "%d.%m.%Y")), ".</i>")) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Sorteret efter billettype og fødselsdato" = 4),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
  row_spec(which(grepl("Afbud", tbl2_deltagere_foreløbig$k_status)),
           strikeout = T, italic = T, color = tbl0_unik$k_farve_1) %>%
	column_spec(1:4, extra_css = "border-top:0.7px solid #999999") %>%
  remove_column(4) %>%
  gsub("\\{\\{&lt;|&lt;", "{{<", .) %>% gsub("&gt;}}", ">}}", .)
}
kbl2_deltagere_foreløbig

#' ## Puljer
#+ eval=F, warning=F, message=F

tbl2_deltagere_puljer <- tbl0_join_aktuel %>%
  filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & grepl("Ping Pong", k_billettype)) %>%
  arrange(
    k_rang1,
    desc(k_rating2),
    k_rang3,
    k_deltager_id) %>%
  add_row(k_deltager_id = rep(
    NA, ifelse(nrow(tbl0_join_aktuel) == 0, 1,
    tbl0_statistik$k_int_antal_puljer*tbl0_unik$k_puljeantal-
      tbl0_statistik$k_int_billetantal_ping_pong+tbl0_statistik$k_int_antal_puljer))) %>%
  mutate(Seedningslag = ceiling(row_number()/tbl0_statistik$k_int_antal_puljer)) %>%
  group_by(Seedningslag) %>%
  mutate(Puljenr. = case_when(
    Seedningslag %% 2 == 1 ~ row_number(),
    Seedningslag %% 1 == 0 ~ rev(row_number()))) %>%
  ungroup() %>%
  mutate(Nr. = row_number()) %>%
  arrange(Puljenr.) %>%
  filter(!is.na(k_deltager_id)) %>%
  select(
    Nr.,
    "&emsp;" = k_logo_klub,
    "Navn"   = k_navn_billettype,
    "Rating" = k_rating,
    Seedningslag)

if(nrow(tbl2_deltagere_puljer) == 0) {
  kbl2_deltagere_puljer <- data.frame() %>% kbl()
} else {
kbl2_deltagere_puljer <- tbl2_deltagere_puljer %>%
  kbl(col.names = NA, align = "cclr", escape = F, table.attr = "data-quarto-disable-processing=true",
      caption = paste0(
        "<b>", var_ikon$k_ping_pong, " ",
        tbl0_unik$k_puljeantal, "-mandspuljer til ", tbl0_unik$k_event_ping_pong_år, "</b>",
        "<br>",
        "<i style=font-size:80%>", tbl0_statistik$k_int_billetantal_ping_pong, " ",
        ifelse(tbl0_statistik$k_int_billetantal_ping_pong == 1, "tilmelding", "tilmeldinger"),
        " fordelt over ", tbl0_statistik$k_int_antal_puljer, " ",
        ifelse(tbl0_statistik$k_int_antal_puljer == 1, "pulje", "puljer"), " efter snake-system.",
        "<br>",
        "Seedet 1-4 efter sidste <q>Final 4</q>.",
        "<br>",
        "Rating er opdateret pr. ",
        format(tbl0_unik$k_ratingopdatering, "%d.%m.%Y"), ".</i>")) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Bemærk: Puljerne kan ændre sig ved drive-in/afbud" = 5),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
  row_spec(which(tbl2_deltagere_puljer$Seedningslag == "1"), bold = T,
           extra_css = "border:hidden;border-top:0.7px solid #111111") %>%
  remove_column(5) %>%
  gsub("\\{\\{&lt;|&lt;", "{{<", .) %>% gsub("&gt;}}", ">}}", .)
}
kbl2_deltagere_puljer

#' ## Kun til festen inkl. afbud
#+ eval=F, warning=F, message=F

tbl2_deltagere_andet <- tbl0_join_aktuel %>%
  filter(
    !is.na(k_deltager_id) &
    !grepl("Ping Pong", k_billettype) &
      sub(" .*", "", k_billetsalg_pr_tilmelding) == 1 |
      grepl("Afbud", k_status)) %>%
  distinct(k_deltager_id, k_status, .keep_all = T) %>%
  arrange(
    k_status,
    desc(sub(" .*", "", k_billetsalg_pr_tilmelding)),
    desc(k_født),
    k_deltager_id) %>%
  group_by(k_status) %>%
  mutate(Nr. = row_number()) %>%
  ungroup() %>%
  select(
    Nr.,
    "&emsp;" = k_logo_klub,
    "Navn"   = k_navn_billettype,
    k_status)

if(nrow(tbl2_deltagere_andet) == 0) {
  kbl2_deltagere_andet <- data.frame() %>% kbl()
} else {
kbl2_deltagere_andet <- tbl2_deltagere_andet %>%
  kbl(col.names = NA, align = "ccl", escape = F, table.attr = "data-quarto-disable-processing=true",
  		caption = "Andet end Ping Pong + evt. afbud") %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  add_header_above(
    c("Sorteret efter fødselsdato" = 4),
    italic = T, align = "c", font_size = "small", escape = F,
    extra_css = "border:hidden;border-bottom:1.5px solid #111111") %>%
  row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
  row_spec(which(grepl("Afbud", tbl2_deltagere_andet$k_status)),
           strikeout = T, color = tbl0_unik$k_farve_1) %>%
  row_spec(which(tbl2_deltagere_andet$Nr. == "1"),
           extra_css = "border:hidden;border-top:0.7px solid #111111") %>%
  remove_column(4) %>%
  gsub("\\{\\{&lt;|&lt;", "{{<", .) %>% gsub("&gt;}}", ">}}", .)
}
kbl2_deltagere_andet

#' ## Manglende klubber
#+ eval=F, warning=F, message=F

tbl2_klubber_mangler <- tbl0_join_alle %>%
	anti_join(y = tbl0_join_aktuel, by = "k_klub") %>%
	group_by(k_klub) %>%
	slice(which.max(k_ordredato)) %>%
	ungroup() %>%
	arrange(k_status, desc(k_eventår), k_region, k_klub) %>%
	group_by(k_status) %>%
	mutate(Nr. = row_number()) %>%
	ungroup() %>%
	select(
		Nr.,
		" "      = k_logo_klub,
		"Klub"   = k_klub,
		"Region" = k_region,
		"Sidst"  = k_eventår,
		k_status)

if(nrow(tbl2_klubber_mangler) == 0) {
	kbl2_deltagere_mangler <- data.frame() %>% kbl()
} else {
	kbl2_klubber_mangler <- tbl2_klubber_mangler %>%
		kbl(col.names = NA, align = "l", escape = F, table.attr = "data-quarto-disable-processing=true",
				caption = paste0(
					var_ikon$k_person, " <b>Manglende klubber</b>",
					"<br>",
					"<i style=font-size:80%>Sorteret efter år, region og klub</i>")) %>%
		kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
		row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
		row_spec(which(grepl("Afbud", tbl2_klubber_mangler$k_status)),
						 strikeout = T, color = tbl0_unik$k_farve_1) %>%
		column_spec(1, width_min = "2em") %>%
		column_spec(1:5, extra_css = "border-top:0.7px solid #999999") %>%
		remove_column(6)
}
kbl2_klubber_mangler

#' ## Manglende deltagere
#+ eval=F, warning=F, message=F

tbl2_deltagere_mangler <- tbl0_join_alle %>%
	anti_join(y = tbl0_join_aktuel, by = "k_deltager_id") %>%
	group_by(k_deltager_id) %>%
	slice(which.max(k_ordredato)) %>%
	ungroup() %>%
	add_count(k_klub) %>%
	arrange(
		k_status, desc(n), ifelse(grepl("Ingen klub|Udlandet", k_klub), 2, 1), k_klub, desc(k_eventår),
		desc(sub(" .*", "", k_billetsalg_pr_tilmelding)), k_billettype, k_navn) %>%
	group_by(k_status) %>%
	mutate(Nr. = row_number()) %>%
	ungroup() %>%
	select(
		Nr.,
		" "     = k_logo_klub,
		"Navn"  = k_navn_billettype,
		"Sidst" = k_eventår,
		k_status)

if(nrow(tbl2_deltagere_mangler) == 0) {
	kbl2_deltagere_mangler <- data.frame() %>% kbl()
} else {
kbl2_deltagere_mangler <- tbl2_deltagere_mangler %>%
	kbl(col.names = NA, align = "l", escape = F, table.attr = "data-quarto-disable-processing=true",
			caption = paste0(
				var_ikon$k_person, " <b>Manglende deltagere</b>",
				"<br>",
				"<i style=font-size:80%>Sorteret efter klubantal og år</i>")) %>%
	kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
	row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
	row_spec(which(grepl("Afbud", tbl2_deltagere_mangler$k_status)),
					 strikeout = T, color = tbl0_unik$k_farve_1) %>%
	column_spec(1, width_min = "2em") %>%
	column_spec(1:4, extra_css = "border-top:0.7px solid #999999") %>%
	remove_column(5)
}
kbl2_deltagere_mangler

#' # Resultater
# Resultater --------------------------------------------------------------

#' ## DM-vindere statistik
#+ eval=F, warning=F, message=F

tbl3_dm_resultater <- tbl0_join_alle %>%
  filter(grepl("1", k_placering) & grepl("A-slutspil", k_slutspil) & grepl("Tilmeldt", k_status)) %>%
  filter(k_eventdato <= Sys.Date()) %>%
  add_row(k_eventår = 2020, k_navn_klub = "Aflyst pga. Covid-19") %>%
  arrange(desc(k_eventår)) %>%
  select(
    "År"     = k_eventår,
    "&emsp;" = k_logo_klub,
    "Navn"   = k_navn_klub)

if(nrow(tbl3_dm_resultater) == 0) {
  kbl3_dm_resultater <- data.frame() %>% kbl()
} else {
kbl3_dm_resultater <- tbl3_dm_resultater %>%
  kbl(col.names = NA, align = "ccl", escape = F, table.attr = "data-quarto-disable-processing=true",
      caption = paste(var_ikon$k_pokal, "<b>DM-vindere statistik</b>")) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  row_spec(0, background = "#1C2833", color = "#FFFFFF") %>%
  row_spec(which(tbl3_dm_resultater$År == "2020"),
           strikeout = T, italic = T, color = "#5D6D7E") %>%
  footnote(
    "<i>Webscraped fra BTDK's hjemmeside
    <a href=https://bordtennisdanmark.dk/statistik/ping-pong-dm
    target=_blank><b>HER</b></a></i>", general_title = "", escape = F)
}
kbl3_dm_resultater

#' ## Resultater sidste DM
#+ eval=F, warning=F, message=F

tbl3_resultater_sidste_dm <- tbl0_join_alle %>%
  filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & !is.na(k_slutspil)) %>%
  filter(k_eventår < tbl0_unik$k_eventår) %>%
  filter(k_eventnr == min(k_eventnr)) %>%
  arrange(k_eventnr, k_slutspil, k_placering) %>%
  slice_head(n = 4) %>%
  select(
    "Placering"   = k_placering,
    "&emsp;"      = k_logo_klub,
    "Navn"        = k_navn_klub,
    "k_slutspil"  = k_slutspil,
    "k_eventsted" = k_eventsted,
    "k_eventdato" = k_eventdato,
    "k_farve_1"   = k_farve_1)

if(nrow(tbl3_resultater_sidste_dm) == 0) {
  kbl3_resultater_sidste_dm <- data.frame() %>% kbl()
} else {
kbl3_resultater_sidste_dm <- tbl3_resultater_sidste_dm %>%
  kbl(col.names = NA, align = "lcl", escape = F, table.attr = "data-quarto-disable-processing=true",
      caption = paste(
        var_ikon$k_pokal, "<b>DM-resultater sidste <q>Final 4</q></b>",
        "<br>",
        "<i style=font-size:80%>Afholdt",
        format(unique(tbl3_resultater_sidste_dm$k_eventdato), "%d.%m.%Y"),
        "i", unique(tbl3_resultater_sidste_dm$k_eventsted), "</i>")) %>%
  kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
  row_spec(0, background = unique(tbl3_resultater_sidste_dm$k_farve_1), color = "#FFFFFF") %>%
	remove_column(4:7) %>%
	footnote(
		paste0(
			"<i style=font-size:80%>", tbl0_join_alle %>%
				filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
				filter(k_eventår < tbl0_unik$k_eventår) %>%
				filter(k_eventnr == min(k_eventnr)) %>%
				count(k_billettype, k_ikon_billettype, k_billetantal_maks, sort = T) %>%
				mutate(pct = percent(n/sum(n), digits = 0)) %>%
				mutate(label = paste0(
					"Der var ", n, " ",  ifelse(n == 1, "tilmelding", "tilmeldinger"), " til ",
					sub(".*? ", "", ifelse(
						grepl("Ping Pong", k_billettype),
						paste(k_billettype, format(unique(tbl3_resultater_sidste_dm$k_eventdato), "%Y")),
						as.character(k_billettype))), " ", k_ikon_billettype)) %>%
				summarise(label = str_c(label, collapse = "<br>")), "</i>"),
		general_title = "", escape = F) %>%
  gsub("\\{\\{&lt;|&lt;", "{{<", .) %>% gsub("&gt;}}", ">}}", .)
}
kbl3_resultater_sidste_dm

#' # Dashboards
# Dashboards --------------------------------------------------------------

#' ## Klubber
#+ eval=F, warning=F, message=F

graf4_klubber <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
	mutate(k_klub = paste0(k_klub, "&emsp;", k_logo_klub)) %>%
	ggplot(mapping = aes(y = fct_rev(fct_infreq(k_klub)), fill = k_billettype)) +
	geom_bar(width = 0.9, position = position_stack(reverse = T)) +
	geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
						vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF", fontface = "bold") +
	scale_fill_manual(values = c(
		"🏓 DM i Ping Pong"    = "#398FCC",
		"🥳 Fest om aftenen"   = "#F1C40F",
		"🎉 The Old Irish Pub" = "#9B59B6",
		"🥪 Frokost"           = "#48C9B0")) +
	guides(fill = guide_legend(title = element_blank())) +
	xlab(element_blank()) + ylab(element_blank()) +
	theme(legend.position      = "right",
				legend.direction     = "vertical",
				plot.title           = element_text(hjust = 0.5),
				plot.subtitle        = element_text(hjust = 0.5),
				legend.text          = element_text(size = 15),
				axis.text.y          = element_markdown(size = 15, face = "bold"),
				axis.ticks.y         = element_blank(),
				axis.text.x          = element_blank(),
				axis.ticks.x         = element_blank(),
				panel.background     = element_blank(),
				panel.grid.major     = element_blank(),
				panel.grid.minor     = element_blank())
graf4_klubber

tbl4_klubber <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id)) %>%
	add_count(k_klub) %>%
	distinct(k_deltager_id, .keep_all = T) %>%
	arrange(
		k_status, desc(n), ifelse(grepl("Ingen klub|Udlandet", k_klub), 2, 1), k_klub,
		desc(sub(" .*", "", k_billetsalg_pr_tilmelding)), k_billettype, k_navn) %>%
	select(
		" "    = k_logo_klub,
		"Navn" = k_navn_billettype,
		"Klub" = k_klub,
		k_status)

kbl4_klubber <- tbl4_klubber %>%
	kbl(col.names = NA, align = "l", escape = F, table.attr = "data-quarto-disable-processing=true",
			caption = "<i style=font-size:80%>Sorteret efter klubantal</i>") %>%
	kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
	row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
	row_spec(which(grepl("Afbud", tbl4_klubber$k_status)),
					 strikeout = T, color = tbl0_unik$k_farve_1) %>%
	column_spec(1, width_min = "2em") %>%
	column_spec(1:3, extra_css = "border-top:0.7px solid #999999") %>%
	remove_column(4) %>%
	scroll_box(height = "400px")
kbl4_klubber

#' ## Deltagere fordelt på Danmarkskort
#+ eval=F, warning=F, message=F

if(nrow(tbl0_join_aktuel) == 0) {
	graf4_DK <- data.frame() %>% kbl()
} else {
	graf4_DK <- tbl0_join_aktuel %>%
		filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status) & !grepl("Ingen klub|Udlandet", k_klub)) %>%
		mutate(across("k_postnr", \(x) as.integer(x))) %>%
		distinct(k_deltager_id, .keep_all = T) %>%
		mutate(k_region = case_when(
			k_region == "Nordjylland" ~ "nordjylland",
			k_region == "Midtjylland" ~ "midtjylland",
			k_region == "Syddanmark"  ~ "syddanmark",
			k_region == "Sjælland"    ~ "sjaelland",
			k_region == "Hovedstaden" ~ "hovedstaden")) %>%
		count(k_postnr, name = "Deltagerantal", .drop = FALSE) %>%
		plotDK::plotDK(
			value        = "Deltagerantal",
			id           = "k_postnr",
			plotlevel    = "zipcode",
			show_missing = T,
			show_borders = F,
			interactive  = F)
}
graf4_DK

tbl4_DK <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id)) %>%
	distinct(k_deltager_id, .keep_all = T) %>%
	mutate(k_samlet_postnr_by = ifelse(
		grepl("Ingen klub|Udlandet", k_klub), k_klub, paste0(k_samlet_postnr_by, ", ", k_region))) %>%
	arrange(
		k_status, k_region, desc(k_postnr),
		desc(sub(" .*", "", k_billetsalg_pr_tilmelding)), k_billettype, k_navn) %>%
	select(
		" "        = k_logo_klub,
		"Navn"     = k_navn_billettype,
		"Lokation" = k_samlet_postnr_by,
		k_status)

kbl4_DK <- tbl4_DK %>%
	kbl(col.names = NA, align = "l", escape = F, table.attr = "data-quarto-disable-processing=true",
			caption = "<i style=font-size:80%>Sorteret efter region og postnr.</i>") %>%
	kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
	row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
	row_spec(which(grepl("Afbud", tbl4_DK$k_status)),
					 strikeout = T, color = tbl0_unik$k_farve_1) %>%
	column_spec(1, width_min = "2em") %>%
	column_spec(1:3, extra_css = "border-top:0.7px solid #999999") %>%
	remove_column(4) %>%
	scroll_box(height = "400px")
kbl4_DK

#' ## Aldersgruppe
#+ eval=F, warning=F, message=F

graf4_aldersgruppe <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
	ggplot(mapping = aes(y = fct_rev(k_aldersgruppe), fill = k_billettype)) +
	geom_bar(width = 0.9, position = position_stack(reverse = T)) +
	geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
						vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF", fontface = "bold") +
	scale_fill_manual(values = c(
		"🏓 DM i Ping Pong"    = "#398FCC",
		"🥳 Fest om aftenen"   = "#F1C40F",
		"🎉 The Old Irish Pub" = "#9B59B6",
		"🥪 Frokost"           = "#48C9B0")) +
	guides(fill = guide_legend(title = element_blank())) +
	xlab(element_blank()) + ylab(element_blank()) +
	theme(legend.position      = "right",
				legend.direction     = "vertical",
				plot.title           = element_text(hjust = 0.5),
				plot.subtitle        = element_text(hjust = 0.5),
				legend.text          = element_text(size = 15),
				axis.text.y          = element_text(size = 15, face = "bold"),
				axis.ticks.y         = element_blank(),
				axis.text.x          = element_blank(),
				axis.ticks.x         = element_blank(),
				panel.background     = element_blank(),
				panel.grid.major     = element_blank(),
				panel.grid.minor     = element_blank())
graf4_aldersgruppe

tbl4_aldersgruppe <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id)) %>%
	distinct(k_deltager_id, .keep_all = T) %>%
	mutate(k_aldersgruppe = paste(
		k_aldersgruppe, var_ikon$k_født, "<br>", format(k_født, "%d.%m.%Y"))) %>%
	arrange(k_status, desc(k_født), k_navn) %>%
	select(
		" "            = k_logo_klub,
		"Navn"         = k_navn_billettype,
		"Aldersgruppe" = k_aldersgruppe,
		k_status)

kbl4_aldersgruppe <- tbl4_aldersgruppe %>%
	kbl(col.names = NA, align = "l", escape = F, table.attr = "data-quarto-disable-processing=true",
			caption = "<i style=font-size:80%>Sorteret efter fødselsdato</i>") %>%
	kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
	row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
	row_spec(which(grepl("Afbud", tbl4_aldersgruppe$k_status)),
					 strikeout = T, color = tbl0_unik$k_farve_1) %>%
	column_spec(1, width_min = "2em") %>%
	column_spec(1:3, extra_css = "border-top:0.7px solid #999999") %>%
	remove_column(4) %>%
	scroll_box(height = "400px")
kbl4_aldersgruppe

#' ## Køn
#+ eval=F, warning=F, message=F

graf4_køn <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
	ggplot(mapping = aes(y = fct_rev(fct_infreq(k_køn)), fill = k_billettype)) +
	geom_bar(width = 0.9, position = position_stack(reverse = T)) +
	geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
						vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF", fontface = "bold") +
	scale_fill_manual(values = c(
		"🏓 DM i Ping Pong"    = "#398FCC",
		"🥳 Fest om aftenen"   = "#F1C40F",
		"🎉 The Old Irish Pub" = "#9B59B6",
		"🥪 Frokost"           = "#48C9B0")) +
	guides(fill = guide_legend(title = element_blank())) +
	xlab(element_blank()) + ylab(element_blank()) +
	theme(legend.position      = "right",
				legend.direction     = "vertical",
				plot.title           = element_text(hjust = 0.5),
				plot.subtitle        = element_text(hjust = 0.5),
				legend.text          = element_text(size = 15),
				axis.text.y          = element_text(size = 15, face = "bold"),
				axis.ticks.y         = element_blank(),
				axis.text.x          = element_blank(),
				axis.ticks.x         = element_blank(),
				panel.background     = element_blank(),
				panel.grid.major     = element_blank(),
				panel.grid.minor     = element_blank())
graf4_køn

tbl4_køn <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id)) %>%
	distinct(k_deltager_id, .keep_all = T) %>%
	mutate(k_køn_ikon = paste(sub(".*? ", "", k_køn), k_ikon_køn)) %>%
	arrange(
		k_status, k_køn,
		desc(sub(" .*", "", k_billetsalg_pr_tilmelding)), k_billettype, k_navn) %>%
	select(
		" "    = k_logo_klub,
		"Navn" = k_navn_billettype,
		"Køn"  = k_køn_ikon,
		k_status)

kbl4_køn <- tbl4_køn %>%
	kbl(col.names = NA, align = "l", escape = F, table.attr = "data-quarto-disable-processing=true",
			caption = "<i style=font-size:80%>Sorteret efter køn</i>") %>%
	kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
	row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
	row_spec(which(grepl("Afbud", tbl4_køn$k_status)),
					 strikeout = T, color = tbl0_unik$k_farve_1) %>%
	column_spec(1, width_min = "2em") %>%
	column_spec(3, width_min = "5em") %>%
	column_spec(1:3, extra_css = "border-top:0.7px solid #999999") %>%
	remove_column(4) %>%
	scroll_box(height = "400px")
kbl4_køn

#' ## Gentilmeldinger
#+ eval=F, warning=F, message=F

graf4_gentilmelding <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
	ggplot(mapping = aes(y = fct_rev(k_antal_gentilmelding), fill = k_billettype)) +
	geom_bar(width = 0.9, position = position_stack(reverse = T)) +
	geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
						vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF", fontface = "bold") +
	scale_fill_manual(values = c(
		"🏓 DM i Ping Pong"    = "#398FCC",
		"🥳 Fest om aftenen"   = "#F1C40F",
		"🎉 The Old Irish Pub" = "#9B59B6",
		"🥪 Frokost"           = "#48C9B0")) +
	guides(fill = guide_legend(title = element_blank())) +
	xlab(element_blank()) + ylab(element_blank()) +
	theme(legend.position      = "right",
				legend.direction     = "vertical",
				plot.title           = element_text(hjust = 0.5),
				plot.subtitle        = element_text(hjust = 0.5),
				legend.text          = element_text(size = 15),
				axis.text.y          = element_text(size = 15, face = "bold"),
				axis.ticks.y         = element_blank(),
				axis.text.x          = element_blank(),
				axis.ticks.x         = element_blank(),
				panel.background     = element_blank(),
				panel.grid.major     = element_blank(),
				panel.grid.minor     = element_blank())
graf4_gentilmelding

tbl4_gentilmelding <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id)) %>%
	group_by(k_deltager_id) %>%
	slice(which.max(sub(" .*", "", k_antal_gentilmelding))) %>%
	ungroup() %>%
	arrange(
		k_status, k_antal_gentilmelding,
		desc(sub(" .*", "", k_billetsalg_pr_tilmelding)), k_billettype, k_navn) %>%
	mutate(k_antal_gentilmelding = paste(
		k_gentilmelding, "<br>", k_antal_gentilmelding, k_ikon_gentilmelding)) %>%
	select(
		" "             = k_logo_klub,
		"Navn"          = k_navn_billettype,
		"Gentilmelding" = k_antal_gentilmelding,
		k_status)

kbl4_gentilmelding <- tbl4_gentilmelding %>%
	kbl(col.names = NA, align = "l", escape = F, table.attr = "data-quarto-disable-processing=true",
			caption = "<i style=font-size:80%>Sorteret efter antal gentilmeldinger</i>") %>%
	kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
	row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
	row_spec(which(grepl("Afbud", tbl4_gentilmelding$k_status)),
					 strikeout = T, color = tbl0_unik$k_farve_1) %>%
	column_spec(1, width_min = "2em") %>%
	column_spec(1:3, extra_css = "border-top:0.7px solid #999999") %>%
	remove_column(4) %>%
	scroll_box(height = "400px")
kbl4_gentilmelding

#' ## Tilmeldingstype
#+ eval=F, warning=F, message=F

graf4_tilmeldingstype <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id)) %>%
	ggplot(mapping = aes(y = fct_rev(k_tilmeldingstype), fill = k_billettype)) +
	geom_bar(width = 0.9, position = position_stack(reverse = T)) +
	geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
						vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF", fontface = "bold") +
	scale_fill_manual(values = c(
		"🏓 DM i Ping Pong"    = "#398FCC",
		"🥳 Fest om aftenen"   = "#F1C40F",
		"🎉 The Old Irish Pub" = "#9B59B6",
		"🥪 Frokost"           = "#48C9B0")) +
	guides(fill = guide_legend(title = element_blank())) +
	xlab(element_blank()) + ylab(element_blank()) +
	theme(legend.position      = "right",
				legend.direction     = "vertical",
				plot.title           = element_text(hjust = 0.5),
				plot.subtitle        = element_text(hjust = 0.5),
				legend.text          = element_text(size = 15),
				axis.text.y          = element_text(size = 15, face = "bold"),
				axis.ticks.y         = element_blank(),
				axis.text.x          = element_blank(),
				axis.ticks.x         = element_blank(),
				panel.background     = element_blank(),
				panel.grid.major     = element_blank(),
				panel.grid.minor     = element_blank())
graf4_tilmeldingstype

tbl4_tilmeldingstype <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id)) %>%
	distinct(k_deltager_id, .keep_all = T) %>%
	arrange(k_status, k_ordredato, k_navn) %>%
	mutate(k_tilmeldingstype = paste(
		format(k_første_ordredato, "%d.%m.%Y"),
		"<br>",
		format(k_første_ordredato, "kl. %H:%M"), k_ikon_tilmeldingstype)) %>%
	select(
		" "         = k_logo_klub,
		"Navn"      = k_navn_billettype,
		"Ordredato" = k_tilmeldingstype,
		k_status)

kbl4_tilmeldingstype <- tbl4_tilmeldingstype %>%
	kbl(col.names = NA, align = "l", escape = F, table.attr = "data-quarto-disable-processing=true",
			caption = "<i style=font-size:80%>Sorteret efter ældste <q>første ordredato</q></i>") %>%
	kable_classic(position = "l", full_width = F, html_font = "verdana") %>%
	row_spec(0, background = tbl0_unik$k_farve_1, color = "#FFFFFF") %>%
	row_spec(which(grepl("Afbud", tbl4_tilmeldingstype$k_status)),
					 strikeout = T, color = tbl0_unik$k_farve_1) %>%
	column_spec(1, width_min = "2em") %>%
	column_spec(3, width_min = "7em") %>%
	column_spec(1:3, extra_css = "border-top:0.7px solid #999999") %>%
	remove_column(4) %>%
	scroll_box(height = "400px")
kbl4_tilmeldingstype

#' ## Klokkeslætsinterval
#+ eval=F, warning=F, message=F

graf4_klokkeslætsinterval <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
	ggplot(mapping = aes(y = fct_rev(k_klokkeslætsinterval), fill = k_billettype)) +
	geom_bar(width = 0.9, position = position_stack(reverse = T)) +
	geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
						vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF", fontface = "bold") +
	scale_fill_manual(values = c(
		"🏓 DM i Ping Pong"    = "#398FCC",
		"🥳 Fest om aftenen"   = "#F1C40F",
		"🎉 The Old Irish Pub" = "#9B59B6",
		"🥪 Frokost"           = "#48C9B0")) +
	guides(fill = guide_legend(title = element_blank())) +
	xlab(element_blank()) + ylab(element_blank()) +
	theme(legend.position      = "right",
				legend.direction     = "vertical",
				plot.title           = element_text(hjust = 0.5),
				plot.subtitle        = element_text(hjust = 0.5),
				legend.text          = element_text(size = 15),
				axis.text.y          = element_text(size = 15, face = "bold"),
				axis.ticks.y         = element_blank(),
				axis.text.x          = element_blank(),
				axis.ticks.x         = element_blank(),
				panel.background     = element_blank(),
				panel.grid.major     = element_blank(),
				panel.grid.minor     = element_blank())
graf4_klokkeslætsinterval

graf4_spillertype <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
	ggplot(mapping = aes(y = fct_rev(k_spillertype), fill = k_billettype)) +
	geom_bar(width = 0.9, position = position_stack(reverse = T)) +
	geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(reverse = T),
						vjust = 0.4, hjust = 1, size = 5, color = "#FFFFFF", fontface = "bold") +
	scale_fill_manual(values = c(
		"🏓 DM i Ping Pong"    = "#398FCC",
		"🥳 Fest om aftenen"   = "#F1C40F",
		"🎉 The Old Irish Pub" = "#9B59B6",
		"🥪 Frokost"           = "#48C9B0")) +
	guides(fill = guide_legend(title = element_blank())) +
	xlab(element_blank()) + ylab(element_blank()) +
	theme(legend.position      = "right",
				legend.direction     = "vertical",
				plot.title           = element_text(hjust = 0.5),
				plot.subtitle        = element_text(hjust = 0.5),
				legend.text          = element_text(size = 15),
				axis.text.y          = element_text(size = 15, face = "bold"),
				axis.ticks.y         = element_blank(),
				axis.text.x          = element_blank(),
				axis.ticks.x         = element_blank(),
				panel.background     = element_blank(),
				panel.grid.major     = element_blank(),
				panel.grid.minor     = element_blank())
graf4_spillertype

#' ## graf4_tid
#+ eval=F, warning=F, message=F

graf4_deltagere_over_tid <- tbl0_join_aktuel %>%
	filter(!is.na(k_deltager_id) & grepl("Tilmeldt", k_status)) %>%
	distinct(k_deltager_id, .keep_all = T) %>%
	mutate(k_tid = week(k_ordredato)) %>%
	add_count(k_tid) %>%
	ggplot(mapping = aes(x = as.Date(k_ordredato), y = n)) +
	geom_line() + 
	xlab("") +
	scale_x_date(date_breaks = "1 week", date_labels = "Uge %W") +
	guides(fill = guide_legend(title = element_blank())) +
	xlab(element_blank()) + ylab(element_blank())
graf4_deltagere_over_tid

#' ## Ping Pong deltagere fordelt årligt
#+ eval=F, warning=F, message=F

graf4_år <- tbl0_join_alle %>%
  filter(!is.na(k_deltager_id) &
           grepl("Tilmeldt", k_status) &
           grepl("Ping Pong", k_billettype) &
           k_2021_eller_senere == T) %>%
  count(k_eventår) %>%
  ggplot(aes(x = k_eventår, y = n)) +
  geom_line(color = "#398FCC", linewidth = 2) +
  geom_text(aes(label = n), vjust = -1, hjust = 0.4, size = 5) +
  geom_label(aes(label = k_eventår), vjust = 0.6, hjust = 0.5, size = 4,
             fill = "#398FCC",  colour = "#FFFFFF", label.size = NA,
             label.r = unit(0.5, "lines"), label.padding = unit(0.4, "lines")) +
  scale_y_continuous(expand = c(0, 20)) +
  labs(title = "🏓 Ping Pong deltagere fordelt årligt") +
  xlab(element_blank()) + ylab(element_blank()) +
  theme(legend.position      = "right",
        legend.direction     = "vertical",
        plot.title           = element_text(hjust = 0.5),
        plot.subtitle        = element_text(hjust = 0.5),
        axis.text.x          = element_blank(),
        axis.text.y          = element_blank(),
        axis.ticks.x         = element_blank(),
        axis.ticks.y         = element_blank(),
        panel.background     = element_blank(),
        panel.grid.major     = element_blank(),
        panel.grid.minor     = element_blank())
graf4_år

#' # TRUE/FALSE
# TRUE/FALSE --------------------------------------------------------------

#' ## BilletFix eventordre
#+ eval=F, warning=F, message=F

if(tbl0_input$k_eventordre_T_F == T) {
  
  # Hentning af eventordre
  list5_eventordre <- content(GET(
    url = paste0("https://billetfix.dk/api/v3/events/", tbl0_unik$k_uuid, "/orders"),
    config = c(
      add_headers(Authorization = paste("Token", tbl0_unik$k_token)),
      content_type("application/json"))))
  
  # Udtrækning af relevante listelementer indsættes i tabel
  tbl5_eventordre <- data.frame(
  	k_id = as.character(do.call(what = rbind, args = as.list(
      list5_eventordre$orders %>% sapply(., '[', "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., '[', "purchase_uuid")))),
  	k_navn = as.character(do.call(what = rbind, args = as.list(
      list5_eventordre$orders %>% sapply(., '[', "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., '[', "full_name")))),
  	k_billettype = as.character(do.call(what = rbind, args = as.list(
      list5_eventordre$orders %>% sapply(., '[', "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., '[', "ticket_type_name")))),
  	k_billetpris = as.character(do.call(what = rbind, args = as.list(
      list5_eventordre$orders %>% sapply(., '[', "tickets") %>%
        sapply(., '[', seq(max(sapply(., length)))) %>%
        sapply(., '[', "price"))))) %>%
    left_join(
      y = data.frame(
      	k_id = gsub("-", "", sapply(list5_eventordre$orders, `[[`, c("uuid"))),
        k_ordredato = sapply(list5_eventordre$orders, `[[`, c("date")),
        k_status    = sapply(list5_eventordre$orders, `[[`, c("state"))),
      by = "k_id") %>%
    mutate(across("k_ordredato", \(x) as_datetime(x) + hours(+2))) %>%
    mutate(across(c("k_billettype", "k_status"), \(x) factor(x, ordered = T))) %>%
    mutate(across("k_billetpris", \(x) as.numeric(x))) %>%
    arrange(desc(k_ordredato)) %>%
    select(k_navn, k_ordredato, k_billettype, k_status, k_billetpris)
  View(tbl5_eventordre)
  shell.exec(normalizePath(tbl0_input$k_data))
  browseURL("https://pingpong.quarto.pub/dm/pr%C3%A6mier_deltagere.html")
  browseURL(paste0("https://billetfix.dk/da/dashboard/", tbl0_unik$k_uuid, "/orders"))
  browseURL("https://bordtennisportalen.dk/DBTU/Ranglister")
  cat(paste0(
    tbl5_eventordre %>%
      filter(grepl("PAID", k_status)) %>%
      summarise(label = paste("💰 kr.", format(sum(k_billetpris), big.mark = "."), "(PAID)")), "\n",
    tbl5_eventordre %>%
      filter(grepl("PAID", k_status)) %>%
      group_by(k_billettype) %>%
      summarise(label = paste("kr.", format(sum(k_billetpris), big.mark = "."), "(PAID)")) %>%
      mutate(label = paste(sub(" .*", "", k_billettype), label)) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n",
    tbl5_eventordre %>%
      count(k_status) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0("🎫 ", k_status, " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n",
    tbl5_eventordre %>%
      count(k_status, k_billettype) %>%
      mutate(pct = percent(n/sum(n), digits = 0)) %>%
      mutate(label = paste0(sub(" .*", "", k_billettype), " ", k_status, " ", n, " (", pct, ")")) %>%
      summarise(label = str_c(label, collapse = "\n")), "\n\n"))
} else if (tbl0_input$k_eventordre_T_F == F) {"tbl0_input$k_eventordre_T_F = F"}

#' ## PDF til PNG for indbydelsesplakat
#+ eval=F, warning=F, message=F

if(tbl0_input$k_plakat_png_T_F == T) {
  pdf_convert(
    pdf       = paste0(
      "Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Teaserplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".pdf"),
    format = "png",
    filenames = paste0(
      "Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Teaserplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".png"),
    verbose   = F,
    dpi       = 300)
  pdf_convert(
    pdf       = paste0(
      "Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Indbydelsesplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".pdf"),
    format = "png",
    filenames = paste0(
      "Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Indbydelsesplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".png"),
    verbose   = F,
    dpi       = 300)
  shell.exec(normalizePath(
    paste0(
      "Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Teaserplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".png")))
  shell.exec(normalizePath(
    paste0(
      "Filer/Event/", unique(max(tbl0_join_alle$k_eventår)),
      "/Indbydelsesplakat-DM-i-Ping-Pong-", unique(max(tbl0_join_alle$k_eventår)), ".png")))
} else if (tbl0_input$k_plakat_png_T_F == F) {"tbl0_input$k_plakat_png_T_F = F"}

#' ## Webscraping af ratinglisten
#+ eval=F, warning=F, message=F

if(tbl0_input$k_webscraping_rating_T_F == T) {
  tbl5_webscraping_rating <- data.frame()
  url1 <- ifelse(
    nrow(rbind(tbl5_webscraping_rating, data.frame(
      "k_deltager_id" = read_html(
        paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
               as.numeric(format(tbl0_unik$k_ratingopdatering, "%Y")), ",",
               format(tbl0_unik$k_ratingopdatering, "%m/%d/%Y"),
               ",,,,True,,,,,", "0", ",,,0,,,,,")) %>% html_nodes(".playerid") %>% html_text(),
      stringsAsFactors = FALSE)) %>% filter(k_deltager_id != "Spiller-Id")) > 0,
    paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
           as.numeric(format(tbl0_unik$k_ratingopdatering, "%Y")), ",",
           format(tbl0_unik$k_ratingopdatering, "%m/%d/%Y")),
    paste0("https://bordtennisportalen.dk/DBTU/Ranglister/Udskriv/?params=,59,4",
           as.numeric(format(tbl0_unik$k_ratingopdatering, "%Y"))-1, ",",
           format(tbl0_unik$k_ratingopdatering, "%m/%d/%Y")))
  
  for (side in seq(from = 1, to = 50, by = 1)) {
  url2 <- paste0(url1, ",,,,True,,,,,", side-1, ",,,0,,,,,")
  
  tbl5_webscraping_rating <- rbind(tbl5_webscraping_rating, data.frame(
    "Plac"          = read_html(url2) %>% html_nodes(".rank")                        %>% html_text(),
    "k_deltager_id" = read_html(url2) %>% html_nodes(".playerid")                    %>% html_text(),
    "Navn"          = read_html(url2) %>% html_nodes(".name")                        %>% html_text(),
    "Rating"        = read_html(url2) %>% html_nodes(".name+ .pointsw")              %>% html_text(),
    "Plus_minus"    = read_html(url2) %>% html_nodes(".pointsw:nth-child(5)")        %>% html_text(),
    "Kampe"         = read_html(url2) %>% html_nodes(".pointsw~ .pointsw+ .pointsw") %>% html_text(),
    stringsAsFactors = FALSE)) %>% filter(k_deltager_id != "Spiller-Id") %>%
    mutate(across(c("Plac", "Rating", "Plus_minus", "Kampe"), \(x) as.numeric(x)))
  print(paste("Side", side))
  }
  
  tbl5_webscraping_rating <- tbl5_webscraping_rating %>%
    separate(Navn, into = c("Navn", "Klub"), sep = ",.", extra = "merge")
  tbl5_join_webscraping_rating <- tbl0_join_aktuel %>%
    arrange(desc(k_ordredato)) %>%
    left_join(
      y = tbl5_webscraping_rating,
      by = "k_deltager_id") %>%
    select(Plac, k_deltager_id, Navn, Klub, Rating, Plus_minus, Kampe)
  
  write_xlsx(
    setNames(
      list(tbl5_webscraping_rating), format(tbl0_unik$k_ratingopdatering, "%d.%m.%Y")),
    path = normalizePath("Filer/Generelt/Webscraping rating.xlsx"))
  write_xlsx(
    setNames(
      list(tbl5_join_webscraping_rating), format(tbl0_unik$k_ratingopdatering, "%d.%m.%Y")),
    path = normalizePath("Filer/Generelt/Webscraping join rating.xlsx"))
  shell.exec(normalizePath("Filer/Generelt/Webscraping join rating.xlsx"))
} else if(tbl0_input$k_webscraping_rating_T_F == F) {"tbl0_input$k_webscraping_rating_T_F = F"}

#' ## Webscraping af BTEX Ping Pong bat
#+ eval=F, warning=F, message=F

tbl5_webscraping_btex <- data.frame()
link <- paste0("https://www.btex.dk/sanwei-wcpp-sandpapirsbat.html")

tbl5_webscraping_btex <- rbind(tbl5_webscraping_btex, data.frame(
  "produkt"      = read_html(link) %>% html_nodes(".name")                        %>% html_text(),
  "pris"         = read_html(link) %>% html_nodes(".price")                       %>% html_text(),
  "lagerstatus"  = read_html(link) %>% html_nodes(".title span")                  %>% html_text(),
  "levering"     = read_html(link) %>% html_nodes("#product_addtocart_form .txt") %>% html_text(),
  stringsAsFactors = FALSE)) %>% mutate(pris = trimws(pris))

#' ## Danmarkskort med lokation
#+ eval=F, warning=F, message=F

ggplot() +
  borders(regions = "Denmark", colour = "black", fill = "#76D7C4") +
  geom_point(aes(y = c(56.2), x = c(10.1)), size = 20, shape = 21, fill = "#943126") +
  theme_void()
# ggsave(filename = "Filer/Generelt/Lokation.png")
