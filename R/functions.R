#' Convert term to sortable format
#'
#' Converts a term to another format so it can be sorted. 2018FA will be 20182019_1, 2018SP will be 20182019_2, and 2018SU will be 20172018_3. This is useful for sorting.
#'
#' @param term
#'
#' @return
#' @export
#'
#' @examples
adj_term <- function(term) {
  year <- as.integer(substr(term, 1, 4))
  season <- substr(term, 5, nchar(term))

  # Determine start year of academic year
  start_year <- dplyr::case_when(
    season %in% c("FA", "OFA") ~ year,
    season %in% c("OWI", "SP", "OSP", "SU", "OSU") ~ year - 1,
    TRUE ~ NA_integer_
  )

  # Short version of end year
  end_year_short <- substr(as.character(start_year + 1), 3, 4)

  # Academic year in YYYY-YY format
  acad_year <- paste0(start_year, "-", end_year_short)

  # Term number according to desired order
  term_num <- dplyr::case_when(
    season == "FA"  ~ "1",
    season == "OFA" ~ "2",
    season == "OWI" ~ "3",
    season == "SP"  ~ "4",
    season == "OSP" ~ "5",
    season == "SU"  ~ "6",
    season == "OSU" ~ "7",
    TRUE ~ NA_character_
  )

  paste0(acad_year, "_", term_num)
}



#' Add academic year
#'
#' Converts a term like 2018FA or 2024OFA to academic year 2018-19 or 2024-25
#'
#' @param term Character string like "2020FA", "2024OFA", "2025OWI"
#'
#' @return Character string representing academic year
#' @export
#'
#' @examples
#' adj_acadyear("2020FA")   # "2020-21"
#' adj_acadyear("2024OFA")  # "2024-25"
#' adj_acadyear("2025OWI")  # "2024-25"
#' adj_acadyear(c("2025OSP", "2025OSU")) # "2024-25" "2024-25"
adj_acadyear <- function(term) {
  year <- as.integer(substr(term, 1, 4))
  season <- substr(term, 5, nchar(term))

  start_year <- dplyr::case_when(
    season %in% c("FA", "OFA") ~ year,
    season %in% c("SP", "SU", "OWI", "OSP", "OSU") ~ year - 1,
    TRUE ~ NA_integer_
  )

  end_year <- start_year + 1
  paste0(start_year, "-", substr(end_year, 3, 4))
}



#' Term Index
#'
#' Creates a corresponding integer for each term in chronological order. Starts at 1700FA just for a random date.
#'
#' @param term_vec
#' @param base_year
#' @param base_season
#'
#' @return
#' @export
#'
#' @examples
term_index_fixed <- function(term_vec, base_year = 1700, base_season = "SP") {
  # Season codes: FA = 2, SP = 0, SU = 1
  season_lookup <- c("FA" = 2, "SP" = 0, "SU" = 1)

  # Extract year and season
  year <- as.integer(substr(term_vec, 1, 4))
  season <- substr(term_vec, 5, 6)

  # Convert season to numeric offset
  season_num <- season_lookup[season]

  # Calculate number of terms since base
  terms_since_base <- (year - base_year) * 3 + season_num - season_lookup[base_season]

  # Index starts at 1 for base term
  return(terms_since_base + 1)
}



#' Quality points
#'
#' Converts grade to quality points
#'
#' @param grade
#'
#' @return
#' @export
#'
#' @examples
quality_points <- function(grade) {
  grade_points <- dplyr::case_when(
    grade == "A" ~ 4.0,
    grade == "A-" ~ 3.7,
    grade == "B+" ~ 3.3,
    grade == "B" ~ 3.0,
    grade == "B-" ~ 2.7,
    grade == "C+" ~ 2.3,
    grade == "C" ~ 2.0,
    grade == "C-" ~ 1.7,
    grade == "D" ~ 1.0,
    grade == "F" ~ 0.0,
    TRUE ~ NA_real_
  )

  return(grade_points)
}




#' DFW
#'
#' Creates a field which indicates if the grade was a D, F, IF, or W. This is useful for calculating DFW rates.
#'
#' @param grade
#'
#' @return
#' @export
#'
#' @examples
grade_dfw <- function(grade) {
  dfw <- dplyr::case_when(
    grade %in% c("D", "F", "IF", "W") ~ 1,
    grade == "P" ~ 0,
    is.na(grade) ~ NA_real_,
    TRUE ~ 0
  )

  return(dfw)
}



#' Passing grade
#'
#' Creates a field which indicates if the grade was a passing grade. This is useful for calculating pass rates.
#'
#' @param grade
#'
#' @return
#' @export
#'
#' @examples
grade_pass <- function(grade) {
  pass <- dplyr::case_when(
    grade %in% c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "P") ~ 1,
    is.na(grade) ~ NA_real_,
    TRUE ~ 0
  )

  return(pass)
}




#' Get program level
#'
#' Returns the academic level (Undergraduate or Graduate) based on the
#' degree prefix of a program code (e.g. "BM4.PERF" returns "Undergraduate").
#'
#' @param program Character vector of program codes
#'
#' @return Character vector of levels
#' @export
#'
#' @examples
#' program_level("BM4.PERF")
#' program_level("MM.MUSED")
program_level <- function(program) {
  
  undergrad_prefixes <- c("ADM", "BA", "BFA", "BFAD", "BFAT", "BM", "BM4", "BM5",
                          "BPS4", "BPS5", "OBA", "PDM4", "PDM5", "PDM")
  
  grad_prefixes <- c("ADP", "CERT", "MA", "MFATM", "MM",
                     "OMA", "OMM", "PRFCRT", "PERF")
  
  nonmatriculated_prefixes <- c("SNM")
  
  prefix <- stringr::str_extract(program, "^[^\\.]+")
  
  dplyr::case_when(
    prefix %in% undergrad_prefixes ~ "UG",
    prefix %in% grad_prefixes      ~ "GR",
    prefix %in% nonmatriculated_prefixes ~ "SNM",
    TRUE                           ~ NA_character_
  )
}




#' Get program learning environment
#'
#' Returns the learning environment based on the first concentration
#' code of a program code (e.g. "BM4.PERF" returns "BCM").
#'
#' @param program Character vector of program codes
#'
#' @return Character vector of campus codes
#' @export
#'
#' @examples
#' program_learningenvironment("BM4.PERF")
#' program_learningenvironment("BFA.DANCE")
program_learning_environment <- function(program) {
  
  lookup <- c(
    # BCM
    COMP = "BCM", CWPR = "BCM", ELPD = "BCM", MSYN = "BCM", JCMP = "BCM",
    IRPD = "BCM", MPED = "BCM", FILM = "BCM", GAIM = "BCM", SONG = "BCM",
    PERF = "BCM", UNDL = "BCM", MBUS = "BCM", MILI = "BCM", MUAU = "BCM",
    MUED = "BCM", MTHE = "BCM", PROM = "BCM", BLMC = "BCM", CPGJ = "BCM",
    MUSAUT = "BCM", MUSED = "BCM", MUSEDA = "BCM",
    # NYC
    CMLE = "NYC", CMLM = "NYC", CMMT = "NYC", CMSP = "NYC", CMWT = "NYC",
    # VAL
    CPPD = "VAL", GEMB = "VAL", MPTI = "VAL", SFTV = "VAL",
    # BCB Theater
    CONTHR = "BCB", MUSTHR = "BCB",
    # BCB Music
    CMPSTN = "BCB", MTVCPD = "BCB", VCLPED = "BCB", BASPRF = "BCB",
    BASTRO = "BCB", BRSPRF = "BCB", BSOPRF = "BCB", CELPRF = "BCB",
    CLAPRF = "BCB", CLPIAN = "BCB", CNDUCT = "BCB", CONMUS = "BCB",
    EUPPRF = "BCB", FLUPRF = "BCB", HARPRF = "BCB", HORPRF = "BCB",
    MRMBAP = "BCB", OBOPRF = "BCB", ORCHCN = "BCB", PERPRF = "BCB",
    PIANOP = "BCB", SAXPRF = "BCB", TROPRF = "BCB", TRUPRF = "BCB",
    TUBPRF = "BCB", VIOPRF = "BCB", VLNPRF = "BCB", WNDMLT = "BCB",
    CHRCND = "BCB", OPERAP = "BCB", VOICEP = "BCB", VOIOPE = "BCB",
    # BCB Dance
    COMDAN = "BCB", CONDAN = "BCB", DANCE = "BCB", DANMOD = "BCB"
  )
  
  conc <- stringr::str_remove(stringr::str_split_fixed(program, "\\.", 3)[, 2], "[0-9]+$")
  
  dplyr::coalesce(lookup[conc], NA_character_)
}




#' Get program division
#'
#' Returns the division for a program code. Use which = 1 for the primary
#' concentration and which = 2 for the secondary (double major).
#'
#' @param program Character vector of program codes
#' @param which Integer, 1 or 2 — which concentration to look up
#'
#' @return Character vector of divisions
#' @export
#'
#' @examples
#' program_division("BM4.PERF")
#' program_division("BM5.PERF.SONG", 2)
program_division <- function(program, which = 1) {
  
  lookup <- c(
    # Writing and Music Technology
    COMP = "Writing and Music Technology", CWPR = "Writing and Music Technology",
    ELPD = "Writing and Music Technology", MSYN = "Writing and Music Technology",
    JCMP = "Writing and Music Technology", IRPD = "Writing and Music Technology",
    MPED = "Writing and Music Technology", FILM = "Writing and Music Technology",
    GAIM = "Writing and Music Technology", SONG = "Writing and Music Technology",
    # Performance
    PERF = "Performance",
    # Undeclared
    UNDL = "Undeclared",
    # Education
    MBUS = "Education", MILI = "Education", MUAU = "Education",
    MUED = "Education", MTHE = "Education", PROM = "Education",
    # Africana Studies
    BLMC = "Africana Studies",
    # Graduate Studies
    CPGJ = "Graduate Studies", MUSAUT = "Graduate Studies",
    MUSED = "Graduate Studies", MUSEDA = "Graduate Studies",
    CMLE = "Graduate Studies", CMLM = "Graduate Studies",
    CMMT = "Graduate Studies", CMSP = "Graduate Studies",
    CMWT = "Graduate Studies", CPPD = "Graduate Studies",
    GEMB = "Graduate Studies", MPTI = "Graduate Studies",
    SFTV = "Graduate Studies",
    # BCB Theater
    CONTHR = "BCB Theater", MUSTHR = "BCB Theater",
    # BCB Music
    CMPSTN = "BCB Music", MTVCPD = "BCB Music", VCLPED = "BCB Music",
    BASPRF = "BCB Music", BASTRO = "BCB Music", BRSPRF = "BCB Music",
    BSOPRF = "BCB Music", CELPRF = "BCB Music", CLAPRF = "BCB Music",
    CLPIAN = "BCB Music", CNDUCT = "BCB Music", CONMUS = "BCB Music",
    EUPPRF = "BCB Music", FLUPRF = "BCB Music", HARPRF = "BCB Music",
    HORPRF = "BCB Music", MRMBAP = "BCB Music", OBOPRF = "BCB Music",
    ORCHCN = "BCB Music", PERPRF = "BCB Music", PIANOP = "BCB Music",
    SAXPRF = "BCB Music", TROPRF = "BCB Music", TRUPRF = "BCB Music",
    TUBPRF = "BCB Music", VIOPRF = "BCB Music", VLNPRF = "BCB Music",
    WNDMLT = "BCB Music", CHRCND = "BCB Music", OPERAP = "BCB Music",
    VOICEP = "BCB Music", VOIOPE = "BCB Music",
    # BCB Dance
    COMDAN = "BCB Dance", CONDAN = "BCB Dance",
    DANCE  = "BCB Dance", DANMOD = "BCB Dance"
  )
  
  col <- which + 1  # column 2 = conc 1, column 3 = conc 2
  conc <- stringr::str_remove(stringr::str_split_fixed(program, "\\.", 3)[, col], "[0-9]+$")
  conc[conc == ""] <- NA_character_
  
  dplyr::coalesce(lookup[conc], NA_character_)
}




#' Get program department
#'
#' Returns the department for a program code. Use which = 1 for the primary
#' concentration and which = 2 for the secondary (double major).
#'
#' @param program Character vector of program codes
#' @param which Integer, 1 or 2 — which concentration to look up
#'
#' @return Character vector of departments
#' @export
#'
#' @examples
#' program_department("BM4.PERF")
#' program_department("BM5.PERF.SONG", 2)
program_department <- function(program, which = 1) {
  
  lookup <- c(
    # Writing and Music Technology
    COMP = "Composition",
    CWPR = "Contemporary Writing and Production",
    ELPD = "Electronic Production and Design",
    MSYN = "Electronic Production and Design",
    JCMP = "Harmony and Jazz Composition",
    IRPD = "Music Production and Engineering",
    MPED = "Music Production and Engineering",
    FILM = "Screen and Media Scoring",
    GAIM = "Screen and Media Scoring",
    SONG = "Songwriting",
    # Performance
    PERF = "Performance",
    # Undeclared
    UNDL = "Undeclared",
    # Education
    MBUS = "Music Business",   MILI = "Music Business",
    MUAU = "Music Education",  MUED = "Music Education",
    MTHE = "Music Therapy",    PROM = "Professional Music",
    # Africana Studies
    BLMC = "Africana Studies",
    # Graduate Studies
    CPGJ = "Graduate Studies", MUSAUT = "Graduate Studies",
    MUSED = "Graduate Studies", MUSEDA = "Graduate Studies",
    CMLE = "Graduate Studies",  CMLM = "Graduate Studies",
    CMMT = "Graduate Studies",  CMSP = "Graduate Studies",
    CMWT = "Graduate Studies",  CPPD = "Graduate Studies",
    GEMB = "Graduate Studies",  MPTI = "Graduate Studies",
    SFTV = "Graduate Studies",
    # BCB Theater
    CONTHR = "Theater", MUSTHR = "Theater",
    # BCB Music
    CMPSTN = "Composition, Contemporary Classical Music, and Core Studies",
    MTVCPD = "Vocal Arts", VCLPED = "Vocal Arts",
    BASPRF = "Instrumental Studies", BASTRO = "Instrumental Studies",
    BRSPRF = "Instrumental Studies", BSOPRF = "Instrumental Studies",
    CELPRF = "Instrumental Studies", CLAPRF = "Instrumental Studies",
    CLPIAN = "Instrumental Studies", CNDUCT = "Instrumental Studies",
    CONMUS = "Composition, Contemporary Classical Music, and Core Studies", EUPPRF = "Instrumental Studies",
    FLUPRF = "Instrumental Studies", HARPRF = "Instrumental Studies",
    HORPRF = "Instrumental Studies", MRMBAP = "Instrumental Studies",
    OBOPRF = "Instrumental Studies", ORCHCN = "Instrumental Studies",
    PERPRF = "Instrumental Studies", PIANOP = "Instrumental Studies",
    SAXPRF = "Instrumental Studies", TROPRF = "Instrumental Studies",
    TRUPRF = "Instrumental Studies", TUBPRF = "Instrumental Studies",
    VIOPRF = "Instrumental Studies", VLNPRF = "Instrumental Studies",
    WNDMLT = "Instrumental Studies", CHRCND = "Vocal Arts",
    OPERAP = "Vocal Arts",           VOICEP = "Vocal Arts",
    VOIOPE = "Vocal Arts",
    # BCB Dance
    COMDAN = "Dance", CONDAN = "Dance", DANCE = "Dance", DANMOD = "Dance"
  )
  
  col <- which + 1
  conc <- stringr::str_remove(stringr::str_split_fixed(program, "\\.", 3)[, col], "[0-9]+$")
  conc[conc == ""] <- NA_character_
  
  dplyr::coalesce(lookup[conc], NA_character_)
}


