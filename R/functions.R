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
  season <- substr(term, 5, 6)

  start_year <- dplyr::case_when(
    season == "FA" ~ year,
    season %in% c("SP", "SU") ~ year - 1,
    TRUE ~ NA_integer_
  )

  end_year <- start_year + 1
  end_year_short <- substr(as.character(end_year), 3, 4)

  acad_year <- paste0(start_year, "-", end_year_short)

  term_num <- dplyr::case_when(
    season == "FA" ~ "1",
    season == "SP" ~ "2",
    season == "SU" ~ "3",
    TRUE ~ NA_character_
  )

  paste0(acad_year, "_", term_num)
}



#' Add academic year
#'
#' Converts a term like 2018FA to academic year 2018-19
#'
#' @param term
#'
#' @return
#' @export
#'
#' @examples
adj_acadyear <- function(term) {
  year <- as.integer(substr(term, 1, 4))
  season <- substr(term, 5, 6)

  start_year <- dplyr::case_when(
    season == "FA" ~ year,
    season %in% c("SP", "SU") ~ year - 1,
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
term_index_fixed <- function(term_vec, base_year = 1700, base_season = "FA") {
  # Season codes: FA = 0, SP = 1, SU = 2
  season_lookup <- c("FA" = 0, "SP" = 1, "SU" = 2)

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
