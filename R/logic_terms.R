# terms =======

get_term_regexp <- function() {
  return("(Spring|Summer|Fall) \\d{4}")
}

check_terms <- function(terms) {
  stringr::str_detect(terms, get_term_regexp())
}

get_current_term <- function(include_summer = TRUE) {
  month <- lubridate::month(lubridate::now())
  year <- lubridate::year(lubridate::now())
  season <- if (month >= 8) {
    "Fall"
  } else if (month >= 5) {
    "Summer"
  } else {
    "Spring"
  }
  if (!include_summer && season == "Summer") {
    season <- "Fall"
  }
  return(paste(season, year))
}

get_past_or_future_term <- function(
  reference_term = get_current_term(),
  years_shift = 0,
  season = get_term_season(reference_term)
) {
  paste(season, get_term_year(reference_term) + years_shift)
}

get_term_year <- function(term) {
  term |> stringr::str_extract("\\d{4}") |> as.integer()
}

get_term_season <- function(term) {
  term |>
    stringr::str_extract("Spring|Summer|Fall") |>
    factor(levels = c("Spring", "Summer", "Fall"), ordered = TRUE)
}

# calculates the difference between two terms
calculate_term_difference <- function(term1, term2) {
  years_diff <- get_term_year(term2) - get_term_year(term1)
  seasons_diff <- as.integer(get_term_season(term2)) -
    as.integer(get_term_season(term1))
  return(years_diff * 3 + seasons_diff)
}

# check if a term is after another one
is_term_after <- function(
  terms,
  after = get_current_term(),
  include_equal = FALSE
) {
  if (include_equal) {
    calculate_term_difference(after, terms) >= 0
  } else {
    calculate_term_difference(after, terms) > 0
  }
}
# check if a term is before another one
is_term_before <- function(
  terms,
  before = get_current_term(),
  include_equal = FALSE
) {
  if (include_equal) {
    calculate_term_difference(before, terms) <= 0
  } else {
    calculate_term_difference(before, terms) < 0
  }
}

# filter terms depending on start/end
filter_terms <- function(
  terms,
  start_term = NULL,
  end_term = NULL,
  inclusive = TRUE
) {
  # saftey checks
  stopifnot(
    "`terms` need to be valid terms" = terms |> check_terms() |> all(),
    "`start_term` needs to be a valid term if provided" = is.null(start_term) ||
      check_terms(start_term),
    "`end_term` needs to be a valid term if provided" = is.null(end_term) ||
      check_terms(end_term)
  )

  if (!is.null(start_term)) {
    terms <- terms[is_term_after(
      terms,
      after = start_term,
      include_equal = inclusive
    )]
  }

  if (!is.null(end_term)) {
    terms <- terms[is_term_before(
      terms,
      before = end_term,
      include_equal = inclusive
    )]
  }

  return(terms)
}

# get available terms based on first term and current term (based on current year) and number of years past current
get_available_terms <- function(first_term, n_years_past_current = 5) {
  current_term <- get_current_term()
  available_terms <-
    paste(
      c("Spring", "Summer", "Fall"),
      rep(
        get_term_year(first_term):(get_term_year(current_term) +
          n_years_past_current +
          1),
        each = 3
      )
    ) |>
    filter_terms(
      start_term = first_term,
      end_term = get_past_or_future_term(years_shift = n_years_past_current)
    )
  return(available_terms)
}

# drop summers
drop_summers <- function(terms) {
  terms[get_term_season(terms) != "Summer"]
}

# sort terms (past, current, future)
get_sorted_terms <- function(terms) {
  current_term <- get_current_term()
  past <- filter_terms(terms, end_term = current_term, inclusive = FALSE) |>
    as.list()
  future <- filter_terms(terms, start_term = current_term, inclusive = FALSE) |>
    as.list()
  c(
    if (length(past) > 0) list(Past = past),
    if (any(calculate_term_difference(terms, current_term) == 0)) {
      list(Current = list(current_term))
    },
    if (length(future) > 0) list(Future = future)
  )
}
