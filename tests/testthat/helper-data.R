# synthetic data in the format read from the spreadsheet
test_classes <- function() {
  dplyr::tibble(
    class = c("ERTH1010", "ERTH 3030", "ERTH4700"),
    title = c("Intro", "Rocks", "Special Topics"),
    credits = c(3L, 4L, 3L),
    inactive = c(NA, NA, TRUE),
    type = c(NA, "lab", NA)
  )
}

test_instructors <- function() {
  dplyr::tibble(
    instructor_id = c("ann", "bob", "cat"),
    last_name = c("Alpha", "Beta", "Gamma"),
    first_name = c("Ann", "Bob", "Cat"),
    department = "ERTH",
    position = "Faculty",
    inactive = c(NA, NA, TRUE)
  )
}

test_not_teaching <- function() {
  dplyr::tibble(
    term = c("Spring 2031", "Spring 2031"),
    instructor_id = c("bob", "bob"),
    reason = c("sabbatical", "family leave"),
    created = as.POSIXct(c("2024-01-01", "2024-02-01"), tz = "UTC"),
    deleted = as.POSIXct(NA, tz = "UTC")
  )
}

test_schedule <- function() {
  dplyr::tibble(
    term = c("Fall 2030", "Fall 2030", "Spring 2031", "Fall 2030", "Fall 2030"),
    class = c("ERTH1010", "ERTH1010", "ERTH3030", "ERTH3030", "ERTH 1010"),
    section = c("001", "002", NA, NA, "003"),
    subtitle = NA_character_,
    instructor_id = c("ann", "ann", "ann", "bob", "bob"),
    preenrollment = NA_integer_,
    enrollment = c(100L, NA, NA, NA, NA),
    enrollment_cap = c(120L, NA, 20L, NA, NA),
    building = c("BESC", "BESC", NA, NA, NA),
    room = c("180.0", "180", NA, NA, NA),
    days = c("MWF", "TTH", NA, NA, NA),
    start_time = c("9:00a", "11:00a", NA, NA, NA),
    end_time = c("9:50a", "12:15p", NA, NA, NA),
    created = as.POSIXct(NA, tz = "UTC"),
    updated = as.POSIXct(NA, tz = "UTC"),
    deleted = as.POSIXct(c(NA, NA, NA, "2024-01-01", NA), tz = "UTC"),
    canceled = c(NA, NA, FALSE, NA, NA),
    confirmed = c(TRUE, TRUE, FALSE, TRUE, TRUE),
    notes = NA_character_
  )
}

