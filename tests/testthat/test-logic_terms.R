test_that("terms are recognized", {
  expect_true(check_terms("Fall 2024"))
  expect_equal(
    check_terms(c("Spring 2020", "Summer 2021", "Winter 2022")),
    c(TRUE, TRUE, FALSE)
  )
  expect_equal(get_term_year(c("Spring 2020", "Fall 2031")), c(2020L, 2031L))
  expect_equal(
    as.character(get_term_season(c("Spring 2020", "Summer 2020", "Fall 2020"))),
    c("Spring", "Summer", "Fall")
  )
})

test_that("term differences and ordering work", {
  expect_equal(calculate_term_difference("Spring 2024", "Fall 2024"), 2)
  expect_equal(calculate_term_difference("Fall 2024", "Spring 2025"), 1)
  expect_equal(calculate_term_difference("Fall 2025", "Spring 2024"), -5)
  expect_true(is_term_after("Fall 2025", after = "Spring 2025"))
  expect_false(is_term_after("Fall 2025", after = "Fall 2025"))
  expect_true(is_term_after(
    "Fall 2025",
    after = "Fall 2025",
    include_equal = TRUE
  ))
  expect_true(is_term_before("Spring 2025", before = "Fall 2025"))
  expect_equal(
    get_past_or_future_term("Fall 2024", years_shift = 2),
    "Fall 2026"
  )
})

test_that("terms can be filtered", {
  terms <- paste(c("Spring", "Summer", "Fall"), rep(2024:2025, each = 3))
  expect_equal(
    filter_terms(terms, start_term = "Fall 2024", end_term = "Summer 2025"),
    c("Fall 2024", "Spring 2025", "Summer 2025")
  )
  expect_equal(
    filter_terms(terms, "Fall 2024", "Summer 2025", inclusive = FALSE),
    "Spring 2025"
  )
  expect_equal(
    drop_summers(terms),
    c("Spring 2024", "Fall 2024", "Spring 2025", "Fall 2025")
  )
  expect_error(filter_terms("Winter 2024"), "valid terms")
})

test_that("available and sorted terms are relative to the current term", {
  current <- get_current_term()
  terms <- get_available_terms(
    first_term = "Spring 2020",
    n_years_past_current = 2
  )
  expect_equal(terms[1], "Spring 2020")
  expect_true(current %in% terms)
  expect_equal(utils::tail(terms, 1), get_past_or_future_term(years_shift = 2))

  sorted <- get_sorted_terms(terms)
  expect_equal(names(sorted), c("Past", "Current", "Future"))
  expect_equal(sorted$Current, list(current))
})
