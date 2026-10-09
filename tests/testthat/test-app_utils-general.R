test_that("hidden_tabs() hides restricted tabs by access level", {
  expect_equal(hidden_tabs("admin"), character(0))
  expect_equal(hidden_tabs("faculty"), "admin")
  expect_equal(hidden_tabs("student"), c("rooms", "stats", "admin"))
})
