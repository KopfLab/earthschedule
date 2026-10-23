test_that("class search urls are generated", {
  expect_equal(
    get_class_search_url("ERTH1010"),
    "https://classes.colorado.edu/?keyword=ERTH%201010&subject=ERTH"
  )
  expect_equal(
    get_class_search_url("erth 5185/4185"),
    "https://classes.colorado.edu/?keyword=ERTH%204185&subject=ERTH"
  )
  expect_error(get_class_search_url("ERTH"), "not a valid class code")
  expect_equal(
    has_class_qrcode(c("ERTH1010", "ERTH4185/5185", "ERTH-new", "ASTR3710", NA)),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_equal(get_class_qrcode_name("ERTH5185/4185"), "ERTH4185_qrcode")
  expect_error(get_class_search_url(c("ERTH1010", "ERTH1030")), "valid class")
})

test_that("class logos show the class number", {
  digit <- function(svg, d) {
    stringr::str_match(svg, sprintf('<path id="digit-%s" d="([^"]+)"/>', d))[, 2]
  }
  logo <- get_class_logo_svg("ERTH1010")
  expect_false(stringr::str_detect(logo, "\\{\\{|<text"))
  slots <- stringr::str_match_all(logo, 'translate\\([0-9.]+ 50.54\\)" d="([^"]+)"')[[1]][-1, 2]
  expect_equal(slots, digit(logo, c(1, 0, 1, 0)))
  # cross-listed classes show the lowest number
  logo <- get_class_logo_svg("ERTH5185/4185")
  slots <- stringr::str_match_all(logo, 'translate\\([0-9.]+ 50.54\\)" d="([^"]+)"')[[1]][-1, 2]
  expect_equal(slots, digit(logo, c(4, 1, 8, 5)))
  expect_error(get_class_logo_svg("MCDB4185"), "only available for ERTH")
})

test_that("class QR codes are created", {
  svg <- create_class_qrcode("ERTH1010")
  expect_match(svg, "^<\\?xml")
  expect_false(stringr::str_detect(svg, "<text"))
  expect_error(create_class_qrcode("ERTH1010", "qr.pdf"), "png or .svg")

  file <- tempfile(fileext = ".svg")
  create_class_qrcode("ERTH1010", file)
  expect_equal(readr::read_file(file), svg)

  file <- tempfile(fileext = ".png")
  create_class_qrcode("ERTH1010", file, size = 300)
  expect_true(file.exists(file))
})
