test_that("get_class_prefix() normalizes class names", {
  expect_equal(
    get_class_prefix(c("ERTH3181", "ERTH 3500", "ERTH4185/5185", "erth4700")),
    c("ERTH3181", "ERTH3500", "ERTH4185_5185", "ERTH4700")
  )
  expect_equal(get_class_prefix(c("ERTH new", "XXXX")), c(NA_character_, NA))
})

test_that("parse_file_class_prefix() finds the class prefix of files", {
  parsed <- parse_file_class_prefix(c(
    "ERTH3181_syllabus.pdf",
    "ERTH-3181 Syllabus.pdf",
    "erth 3181.docx",
    "ERTH4185_5185 ad.pptx",
    "ERTH4185/5185_ad.pdf",
    "ERTH3181_2025.pdf",
    "syllabus.pdf"
  ))
  expect_equal(
    parsed$prefix,
    c(
      "ERTH3181",
      "ERTH3181",
      "ERTH3181",
      "ERTH4185_5185",
      "ERTH4185_5185",
      "ERTH3181",
      NA
    )
  )
  expect_equal(
    parsed$rest,
    c(
      "_syllabus.pdf",
      " Syllabus.pdf",
      ".docx",
      " ad.pptx",
      "_ad.pdf",
      "_2025.pdf",
      "syllabus.pdf"
    )
  )
  expect_equal(nrow(parse_file_class_prefix(character(0))), 0L)
})

test_that("get_class_file_name() adds the class prefix", {
  expect_equal(
    get_class_file_name(
      c(
        "syllabus.pdf",
        "ERTH3181_syllabus.pdf",
        "ERTH-3181 Syllabus.pdf",
        "erth3181syllabus.pdf",
        "ERTH3181_2025.pdf",
        "ERTH1010_syllabus.pdf"
      ),
      "ERTH3181"
    ),
    c(
      "ERTH3181_syllabus.pdf",
      "ERTH3181_syllabus.pdf",
      "ERTH3181 Syllabus.pdf",
      "ERTH3181_syllabus.pdf",
      "ERTH3181_2025.pdf",
      "ERTH3181_ERTH1010_syllabus.pdf"
    )
  )
  expect_equal(
    get_class_file_name(
      c("ERTH4185_ad.pdf", "ERTH-4185-5185 ad.pdf", "ad.pdf"),
      "ERTH4185/5185"
    ),
    c("ERTH4185_5185_ad.pdf", "ERTH4185_5185 ad.pdf", "ERTH4185_5185_ad.pdf")
  )
  expect_error(get_class_file_name("ad.pdf", "ERTH new"), "course number")
})

test_that("is_class_file_extension() checks the file type", {
  expect_equal(
    is_class_file_extension(c("a.pdf", "a.DOCX", "a.pptx", "a.doc", "a")),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})
