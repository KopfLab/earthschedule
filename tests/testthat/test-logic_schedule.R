test_that("data preparation works", {
  classes <- prepare_classes(test_classes())
  expect_true(is.factor(classes$class))
  expect_true(all(c("ERTH3030", "XXXX0000", "XXXX9999") %in% classes$class))
  expect_equal(classes$inactive[1:3], c(FALSE, FALSE, TRUE))

  instructors <- prepare_instructors(test_instructors())
  expect_equal(instructors$full_name, c("Ann Alpha", "Bob Beta", "Cat Gamma"))

  # only the latest not teaching record per term is kept
  not_teaching <- prepare_not_teaching(test_not_teaching())
  expect_equal(nrow(not_teaching), 1L)
  expect_equal(not_teaching$reason, "family leave")
  expect_equal(not_teaching$idx, 2L)

  # deleted records removed, room suffix removed, class whitespace removed
  schedule <- prepare_schedule(test_schedule())
  expect_equal(nrow(schedule), 4L)
  expect_equal(schedule$instructor_id, c("ann", "ann", "ann", "bob"))
  expect_equal(schedule$idx, c(1L, 2L, 3L, 5L))
  expect_equal(schedule$class[4], "ERTH1010")
  expect_equal(schedule$room[1], "180")
  expect_false(any(schedule$canceled))

  # co-taught classes are split into one record per instructor
  cotaught <- test_schedule() |>
    dplyr::mutate(instructor_id = c("ann", "ann", "ann, bob", "bob", "bob")) |>
    prepare_schedule()
  expect_equal(cotaught$instructor_id, c("ann", "ann", "ann", "bob", "bob"))
  expect_equal(cotaught$idx, c(1L, 2L, 3L, 3L, 5L))
})

test_that("rooms and teaching times are extracted", {
  expect_equal(prepare_rooms(test_schedule()), "BESC 180")
  expect_equal(
    prepare_teaching_times(test_schedule()),
    c("MWF: 9:00a-9:50a", "TTH: 11:00a-12:15p")
  )
})

test_that("class information is combined", {
  info <- combine_information(
    section = c("001", NA),
    days = c("MWF", NA),
    start_time = c("9:00a", NA),
    end_time = c("9:50a", NA),
    building = c("BESC", NA),
    room = c("180", NA),
    enrollment = c(100L, NA),
    enrollment_cap = c(120L, NA),
    confirmed = c(TRUE, FALSE),
    include_section_nr = TRUE,
    include_day_time = TRUE,
    include_location = TRUE,
    include_enrollment = TRUE
  )
  expect_equal(
    info,
    c(
      "#001: MWF 9:00a-9:50a in BESC180, 100 / 120 students",
      "<i><u>yes</u></i>"
    )
  )
  expect_equal(
    combine_information(
      section = character(0),
      include_section_nr = TRUE,
      include_day_time = TRUE,
      include_location = TRUE,
      include_enrollment = TRUE
    ),
    character(0)
  )
})

test_that("schedule is combined into a table", {
  terms <- c("Fall 2030", "Spring 2031")
  not_teaching <- prepare_not_teaching(test_not_teaching())
  combine <- function(...) {
    combine_schedule(
      schedule = prepare_schedule(test_schedule()),
      not_teaching = not_teaching,
      instructors = prepare_instructors(test_instructors()),
      classes = prepare_classes(test_classes()),
      selected_terms = terms,
      recognized_reasons = not_teaching$reason,
      ...
    )
  }

  combined <- combine(
    include_section_nr = FALSE,
    include_day_time = FALSE,
    include_location = FALSE,
    include_enrollment = FALSE
  )
  expect_equal(
    names(combined),
    c("row", "class", "full_title", "instructor_id", "instructor", terms)
  )
  expect_equal(combined$instructor_id, c("ann", "bob", "ann"))
  expect_equal(
    as.character(combined$class),
    c("ERTH1010", "ERTH1010", "ERTH3030")
  )
  expect_equal(combined$full_title[3], "ERTH3030 (4) - Rocks (LAB)")
  # multiple sections are combined, a class not taught while teaching others
  # is "no", future terms without any info are "?" unless there is a
  # recognized absence reason
  expect_equal(combined[["Fall 2030"]], c("yes\nyes", "yes", "no"))
  expect_equal(combined[["Spring 2031"]], c("no", "family leave", "yes"))

  # with details, unconfirmed classes are marked
  detailed <- combine(
    include_section_nr = FALSE,
    include_day_time = TRUE,
    include_location = FALSE,
    include_enrollment = FALSE
  )
  expect_equal(
    detailed[["Fall 2030"]][1],
    "MWF 9:00a-9:50a\nTTH 11:00a-12:15p"
  )
  expect_equal(detailed[["Spring 2031"]][3], "<i><u>yes</u></i>")

  # instructor focus keeps all classes that instructor ever taught
  bob <- combine(instructor_schedule = "bob")
  expect_equal(as.character(unique(bob$class)), "ERTH1010")
  expect_equal(bob$instructor_id, c("ann", "bob"))

  # table columns are escaped but keep formatting and line breaks
  table <- detailed |>
    dplyr::mutate(instructor = c("<b>Ann</b>", "Bob", "Ann")) |>
    prepare_schedule_table_columns()
  expect_equal(names(table)[5], "Instructor")
  expect_equal(table$Instructor[1], "&lt;b&gt;Ann&lt;/b&gt;")
  expect_equal(table[["Fall 2030"]][1], "MWF 9:00a-9:50a<br>TTH 11:00a-12:15p")
  expect_equal(table[["Spring 2031"]][3], "<i><u>yes</u></i>")

  # export removes the internal columns and the html
  export <- table |> prepare_schedule_table_export()
  expect_equal(
    table |>
      dplyr::mutate(`Fall 2030` = c("no class", "?", "#?: MWF")) |>
      prepare_schedule_table_export() |>
      dplyr::pull("Fall 2030"),
    c("", "", "#?: MWF")
  )
  expect_equal(names(export)[1:2], c("Class", "Instructor"))
  expect_equal(export$Instructor[1], "<b>Ann</b>")
  expect_equal(export[["Fall 2030"]][1], "MWF 9:00a-9:50a\nTTH 11:00a-12:15p")
  expect_equal(export[["Spring 2031"]][3], "yes")
  # each class is only listed once and "no" is left empty
  expect_equal(
    export$Class,
    c("ERTH1010 (3) - Intro", "", "ERTH3030 (4) - Rocks (LAB)")
  )
  expect_equal(export[["Spring 2031"]][1], "")
  expect_equal(export[["Fall 2030"]][3], "")

  # grouping by instructor sorts by instructor, then class
  by_instructor <- detailed |>
    prepare_schedule_table_columns(group_by = "instructor")
  expect_equal(names(by_instructor)[4:5], c("Instructor", "Class"))
  by_instructor_export <- prepare_schedule_table_export(by_instructor)
  expect_equal(names(by_instructor_export)[1:2], c("Instructor", "Class"))
  expect_equal(by_instructor_export$Instructor, c("Ann Alpha", "", "Bob Beta"))
  expect_equal(
    as.character(by_instructor$Instructor),
    sort(as.character(by_instructor$Instructor))
  )
  expect_equal(
    by_instructor$Class,
    by_instructor |>
      dplyr::arrange(.data$Instructor, .data$Class) |>
      dplyr::pull("Class")
  )
})

test_that("filter_schedule_by_class_level() works", {
  df <- dplyr::tibble(
    class = factor(c("ERTH1010", "ERTH5100", "ERTH4700/5700", "ERTHnew"))
  )
  filtered_classes <- function(...) {
    as.character(filter_schedule_by_class_level(df, ...)$class)
  }
  expect_equal(filtered_classes(), as.character(df$class))
  expect_equal(
    filtered_classes(include_grad = FALSE),
    c("ERTH1010", "ERTH4700/5700", "ERTHnew")
  )
  expect_equal(
    filtered_classes(include_undergrad = FALSE),
    c("ERTH5100", "ERTH4700/5700", "ERTHnew")
  )
  expect_equal(
    filtered_classes(include_undergrad = FALSE, include_grad = FALSE),
    "ERTHnew"
  )
})

test_that("future summers without classes are 'no' instead of '?'", {
  terms <- c("Fall 2030", "Spring 2031", "Summer 2031", "Fall 2031")
  combined <- combine_schedule(
    schedule = prepare_schedule(test_schedule()),
    not_teaching = prepare_not_teaching(test_not_teaching()),
    instructors = prepare_instructors(test_instructors()),
    classes = prepare_classes(test_classes()),
    selected_terms = terms,
    recognized_reasons = test_not_teaching()$reason
  )
  expect_equal(combined[["Summer 2031"]], c("no", "no", "no"))
  expect_equal(combined[["Fall 2031"]], c("?", "?", "?"))
})
