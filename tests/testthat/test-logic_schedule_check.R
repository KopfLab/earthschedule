# synthetic course export in the format of the scheduling software
write_test_export <- function() {
  file <- tempfile(fileext = ".csv")
  header <- ",CLSS ID,Course,Section #,Component,Meetings,Instructor,Room,Status,Combined Sections"
  ann <- "\"Alpha, Ann (123) [Primary Instructor, Post, Print]\""
  bob <- "\"Beta, Bob (456) [Primary Instructor, Post, Print]\""
  cat <- "\"Gamma, Cat (789) [Primary Instructor, Post, Print]\""
  writeLines(
    c(
      "\ufeffSpring 2031 CU Boulder,,,,,,,,,",
      "\"Generated 10/8/2026, 12:11:11 PM\",,,,,,,,,",
      header,
      "ERTH 1010 - Intro,,,,,,,,,",
      paste0(",1,ERTH 1010,1,Lecture,MWF 12:20pm-1:10pm,", ann, ",Benson Earth Sciences Bldg 180,Active,"),
      ",2,ERTH 1010,2,Laboratory,T 1pm-3:50pm,\"Alpha, Ann (123) [Primary Instructor, Post, No Print]\",Benson Earth Sciences Bldg 385,Active,",
      ",3,ERTH 1010,3,Laboratory,No Time Assigned,Staff [Primary Instructor],No Room Needed,Cancelled Section,",
      "ERTH 4021 - Cross,,,,,,,,,",
      paste0(",4,ERTH 4021,1,Lecture,TTh 11am-12:15pm,", bob, ",Benson Earth Sciences Bldg 455,Active,"),
      "ERTH 5021 - Cross,,,,,,,,,",
      paste0(",5,ERTH 5021,1,Lecture,TTh 11am-12:15pm,", cat, ",Benson Earth Sciences Bldg 455,Active,"),
      "ERTH 6000 - New,,,,,,,,,",
      paste0(",6,ERTH 6000,1,Seminar,No Time Assigned,", bob, ",See DEPT,Active,"),
      "ERTH 4640 - Combined,,,,,,,,,",
      paste0(",7,ERTH 4640,1,Lecture,MW 9am-9:50am,", bob, ",Benson Earth Sciences Bldg 355,Active,Also ERTH 5640-001"),
      "ERTH 5640 - Combined,,,,,,,,,",
      paste0(",8,ERTH 5640,1,Lecture,MW 9am-9:50am,", bob, ",Benson Earth Sciences Bldg 355,Active,See ERTH 4640-001"),
      "ERTH 4980 - Combined New,,,,,,,,,",
      paste0(",9,ERTH 4980,1,Seminar,No Time Assigned,", cat, ",See DEPT,Active,Also ERTH 5980-001"),
      "ERTH 5980 - Combined New,,,,,,,,,",
      paste0(",10,ERTH 5980,1,Seminar,No Time Assigned,", cat, ",See DEPT,Active,See ERTH 4980-001")
    ),
    file
  )
  file
}

test_check_schedule <- function() {
  dplyr::tibble(
    term = "Spring 2031",
    class = c(
      "ERTH1010",
      "ERTH1010",
      "ERTH4021/5021",
      "ERTH3030",
      "XXXX9999",
      "ERTH1010",
      "ERTH4640/5640"
    ),
    section = NA_character_,
    instructor_id = c("ann", "grad", "bob", "cat", "ann", "ann", "bob"),
    building = c("BESC", "BESC", "BESC", NA, NA, "BESC", "BESC"),
    room = c("180.0", "385", "455", NA, NA, "265", "355"),
    days = c("MWF", "T", "TTH", NA, NA, "W", "MW"),
    start_time = c("12:20p", "1p", "11:00a", NA, NA, "2p", "9:00a"),
    end_time = c("1:10p", "3:50p", "12:15p", NA, NA, "4:50p", "9:50a"),
    deleted = as.POSIXct(c(NA, NA, NA, NA, NA, "2024-01-01", NA), tz = "UTC"),
    canceled = NA
  )
}

test_that("course export is read correctly", {
  export <- read_course_export(write_test_export())
  expect_equal(export$term, "Spring 2031")
  expect_equal(nrow(export$sections), 10L)
  expect_equal(export$sections$class[1], "ERTH1010")
  expect_equal(export$sections$canceled, 1:10 == 3)
  expect_equal(
    get_export_combined_group(
      export$sections$class,
      export$sections$section,
      export$sections$combined
    )[7:8],
    rep("ERTH4640-1 ERTH5640-1", 2)
  )
  # no Course Title column
  expect_true(all(is.na(export$sections$title)))

  bad <- tempfile(fileext = ".csv")
  writeLines(c("no term here", "a,b"), bad)
  expect_error(read_course_export(bad), "could not find the term")
})

test_that("normalization works", {
  expect_equal(
    normalize_time(c("12:20pm", "12:20p", "1p", "11:00a", "11am", "x", NA)),
    c("12:20pm", "12:20pm", "1:00pm", "11:00am", "11:00am", NA, NA)
  )
  expect_equal(
    normalize_section(c("001", "1", "1.0", "801", NA)),
    c("1", "1", "1", "801", NA)
  )
  expect_equal(
    get_meeting_key(c("TTh", "MW", "TBA"), c("11a", NA, NA), c("12:15p", NA, NA)),
    c("TTH 11:00am-12:15pm", "MW ?-?", "")
  )
  expect_equal(
    get_export_meeting_key(c("TTh 11am-12:15pm", "No Time Assigned")),
    c("TTH 11:00am-12:15pm", "")
  )
  expect_equal(
    get_export_room_key(c("Benson Earth Sciences Bldg 180", "See DEPT", "No Room Needed")),
    c("BESC 180", "", "")
  )
  expect_equal(get_app_room_key(c("BESC", NA), c("180.0", NA)), c("BESC 180", ""))
  expect_equal(split_cross_listed_class("ERTH4021/5021"), c("ERTH4021", "ERTH5021"))
  expect_equal(combine_class_names(c("ERTH4021", "ERTH5021")), "ERTH4021/5021")
  expect_true(is_same_last_name("Small Tilton", "Tilton"))
  expect_false(is_same_last_name("Tilton", "Tucker"))
})

test_that("instructor comparison ignores unprinted instructors of TA sections", {
  expect_true(is_same_instructors("Alpha", FALSE, character()))
  expect_false(is_same_instructors("Alpha", TRUE, character()))
  expect_true(is_same_instructors(c("Alpha", "Beta"), c(TRUE, TRUE), c("Beta", "Alpha")))
  expect_false(is_same_instructors("Alpha", TRUE, "Beta"))
})

test_that("schedule is compared to the course export", {
  export <- read_course_export(write_test_export())
  diffs <- compare_schedule_to_export(
    export$sections,
    schedule = test_check_schedule(),
    instructors = test_instructors(),
    term = export$term,
    classes = test_classes()
  )
  all <- diffs
  diffs <- dplyr::filter(all, .data$is_difference)
  # ERTH1010 lecture and lab match, canceled lab and deleted section are ignored
  expect_equal(all$section[all$class == "ERTH1010"], c("1", "2"))
  expect_false("ERTH1010" %in% diffs$class)
  # cross-listed class in the app but separate sections in the upload: the
  # app class matches ERTH4021 (same instructor) and ERTH5021 is missing
  expect_equal(diffs$difference[diffs$class == "ERTH4021/5021"], "class")
  expect_equal(diffs$export_class[diffs$class == "ERTH4021/5021"], "ERTH4021")
  expect_equal(diffs$app_class[diffs$class == "ERTH4021/5021"], "ERTH4021/5021")
  expect_equal(
    diffs$difference[diffs$class == "ERTH5021"],
    "section not in app"
  )
  # combined sections in the upload match the cross-listed class in the app
  expect_equal(all$class[all$app_idx %in% 7], "ERTH4640/5640")
  expect_false("ERTH4640/5640" %in% diffs$class)
  expect_equal(diffs$difference[diffs$class == "ERTH4980/5980"], "class not in app")
  # classes missing on either side (placeholder classes are ignored)
  expect_equal(diffs$difference[diffs$class == "ERTH3030"], "class not in upload")
  expect_equal(diffs$difference[diffs$class == "ERTH6000"], "class not in app")
  expect_false(any(stringr::str_detect(all$class, "XXXX")))
  expect_equal(nrow(diffs), 5L)
  expect_equal(nrow(all), 8L)
  # titles are included but title differences alone are not listed
  expect_equal(diffs$app_title[diffs$class == "ERTH3030"], "Rocks")
  expect_true(all(is.na(diffs$export_title)))

  # no differences
  same <- compare_schedule_to_export(
    export$sections[1:2, ],
    schedule = test_check_schedule()[1:2, ],
    instructors = test_instructors(),
    term = export$term
  )
  expect_false(any(same$is_difference))
  expect_equal(nrow(same), 2L)
})

test_that("special topics classes are compared by topic", {
  expect_equal(
    normalize_topic(c(
      "Topic: Lab Methods",
      "Seminar Topic: Mineral Physics",
      "Geological Topics Seminar: Snow",
      "Snow: Past and Future",
      NA
    )),
    c("Lab Methods", "Mineral Physics", "Snow", "Snow: Past and Future", "")
  )
  expect_equal(pick_topic(c("226", NA, "Topic: Rocks")), "Rocks")
  expect_equal(pick_topic(c("226", "Snow Hydrology")), "Snow Hydrology")
  expect_equal(pick_topic(c("226", NA, "")), "")

  export_sections <- dplyr::tibble(
    class = c("ERTH4700", "ERTH4700", "ERTH1010"),
    title = NA_character_,
    section = c("1", "2", "1"),
    component = "Lecture",
    instructor = "Beta, Bob (456) [Primary Instructor, Post, Print]",
    room = "See DEPT",
    meetings = "No Time Assigned",
    canceled = FALSE,
    combined = NA_character_,
    notes1 = c("Topic: Rocks", NA, "Topic: ignored"),
    notes2 = c(NA, "Topic: Minerals", NA)
  )
  schedule <- dplyr::tibble(
    term = "Spring 2031",
    class = c("ERTH4700", "ERTH4700", "ERTH1010"),
    section = c("1", "2", "1"),
    subtitle = c("Rocks", "Fossils", "something else"),
    instructor_id = "bob",
    building = NA_character_,
    room = NA_character_,
    days = NA_character_,
    start_time = NA_character_,
    end_time = NA_character_,
    deleted = as.POSIXct(NA, tz = "UTC"),
    canceled = NA
  )
  compared <- compare_schedule_to_export(
    export_sections,
    schedule = schedule,
    instructors = test_instructors(),
    term = "Spring 2031"
  )
  expect_equal(compared$is_topic_class, c(FALSE, TRUE, TRUE))
  expect_equal(compared$diff_topic, c(FALSE, FALSE, TRUE))
  expect_equal(compared$difference, c("", "", "topic"))
  expect_equal(compared$export_topic[3], "Minerals")
})

test_that("offerings are matched by section or by instructor", {
  offerings <- function(classes, sections, instructors) {
    dplyr::tibble(
      class_numbers = as.list(classes),
      section = sections,
      instructor_last_names = as.list(instructors),
      instructor_print = list(TRUE),
      meeting_key = "",
      room_key = "",
      topic = "",
      canceled = FALSE,
      is_topic_class = classes %in% get_topic_classes()
    )
  }
  # regular classes: by section number
  expect_equal(
    match_offerings(
      offerings("ERTH1010", c("1", "2"), c("Alpha", "Beta")),
      offerings("ERTH1010", c("1", "2"), c("Beta", "Gamma"))
    ),
    dplyr::tibble(export_row = 1:2, app_row = 1:2)
  )
  # special topics: by instructor (different instructors are not matched)
  expect_equal(
    match_offerings(
      offerings("ERTH4700", c("1", "2"), c("Alpha", "Beta")),
      offerings("ERTH4700", c("1", "2"), c("Beta", "Gamma"))
    ),
    dplyr::tibble(export_row = c(2L, 1L, NA), app_row = c(1L, NA, 2L))
  )
  # only offerings that share a class number are matched
  expect_equal(
    match_offerings(
      offerings("ERTH1010", "1", "Alpha"),
      offerings("ERTH1020", "1", "Alpha")
    ),
    dplyr::tibble(export_row = c(1L, NA), app_row = c(NA, 1L))
  )
})

test_that("combined sections in the upload are compared as cross-listed classes", {
  export_sections <- dplyr::tibble(
    class = c("ERTH4700", "ERTH5700", "ERTH4700"),
    title = NA_character_,
    section = c("1", "2", "3"),
    component = "Lecture",
    instructor = c(
      "Alpha, Ann (1) [Primary Instructor, Post, Print]",
      "Alpha, Ann (1) [Primary Instructor, Post, Print]",
      "Beta, Bob (2) [Primary Instructor, Post, Print]"
    ),
    room = "See DEPT",
    meetings = "No Time Assigned",
    canceled = FALSE,
    combined = c("Also ERTH 5700-002", "See ERTH 4700-001", NA),
    # topic in the notes of the second combined section
    notes1 = c("226", NA, NA),
    notes2 = c(NA, "Topic: Rocks", NA)
  )
  schedule <- dplyr::tibble(
    term = "Spring 2031",
    class = c("ERTH4700", "ERTH4700/5700"),
    section = c("1", "3"),
    subtitle = NA_character_,
    instructor_id = c("ann", "bob"),
    building = NA_character_,
    room = NA_character_,
    days = NA_character_,
    start_time = NA_character_,
    end_time = NA_character_,
    deleted = as.POSIXct(NA, tz = "UTC"),
    canceled = NA
  )
  compared <- compare_schedule_to_export(
    export_sections,
    schedule = schedule,
    instructors = test_instructors(),
    term = "Spring 2031"
  )
  expect_equal(compared$class, c("ERTH4700/5700", "ERTH4700/5700"))
  expect_equal(compared$export_class, c("ERTH4700", "ERTH4700/5700"))
  expect_equal(compared$app_class, c("ERTH4700/5700", "ERTH4700"))
  expect_equal(compared$export_section, c("3", "4700-1/5700-2"))
  expect_equal(compared$difference, c("class", "class, section, topic"))
  expect_equal(compared$export_topic, c("", "Rocks"))
})
