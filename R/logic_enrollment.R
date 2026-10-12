# enrollment, student credit hour (SCH) and TA equivalent calculations =====
# based on the enrollment & SCH data export (Detail tab) of the GEOL/ERTH
# sponsored sections

# constants =====

# SCH categories by section activity type, all other types (DIS, THE, IND, OTH)
# are 100% other SCH. LEC sections of classes that also have LAB sections are
# recoded as LEC+LAB and get 1 lab credit hour per enrolled student.
sch_lecture_types <- c("LEC", "REC", "SEM") # 100% lecture SCH
sch_lab_types <- c("MLS", "LAB", "FLD") # 100% lab SCH

# SCH per TA equivalent
sch_per_ta_lecture <- 360
sch_per_ta_lab <- 54

# instructor job titles that count as a TA (if the data has no Has_TA column)
sch_ta_job_titles <- c("TEACHING ASSISTANT", "RESEARCH ASSISTANT")

# semester codes in YearTerm (e.g. Fall 2016 = 20167)
sch_semester_codes <- c(Spring = 1L, Summer = 4L, Fall = 7L)

# loading =====

# read the enrollment data
# reads the Detail tab of the enrollment & SCH data export (header in row 14)
# and keeps only the sections sponsored by one of the `subjects`. Note that
# the file has two sets of SECTIONS/Enrl columns: the 1st set pairs with SP_SCH
# (credited to the sponsoring section, incl. all cross-listed enrollment), the
# 2nd with Ch_SCH.
# @param file path to the excel file
# @param subjects the sponsoring subjects to keep
# @return data frame with one row per section plus Spnsr_Course (sponsoring
#   course without the section number, e.g. GEOL4241), Course (the section's
#   own, possibly cross-listed, course e.g. GEOG4241) and Has_TA (1 if the
#   section has a TA). If the file has no Has_TA column, it is derived from
#   the instructor job titles (any of JobTitle1-4 is one of sch_ta_job_titles).
sch_read_data <- function(file, subjects = c("GEOL", "ERTH")) {
  data <- readxl::read_excel(
    file,
    sheet = "Detail",
    skip = 13,
    guess_max = 2e5,
    .name_repair = "minimal"
  ) |>
    dplyr::rename(SECTIONS = 17, Enrl = 18, Ch_SECTIONS = 20, Ch_Enrl = 21) |>
    dplyr::filter(stringr::str_sub(.data$Spnsr_ID, 1, 4) %in% subjects) |>
    dplyr::mutate(
      # extracted via pattern since a few sections have 4 characters (e.g. 430R)
      Spnsr_Course = stringr::str_extract(.data$Spnsr_ID, "^[A-Z]{4}[0-9]{4}"),
      Course = paste0(.data$Subject, .data$CrsNum),
      # enrollment and SCH are integers (the export has floating point noise,
      # e.g. 30.9997)
      dplyr::across(
        c("SECTIONS", "Enrl", "SP_SCH", "Ch_SECTIONS", "Ch_Enrl", "Ch_SCH"),
        round
      )
    )

  if (!"Has_TA" %in% names(data)) {
    data <- data |>
      dplyr::mutate(
        Has_TA = dplyr::if_any(
          dplyr::starts_with("JobTitle"),
          \(x) x %in% sch_ta_job_titles
        ) |>
          as.integer()
      )
  }
  data
}

# read the enrollment data and summarize the courses
# @inheritParams sch_read_data
# @return course summary from sch_summarize_courses()
sch_read_courses <- function(file, subjects = c("GEOL", "ERTH")) {
  data <- sch_read_data(file, subjects)
  data |>
    sch_calculate_sections() |>
    sch_summarize_courses() |>
    sch_add_section_info(data)
}

# section information =====

# combine names (e.g. instructors), each element can already have several
# names separated by "; "
# @param x character vector of names
# @return single string with the unique names separated by "; " (NA if none)
sch_combine_names <- function(x) {
  x <- x[!is.na(x)] |> stringr::str_split("; ") |> unlist() |> unique()
  if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
}

# get the instructor or TA names of each section (row)
# @param data sections from sch_read_data()
# @param tas whether to get the TAs (instructors with one of the
#   sch_ta_job_titles) instead of the instructors
# @return character vector with the names (separated by "; ", NA if none)
sch_get_instructor_names <- function(data, tas = FALSE) {
  purrr::map(1:4, \(i) {
    name <- data[[paste0("InstName", i)]]
    is_ta <- data[[paste0("JobTitle", i)]] %in% sch_ta_job_titles
    dplyr::if_else(!is.na(name) & is_ta == tas, name, NA_character_)
  }) |>
    purrr::pmap_chr(\(...) sch_combine_names(c(...)))
}

# summarize the information of all sponsoring sections of each course
# includes all sections (incl. subsections such as REC and the LAB sections
# of LEC+LAB classes that carry no SCH)
# @param data sections from sch_read_data()
# @return data frame with one row per term and sponsoring course (TERM,
#   Spnsr_Course, CRSTITLE) with section_info (a nested data frame with one row per section with section, section_number,
#   activity, primary, enrl, max_enrl, sch, instructors, tas and has_ta)
sch_summarize_section_info <- function(data) {
  data |>
    dplyr::mutate(
      instructors = sch_get_instructor_names(data),
      tas = sch_get_instructor_names(data, tas = TRUE)
    ) |>
    dplyr::summarize(
      section_number = stringr::str_sub(.data$Spnsr_ID[1], 9),
      activity = unique(.data$ACTVTYP) |> paste(collapse = "/"),
      primary = .data$Primary_Sub_Section[1],
      enrl = sum(
        .data$Enrl[.data$Primary_Sub_Section == "Primary Section"],
        na.rm = TRUE
      ),
      max_enrl = .data$Max_Enrollment[1],
      sch = sum(.data$SP_SCH, na.rm = TRUE),
      instructors = sch_combine_names(.data$instructors),
      tas = sch_combine_names(.data$tas),
      has_ta = any(.data$Has_TA == 1, na.rm = TRUE),
      .by = c("TERM", "Spnsr_Course", "CRSTITLE", "Spnsr_ID")
    ) |>
    dplyr::rename(section = "Spnsr_ID") |>
    dplyr::arrange(.data$section) |>
    tidyr::nest(section_info = -c("TERM", "Spnsr_Course", "CRSTITLE"))
}

# add the section information to the course summary
# @param df course summary from sch_summarize_courses()
# @param data sections from sch_read_data()
# @return `df` with section_info (see
#   sch_summarize_section_info(), plus the lecture and lab enrollment and SCH
#   from the course's sections)
sch_add_section_info <- function(df, data) {
  df |>
    dplyr::left_join(
      sch_summarize_section_info(data),
      by = c("TERM", "Spnsr_Course", "CRSTITLE")
    ) |>
    dplyr::mutate(
      section_info = purrr::map2(
        .data$section_info,
        .data$sections,
        \(info, sections) {
          if (is.null(info)) {
            return(info)
          }
          info |>
            dplyr::left_join(
              sections |>
                dplyr::select(
                  "section",
                  "lecture_enrl",
                  "lab_enrl",
                  "lecture_sch",
                  "lab_sch"
                ),
              by = "section"
            )
        }
      )
    )
}

# modern course codes (GEOL -> ERTH)
# @param x course codes, section ids or combined course codes (e.g.
#   "GEOL4241/GEOG4241")
# @return `x` with GEOL replaced by ERTH
sch_modernize_codes <- function(x) {
  stringr::str_replace_all(x, "GEOL(?=[0-9]{4})", "ERTH")
}

# section and course SCH =====

# split each section's SCH into lecture, lab and other SCH
# the LAB sections of LEC+LAB classes carry no SCH (it's all recorded in the
# LEC section) and are removed.
# @param data sections from sch_read_data()
# @return data frame of sections with lecture_sch, lab_sch, other_sch and
#   has_ta (whether any section of the course has a TA, incl. the removed LAB
#   sections)
sch_calculate_sections <- function(data) {
  sections <- data |>
    dplyr::mutate(
      ACTVTYP = dplyr::if_else(
        .data$ACTVTYP == "LEC" & any(.data$ACTVTYP == "LAB"),
        "LEC+LAB",
        .data$ACTVTYP
      ),
      .by = c("TERM", "Spnsr_Course")
    ) |>
    dplyr::mutate(
      lab_sch = dplyr::case_when(
        .data$ACTVTYP %in% sch_lab_types ~ .data$SP_SCH,
        .data$ACTVTYP == "LEC+LAB" ~ .data$Enrl,
        .default = 0
      ),
      lecture_sch = dplyr::case_when(
        .data$ACTVTYP %in% sch_lecture_types ~ .data$SP_SCH,
        .data$ACTVTYP == "LEC+LAB" ~ .data$SP_SCH - .data$lab_sch,
        .default = 0
      ),
      other_sch = .data$SP_SCH - .data$lecture_sch - .data$lab_sch,
      has_ta = any(.data$Has_TA == 1, na.rm = TRUE),
      is_leclab_lab = .data$ACTVTYP == "LAB" & any(.data$ACTVTYP == "LEC+LAB"),
      .by = c("TERM", "Spnsr_Course")
    )

  leclab_lab_sch <- sections$SP_SCH[sections$is_leclab_lab]
  if (!all(leclab_lab_sch == 0, na.rm = TRUE)) {
    abort("LAB sections of LEC+LAB classes have SP_SCH")
  }

  sections |>
    dplyr::filter(!.data$is_leclab_lab) |>
    dplyr::select(-"is_leclab_lab")
}

# combine the unique course codes, sponsoring course first
# @param courses course codes (e.g. c("GEOL4241", "GEOG4241", "GEOL4241"))
# @param sponsor the sponsoring course code
# @return single string (e.g. "GEOL4241/GEOG4241")
sch_combine_courses <- function(courses, sponsor) {
  courses <- unique(courses)
  c(intersect(sponsor, courses), sort(setdiff(courses, sponsor))) |>
    paste(collapse = "/")
}

# summarize the SCH of each course (combining cross-listed sections)
# @param sections sections from sch_calculate_sections()
# @return data frame with one row per term and sponsoring course incl. the
#   course number (Class, consistent across the GEOL -> ERTH rename), the
#   combined (cross-listed) course codes and activity types, the UG/Grad level,
#   enrollment, SCH, fiscal year (FY, e.g. FY24 = Summer 2023 + Fall 2023 +
#   Spring 2024), the lecture and lab enrollment (lecture_enrl = students in
#   sections with lecture SCH incl. LEC+LAB, lab_enrl = students in lab-only
#   sections), whether the course has a TA (has_ta) and the enrollment and SCH
#   of each sponsoring section (sections, a nested data frame with section,
#   lecture_enrl, lab_enrl, lecture_sch and lab_sch)
sch_summarize_courses <- function(sections) {
  sections |>
    dplyr::summarize(
      # enrollment and SCH of each sponsoring section (first, since the
      # columns are overwritten by their sums below)
      sections = list(
        dplyr::pick(
          "Spnsr_ID",
          "Primary_Sub_Section",
          "Enrl",
          "lecture_sch",
          "lab_sch"
        ) |>
          dplyr::rename(section = "Spnsr_ID") |>
          sch_summarize_sections()
      ),
      Courses = sch_combine_courses(.data$Course, .data$Spnsr_Course[1]),
      ACTVTYP = unique(.data$ACTVTYP) |> sort() |> paste(collapse = "/"),
      # UG/Grad level of the sections that carry the SCH (i.e. the sponsoring
      # sections, which also carry the SCH of any cross-listed sections)
      GRAD_UG = c(
        .data$GRAD_UG[.data$SP_SCH > 0],
        .data$GRAD_UG[.data$COURSE == .data$Spnsr_ID],
        .data$GRAD_UG
      )[1],
      # only count primary sections, subsections (REC) enroll the same
      # students again and carry no SCH
      Enrl = sum(
        .data$Enrl[.data$Primary_Sub_Section == "Primary Section"],
        na.rm = TRUE
      ),
      dplyr::across(
        c("SP_SCH", "lecture_sch", "lab_sch", "other_sch"),
        \(x) sum(x, na.rm = TRUE)
      ),
      has_ta = any(.data$has_ta),
      .by = c("TERM", "YearTerm", "FYC", "Spnsr_Course", "CRSTITLE")
    ) |>
    dplyr::relocate("sections", .after = dplyr::last_col()) |>
    dplyr::mutate(
      lecture_enrl = purrr::map_dbl(.data$sections, \(x) sum(x$lecture_enrl)),
      lab_enrl = purrr::map_dbl(.data$sections, \(x) sum(x$lab_enrl)),
      Class = stringr::str_sub(.data$Spnsr_Course, 5),
      FY = sprintf("FY%02d", .data$FYC %% 100)
    ) |>
    dplyr::arrange(.data$Spnsr_Course, .data$YearTerm)
}

# summarize the enrollment and SCH of each sponsoring section
# cross-listed sections are combined with their sponsoring section, the
# enrollment only counts primary sections (subsections such as REC enroll the
# same students again and carry no SCH).
# @param df sections of one course with section (the sponsoring section),
#   Primary_Sub_Section, Enrl, lecture_sch and lab_sch
# @return data frame with one row per section with lecture or lab SCH and the
#   section's lecture_enrl (if it has lecture SCH, incl. LEC+LAB), lab_enrl
#   (if it only has lab SCH), lecture_sch and lab_sch
sch_summarize_sections <- function(df) {
  df |>
    dplyr::summarize(
      enrl = sum(
        .data$Enrl[.data$Primary_Sub_Section == "Primary Section"],
        na.rm = TRUE
      ),
      dplyr::across(c("lecture_sch", "lab_sch"), \(x) sum(x, na.rm = TRUE)),
      .by = "section"
    ) |>
    dplyr::filter(.data$lecture_sch > 0 | .data$lab_sch > 0) |>
    dplyr::mutate(
      lecture_enrl = dplyr::if_else(.data$lecture_sch > 0, .data$enrl, 0),
      lab_enrl = dplyr::if_else(.data$lecture_sch == 0, .data$enrl, 0)
    ) |>
    dplyr::select(
      "section",
      "lecture_enrl",
      "lab_enrl",
      "lecture_sch",
      "lab_sch"
    )
}

# calculate TA equivalents from SCH
# 1 TA equivalent = 360 lecture SCH or 54 lab SCH (other SCH is not counted).
# @param df course summary from sch_summarize_courses()
# @param lecture_sch_per_ta lecture SCH per TA equivalent
# @param lab_sch_per_ta lab SCH per TA equivalent
# @return `df` with the additional columns lecture_ta, lab_ta and ta (total),
#   and lecture_ta and lab_ta added to the nested sections
sch_calculate_ta_equivalents <- function(
  df,
  lecture_sch_per_ta = sch_per_ta_lecture,
  lab_sch_per_ta = sch_per_ta_lab
) {
  df |>
    dplyr::mutate(
      lecture_ta = .data$lecture_sch / lecture_sch_per_ta,
      lab_ta = .data$lab_sch / lab_sch_per_ta,
      ta = .data$lecture_ta + .data$lab_ta,
      sections = purrr::map(
        .data$sections,
        \(x) {
          x |>
            dplyr::mutate(
              lecture_ta = .data$lecture_sch / lecture_sch_per_ta,
              lab_ta = .data$lab_sch / lab_sch_per_ta
            )
        }
      )
    )
}

# filtering =====

# convert semester names to YearTerm codes
# @param term semester name(s) (e.g. "Fall 2016")
# @return integer YearTerm code(s) (e.g. 20167)
sch_term_to_yearterm <- function(term) {
  season <- stringr::str_extract(term, "^[A-Za-z]+")
  year <- stringr::str_extract(term, "[0-9]{4}$") |> as.integer()
  if (!all(season %in% names(sch_semester_codes)) || anyNA(year)) {
    abort("semesters must be formatted as e.g. 'Fall 2016'")
  }
  year * 10L + unname(sch_semester_codes[season])
}

# filter the course summary by term and course level
# @param df course summary from sch_summarize_courses()
# @param include_summer whether to include summer terms
# @param include_grad whether to include graduate courses (if FALSE, only
#   GRAD_UG == "UG" are included). Note: this might not be necessary since
#   cross-listed graduate sections carry no SCH of their own (graduate
#   students in UG-sponsored classes are counted either way) but kept for now.
# @param include_grad_field whether to include graduate field classes
#   (GRAD_UG == "Grad" with an FLD activity type), only relevant if
#   include_grad = TRUE
# @param from first semester to include (e.g. "Fall 2016"), NULL for no limit
# @param to last semester to include (e.g. "Spring 2026"), NULL for no limit
# @return the filtered data frame
sch_filter_courses <- function(
  df,
  include_summer = TRUE,
  include_grad = FALSE,
  include_grad_field = FALSE,
  from = NULL,
  to = NULL
) {
  from_yearterm <- if (is.null(from)) -Inf else sch_term_to_yearterm(from)
  to_yearterm <- if (is.null(to)) Inf else sch_term_to_yearterm(to)
  df |>
    dplyr::filter(
      include_summer | !stringr::str_detect(.data$TERM, "Summer"),
      include_grad | .data$GRAD_UG == "UG",
      include_grad_field |
        !(.data$GRAD_UG == "Grad" & stringr::str_detect(.data$ACTVTYP, "FLD")),
      dplyr::between(as.integer(.data$YearTerm), from_yearterm, to_yearterm)
    )
}

# the semesters in the course summary (chronological, without summers)
# @param df course summary from sch_summarize_courses()
# @return character vector of semesters (e.g. "Fall 2018")
sch_get_semesters <- function(df) {
  df |>
    dplyr::filter(!stringr::str_detect(.data$TERM, "Summer")) |>
    dplyr::select("YearTerm", "TERM") |>
    dplyr::distinct() |>
    dplyr::arrange(.data$YearTerm) |>
    dplyr::pull(.data$TERM)
}

# periods =====

# time periods for the summaries and plots: semesters, academic years (AY24 =
# Fall 2023 + Spring 2024, summers are never included) and running averages
# of academic years (e.g. AY22-24 = mean of AY22, AY23 and AY24)
sch_periods <- c("semester", "AY", "running")

# columns that are summed (or averaged) over a period
sch_period_value_cols <- c(
  "Enrl",
  "SP_SCH",
  "lecture_sch",
  "lab_sch",
  "other_sch",
  "lecture_enrl",
  "lab_enrl",
  "lecture_ta",
  "lab_ta",
  "ta"
)

# find the academic years that are missing from the data (neither fall nor
# spring semester, e.g. AY21 is not part of the enrollment data export)
# @param df course summary from sch_summarize_courses()
# @param from first semester to consider (e.g. "Fall 2018"), NULL for all
# @param to last semester to consider, NULL for all
# @return academic years (e.g. "AY21") between the first and last academic
#   year in the range that have no data
sch_find_missing_years <- function(df, from = NULL, to = NULL) {
  years <- df |>
    sch_filter_courses(
      include_summer = FALSE,
      include_grad = TRUE,
      include_grad_field = TRUE,
      from = from,
      to = to
    ) |>
    dplyr::pull(.data$FYC) |>
    unique()
  if (length(years) == 0) {
    return(character(0))
  }
  missing <- setdiff(seq(min(years), max(years)), years)
  sprintf("AY%02d", missing %% 100)
}

# describe a period
# @param period one of sch_periods
# @param n_years number of academic years in the running averages
# @return description (e.g. "per academic year (fall + spring)")
sch_describe_period <- function(period, n_years = 3) {
  switch(
    match.arg(period, sch_periods),
    semester = "per semester",
    AY = "per academic year (fall + spring)",
    running = paste0(
      n_years,
      "-year running averages of academic years (fall + spring)"
    )
  )
}

# assign the courses to periods
# academic years only include complete years (fall + spring). For running
# averages, each course contributes 1/n_years of its values (incl. the nested
# sections) to each window it is part of, windows that include a missing or
# incomplete academic year are omitted.
# @param df course summary from sch_summarize_courses()
# @param period "semester", "AY" (academic year) or "running" (running
#   average of academic years)
# @param n_years number of academic years in the running averages
# @return `df` with the period (factor in chronological order), for running
#   averages with each course repeated for every window it is part of
sch_assign_periods <- function(df, period = "semester", n_years = 3) {
  period <- match.arg(period, sch_periods)

  if (period == "semester") {
    return(
      df |>
        dplyr::arrange(.data$YearTerm) |>
        dplyr::mutate(period = forcats::fct_inorder(.data$TERM))
    )
  }

  # complete academic years (FYC without summer)
  df <- df |>
    dplyr::filter(!stringr::str_detect(.data$TERM, "Summer")) |>
    dplyr::filter(dplyr::n_distinct(.data$TERM) == 2, .by = "FYC") |>
    dplyr::arrange(.data$YearTerm)

  if (period == "AY") {
    return(
      df |>
        dplyr::mutate(
          period = forcats::fct_inorder(sprintf("AY%02d", .data$FYC %% 100))
        )
    )
  }

  # running average windows (by their last year) without missing years
  years <- unique(df$FYC)
  windows <- dplyr::tibble(last_year = years) |>
    dplyr::filter(purrr::map_lgl(
      .data$last_year,
      \(y) all(seq(y - n_years + 1, y) %in% years)
    )) |>
    dplyr::mutate(
      FYC = purrr::map(.data$last_year, \(y) seq(y - n_years + 1, y))
    ) |>
    tidyr::unnest("FYC")

  df |>
    dplyr::inner_join(windows, by = "FYC", relationship = "many-to-many") |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(sch_period_value_cols), \(x) x / n_years),
      sections = purrr::map(
        .data$sections,
        \(x) {
          x |>
            dplyr::mutate(dplyr::across(
              tidyselect::where(is.numeric),
              \(v) v / n_years
            ))
        }
      )
    ) |>
    dplyr::arrange(.data$last_year, .data$YearTerm) |>
    dplyr::mutate(
      period = sprintf(
        "AY%02d-%02d",
        (.data$last_year - n_years + 1) %% 100,
        .data$last_year %% 100
      ) |>
        forcats::fct_inorder()
    ) |>
    dplyr::select(-"last_year")
}

# summaries by class, class type and total =====

# label the classes by their modern course codes and most recent title
# @param df course summary from sch_summarize_courses()
# @return data frame with Class, code (e.g. "ERTH4185"), courses (all
#   cross-listed codes the last time the class was taught, e.g.
#   "ERTH4185/5185/MCDB4185"), title and label (e.g.
#   "ERTH4185/5185/MCDB4185 Geomicrobiology")
sch_label_classes <- function(df) {
  df |>
    dplyr::summarize(
      code = dplyr::last(.data$Spnsr_Course, order_by = .data$YearTerm) |>
        sch_modernize_codes(),
      courses = dplyr::last(.data$Courses, order_by = .data$YearTerm) |>
        sch_modernize_codes() |>
        sch_shorten_codes(),
      title = dplyr::last(.data$CRSTITLE, order_by = .data$YearTerm),
      label = paste(.data$courses, .data$title),
      .by = "Class"
    ) |>
    dplyr::arrange(.data$Class)
}

# shorten combined course codes by grouping them by subject (in order of
# first appearance) and omitting repeated subjects
# @param x combined course codes (e.g. "ERTH4185/MCDB4185/ERTH5185")
# @return shortened codes (e.g. "ERTH4185/5185/MCDB4185")
sch_shorten_codes <- function(x) {
  purrr::map_chr(stringr::str_split(x, "/"), \(codes) {
    subjects <- stringr::str_extract(codes, "^[A-Z]{4}")
    order <- order(match(subjects, unique(subjects)))
    codes <- codes[order]
    subjects <- subjects[order]
    repeated <- !is.na(subjects) &
      subjects == dplyr::lag(subjects, default = "")
    codes[repeated] <- stringr::str_remove(codes[repeated], "^[A-Z]{4}")
    paste(codes, collapse = "/")
  })
}

# find the classes of a course code (e.g. from the schedule)
# @param df course summary from sch_summarize_courses()
# @param code course code, cross-listed course numbers can be separated by /
#   (e.g. "ERTH4444/5444")
# @return the codes (see sch_label_classes()) of the classes that include any
#   of the course numbers (as sponsoring or cross-listed course)
sch_find_classes <- function(df, code) {
  subject <- stringr::str_extract(code, "^[A-Z]{4}")
  codes <- paste0(subject, stringr::str_extract_all(code, "\\d{4}")[[1]])
  classes <- df |>
    dplyr::filter(purrr::map_lgl(
      stringr::str_split(sch_modernize_codes(.data$Courses), "/"),
      \(x) any(x %in% codes)
    )) |>
    dplyr::pull(.data$Class) |>
    unique()
  sch_label_classes(df) |>
    dplyr::filter(.data$Class %in% classes) |>
    dplyr::pull(.data$code)
}

# select classes by their modern course code
# @param df course summary from sch_summarize_courses()
# @param classes course codes (e.g. c("ERTH1010", "ERTH1030")), matched by
#   course number so they include the classes' GEOL versions, NULL for all
# @return the selected classes
sch_select_classes <- function(df, classes = NULL) {
  if (length(classes) == 0) {
    return(df)
  }
  missing <- classes[!stringr::str_sub(classes, 5) %in% df$Class]
  if (length(missing) > 0) {
    abort(paste(
      "classes not found in the data:",
      paste(missing, collapse = ", ")
    ))
  }
  df |> dplyr::filter(.data$Class %in% stringr::str_sub(classes, 5))
}

# class types: lab/field classes (coded as MLS or FLD) and all other classes
# by their level
sch_class_types <- c(
  "1000 level",
  "2000 level",
  "3000 level",
  "4000 level",
  "graduate (5000+)",
  "lab/field (MLS, FLD)"
)

# add the class type
# @param df course summary from sch_summarize_courses()
# @return `df` with the class_type (factor, see sch_class_types)
sch_add_class_type <- function(df) {
  df |>
    dplyr::mutate(
      class_type = dplyr::case_when(
        stringr::str_detect(.data$ACTVTYP, "MLS|FLD") ~ "lab/field (MLS, FLD)",
        stringr::str_sub(.data$Class, 1, 1) %in% c("1", "2", "3", "4") ~
          paste0(stringr::str_sub(.data$Class, 1, 1), "000 level"),
        .default = "graduate (5000+)"
      ) |>
        factor(levels = sch_class_types)
    )
}

# prepare the course summary for the summaries and plots
# filters the courses, calculates the TA equivalents, adds the class type,
# assigns the periods and selects the classes.
# @inheritParams sch_filter_courses
# @inheritParams sch_assign_periods
# @inheritParams sch_select_classes
# @return data frame from sch_assign_periods() with the TA equivalents and
#   class_type
sch_prepare_data <- function(
  df,
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_summer = FALSE,
  include_grad = TRUE,
  include_grad_field = FALSE
) {
  # the periods (complete academic years and running average windows) are
  # determined from all courses before selecting classes, otherwise classes
  # taught only once a year would never have a complete academic year
  df |>
    sch_filter_courses(
      include_summer,
      include_grad,
      include_grad_field,
      from,
      to
    ) |>
    sch_calculate_ta_equivalents() |>
    sch_add_class_type() |>
    sch_assign_periods(period, n_years) |>
    sch_select_classes(classes)
}

# summarize enrollment, SCH and TA equivalents per period
# @param df data from sch_prepare_data()
# @param by additional columns to summarize by
# @return data frame with one row per period (and `by`) with the number of
#   classes (n_classes) and classes with a TA (n_classes_with_ta), total,
#   lecture and lab enrollment, SCH and TA equivalents (without other SCH)
sch_summarize_values <- function(df, by = character()) {
  df |>
    dplyr::summarize(
      n_classes = dplyr::n_distinct(.data$Class),
      n_classes_with_ta = dplyr::n_distinct(.data$Class[.data$has_ta]),
      dplyr::across(
        c(
          "lecture_enrl",
          "lab_enrl",
          "lecture_sch",
          "lab_sch",
          "lecture_ta",
          "lab_ta"
        ),
        sum
      ),
      .by = dplyr::all_of(c("period", by))
    ) |>
    dplyr::mutate(
      enrl = .data$lecture_enrl + .data$lab_enrl,
      sch = .data$lecture_sch + .data$lab_sch,
      ta = .data$lecture_ta + .data$lab_ta,
      .after = "n_classes_with_ta"
    ) |>
    dplyr::arrange(.data$period, dplyr::pick(dplyr::all_of(by)))
}

# summarize enrollment, SCH and TA equivalents by class
# values are totals per semester or academic year, or the averages of the
# academic year totals for running averages (TA equivalents per academic
# year are the sum of both semesters). Summer terms are excluded by default,
# other SCH (DIS, THE, IND, OTH) and its enrollment are not included.
# @inheritParams sch_prepare_data
# @return data frame with one row per period and class (Class and label) with
#   has_ta and the values from sch_summarize_values()
sch_summarize_classes <- function(
  df,
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_summer = FALSE,
  include_grad = TRUE,
  include_grad_field = FALSE
) {
  df |>
    sch_prepare_data(
      classes,
      period,
      n_years,
      from,
      to,
      include_summer,
      include_grad,
      include_grad_field
    ) |>
    sch_summarize_values(by = "Class") |>
    dplyr::left_join(
      sch_label_classes(df) |>
        dplyr::select("Class", "code", "courses", "label"),
      by = "Class"
    ) |>
    dplyr::mutate(has_ta = .data$n_classes_with_ta > 0) |>
    dplyr::select(
      "period",
      "Class",
      "label",
      "has_ta",
      dplyr::everything(),
      -"n_classes",
      -"n_classes_with_ta"
    )
}

# summarize enrollment, SCH and TA equivalents by class type
# class types are lab/field classes (coded as MLS or FLD) and all other
# classes by their level (1000, 2000, 3000, 4000 or graduate 5000+ level).
# @inheritParams sch_summarize_classes
# @return data frame with one row per period and class type with the values
#   from sch_summarize_values()
sch_summarize_class_types <- function(
  df,
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_summer = FALSE,
  include_grad = TRUE,
  include_grad_field = FALSE
) {
  df |>
    sch_prepare_data(
      classes,
      period,
      n_years,
      from,
      to,
      include_summer,
      include_grad,
      include_grad_field
    ) |>
    sch_summarize_values(by = "class_type")
}

# summarize total enrollment, SCH and TA equivalents
# @inheritParams sch_summarize_classes
# @return data frame with one row per period with the values from
#   sch_summarize_values()
sch_summarize_total <- function(
  df,
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_summer = FALSE,
  include_grad = TRUE,
  include_grad_field = FALSE
) {
  df |>
    sch_prepare_data(
      classes,
      period,
      n_years,
      from,
      to,
      include_summer,
      include_grad,
      include_grad_field
    ) |>
    sch_summarize_values()
}

# plotting data =====

# categorical colors (ColorBrewer Dark2 without its last color, which is gray
# and could be confused with "other"), "other" is always gray
sch_class_colors <- c(
  "#1B9E77",
  "#D95F02",
  "#7570B3",
  "#E7298A",
  "#66A61E",
  "#E6AB02",
  "#A6761D"
)
sch_other_color <- "#8c8c8c"

# maximum number of top classes
sch_max_top <- 20

# get the categorical colors for n groups
# @param n number of groups
# @return sch_class_colors (if there are enough) or the default ggplot2 hues
sch_get_class_colors <- function(n) {
  if (n <= length(sch_class_colors)) {
    return(sch_class_colors[seq_len(n)])
  }
  scales::hue_pal()(n)
}

# values that can be plotted with their lecture/lab columns and labels
sch_plot_values <- dplyr::tibble(
  value = c("enrollment", "SCH", "TA"),
  lecture_col = c("lecture_enrl", "lecture_sch", "lecture_ta"),
  lab_col = c("lab_enrl", "lab_sch", "lab_ta"),
  label = c("enrollment", "student credit hours (SCH)", "TA equivalents")
)

# match value names
# @param values one or several of "enrollment", "SCH" and "TA" (or "TAs")
# @return the matched values
sch_match_values <- function(values) {
  values |>
    stringr::str_replace("^TAs$", "TA") |>
    match.arg(sch_plot_values$value, several.ok = TRUE)
}

# prepare the course summary for plotting
# prepares the data (summers are always excluded) and converts it to a long
# format with one row per value and course.
# @inheritParams sch_prepare_data
# @param values the values to include (see sch_match_values())
# @return data from sch_prepare_data() plus value (factor in the order of
#   `values`), lecture, lab, lecture_segments (the lecture value of each
#   section) and lab_segments (the lab value of each section)
sch_prepare_plot_data <- function(
  df,
  values,
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_grad = TRUE,
  include_grad_field = FALSE
) {
  df <- df |>
    sch_prepare_data(
      classes,
      period,
      n_years,
      from,
      to,
      include_summer = FALSE,
      include_grad,
      include_grad_field
    )

  sch_plot_values |>
    dplyr::filter(.data$value %in% values) |>
    dplyr::mutate(value = factor(.data$value, levels = values)) |>
    dplyr::arrange(.data$value) |>
    purrr::pmap(\(value, lecture_col, lab_col, label) {
      df |>
        dplyr::mutate(
          value = .env$value,
          lecture = .data[[lecture_col]],
          lab = .data[[lab_col]],
          lecture_segments = purrr::map(
            .data$sections,
            \(x) {
              dplyr::tibble(segment = x$section, lecture = x[[lecture_col]])
            }
          ),
          lab_segments = purrr::map(
            .data$sections,
            \(x) dplyr::tibble(segment = x$section, lab = x[[lab_col]])
          )
        )
    }) |>
    purrr::list_rbind()
}

# unnest the segments of the bars
# @param df data frame with the bar columns and a segments list column
# @param bar_cols the columns identifying each bar
# @param col the segments list column
# @return data frame with the bar columns and the unnested segments, NULL if
#   there are no segments
sch_unnest_segments <- function(df, bar_cols, col) {
  df <- df |> dplyr::filter(!purrr::map_lgl(.data[[col]], is.null))
  if (nrow(df) == 0) {
    return(NULL)
  }
  df |>
    dplyr::select(dplyr::all_of(bar_cols), dplyr::all_of(col)) |>
    tidyr::unnest(dplyr::all_of(col))
}

# summarize the bars (and their segments) for plotting
# @param df data from sch_prepare_plot_data() with a group column (factor,
#   one bar per group in each period)
# @param split whether to split the bars into segments (sections or
#   classes), if FALSE each bar has a single lecture and lab segment
# @param section_groups the groups whose lecture and lab are split into
#   their sections and labeled if they have a TA (i.e. individual
#   classes), all other groups are split into their classes (both lecture and
#   lab)
# @return data frame with one row per value, period and group with the
#   lecture and lab values (0 if no classes), has_ta (whether the class has a
#   TA, only for `section_groups`), class_number (the course number, only for
#   `section_groups`), lecture_segments and lab_segments
sch_summarize_bars <- function(
  df,
  section_groups = character(),
  split = TRUE
) {
  df <- df |> dplyr::mutate(group = forcats::fct_drop(.data$group))
  bar_cols <- c("value", "period", "group")

  # groups split by class (in order of course number)
  by_class <- df |>
    dplyr::filter(!.data$group %in% section_groups) |>
    dplyr::summarize(
      lecture = sum(.data$lecture),
      lab = sum(.data$lab),
      .by = dplyr::all_of(c(bar_cols, "Class"))
    ) |>
    dplyr::arrange(.data$Class) |>
    dplyr::rename(segment = "Class")

  # groups split by section (of each semester in the period)
  section_df <- df |> dplyr::filter(.data$group %in% section_groups)
  by_section <- section_df |>
    sch_unnest_segments(bar_cols, "lecture_segments")
  by_section_lab <- section_df |>
    sch_unnest_segments(bar_cols, "lab_segments")

  lecture_segments <- dplyr::bind_rows(
    by_section,
    by_class |> dplyr::select(-"lab")
  ) |>
    dplyr::filter(.data$lecture > 0) |>
    tidyr::nest(lecture_segments = c("segment", "lecture"))

  lab_segments <- dplyr::bind_rows(
    by_section_lab,
    by_class |> dplyr::select(-"lecture")
  ) |>
    dplyr::filter(.data$lab > 0) |>
    tidyr::nest(lab_segments = c("segment", "lab"))

  df |>
    dplyr::summarize(
      lecture = sum(.data$lecture),
      lab = sum(.data$lab),
      has_ta = .data$group[1] %in% section_groups & any(.data$has_ta),
      class_number = if (.data$group[1] %in% section_groups) {
        .data$Class[1]
      } else {
        NA_character_
      },
      .by = dplyr::all_of(bar_cols)
    ) |>
    # keep a slot for every group in every period so the bars line up
    tidyr::complete(
      !!!syms(bar_cols),
      fill = list(lecture = 0, lab = 0, has_ta = FALSE)
    ) |>
    dplyr::left_join(lecture_segments, by = bar_cols) |>
    dplyr::left_join(lab_segments, by = bar_cols) |>
    dplyr::arrange(.data$value, .data$period, .data$group) |>
    sch_merge_segments(merge = !split)
}

# merge the segments of each bar into a single lecture and lab segment
# @param df data frame from sch_summarize_bars()
# @param merge whether to merge the segments
# @return `df` with merged lecture_segments and lab_segments
sch_merge_segments <- function(df, merge = TRUE) {
  if (!merge) {
    return(df)
  }
  df |>
    dplyr::mutate(
      lecture_segments = purrr::map(.data$lecture, \(x) {
        if (x > 0) dplyr::tibble(segment = "all", lecture = x)
      }),
      lab_segments = purrr::map(.data$lab, \(x) {
        if (x > 0) dplyr::tibble(segment = "all", lab = x)
      })
    )
}

# summarize lecture and lab values per period for the top classes
# classes are identified by their course number (Class) so they stay the same
# across the GEOL -> ERTH rename, and labeled with their most recent course
# code and title. Summer terms are always excluded, other SCH (DIS, THE, IND,
# OTH) and its enrollment are not included.
# @inheritParams sch_prepare_plot_data
# @param values which values to summarize, one or several of "enrollment",
#   "SCH" (student credit hours) and "TA" (TA equivalents, see
#   sch_calculate_ta_equivalents())
# @param top_based_on which value ("enrollment", "SCH" or "TA") determines the
#   top classes, by default the first of the `values`
# @param n_top number of classes with the highest lecture + lab `top_based_on`
#   value (over all shown periods) to show individually, the rest are combined
#   as "other"
# @param show_other whether to include the "other" classes
# @return data frame from sch_summarize_bars() where group is the class
#   (in order of course number, "other" last)
sch_summarize_top_classes <- function(
  df,
  values = "SCH",
  top_based_on = values[1],
  classes = NULL,
  n_top = 5,
  show_other = TRUE,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_grad = TRUE,
  include_grad_field = FALSE
) {
  values <- sch_match_values(values)
  top_based_on <- sch_match_values(top_based_on)
  if (length(top_based_on) != 1) {
    abort("`top_based_on` must be a single value")
  }
  if (n_top < 1 || n_top > sch_max_top) {
    abort(sprintf("`n_top` must be between 1 and %d", sch_max_top))
  }

  class_labels <- sch_label_classes(df)
  df <- df |>
    sch_prepare_plot_data(
      union(values, top_based_on),
      classes,
      period,
      n_years,
      from,
      to,
      include_grad,
      include_grad_field
    )

  # top classes by lecture + lab of the top_based_on value
  top_classes <- df |>
    dplyr::filter(.data$value == top_based_on) |>
    dplyr::summarize(total = sum(.data$lecture + .data$lab), .by = "Class") |>
    dplyr::slice_max(.data$total, n = n_top, with_ties = FALSE) |>
    dplyr::left_join(class_labels, by = "Class") |>
    dplyr::arrange(.data$Class)
  groups <- c(
    top_classes$label,
    if (show_other && dplyr::n_distinct(df$Class) > n_top) "other"
  )

  df |>
    dplyr::filter(.data$value %in% values) |>
    dplyr::mutate(value = factor(.data$value, levels = values)) |>
    dplyr::left_join(
      top_classes |> dplyr::select("Class", "label"),
      by = "Class"
    ) |>
    dplyr::mutate(
      group = dplyr::coalesce(.data$label, "other") |>
        factor(levels = groups)
    ) |>
    dplyr::filter(!is.na(.data$group)) |>
    # (no segments for running averages)
    sch_summarize_bars(
      section_groups = top_classes$label,
      split = period != "running"
    )
}

# plotting =====

# plot lecture and lab bars per period
# for each period, shows one bar per group with the lecture value (solid,
# outlined by section or class) and the lab value (transparent, outlined by
# class if available) stacked on top. Multiple values (enrollment, SCH, TA
# equivalents) are shown in separate panels with a shared x-axis. Groups that
# have a TA are labeled with a #.
# @param plot_df data frame from sch_summarize_bars()
# @param colors named vector of fill colors for the groups
# @param subtitle plot subtitle
# @param show_group_legend whether to show the legend of the group colors
#   (e.g. not needed for the total)
# @param show_class_numbers whether to show the class numbers (if available,
#   see sch_summarize_bars()) above the bars (rotated, together with the TA
#   label)
# @param n_breaks approximate number of y-axis breaks
# @param bar_width share of each period's width taken up by its bars
# @param base_size base font size
# @param theme ggplot2 theme function
# @return ggplot
sch_plot_bars <- function(
  plot_df,
  colors,
  subtitle = NULL,
  show_group_legend = TRUE,
  show_class_numbers = FALSE,
  n_breaks = 5,
  bar_width = 0.85,
  base_size = 12,
  theme = ggplot2::theme_bw
) {
  if (nrow(plot_df) == 0) {
    abort("no data for the selected classes and semesters")
  }
  groups <- levels(plot_df$group)
  value_labels <- stats::setNames(sch_plot_values$label, sch_plot_values$value)
  bar_cols <- c("value", "period", "group")

  # x position of each bar (dodged by group within each period)
  width <- bar_width / length(groups)
  plot_df <- plot_df |>
    dplyr::mutate(
      x = as.integer(.data$period) +
        (as.integer(.data$group) - (length(groups) + 1) / 2) * width,
      xmin = .data$x - width / 2,
      xmax = .data$x + width / 2,
      # labels above the bars (class number and/or TA)
      bar_label = dplyr::case_when(
        .data$lecture + .data$lab <= 0 ~ NA_character_,
        show_class_numbers & !is.na(.data$class_number) & .data$has_ta ~
          paste0(.data$class_number, "#"),
        show_class_numbers & !is.na(.data$class_number) ~ .data$class_number,
        .data$has_ta ~ "#",
        .default = NA_character_
      )
    )
  rotate_labels <- show_class_numbers && any(!is.na(plot_df$class_number))

  # lecture segments stacked from 0, lab segments stacked on top of the lecture
  lecture_segments <- plot_df |>
    dplyr::select(
      dplyr::all_of(bar_cols),
      "x",
      "xmin",
      "xmax",
      "lecture_segments"
    ) |>
    # (no lecture segments if all classes are lab/field classes)
    dplyr::filter(!purrr::map_lgl(.data$lecture_segments, is.null)) |>
    tidyr::unnest("lecture_segments") |>
    dplyr::bind_rows(dplyr::tibble(lecture = double())) |>
    dplyr::mutate(
      ymax = cumsum(.data$lecture),
      ymin = .data$ymax - .data$lecture,
      .by = dplyr::all_of(bar_cols)
    )
  # (no lab segments if all groups are split by section, e.g. without "other")
  lab_segments <- plot_df |>
    dplyr::select(
      dplyr::all_of(bar_cols),
      "x",
      "xmin",
      "xmax",
      total_lecture = "lecture",
      "lab_segments"
    ) |>
    dplyr::filter(!purrr::map_lgl(.data$lab_segments, is.null)) |>
    tidyr::unnest("lab_segments") |>
    dplyr::bind_rows(dplyr::tibble(lab = double())) |>
    dplyr::mutate(
      ymax = .data$total_lecture + cumsum(.data$lab),
      ymin = .data$ymax - .data$lab,
      .by = dplyr::all_of(bar_cols)
    )
  segment_aes <- function(alpha) {
    ggplot2::aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax,
      alpha = alpha
    )
  }

  plot_df |>
    ggplot2::ggplot(ggplot2::aes(x = .data$x, fill = .data$group)) +
    # lab bars without segments
    ggplot2::geom_col(
      data = \(d) dplyr::filter(d, purrr::map_lgl(.data$lab_segments, is.null)),
      ggplot2::aes(y = .data$lecture + .data$lab, alpha = "lab"),
      width = width
    ) +
    ggplot2::geom_rect(
      data = lab_segments,
      mapping = segment_aes("lab"),
      color = "black",
      linewidth = 0.2
    ) +
    ggplot2::geom_rect(
      data = lecture_segments,
      mapping = segment_aes("lecture"),
      color = "black",
      linewidth = 0.2
    ) +
    # labels above the bars (class number and/or TA)
    ggplot2::geom_text(
      data = \(d) dplyr::filter(d, !is.na(.data$bar_label)),
      ggplot2::aes(
        y = .data$lecture + .data$lab,
        label = .data$bar_label,
        # (larger font for just the #)
        size = I(base_size / dplyr::if_else(.data$bar_label == "#", 3.5, 4.8))
      ),
      angle = if (rotate_labels) 90 else 0,
      hjust = if (rotate_labels) -0.1 else 0.5,
      vjust = if (rotate_labels) 0.5 else 0.2,
      color = "gray20",
      show.legend = FALSE
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$value),
      scales = "free_y",
      switch = "y",
      labeller = ggplot2::as_labeller(value_labels)
    ) +
    ggplot2::scale_fill_manual(
      values = colors,
      breaks = groups,
      labels = \(x) stringr::str_wrap(x, 40)
    ) +
    ggplot2::scale_alpha_manual(
      values = c(lecture = 1, lab = 0.3),
      breaks = c("lecture", "lab")
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(levels(plot_df$period)),
      labels = levels(plot_df$period) |> stringr::str_replace(" ", "\n"),
      expand = ggplot2::expansion(add = 0.3)
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = n_breaks),
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0, if (rotate_labels) 0.25 else 0.08))
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      alpha = NULL,
      subtitle = subtitle
    ) +
    theme(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(1.5, "lines"),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        size = base_size * 1.1
      ),
      axis.text = ggplot2::element_text(size = base_size * 0.9),
      legend.position = "right",
      legend.direction = "vertical",
      legend.key.spacing.y = ggplot2::unit(4, "pt")
    ) +
    ggplot2::guides(
      fill = if (show_group_legend) {
        ggplot2::guide_legend(order = 1, ncol = 1)
      } else {
        "none"
      },
      alpha = ggplot2::guide_legend(
        order = 2,
        override.aes = list(fill = "gray30", color = NA)
      )
    )
}

# plot lecture and lab values per period for the top classes
# for each period, shows one bar per class (top classes in order of course
# number + "other") with the lecture value (solid, outlined by section) and
# the lab value (transparent) stacked on top. For "other", the lecture and lab
# values are outlined by class. Top classes that have a TA are labeled with
# a #.
# Multiple values (enrollment, SCH, TA equivalents) are shown in separate
# panels with a shared x-axis, the top classes are the same in all panels.
# Other SCH (DIS, THE, IND, OTH) and its enrollment are not included.
# @inheritParams sch_summarize_top_classes
# @inheritParams sch_plot_bars
# @param show_class_numbers whether to show the course number above each bar
# @return ggplot
sch_plot_top_classes <- function(
  df,
  values = "SCH",
  top_based_on = values[1],
  classes = NULL,
  n_top = 5,
  show_other = TRUE,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_grad = TRUE,
  include_grad_field = FALSE,
  show_class_numbers = TRUE,
  n_breaks = 5,
  bar_width = 0.85,
  base_size = 12
) {
  top_based_on <- sch_match_values(top_based_on)
  plot_df <- df |>
    sch_summarize_top_classes(
      values,
      top_based_on,
      classes,
      n_top,
      show_other,
      period,
      n_years,
      from,
      to,
      include_grad,
      include_grad_field
    )

  groups <- levels(plot_df$group)
  colors <- rep(sch_other_color, length(groups))
  colors[groups != "other"] <- sch_get_class_colors(sum(groups != "other"))
  colors <- stats::setNames(colors, groups)
  top_label <- sch_plot_values$label[sch_plot_values$value == top_based_on]
  subtitle <- if (length(classes) > 0 && length(unique(classes)) <= n_top) {
    # all selected classes are shown
    paste0("selected classes, ", sch_describe_period(period, n_years))
  } else {
    paste0(
      "top ",
      n_top,
      if (length(classes) > 0) " of the selected",
      " classes by ",
      top_label,
      ", ",
      sch_describe_period(period, n_years)
    )
  }

  sch_plot_bars(
    plot_df,
    colors,
    subtitle,
    show_class_numbers = show_class_numbers,
    n_breaks = n_breaks,
    bar_width = bar_width,
    base_size = base_size
  )
}

# plot lecture and lab values per period by class type
# for each period, shows one bar per class type (see sch_add_class_type())
# with the lecture value (solid, outlined by class) and the lab value
# (transparent, outlined by class) stacked on top. Multiple values
# (enrollment, SCH, TA equivalents) are shown in separate panels with a shared
# x-axis. Other SCH (DIS, THE, IND, OTH) and its enrollment are not included.
# @inheritParams sch_summarize_top_classes
# @inheritParams sch_plot_bars
# @return ggplot
sch_plot_class_types <- function(
  df,
  values = "SCH",
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_grad = TRUE,
  include_grad_field = FALSE,
  n_breaks = 5,
  bar_width = 0.85,
  base_size = 12
) {
  plot_df <- df |>
    sch_prepare_plot_data(
      sch_match_values(values),
      classes,
      period,
      n_years,
      from,
      to,
      include_grad,
      include_grad_field
    ) |>
    dplyr::mutate(group = .data$class_type) |>
    # (no segments for running averages)
    sch_summarize_bars(split = period != "running")

  groups <- levels(plot_df$group)
  colors <- sch_class_colors[match(groups, sch_class_types)] |>
    stats::setNames(groups)
  subtitle <- paste0(
    if (length(classes) > 0) "selected classes ",
    "by class type, ",
    sch_describe_period(period, n_years)
  )

  sch_plot_bars(
    plot_df,
    colors,
    subtitle,
    n_breaks = n_breaks,
    bar_width = bar_width,
    base_size = base_size
  )
}

# plot total lecture and lab values per period
# for each period, shows one bar with the total lecture value (solid,
# outlined by class) and lab value (transparent, outlined by class) stacked
# on top. Multiple values (enrollment, SCH, TA equivalents) are shown in
# separate panels with a shared x-axis. Other SCH (DIS, THE, IND, OTH) and its
# enrollment are not included.
# @inheritParams sch_summarize_top_classes
# @inheritParams sch_plot_bars
# @return ggplot
sch_plot_total <- function(
  df,
  values = "SCH",
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_grad = TRUE,
  include_grad_field = FALSE,
  n_breaks = 5,
  bar_width = 0.6,
  base_size = 12
) {
  group <- if (length(classes) == 0) "all classes" else "selected classes"
  plot_df <- df |>
    sch_prepare_plot_data(
      sch_match_values(values),
      classes,
      period,
      n_years,
      from,
      to,
      include_grad,
      include_grad_field
    ) |>
    dplyr::mutate(group = factor(group)) |>
    # (no segments for running averages)
    sch_summarize_bars(split = period != "running")

  sch_plot_bars(
    plot_df,
    stats::setNames(sch_class_colors[1], group),
    subtitle = paste0(group, ", ", sch_describe_period(period, n_years)),
    show_group_legend = FALSE,
    n_breaks = n_breaks,
    bar_width = bar_width,
    base_size = base_size
  )
}

# data export =====

# round the enrollment and SCH (to integers) and TA equivalents (to 2 digits)
# @param df data frame
# @return `df` with rounded enrollment, SCH and TA columns
sch_round_values <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric) &
          dplyr::matches("enrl|sch", ignore.case = TRUE),
        round
      ),
      dplyr::across(
        tidyselect::where(is.numeric) & dplyr::matches("(^|_)ta$"),
        \(x) round(x, 2)
      )
    )
}

# format the courses for export and display (modern course codes and titles)
# @param courses data from sch_prepare_data()
# @param class_labels class labels from sch_label_classes()
# @return data frame with one row per course and semester
sch_format_courses <- function(courses, class_labels) {
  courses |>
    dplyr::left_join(
      class_labels |> dplyr::select("Class", "code", "title"),
      by = "Class"
    ) |>
    dplyr::arrange(.data$YearTerm, .data$Class) |>
    dplyr::transmute(
      term = .data$TERM,
      AY = sprintf("AY%02d", .data$FYC %% 100),
      course = .data$code,
      title = .data$title,
      cross_listed = sch_modernize_codes(.data$Courses),
      activity = .data$ACTVTYP,
      level = .data$GRAD_UG,
      class_type = as.character(.data$class_type),
      has_ta = .data$has_ta,
      enrl = .data$Enrl,
      lecture_enrl = .data$lecture_enrl,
      lab_enrl = .data$lab_enrl,
      sch = .data$SP_SCH,
      lecture_sch = .data$lecture_sch,
      lab_sch = .data$lab_sch,
      other_sch = .data$other_sch,
      ta = .data$ta,
      lecture_ta = .data$lecture_ta,
      lab_ta = .data$lab_ta
    ) |>
    sch_round_values()
}

# format the sections of the courses for export and display (modern course
# codes and titles)
# @inheritParams sch_format_courses
# @return data frame with one row per section and semester
sch_format_sections <- function(courses, class_labels) {
  courses |>
    dplyr::select("TERM", "YearTerm", "FYC", "Class", "section_info") |>
    dplyr::filter(!purrr::map_lgl(.data$section_info, is.null)) |>
    tidyr::unnest("section_info") |>
    dplyr::left_join(
      class_labels |> dplyr::select("Class", "code", "title"),
      by = "Class"
    ) |>
    dplyr::arrange(.data$YearTerm, .data$section) |>
    dplyr::transmute(
      term = .data$TERM,
      AY = sprintf("AY%02d", .data$FYC %% 100),
      course = .data$code,
      title = .data$title,
      section = sch_modernize_codes(.data$section),
      section_number = .data$section_number,
      activity = .data$activity,
      primary = .data$primary,
      enrl = .data$enrl,
      max_enrl = .data$max_enrl,
      sch = .data$sch,
      lecture_sch = dplyr::coalesce(.data$lecture_sch, 0),
      lab_sch = dplyr::coalesce(.data$lab_sch, 0),
      instructors = .data$instructors,
      tas = .data$tas,
      has_ta = .data$has_ta
    ) |>
    sch_round_values()
}

# the data underlying a plot
# all course codes are the modern (ERTH) codes and the titles the most recent
# ones, enrollment and SCH are rounded to integers
# @param plot_type "classes" (sch_plot_top_classes()), "class_types"
#   (sch_plot_class_types()) or "total" (sch_plot_total())
# @inheritParams sch_prepare_data
# @return named list of data frames: summary (the values per period and
#   class, class type or total from sch_summarize_classes(),
#   sch_summarize_class_types() or sch_summarize_total(), for all classes),
#   courses (the courses per semester incl. their main instructor, see
#   sch_format_courses()) and sections (all sections of the courses incl.
#   their instructors and TAs, see sch_format_sections())
sch_get_export_data <- function(
  df,
  plot_type = c("classes", "class_types", "total"),
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_grad = TRUE,
  include_grad_field = FALSE
) {
  plot_type <- arg_match(plot_type)
  summarize_func <- switch(
    plot_type,
    classes = sch_summarize_classes,
    class_types = sch_summarize_class_types,
    total = sch_summarize_total
  )
  summary <- summarize_func(
    df,
    classes = classes,
    period = period,
    n_years = n_years,
    from = from,
    to = to,
    include_summer = FALSE,
    include_grad = include_grad,
    include_grad_field = include_grad_field
  ) |>
    dplyr::mutate(period = as.character(.data$period)) |>
    sch_round_values()
  if (plot_type == "classes") {
    summary <- summary |>
      dplyr::select(
        "period",
        course = "courses",
        dplyr::everything(),
        -"Class",
        -"code"
      )
  }
  if (plot_type == "class_types") {
    summary <- summary |>
      dplyr::mutate(class_type = as.character(.data$class_type))
  }
  class_labels <- sch_label_classes(df)
  courses <- df |>
    sch_prepare_data(
      classes,
      period = "semester",
      from = from,
      to = to,
      include_summer = FALSE,
      include_grad = include_grad,
      include_grad_field = include_grad_field
    )
  list(
    summary = summary,
    courses = sch_format_courses(courses, class_labels),
    sections = sch_format_sections(courses, class_labels)
  )
}

# plot details (on click) =====

# find the bar (and segment) at a position of a plot
# @param p ggplot from sch_plot_bars()
# @param x,y position (in data coordinates)
# @param value the value of the panel (e.g. "SCH"), NULL for any
# @return list with value, period, group, segment (the section or class,
#   NULL if no segment was clicked or the bar has no segments) and values
#   (the bar's lecture and lab values for all shown values), NULL if there is
#   no bar at the position
sch_find_bar <- function(p, x, y, value = NULL) {
  bars <- p$data |>
    dplyr::filter(.data$xmin <= .env$x, .data$xmax >= .env$x)
  if (!is.null(value)) {
    bars <- bars |> dplyr::filter(as.character(.data$value) == .env$value)
  }
  if (nrow(bars) == 0) {
    return(NULL)
  }
  bar <- bars[1, ]
  in_bar <- function(d) {
    d |>
      dplyr::filter(
        .data$value == bar$value,
        .data$period == bar$period,
        .data$group == bar$group
      )
  }
  # segments (rectangle layers with segment data)
  segment <- p$layers |>
    purrr::map(\(l) l$data) |>
    purrr::keep(\(d) is.data.frame(d) && all(c("segment", "ymin") %in% names(d))) |>
    purrr::map(\(d) {
      in_bar(d) |>
        dplyr::filter(.data$ymin <= .env$y, .data$ymax >= .env$y) |>
        dplyr::select("segment")
    }) |>
    purrr::list_rbind()
  list(
    value = as.character(bar$value),
    period = as.character(bar$period),
    group = as.character(bar$group),
    segment = if (nrow(segment) > 0 && segment$segment[1] != "all") {
      segment$segment[1]
    },
    values = p$data |>
      dplyr::filter(.data$period == bar$period, .data$group == bar$group) |>
      dplyr::select("value", "lecture", "lab")
  )
}

# details of a bar (or segment) of a plot
# @param bar the bar from sch_find_bar()
# @param plot_type "classes", "class_types" or "total"
# @param groups the groups of the plot (for "classes", the labels of the top
#   classes + "other")
# @inheritParams sch_prepare_data
# @return list with title, subtitle, section (the selected section, NULL if
#   none), values (the lecture and lab values of the selection for the values
#   of the bar, for running averages the bar's values), courses (the courses of the bar or segment per semester, see
#   sch_format_courses()), sections (their sections, see
#   sch_format_sections()) and classes (the total values of each class over
#   the semesters of the period)
sch_get_bar_details <- function(
  df,
  bar,
  plot_type = c("classes", "class_types", "total"),
  groups = character(),
  classes = NULL,
  period = "semester",
  n_years = 3,
  from = "Fall 2018",
  to = NULL,
  include_grad = TRUE,
  include_grad_field = FALSE
) {
  plot_type <- arg_match(plot_type)
  class_labels <- sch_label_classes(df)
  prepare <- function(period) {
    sch_prepare_data(
      df,
      classes,
      period,
      n_years,
      from,
      to,
      include_summer = FALSE,
      include_grad,
      include_grad_field
    )
  }

  # semesters of the period
  terms <- prepare(period) |>
    dplyr::filter(as.character(.data$period) == bar$period) |>
    dplyr::arrange(.data$YearTerm) |>
    dplyr::pull(.data$TERM) |>
    unique()
  courses <- prepare("semester") |>
    dplyr::filter(.data$TERM %in% terms)

  # classes of the bar
  courses <- switch(
    plot_type,
    classes = if (bar$group == "other") {
      top <- class_labels$Class[class_labels$label %in% groups]
      courses |> dplyr::filter(!.data$Class %in% top)
    } else {
      class <- class_labels$Class[class_labels$label == bar$group]
      courses |> dplyr::filter(.data$Class %in% class)
    },
    class_types = courses |>
      dplyr::filter(as.character(.data$class_type) == bar$group),
    total = courses
  )
  title <- if (plot_type == "classes" && bar$group == "other") {
    "other classes"
  } else if (plot_type == "total") {
    "all classes"
  } else {
    bar$group
  }

  # segment: a class or a section
  section <- NULL
  if (!is.null(bar$segment)) {
    if (bar$segment %in% courses$Class) {
      courses <- courses |> dplyr::filter(.data$Class == bar$segment)
      title <- class_labels$label[class_labels$Class == bar$segment]
    } else {
      section <- bar$segment
      courses <- courses |>
        dplyr::filter(purrr::map_lgl(
          .data$section_info,
          \(x) !is.null(x) && section %in% x$section
        ))
      section <- sch_modernize_codes(section)
    }
  }

  # lecture and lab values of the selection (the bar's values for running
  # averages since they are averages, not totals of the semesters)
  values <- if (period == "running") {
    bar$values
  } else {
    sch_plot_values |>
      dplyr::filter(.data$value %in% as.character(bar$values$value)) |>
      dplyr::mutate(
        value = factor(.data$value, levels = levels(bar$values$value)),
        lecture = purrr::map_dbl(.data$lecture_col, \(col) sum(courses[[col]])),
        lab = purrr::map_dbl(.data$lab_col, \(col) sum(courses[[col]]))
      ) |>
      dplyr::arrange(.data$value) |>
      dplyr::select("value", "lecture", "lab")
  }

  formatted <- sch_format_courses(courses, class_labels)
  list(
    title = title,
    subtitle = paste0(
      bar$period,
      if (length(terms) > 1) {
        sprintf(" (%s to %s)", terms[1], utils::tail(terms, 1))
      }
    ),
    section = section,
    values = values,
    courses = formatted,
    sections = sch_format_sections(courses, class_labels),
    classes = formatted |>
      dplyr::summarize(
        title = .data$title[1],
        semesters = dplyr::n_distinct(.data$term),
        enrl = sum(.data$enrl),
        sch = sum(.data$sch),
        ta = sum(.data$ta),
        has_ta = any(.data$has_ta),
        .by = "course"
      ) |>
      dplyr::arrange(.data$course)
  )
}
