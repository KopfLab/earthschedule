# schedule check =====
# compares a course export CSV (from the university scheduling software) to the
# schedule in the app data for the same term

# known building names in the course export and their codes in the app data
# (buildings that are not listed here are compared by their full name)
get_export_building_codes <- function() {
  c(
    "Benson Earth Sciences Bldg" = "BESC",
    "Bruce Curtis Bldg" = "MCOL",
    "Cristol Chem & Biochem Bldg" = "CRISTOL"
  )
}

# special topics classes whose topic is the subtitle in the app data and is
# listed in the Notes#1 column of the course export
get_topic_classes <- function() {
  c("ERTH4700", "ERTH5700", "ERTH4725", "ERTH5725")
}

# placeholder instructor ids in the app data (not actual people)
get_placeholder_instructor_ids <- function() {
  c("none", "grad", "postdoc", "other")
}

# read course export -----

# read a course export CSV file: the first line holds the term (e.g. "Spring
# 2027 CU Boulder"), the column header row starts with ",CLSS ID", and each
# course is introduced by a title row (e.g. "ERTH 1010 - Exploring Earth")
# without any other values
# @return list with the term and a tibble with one row per section
read_course_export <- function(file) {
  lines <- readr::read_lines(file, progress = FALSE)
  if (length(lines) == 0) {
    abort("the uploaded file is empty")
  }

  # term
  term <- stringr::str_extract(lines[1], get_term_regexp())
  if (is.na(term)) {
    abort(sprintf(
      "could not find the term (e.g. 'Spring 2027') in the first line of the file: '%s'",
      lines[1]
    ))
  }

  # sections
  header_idx <- which(stringr::str_detect(lines, "^\"?,\"?CLSS ID"))[1]
  if (is.na(header_idx)) {
    abort("could not find the column header row (starting with 'CLSS ID')")
  }
  sections <- readr::read_csv(
    I(paste(lines[header_idx:length(lines)], collapse = "\n")),
    col_types = readr::cols(.default = "c"),
    name_repair = "minimal",
    progress = FALSE
  )
  required <- c(
    "Course",
    "Section #",
    "Component",
    "Meetings",
    "Instructor",
    "Room",
    "Status"
  )
  missing <- setdiff(required, names(sections))
  if (length(missing) > 0) {
    abort(sprintf(
      "the file is missing required columns: %s",
      paste(missing, collapse = ", ")
    ))
  }
  optional_cols <- c("Course Title", "Combined Sections", "Notes#1")
  for (optional in optional_cols) {
    if (!optional %in% names(sections)) sections[[optional]] <- NA_character_
  }
  sections <- sections[c(required, optional_cols)] |>
    # drop the course title rows
    dplyr::filter(!is.na(.data$Course)) |>
    dplyr::transmute(
      class = stringr::str_remove_all(.data$Course, "\\s"),
      title = .data$`Course Title`,
      section = .data$`Section #`,
      component = .data$Component,
      instructor = .data$Instructor,
      room = .data$Room,
      meetings = .data$Meetings,
      canceled = stringr::str_detect(.data$Status, "(?i)cancel"),
      combined = .data$`Combined Sections`,
      notes = .data$`Notes#1`
    )
  list(term = term, sections = sections)
}

# normalization -----

# normalize section numbers (e.g. "001" and "1" are the same)
normalize_section <- function(section) {
  section <- stringr::str_trim(section)
  ifelse(
    stringr::str_detect(section, "^\\d+$"),
    as.character(suppressWarnings(as.integer(section))),
    section
  )
}

# normalize a special topics class topic by removing prefixes such as
# "Topic:", "Seminar Topic:", or "Geological Topics Seminar:", "" if no topic
normalize_topic <- function(topic) {
  topic |>
    stringr::str_remove("^.*?(?i)topics?( seminar)?\\s*:\\s*") |>
    stringr::str_squish() |>
    dplyr::coalesce("")
}

# normalize a person's name for comparison (lower case, letters only)
normalize_name <- function(name) {
  name |>
    iconv(to = "ASCII//TRANSLIT", sub = "") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z]+", " ") |>
    stringr::str_squish()
}

# normalize a time (e.g. "12:20pm", "12:20p", "1p", "11:00a") to the format
# "12:20pm", NA if it is not a recognizable time
normalize_time <- function(time) {
  m <- stringr::str_match(
    stringr::str_to_lower(stringr::str_squish(time)),
    "^(\\d{1,2})(?::(\\d{2}))? ?([ap])m?$"
  )
  hour <- as.integer(m[, 2])
  minute <- ifelse(is.na(m[, 3]), 0L, as.integer(m[, 3]))
  ifelse(
    is.na(m[, 1]) | hour < 1L | hour > 12L | minute > 59L,
    NA_character_,
    sprintf("%d:%02d%sm", hour, minute, m[, 4])
  )
}

# normalize meeting days (e.g. "TTh" and "TTH" are the same), NA if no days
normalize_days <- function(days) {
  days <- stringr::str_to_upper(stringr::str_remove_all(days, "\\s"))
  ifelse(
    is.na(days) | days %in% c("", "-", "TBA", "TBD"),
    NA_character_,
    days
  )
}

# normalized meeting key from days and times, "" if there is no meeting info
get_meeting_key <- function(days, start_time, end_time) {
  days <- normalize_days(days)
  start <- normalize_time(start_time)
  end <- normalize_time(end_time)
  ifelse(
    is.na(days) & is.na(start) & is.na(end),
    "",
    paste(
      dplyr::coalesce(days, "?"),
      paste0(dplyr::coalesce(start, "?"), "-", dplyr::coalesce(end, "?"))
    )
  )
}

# parse the meetings of the export (e.g. "MWF 12:20pm-1:10pm", multiple
# meetings separated by ";") into a normalized meeting key
get_export_meeting_key <- function(meetings) {
  purrr::map_chr(meetings, function(x) {
    if (is.na(x) || stringr::str_detect(x, "(?i)no time assigned")) {
      return("")
    }
    parts <- stringr::str_match(
      stringr::str_squish(stringr::str_split(x, ";")[[1]]),
      "^(\\S+) (\\S+)-(\\S+)$"
    )
    keys <- ifelse(
      is.na(parts[, 1]),
      stringr::str_squish(stringr::str_split(x, ";")[[1]]),
      get_meeting_key(parts[, 2], parts[, 3], parts[, 4])
    )
    paste(sort(keys), collapse = "; ")
  })
}

# normalized room key from the export (e.g. "Benson Earth Sciences Bldg 180"
# becomes "BESC 180"), "" if no room is assigned
get_export_room_key <- function(room) {
  room <- stringr::str_squish(room)
  no_room <- is.na(room) | stringr::str_detect(room, "(?i)^(no room|see )")
  codes <- get_export_building_codes()
  for (building in names(codes)) {
    room <- stringr::str_replace(
      room,
      paste0("^", stringr::fixed(building), " "),
      paste0(codes[[building]], " ")
    )
  }
  ifelse(no_room, "", stringr::str_to_upper(room))
}

# normalized room key from the app data, "" if no room is assigned
get_app_room_key <- function(building, room) {
  building <- stringr::str_squish(building)
  room <- stringr::str_remove(stringr::str_squish(room), "\\.0+$")
  ifelse(
    is.na(building) & is.na(room),
    "",
    stringr::str_to_upper(paste(
      dplyr::coalesce(building, "?"),
      dplyr::coalesce(room, "?")
    ))
  )
}

# parse the instructors of the export (e.g. "Overeem, Irina (100892203)
# [Primary Instructor, Post, Print]; ...") into a tibble with name (as
# displayed), last_name, and whether the instructor is printed in the schedule
# ("Staff" is not an actual instructor and is dropped)
parse_export_instructors <- function(instructor) {
  if (is.na(instructor)) {
    return(dplyr::tibble(name = character(), last_name = character(), print = logical()))
  }
  parts <- stringr::str_squish(stringr::str_split(instructor, ";")[[1]])
  m <- stringr::str_match(parts, "^([^,(\\[]+)(?:, ?([^(\\[]+))?")
  dplyr::tibble(
    last_name = stringr::str_squish(m[, 2]),
    first_name = stringr::str_squish(m[, 3]),
    print = !stringr::str_detect(parts, "(?i)no print")
  ) |>
    dplyr::filter(!is.na(.data$last_name), .data$last_name != "Staff") |>
    dplyr::mutate(
      name = ifelse(
        is.na(.data$first_name),
        .data$last_name,
        paste(.data$first_name, .data$last_name)
      )
    ) |>
    dplyr::select("name", "last_name", "print")
}

# whether two last names belong to the same person (one contains the other as
# whole words, e.g. "Small Tilton" and "Tilton")
is_same_last_name <- function(a, b) {
  a <- normalize_name(a)
  b <- normalize_name(b)
  nchar(a) > 0 &
    nchar(b) > 0 &
    (stringr::str_detect(paste0(" ", a, " "), stringr::fixed(paste0(" ", b, " "))) |
      stringr::str_detect(paste0(" ", b, " "), stringr::fixed(paste0(" ", a, " "))))
}

# whether the instructors of the export match the instructors of the app
# (lists of last names), instructors in the export that are not printed in the
# schedule (e.g. the faculty of record for a TA-led lab) are ignored if the app
# lists no actual instructor (e.g. only a graduate student)
is_same_instructors <- function(export_last_names, export_print, app_last_names) {
  if (length(app_last_names) == 0) {
    export_last_names <- export_last_names[export_print]
  }
  all(purrr::map_lgl(export_last_names, ~ any(is_same_last_name(.x, app_last_names)))) &&
    all(purrr::map_lgl(app_last_names, ~ any(is_same_last_name(.x, export_last_names))))
}

# prepare sections -----

# identify the sections that are combined (cross-listed) in the export (e.g.
# "Also ERTH 5021-001" and "See ERTH 4021-001") by a group id listing all
# combined sections (e.g. "ERTH4021-1 ERTH5021-1"), NA if not combined
get_export_combined_group <- function(class, section, combined) {
  own <- paste0(class, "-", normalize_section(section))
  purrr::map2_chr(own, combined, function(own, combined) {
    if (is.na(combined)) {
      return(NA_character_)
    }
    links <- stringr::str_match_all(combined, "([A-Z]+) ?(\\d+)-(\\w+)")[[1]]
    if (nrow(links) == 0) {
      return(NA_character_)
    }
    others <- paste0(links[, 2], links[, 3], "-", normalize_section(links[, 4]))
    paste(sort(unique(c(own, others))), collapse = " ")
  })
}

# combine class numbers into a cross-listed class name (e.g. ERTH4021 and
# ERTH5021 become ERTH4021/5021)
combine_class_names <- function(classes) {
  classes <- unique(classes)
  subjects <- stringr::str_extract(classes, "^[A-Z]+")
  if (length(classes) > 1 && !anyNA(subjects) && all(subjects == subjects[1])) {
    return(paste0(
      classes[1],
      "/",
      paste(stringr::str_remove(classes[-1], subjects[1]), collapse = "/")
    ))
  }
  paste(classes, collapse = "/")
}

# prepare the export sections for comparison
prepare_export_sections <- function(sections) {
  instructors <- purrr::map(sections$instructor, parse_export_instructors)
  sections |>
    dplyr::mutate(
      section = normalize_section(.data$section),
      combined_group = get_export_combined_group(
        .data$class,
        .data$section,
        .data$combined
      ),
      instructor_display = purrr::map_chr(
        instructors,
        ~ if (nrow(.x) == 0) "Staff" else paste(.x$name, collapse = ", ")
      ),
      instructor_last_names = purrr::map(instructors, "last_name"),
      instructor_print = purrr::map(instructors, "print"),
      room_key = get_export_room_key(.data$room),
      # normalized values (e.g. "BESC 180"), unless there is no room / time
      # (e.g. "See DEPT", "No Time Assigned")
      room_display = ifelse(
        .data$room_key == "",
        dplyr::coalesce(.data$room, ""),
        .data$room_key
      ),
      meeting_key = get_export_meeting_key(.data$meetings),
      meeting_display = ifelse(
        .data$meeting_key == "",
        dplyr::coalesce(.data$meetings, ""),
        .data$meeting_key
      ),
      is_topic_class = .data$class %in% get_topic_classes(),
      topic = ifelse(.data$is_topic_class, normalize_topic(.data$notes), "")
    )
}

# prepare the app schedule for comparison: only the sections of the term that
# are not deleted, cross-listed classes (e.g. ERTH4021/5021) are split into
# their individual class numbers (e.g. ERTH4021 and ERTH5021), and placeholder
# classes (XXXX...) are dropped
prepare_app_sections <- function(schedule, instructors, term) {
  instructors <- prepare_instructors(instructors)
  if (!"subtitle" %in% names(schedule)) {
    schedule$subtitle <- NA_character_
  }
  schedule |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    dplyr::filter(.data$term == !!term, is.na(.data$deleted)) |>
    dplyr::mutate(
      app_class = stringr::str_remove_all(.data$class, "[ \\r\\n]"),
      class = purrr::map(.data$app_class, split_cross_listed_class)
    ) |>
    tidyr::unnest("class") |>
    dplyr::filter(!stringr::str_detect(.data$class, "^XXXX")) |>
    dplyr::mutate(
      section = normalize_section(.data$section),
      canceled = !is.na(.data$canceled) & .data$canceled,
      instructor_ids = purrr::map(
        stringr::str_split(dplyr::coalesce(.data$instructor_id, ""), ","),
        ~ setdiff(stringr::str_remove_all(.x, "\\s"), c("", get_placeholder_instructor_ids()))
      ),
      instructor_last_names = purrr::map(
        .data$instructor_ids,
        ~ dplyr::coalesce(
          instructors$last_name[match(.x, instructors$instructor_id)],
          .x
        )
      ),
      instructor_display = purrr::map2_chr(
        .data$instructor_ids,
        .data$instructor_id,
        function(ids, raw) {
          if (length(ids) == 0) {
            return(
              instructors$full_name[match(
                stringr::str_remove_all(dplyr::coalesce(raw, "none"), "\\s"),
                instructors$instructor_id
              )] |>
                dplyr::coalesce("None Assigned")
            )
          }
          paste(
            dplyr::coalesce(
              instructors$full_name[match(ids, instructors$instructor_id)],
              ids
            ),
            collapse = ", "
          )
        }
      ),
      room_key = get_app_room_key(.data$building, .data$room),
      room_display = .data$room_key,
      meeting_key = get_meeting_key(.data$days, .data$start_time, .data$end_time),
      meeting_display = .data$meeting_key,
      is_topic_class = .data$class %in% get_topic_classes(),
      topic = ifelse(.data$is_topic_class, normalize_topic(.data$subtitle), "")
    )
}

# split a cross-listed class (e.g. ERTH4021/5021 or ERTH4021/GEOG5021) into
# its individual class numbers
split_cross_listed_class <- function(class) {
  parts <- stringr::str_split(class, "/")[[1]]
  subject <- stringr::str_extract(parts[1], "^[A-Z]+")
  ifelse(
    stringr::str_detect(parts, "^\\d") & !is.na(subject),
    paste0(subject, parts),
    parts
  )
}

# compare -----

# match the sections of one class between the export and the app: sections
# with the same section number are matched first, the remaining sections are
# matched greedily by how similar they are (instructors, meetings, room)
# @return tibble with export_row and app_row (NA if a section has no match)
match_class_sections <- function(export, app) {
  n_export <- nrow(export)
  n_app <- nrow(app)
  if (n_export == 0 || n_app == 0) {
    return(dplyr::tibble(
      export_row = c(seq_len(n_export), rep(NA_integer_, n_app)),
      app_row = c(rep(NA_integer_, n_export), seq_len(n_app))
    ))
  }

  # similarity scores
  scores <- outer(seq_len(n_export), seq_len(n_app), Vectorize(function(i, j) {
    same_section <- !is.na(export$section[i]) &&
      !is.na(app$section[j]) &&
      export$section[i] == app$section[j]
    100 * same_section +
      4 * (export$meeting_key[i] == app$meeting_key[j]) +
      2 *
        is_same_instructors(
          export$instructor_last_names[[i]],
          export$instructor_print[[i]],
          app$instructor_last_names[[j]]
        ) +
      1 * (export$room_key[i] == app$room_key[j]) +
      2 * (normalize_name(export$topic[i]) == normalize_name(app$topic[j])) +
      # prefer matching active with active and canceled with canceled
      1 * (export$canceled[i] == app$canceled[j])
  }))

  # greedy matching (highest score first, ties by order)
  matches <- dplyr::tibble(export_row = integer(), app_row = integer())
  while (any(!is.na(scores))) {
    best <- which(scores == max(scores, na.rm = TRUE), arr.ind = TRUE)[1, ]
    matches <- dplyr::bind_rows(
      matches,
      dplyr::tibble(export_row = best[[1]], app_row = best[[2]])
    )
    scores[best[[1]], ] <- NA
    scores[, best[[2]]] <- NA
  }
  dplyr::bind_rows(
    matches,
    dplyr::tibble(export_row = setdiff(seq_len(n_export), matches$export_row)),
    dplyr::tibble(app_row = setdiff(seq_len(n_app), matches$app_row))
  )
}

# find all differences between the course export and the app schedule for a
# term
# @param export_sections the sections from read_course_export()
# @param schedule the schedule data (all terms)
# @param instructors the instructors data
# @param term the term to compare
# @param classes the classes data (for the class titles), NULL to omit titles
# @return tibble with one row per section (class, section, component,
# is_difference, difference, and the export and app values of title,
# topic (special topics classes only, see get_topic_classes()), instructor,
# room, meetings, status, plus the logical columns in_export, in_app,
# diff_title, diff_topic, diff_instructor, diff_room, diff_meetings, diff_status)
# sorted by class and section, sections of cross-listed classes are merged
# (see merge_cross_listed_sections()), note that title differences alone do
# not count as a difference
compare_schedule_to_export <- function(
  export_sections,
  schedule,
  instructors,
  term,
  classes = NULL
) {
  export <- prepare_export_sections(export_sections)
  app <- prepare_app_sections(schedule, instructors, term)
  # class titles (cross-listed classes are listed by their combined name)
  if (!is.null(classes)) {
    classes <- dplyr::mutate(
      classes,
      class = stringr::str_remove_all(.data$class, "[ \\r\\n]")
    )
    app$title <- dplyr::coalesce(
      classes$title[match(app$app_class, classes$class)],
      classes$title[match(app$class, classes$class)]
    )
  } else {
    app$title <- rep(NA_character_, nrow(app))
  }
  class_numbers <- sort(unique(c(export$class, app$class)))
  if (length(class_numbers) == 0) {
    abort(sprintf("there are no classes to compare for %s", term))
  }

  diffs <- purrr::map(class_numbers, function(class) {
    e <- export[export$class == class, ]
    a <- app[app$class == class, ]
    matches <- match_class_sections(e, a)
    ei <- matches$export_row
    ai <- matches$app_row
    in_export <- !is.na(ei)
    in_app <- !is.na(ai)

    # values (NA if the section does not exist on that side)
    get <- function(df, i, col) df[[col]][as.integer(i)]
    out <- dplyr::tibble(
      class = class,
      app_class = get(a, ai, "app_class"),
      section = dplyr::coalesce(get(e, ei, "section"), get(a, ai, "section")),
      component = get(e, ei, "component"),
      export_title = get(e, ei, "title"),
      app_title = get(a, ai, "title"),
      is_topic_class = class %in% get_topic_classes(),
      export_topic = get(e, ei, "topic"),
      app_topic = get(a, ai, "topic"),
      export_instructor = get(e, ei, "instructor_display"),
      app_instructor = get(a, ai, "instructor_display"),
      export_room = get(e, ei, "room_display"),
      app_room = get(a, ai, "room_display"),
      export_meetings = get(e, ei, "meeting_display"),
      app_meetings = get(a, ai, "meeting_display"),
      export_canceled = get(e, ei, "canceled"),
      export_combined_group = get(e, ei, "combined_group"),
      app_canceled = get(a, ai, "canceled"),
      app_idx = get(a, ai, "idx"),
      in_export = in_export,
      in_app = in_app,
      diff_title = in_export &
        in_app &
        normalize_name(dplyr::coalesce(get(e, ei, "title"), "")) !=
          normalize_name(dplyr::coalesce(get(a, ai, "title"), "")),
      diff_instructor = in_export &
        in_app &
        !purrr::map_lgl(seq_along(ei), function(k) {
          if (!in_export[k] || !in_app[k]) return(TRUE)
          is_same_instructors(
            e$instructor_last_names[[ei[k]]],
            e$instructor_print[[ei[k]]],
            a$instructor_last_names[[ai[k]]]
          )
        }),
      diff_room = in_export &
        in_app &
        dplyr::coalesce(get(e, ei, "room_key") != get(a, ai, "room_key"), FALSE),
      diff_meetings = in_export &
        in_app &
        dplyr::coalesce(
          get(e, ei, "meeting_key") != get(a, ai, "meeting_key"),
          FALSE
        ),
      diff_status = in_export &
        in_app &
        dplyr::coalesce(get(e, ei, "canceled") != get(a, ai, "canceled"), FALSE),
      diff_topic = in_export &
        in_app &
        normalize_name(dplyr::coalesce(get(e, ei, "topic"), "")) !=
          normalize_name(dplyr::coalesce(get(a, ai, "topic"), ""))
    )

    # what is different
    out |>
      dplyr::mutate(
        difference = dplyr::case_when(
          !.data$in_app & nrow(a) == 0 ~ "class not in app",
          !.data$in_export & nrow(e) == 0 ~ "class not in upload",
          !.data$in_app ~ "section not in app",
          !.data$in_export ~ "section not in upload",
          TRUE ~
            purrr::pmap_chr(
              list(
                .data$diff_topic,
                .data$diff_instructor,
                .data$diff_room,
                .data$diff_meetings,
                .data$diff_status
              ),
              function(...) {
                fields <- c("topic", "instructor", "room", "meetings", "status")
                paste(fields[c(...)], collapse = ", ")
              }
            )
        )
      ) |>
      # canceled sections that only exist on one side are irrelevant
      dplyr::filter(
        !(!.data$in_app & .data$export_canceled %in% TRUE),
        !(!.data$in_export & .data$app_canceled %in% TRUE)
      )
  })

  dplyr::bind_rows(diffs) |>
    merge_cross_listed_sections() |>
    dplyr::mutate(
      is_difference = .data$difference != "",
      export_status = ifelse(.data$export_canceled, "canceled", "active"),
      app_status = ifelse(.data$app_canceled, "canceled", "active"),
      section_nr = suppressWarnings(as.integer(.data$section))
    ) |>
    dplyr::arrange(.data$class, .data$section_nr, .data$section) |>
    dplyr::select(
      "class",
      "app_class",
      "section",
      "component",
      "is_difference",
      "difference",
      "export_title",
      "app_title",
      "is_topic_class",
      "export_topic",
      "app_topic",
      "export_instructor",
      "app_instructor",
      "export_room",
      "app_room",
      "export_meetings",
      "app_meetings",
      "export_status",
      "app_status",
      "in_export",
      "in_app",
      "diff_title",
      "diff_topic",
      "diff_instructor",
      "diff_room",
      "diff_meetings",
      "diff_status",
      "app_idx"
    )
}

# merge the compared sections of cross-listed classes into a single row: rows
# are merged if they are the same app section with identical upload
# information (e.g. ERTH4021/5021 in the app compared to ERTH4021-001 and
# ERTH5021-001 in the upload), or if they are combined sections in the upload
# that are both not in the app
merge_cross_listed_sections <- function(compared) {
  if (nrow(compared) == 0) {
    return(compared)
  }
  compared |>
    dplyr::mutate(
      merge_key = dplyr::case_when(
        !is.na(.data$app_idx) ~
          paste(
            "app",
            .data$app_idx,
            .data$export_instructor,
            .data$export_room,
            .data$export_meetings,
            .data$export_canceled,
            .data$export_topic
          ),
        !is.na(.data$export_combined_group) ~
          paste("combined", .data$export_combined_group),
        TRUE ~ paste("row", dplyr::row_number())
      )
    ) |>
    dplyr::mutate(
      .by = "merge_key",
      section = if (dplyr::n_distinct(.data$section) == 1L) {
        .data$section
      } else {
        # e.g. 4700-4/5700-3
        paste(
          stringr::str_extract(.data$class, "\\d+"),
          .data$section,
          sep = "-",
          collapse = "/"
        )
      },
      class = combine_class_names(.data$class)
    ) |>
    dplyr::filter(!duplicated(.data$merge_key)) |>
    dplyr::select(-"merge_key", -"export_combined_group")
}
