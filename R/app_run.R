#' ERTH teaching schedule app
#'
#' Creates the shiny app for viewing and planning the teaching schedule of the
#' Department of Earth Sciences. The schedule data lives in a Google
#' spreadsheet (with tabs `classes`, `instructors`, `not_teaching`, and
#' `schedule`) that the app reads and writes with a Google service account.
#'
#' When the app runs in shiny's developer mode ([shiny::devmode()]), the
#' downloaded spreadsheet is cached in `dev_cache_file` so it is not downloaded
#' again on every app reload (the Reload button clears the cache), and a
#' dropdown in the sidebar allows switching between the access levels.
#'
#' @param data_sheet_id id of the Google spreadsheet with the schedule data
#' @param gs_key_file path to the json key file of the Google service account
#'   that has access to the spreadsheet
#' @param access_level which view of the app to provide: `"faculty"` can
#'   select an instructor and then schedule classes and absences for that
#'   instructor, `"admin"` can schedule classes and absences for any
#'   instructor, and `"student"` can only view (and download) the schedule
#' @param start_tab which tab of the navbar to show when the app starts, the
#'   `"admin"` tab is only available for the `"admin"` access level
#' @param enrollment_data_file path to the enrollment & SCH data export (xlsx
#'   file with a `Detail` tab) for the Stats tab, set to `NULL` if not
#'   available. The file is read once (when the Stats tab is first opened) and
#'   shared by all sessions of the app.
#' @param class_files_folder_id id of the Google Drive folder with the class
#'   files, set to `NULL` if not available. The folder must have an
#'   `advertisement` and a `syllabus` subfolder (created if missing) and the
#'   service account needs edit access to it. Each file in the subfolders
#'   starts with the course number of its class (e.g. `ERTH3181_syllabus.pdf`
#'   or `ERTH4185_5185_syllabus.pdf` for the cross-listed ERTH4185/5185). The
#'   schedule table shows icons next to each class to download these files,
#'   and in faculty and admin mode to also upload (pdf, docx, pptx) and delete
#'   them.
#' @param dev_cache_file local xlsx file to cache the spreadsheet in developer
#'   mode, set to `NULL` to never cache
#' @param paths_app_url url of the ERTH paths app to link to in the navbar,
#'   set to `NULL` to omit the link
#' @inheritParams shiny::shinyApp
#' @return a [shiny::shinyApp()] object, print it or pass it to
#'   [shiny::runApp()] to launch it (return it at the end of an `app.R` file to
#'   deploy it)
#' @examples
#' if (interactive()) {
#'   earthschedule_app(
#'     data_sheet_id = "google spreadsheet id",
#'     gs_key_file = "gs_key_file.json",
#'     enrollment_data_file = "enrollment_data.xlsx",
#'     access_level = "faculty"
#'   )
#' }
#' @export
earthschedule_app <- function(
  data_sheet_id,
  gs_key_file,
  access_level = c("faculty", "admin", "student"),
  start_tab = c("schedule", "rooms", "stats", "admin"),
  enrollment_data_file = NULL,
  class_files_folder_id = NULL,
  dev_cache_file = "local_data_schedule.xlsx",
  paths_app_url = "https://apps.kopflab.org/earthpaths",
  options = list()
) {
  # safety checks
  if (missing(data_sheet_id) || !is_scalar_character(data_sheet_id)) {
    abort("`data_sheet_id` must be the id of the google spreadsheet")
  }
  if (
    missing(gs_key_file) ||
      !is_scalar_character(gs_key_file) ||
      !file.exists(gs_key_file)
  ) {
    abort(
      "`gs_key_file` must be the path to an existing service account key file"
    )
  }
  if (
    !is.null(enrollment_data_file) &&
      (!is_scalar_character(enrollment_data_file) ||
        !file.exists(enrollment_data_file))
  ) {
    abort("`enrollment_data_file` must be the path to an existing file")
  }
  if (!is.null(class_files_folder_id) && !is_scalar_character(class_files_folder_id)) {
    abort("`class_files_folder_id` must be the id of the google drive folder")
  }
  access_level <- arg_match(access_level)
  start_tab <- arg_match(start_tab)
  if (start_tab == "admin" && access_level != "admin") {
    abort("`start_tab = \"admin\"` requires `access_level = \"admin\"`")
  }

  shinyApp(
    ui = app_ui(
      access_level = access_level,
      start_tab = start_tab,
      paths_app_url = paths_app_url
    ),
    server = app_server(
      data_sheet_id = data_sheet_id,
      gs_key_file = gs_key_file,
      access_level = access_level,
      dev_cache_file = dev_cache_file,
      get_courses = if (!is.null(enrollment_data_file)) {
        cache_enrollment_data(enrollment_data_file)
      },
      class_files_folder_id = class_files_folder_id
    ),
    options = options
  )
}

# read the enrollment data only once (on first use) for all app sessions
# the processed course summary is cached in an .rds file next to the data file
# (if it can be written) that is used instead of re-reading the data file as
# long as neither the data file nor the processing functions changed
# @param file path to the enrollment & SCH data export
# @return function returning the course summary (see sch_read_courses())
cache_enrollment_data <- function(file) {
  courses <- NULL
  function() {
    if (is.null(courses)) {
      courses <<- read_enrollment_data_cached(file)
    }
    courses
  }
}

# read the enrollment data from the .rds cache (if up to date) or the data file
# @param file path to the enrollment & SCH data export
# @return the course summary (see sch_read_courses())
read_enrollment_data_cached <- function(file) {
  cache_file <- paste0(tools::file_path_sans_ext(file), ".rds")
  key <- get_enrollment_cache_key(file)

  # cached
  if (file.exists(cache_file)) {
    cache <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (identical(cache$key, key)) {
      log_info("reading enrollment data from cache '", basename(cache_file), "'")
      return(cache$courses)
    }
  }

  # read and cache
  log_info("reading enrollment data from '", basename(file), "'")
  courses <- sch_read_courses(file)
  tryCatch(
    {
      tmp_file <- tempfile(tmpdir = dirname(cache_file), fileext = ".rds")
      saveRDS(list(key = key, courses = courses), tmp_file)
      if (!file.rename(tmp_file, cache_file)) {
        unlink(tmp_file)
        abort("could not write the cache file")
      }
      log_info("cached enrollment data in '", basename(cache_file), "'")
    },
    error = function(e) {
      log_warning("could not cache the enrollment data: ", conditionMessage(e))
    }
  )
  courses
}

# cache key of the enrollment data: hash of the data file contents and of the
# processing functions (all sch_ objects of the package)
# @param file path to the enrollment & SCH data export
get_enrollment_cache_key <- function(file) {
  ns <- asNamespace("earthschedule")
  objects <- sort(ls(ns, pattern = "^sch_"))
  processing <- lapply(objects, \(x) {
    obj <- get(x, envir = ns)
    if (is.function(obj)) deparse(body(obj)) else obj
  })
  list(
    file = rlang::hash_file(file),
    processing = rlang::hash(processing)
  )
}
