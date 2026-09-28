#' ERTH teaching schedule app
#'
#' Creates the shiny app for viewing and planning the teaching schedule of the
#' Department of Earth Sciences. The schedule data lives in a Google
#' spreadsheet (with tabs `classes`, `instructors`, `not_teaching`, and
#' `schedule`) that the app reads and writes with a Google service account.
#'
#' When the app runs in shiny's developer mode ([shiny::devmode()]), the
#' downloaded spreadsheet is cached in `dev_cache_file` so it is not downloaded
#' again on every app reload (the Reload button clears the cache).
#'
#' @param data_sheet_id id of the Google spreadsheet with the schedule data
#' @param gs_key_file path to the json key file of the Google service account
#'   that has access to the spreadsheet
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
#'     gs_key_file = "gs_key_file.json"
#'   )
#' }
#' @export
earthschedule_app <- function(
  data_sheet_id,
  gs_key_file,
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

  shinyApp(
    ui = app_ui(paths_app_url = paths_app_url),
    server = app_server(
      data_sheet_id = data_sheet_id,
      gs_key_file = gs_key_file,
      dev_cache_file = dev_cache_file
    ),
    options = options
  )
}
