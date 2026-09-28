# the spreadsheet tabs (sheets) and their column definitions the app reads
# (see read_excel_sheet() for the column format), additional columns in the
# spreadsheet are ignored
app_data_sheets <- function() {
  list(
    classes = c(
      "class",
      "title",
      "credits" = "integer",
      "inactive" = "logical",
      "type"
    ),
    instructors = c(
      "instructor_id",
      "last_name",
      "first_name",
      "department",
      "position",
      "inactive" = "logical"
    ),
    not_teaching = c(
      "term",
      "instructor_id",
      "reason",
      "created" = "datetime",
      "deleted" = "datetime"
    ),
    schedule = c(
      "term",
      "class",
      "section",
      "subtitle",
      "instructor_id",
      "preenrollment" = "integer",
      "enrollment" = "integer",
      "enrollment_cap" = "integer",
      "building",
      "room",
      "days",
      "start_time",
      "end_time",
      "created" = "datetime",
      "updated" = "datetime",
      "deleted" = "datetime",
      "canceled" = "logical",
      "confirmed" = "logical",
      "notes"
    )
  )
}

# app server
app_server <- function(data_sheet_id, gs_key_file, dev_cache_file = NULL) {
  function(input, output, session) {
    log_info("\n\n========================================================")
    log_info(
      "starting earthschedule GUI",
      if (shiny::in_devmode()) " in DEV mode"
    )

    # data module
    data <- module_data_server(
      "data",
      data_sheet_id = data_sheet_id,
      gs_key_file = gs_key_file,
      sheets = app_data_sheets(),
      dev_cache_file = dev_cache_file
    )

    # schedule module
    module_schedule_server("schedule", data = data)

    # dev mode
    observeEvent(input$dev_mode_toggle, {
      shiny::devmode(!shiny::in_devmode())
    })
  }
}
