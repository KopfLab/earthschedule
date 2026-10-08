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
# @param access_level the access level the app starts with (see access_levels())
app_server <- function(
  data_sheet_id,
  gs_key_file,
  access_level = "faculty",
  dev_cache_file = NULL
) {
  function(input, output, session) {
    log_info("\n\n========================================================")
    log_info(
      "starting earthschedule GUI",
      if (shiny::in_devmode()) " in DEV mode",
      " with access level '",
      access_level,
      "'"
    )

    # access level (can only change in dev mode)
    get_access_level <- reactiveVal(access_level)
    observeEvent(input$dev_access_level, {
      req(shiny::in_devmode())
      req(input$dev_access_level %in% access_levels())
      if (!identical(input$dev_access_level, get_access_level())) {
        log_info("switching to access level '", input$dev_access_level, "'")
        get_access_level(input$dev_access_level)
      }
    })
    output$access_level_title <- renderText({
      access_level_title(get_access_level())
    })
    observeEvent(get_access_level(), ignoreInit = TRUE, {
      shinyjs::runjs(sprintf(
        "document.title = document.title.replace(/ \\| \\w+ View$/, '%s');",
        access_level_title(get_access_level())
      ))
    })

    # admin tab (admin only)
    observeEvent(get_access_level(), {
      if (get_access_level() == "admin") {
        bslib::nav_show("main_nav", "admin")
      } else {
        if (identical(input$main_nav, "admin")) {
          bslib::nav_select("main_nav", "schedule")
        }
        bslib::nav_hide("main_nav", "admin")
      }
    })

    # database backup (admin only)
    observeEvent(get_access_level(), {
      shinyjs::toggle(
        "download_backup",
        condition = get_access_level() == "admin"
      )
    })
    output$download_backup <- downloadHandler(
      filename = function() {
        sprintf(
          "earthschedule_backup_%s.xlsx",
          format(Sys.time(), "%Y-%m-%d_%H%M")
        )
      },
      content = function(file) {
        if (get_access_level() != "admin") {
          abort("database backups are only available in admin mode")
        }
        log_info("downloading database backup")
        backup <- download_gs(data_sheet_id, gs_key_file = gs_key_file)
        file.copy(backup, file, overwrite = TRUE)
      }
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
    module_schedule_server(
      "schedule",
      data = data,
      get_access_level = get_access_level
    )

    # admin module
    module_admin_server(
      "admin",
      data = data,
      get_access_level = get_access_level
    )

    # dev mode
    observeEvent(input$dev_mode_toggle, {
      shiny::devmode(!shiny::in_devmode())
    })
  }
}
