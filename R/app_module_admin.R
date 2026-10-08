# admin module server (admin access level only)
# @param data the data module (see module_data_server())
# @param get_access_level reactive returning the access level (see access_levels())
module_admin_server <- function(id, data, get_access_level) {
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # reactive values
    values <- reactiveValues(
      export = NULL,
      export_file_name = NULL
    )

    # check schedule =====

    # read uploaded course export
    observeEvent(input$schedule_file, {
      req(input$schedule_file)
      tryCatch(
        {
          if (get_access_level() != "admin") {
            abort("schedule checks are only available in admin mode")
          }
          log_info(
            "reading course export file '",
            input$schedule_file$name,
            "'"
          )
          export <- read_course_export(input$schedule_file$datapath)
          values$export <- export
          values$export_file_name <- input$schedule_file$name
          log_success(
            "read ",
            nrow(export$sections),
            " sections for ",
            export$term,
            user_msg = sprintf(
              "Read %d sections for %s from '%s'.",
              nrow(export$sections),
              export$term,
              input$schedule_file$name
            )
          )
        },
        error = function(e) {
          values$export <- NULL
          values$export_file_name <- NULL
          log_error(
            ns = ns,
            "failed to read course export",
            user_msg = "Could not read the uploaded course export file",
            error = e
          )
        }
      )
    })

    # differences between the course export and the app data (updates when
    # the app data is reloaded)
    get_differences <- reactive({
      validate(need(
        values$export,
        "Upload a course export file (.csv) under 'Check schedule' in the sidebar to compare it to the schedule."
      ))
      req(
        data$schedule$get_data(),
        data$instructors$get_data(),
        data$classes$get_data()
      )
      tryCatch(
        compare_schedule_to_export(
          export_sections = values$export$sections,
          schedule = data$schedule$get_data(),
          instructors = data$instructors$get_data(),
          term = values$export$term,
          classes = data$classes$get_data()
        ),
        error = function(e) {
          log_error(
            ns = ns,
            "failed to compare course export",
            user_msg = "Could not compare the course export to the schedule",
            error = e
          )
          NULL
        }
      )
    })

    output$check_summary <- renderText({
      if (is.null(values$export)) {
        return("No course export uploaded")
      }
      req(get_differences())
      n_diffs <- sum(get_differences()$is_difference)
      sprintf(
        "%s: %d difference%s vs '%s'",
        values$export$term,
        n_diffs,
        if (n_diffs == 1L) "" else "s",
        values$export_file_name
      )
    })

    # differences formatted for the table: one column per information with
    # a single value if the app and upload agree, and both values (App: ...,
    # Upload: ...) highlighted in red if they differ
    get_differences_for_table <- reactive({
      diffs <- get_differences()
      req(diffs)
      # sections with differences
      diffs$class_status <- ifelse(diffs$is_difference, "different", "same")
      if (!identical(input$only_differences, FALSE)) {
        diffs <- dplyr::filter(diffs, .data$is_difference)
      }
      missing_html <- "<i class='text-danger'>missing</i>"
      value <- function(x) {
        ifelse(
          is.na(x) | !nzchar(stringr::str_trim(x)),
          missing_html,
          htmltools::htmlEscape(dplyr::coalesce(x, ""))
        )
      }
      # title (html) with the topic of special topics classes in a new line
      # (missing if there is no topic), just missing if the section does not
      # exist on that side
      title_with_topic <- function(title, topic, exists) {
        ifelse(
          diffs$is_topic_class & exists,
          paste0(value(title), ":<br>", value(topic)),
          value(title)
        )
      }
      in_both <- diffs$in_app & diffs$in_export
      # compare html values
      compare_html <- function(app, upload, differ) {
        differ <- differ | !in_both
        ifelse(
          differ,
          sprintf(
            "<span class='text-danger'>App: %s<br>Upload: %s</span>",
            app,
            upload
          ),
          app
        )
      }
      compare <- function(app, upload, differ) {
        compare_html(value(app), value(upload), differ)
      }
      diffs |>
        dplyr::mutate(
          row = dplyr::row_number(),
          # class with the app and upload classes if they differ (e.g.
          # ERTH4700/5700 in the app but only ERTH4700 in the upload)
          Class = ifelse(
            .data$diff_class,
            sprintf(
              "%s<br><small class='text-danger'>App: %s<br>Upload: %s</small>",
              .data$class,
              value(.data$app_class),
              value(.data$export_class)
            ),
            .data$class
          ),
          Title = compare_html(
            title_with_topic(.data$app_title, .data$app_topic, .data$in_app),
            title_with_topic(
              .data$export_title,
              .data$export_topic,
              .data$in_export
            ),
            .data$diff_title | .data$diff_topic
          ),
          Section = ifelse(
            .data$diff_section,
            compare(.data$app_section, .data$export_section, TRUE),
            value(.data$section)
          ),
          Type = value(.data$component),
          Instructor = compare(
            .data$app_instructor,
            .data$export_instructor,
            .data$diff_instructor
          ),
          Room = compare(.data$app_room, .data$export_room, .data$diff_room),
          Meetings = compare(
            .data$app_meetings,
            .data$export_meetings,
            .data$diff_meetings
          ),
          Status = compare(
            .data$app_status,
            .data$export_status,
            .data$diff_status
          )
        ) |>
        dplyr::select(
          "row",
          "Class",
          "Title",
          "Section",
          "Type",
          "Instructor",
          "Room",
          "Meetings",
          "Status",
          # for the Class column color (hidden)
          "class_status"
        )
    })

    module_selector_table_server(
      "check_table",
      get_data = get_differences_for_table,
      id_column = "row",
      available_columns = list(dplyr::across(-"row")),
      render_html = dplyr::everything(),
      allow_view_all = TRUE,
      initial_page_length = -1,
      dom = "ft",
      scrollX = TRUE,
      selection = "none",
      no_data_message = "No differences found.",
      columnDefs = list(list(visible = FALSE, targets = 8)),
      formatting_calls = list(
        list(
          func = DT::formatStyle,
          columns = "Class",
          valueColumns = "class_status",
          backgroundColor = DT::styleEqual(
            levels = c("different", "same"),
            values = c("lightpink", "lightgreen")
          )
        )
      )
    )
  })
}

# admin sidebar (menu)
module_admin_sidebar <- function(id) {
  ns <- NS(id)
  bslib::accordion(
    id = ns("menu"),
    bslib::accordion_panel(
      title = "Check schedule",
      value = "check_schedule",
      icon = icon("list-check"),
      fileInput(
        ns("schedule_file"),
        "Course export (.csv)",
        accept = ".csv"
      ),
      checkboxInput(
        ns("only_differences"),
        "Only show differences",
        value = TRUE
      )
    )
  )
}

# admin main UI
module_admin_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      h2(textOutput(ns("check_summary"), inline = TRUE))
    ),
    module_selector_table_ui(ns("check_table")),
    bslib::card_footer(
      "Class sections with differences are marked in red, sections without in green. ",
      "Special topics classes are listed with their topic below the title, which is the subtitle in the app and the Notes #1 in the upload. ",
      "Information that differs between the app and the upload is shown in red. ",
      "Combined sections in the upload (see its Combined Sections column) are compared as a single cross-listed class (e.g. ERTH4021/5021). ",
      "Special topics classes are matched by instructor and their section numbers are compared. ",
      "Use the search bar in the upper right to filter the table."
    )
  )
}
