# schedule module server
# @param data the data module (see module_data_server())
# @param get_access_level reactive returning the access level (see access_levels())
module_schedule_server <- function(id, data, get_access_level) {
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # constants
    data_err_prefix <- "Encountered database issue, the app may not function properly: "

    # reactive values
    values <- reactiveValues(
      show_options = c(
        "Undergraduate Classes",
        "Graduate Classes",
        "Day/Time",
        "Location",
        "Enrollment"
      ),
      first_term = NULL,
      last_term = NULL,
      instructor_id = NULL,
      instructor = NULL,
      edit = list(),
      class_edit_idx = NULL,
      class_dialog_warn = FALSE
    )

    # data functions ===========

    # available terms
    get_terms <- reactive({
      validate(need(
        data$schedule$get_data(),
        "something went wrong retrieving the data"
      ))
      get_available_terms(first_term = data$schedule$get_data()$term[1])
    })

    # monitor terms
    observeEvent(input$first_term, {
      if (
        is.null(values$first_term) ||
          !identical(values$first_term, input$first_term)
      ) {
        values$first_term <- input$first_term
      }
    })
    observeEvent(input$last_term, {
      if (
        is.null(values$last_term) ||
          !identical(values$last_term, input$last_term)
      ) {
        values$last_term <- input$last_term
      }
    })

    # monitor information to display (NULL if nothing is selected)
    observeEvent(
      input$show_options,
      {
        values$show_options <- input$show_options
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # monitor instructor
    observeEvent(input$instructor_id, {
      if (
        is.null(values$instructor_id) ||
          !identical(values$instructor_id, input$instructor_id)
      ) {
        log_debug(
          ns = ns,
          "new instructor_id selected: '",
          input$instructor_id,
          "'"
        )
        if (
          !is.na(input$instructor_id) &&
            input$instructor_id != "NA" &&
            nchar(input$instructor_id) > 0
        ) {
          values$instructor_id <- input$instructor_id
          values$instructor <- dplyr::filter(
            get_instructors(),
            .data$instructor_id == values$instructor_id
          )[1, ]
        } else {
          values$instructor_id <- NULL
          values$instructor <- NULL
        }
      }
    })

    # access levels ===========

    # whether the schedule can be modified: always in admin mode, only once an
    # instructor is selected in faculty mode, never in student mode
    can_edit <- reactive({
      switch(
        get_access_level(),
        admin = TRUE,
        faculty = !is.null(values$instructor_id),
        FALSE
      )
    })

    # whether a specific instructor's records can be modified
    can_edit_instructor <- function(instructor_id) {
      can_edit() &&
        (get_access_level() == "admin" ||
          identical(values$instructor_id, instructor_id))
    }

    # the show entire cohort option is only available for faculty with a
    # selected instructor
    show_cohort_option <- reactive({
      get_access_level() == "faculty" && !is.null(values$instructor_id)
    })
    observe({
      shinyjs::toggle("cohort_select", condition = show_cohort_option())
    })

    # show the edit buttons
    observe({
      c(
        "add_leave",
        "delete_leave",
        "add_class",
        "edit_class",
        "delete_class"
      ) |>
        purrr::walk(~ shinyjs::toggle(.x, condition = can_edit()))
    })

    # the instructor selection is only available in faculty mode and the
    # actions sidebar is not available in student mode
    observeEvent(get_access_level(), {
      is_faculty <- get_access_level() == "faculty"
      is_student <- get_access_level() == "student"
      shinyjs::toggle("instructor_select", condition = is_faculty)
      if (!is_faculty && !is.null(values$instructor_id)) {
        values$instructor_id <- NULL
        values$instructor <- NULL
        updateSelectizeInput(session, "instructor_id", selected = "NA")
      }
      shinyjs::toggle("toggle_lock", condition = get_access_level() == "admin")
      bslib::toggle_sidebar("actions_sidebar", open = !is_student)
      shinyjs::toggleClass(
        "schedule_box",
        class = "no-actions",
        condition = is_student
      )
    })

    # selected terms
    get_selected_terms <- reactive({
      req(get_terms())
      req(values$first_term)
      req(values$last_term)
      terms <- get_terms() |> filter_terms(values$first_term, values$last_term)

      # include summers?
      if (!"Summers" %in% input$show_options) {
        terms <- terms |> drop_summers()
      }
      return(terms)
    })

    # future terms
    get_future_terms <- reactive({
      req(get_terms())
      # start in term after current
      get_terms() |>
        filter_terms(start_term = get_current_term(), inclusive = FALSE)
    })

    # classes
    get_classes <- reactive({
      req(data$classes$get_data())
      prepare_classes(data$classes$get_data())
    })

    # instructors
    get_instructors <- reactive({
      req(data$instructors$get_data())
      prepare_instructors(data$instructors$get_data())
    })

    # active ERTH instructors for dropdown
    get_active_ERTH_instructors <- reactive({
      req(get_instructors())
      get_instructors() |>
        dplyr::filter(!.data$inactive, .data$department == "ERTH") |>
        dplyr::arrange(.data$full_name) |>
        dplyr::select("full_name", "instructor_id") |>
        tibble::deframe()
    })

    # not teaching
    get_not_teaching <- reactive({
      req(data$not_teaching$get_data())
      req(get_instructors())
      not_teaching <- prepare_not_teaching(data$not_teaching$get_data())
      missing <- not_teaching |>
        dplyr::anti_join(get_instructors(), by = "instructor_id")
      if (nrow(missing) > 0) {
        msg <- sprintf(
          "missing instructor_id in 'not_teaching': %s",
          paste(unique(missing$instructor_id), collapse = ", ")
        )
        log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
      }
      wrong <- not_teaching |>
        dplyr::filter(!stringr::str_detect(.data$term, get_term_regexp()))
      if (nrow(wrong) > 0) {
        msg <- sprintf(
          "incorrect term formatting in 'not_teaching': %s",
          paste(unique(wrong$term), collapse = ", ")
        )
        log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
      }
      not_teaching
    })

    # reasons
    get_reasons <- reactive({
      req(get_not_teaching())
      get_not_teaching()$reason |> unique() |> stats::na.omit()
    })

    # rooms
    get_rooms <- reactive({
      req(data$schedule$get_data())
      rooms <- data$schedule$get_data() |> prepare_rooms()
      # make sure "other" is the first option
      other_option <- "Other (see notes)"
      rooms <- rooms[rooms != other_option]
      c(other_option, rooms)
    })

    # teaching times
    get_teaching_times <- reactive({
      req(data$schedule$get_data())
      ttimes <- data$schedule$get_data() |> prepare_teaching_times()
      # make sure "other" is the first option
      other_option <- "Other (see notes)"
      ttimes <- ttimes[ttimes != other_option]
      c(other_option, ttimes)
    })

    # schedule
    get_schedule <- reactive({
      req(data$schedule$get_data())
      req(get_terms())
      req(get_instructors())
      req(get_classes())
      req(get_not_teaching())

      # safety checks
      schedule <- prepare_schedule(data$schedule$get_data())

      # filter out canceled classes
      if (!"Canceled" %in% input$show_options) {
        schedule <- schedule |> dplyr::filter(!.data$canceled)
      }

      missing <- schedule |>
        dplyr::anti_join(get_instructors(), by = "instructor_id")
      if (nrow(missing) > 0) {
        msg <- sprintf(
          "unrecognized `instructor_id` in `schedule`: '%s'",
          paste(unique(missing$instructor_id), collapse = "', '")
        )
        log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
      }

      missing <- schedule |> dplyr::anti_join(get_classes(), by = "class")
      if (nrow(missing) > 0) {
        msg <- sprintf(
          "unrecognized `class` in `classes`: '%s'",
          paste(unique(missing$class), collapse = "', '")
        )
        log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
      }

      wrong <- schedule |>
        dplyr::filter(!stringr::str_detect(.data$term, get_term_regexp()))
      if (nrow(wrong) > 0) {
        msg <- sprintf(
          "incorrect term formatting in `schedule`: %s",
          paste(unique(wrong$term), collapse = ", ")
        )
        log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
      }

      # return schedule
      return(schedule)
    })

    # schedule for data table
    get_schedule_for_table <- reactive({
      req(get_schedule())
      req(get_not_teaching())
      req(get_instructors())
      req(get_classes())
      req(get_selected_terms())

      # always reset visible columns to load new selection
      schedule_table$reset_visible_columns()

      # combine schedule information
      schedule <- combine_schedule(
        schedule = get_schedule(),
        not_teaching = get_not_teaching(),
        instructors = get_instructors(),
        classes = get_classes(),
        selected_terms = get_selected_terms(),
        recognized_reasons = get_reasons(),
        include_section_nr = "Section #" %in% input$show_options,
        include_day_time = "Day/Time" %in% input$show_options,
        include_location = "Location" %in% input$show_options,
        include_enrollment = "Enrollment" %in% input$show_options,
        instructor_schedule = values$instructor_id
      ) |>
        filter_schedule_by_class_level(
          include_undergrad = "Undergraduate Classes" %in% input$show_options,
          include_grad = "Graduate Classes" %in% input$show_options
        )

      # only the selected instructor's records (unless showing the entire cohort)
      if (!is.null(values$instructor_id) && !isTRUE(input$show_cohort)) {
        schedule <- schedule |>
          dplyr::filter(.data$instructor_id == !!values$instructor_id)
      }

      schedule |>
        prepare_schedule_table_columns(
          group_by = if (identical(input$group_by, "instructor")) {
            "instructor"
          } else {
            "class"
          }
        )
    })

    # generate UI =====================

    # sidebar GUI
    output$sidebar <- renderUI({
      req(get_terms())
      req(get_active_ERTH_instructors())
      log_info("generating sidebar")
      terms <- get_terms() |> drop_summers()
      tagList(
        selectizeInput(
          ns("first_term"),
          "Select first term to display:",
          multiple = FALSE,
          choices = get_sorted_terms(terms),
          selected = isolate({
            if (!is.null(values$first_term) && values$first_term %in% terms) {
              values$first_term
            } else {
              get_current_term(include_summer = FALSE)
            }
          })
        ),
        selectizeInput(
          ns("last_term"),
          "Select last term to display:",
          multiple = FALSE,
          choices = get_sorted_terms(terms),
          selected = isolate({
            if (!is.null(values$last_term) && values$last_term %in% terms) {
              values$last_term
            } else {
              get_past_or_future_term(
                get_current_term(include_summer = FALSE),
                years_shift = +2
              )
            }
          })
        ),
        radioButtons(
          ns("group_by"),
          "Group information:",
          choices = c("By class" = "class", "By instructor" = "instructor"),
          selected = isolate(
            if (is.null(input$group_by)) "class" else input$group_by
          )
        ),
        checkboxGroupInput(
          ns("show_options"),
          "Select information to display:",
          choices = c(
            "Undergraduate Classes",
            "Graduate Classes",
            "Summers",
            "Canceled",
            "Section #",
            "Day/Time",
            "Location",
            "Enrollment"
          ),
          # keep the selection when the sidebar is regenerated (e.g. on reload)
          selected = isolate(values$show_options)
        ),
        # show the entire cohort (only for faculty with a selected instructor)
        div(
          id = ns("cohort_select"),
          # pull up to be part of the list of information to display
          style = paste(
            "margin-top: -0.75rem;",
            if (!isolate(show_cohort_option())) "display: none;"
          ),
          checkboxInput(
            ns("show_cohort"),
            "Show entire cohort",
            value = isolate(
              if (is.null(input$show_cohort)) TRUE else input$show_cohort
            )
          ) |>
            add_tooltip(
              "Show everyone who teaches the selected instructor's classes. If unchecked, only the selected instructor's classes and absences are shown."
            )
        )
      )
    })

    # instructor selection GUI (faculty only)
    output$instructor_select_ui <- renderUI({
      req(get_active_ERTH_instructors())
      selectizeInput(
        ns("instructor_id"),
        "Select your name to change your schedule:",
        multiple = FALSE,
        width = "100%",
        choices = c(
          list("Show all" = NA_character_),
          get_active_ERTH_instructors()
        ),
        selected = isolate({
          if (
            !is.null(values$instructor_id) &&
              values$instructor_id %in%
                as.character(get_active_ERTH_instructors())
          ) {
            values$instructor_id
          } else {
            NA_character_
          }
        })
      )
    })

    output$instructor_name <- renderText({
      if (!is.null(values$instructor_id)) {
        paste("for", values$instructor$full_name)
      } else {
        ""
      }
    })

    # check for selected terms
    observeEvent(
      get_selected_terms(),
      {
        has_terms <- length(get_selected_terms()) > 0
        if (!has_terms) {
          log_warning(
            "invalid range",
            user_msg = "No terms fall into the selected terms range."
          )
        } else {
          log_info(
            "generating schedule table",
            user_msg = sprintf(
              "Loading schedule from %s to %s (%d terms)",
              get_selected_terms()[1],
              utils::tail(get_selected_terms(), 1),
              length(get_selected_terms())
            )
          )
        }
        shinyjs::toggle("schedule_box", condition = has_terms)
      },
      ignoreNULL = FALSE,
      priority = 100
    )

    # generate table ======

    schedule_table <- module_selector_table_server(
      "schedule",
      get_data = get_schedule_for_table,
      id_column = "row",
      # row grouping
      render_html = dplyr::everything(),
      extensions = "RowGroup",
      rowGroup = list(dataSrc = 3),
      columnDefs = list(
        list(visible = FALSE, targets = 0:3)
      ),
      # view all & scrolling
      allow_view_all = TRUE,
      initial_page_length = -1,
      dom = "ft",
      ordering = FALSE,
      scrollX = TRUE,
      selection = list(mode = "single", target = "cell")
    )

    # formatting the schedule for easy visibility
    observeEvent(
      get_reasons(),
      {
        log_debug(ns = ns, "update formatting with reasons")
        schedule_table$change_formatting_calls(
          list(
            list(
              func = DT::formatStyle,
              columns_expr = expr(dplyr::matches(get_term_regexp())),
              backgroundColor = DT::styleEqual(
                levels = c("?", "no", "canceled", get_reasons()),
                values = c(
                  "lightgray",
                  "lightpink",
                  "lightpink",
                  rep("lightyellow", length(get_reasons()))
                ),
                default = "lightgreen"
              )
            )
          )
        )
      },
      priority = 99
    )

    # formatting the headers based on which terms are selected
    observeEvent(
      get_selected_terms(),
      {
        log_debug(ns = ns, "updating header backgrounds")
        future_idx <- is_term_after(
          get_selected_terms(),
          after = get_current_term()
        ) |>
          which()
        header_calls <- sprintf(
          "$(thead).closest('thead').find('th').eq(%s).css('background-color', 'yellow');",
          future_idx
        )
        schedule_table$update_options(
          headerCallback = DT::JS(
            sprintf(
              "function( thead, data, start, end, display ) { %s }",
              paste(header_calls, collapse = " ")
            )
          )
        )
      },
      priority = 98
    )

    # process record selection ================

    # store the information of the selected cell (whether it can be modified
    # is checked when an action button is pressed, see check_edit())
    observeEvent(schedule_table$get_selected_cells(), {
      if (
        !rlang::is_empty(schedule_table$get_selected_ids()) &&
          check_terms(as.character(schedule_table$get_selected_cells()))
      ) {
        log_debug(ns = ns, "new cell selected")
        selected_items <- schedule_table$get_selected_items()
        selected_term <- as.character(schedule_table$get_selected_cells())
        values$edit <- list(
          instructor_id = selected_items$instructor_id,
          instructor = selected_items$Instructor,
          class = as.character(selected_items$class),
          term = selected_term,
          info = selected_items[[selected_term]]
        )
      } else {
        log_debug(ns = ns, "nothing selected")
        values$edit <- list()
      }
    })

    # the class buttons only make sense once a (non-empty) cell is selected
    observe({
      has_selection <- length(values$edit) > 0 &&
        !identical(values$edit$info, "no")
      c("edit_class", "delete_class", "toggle_lock") |>
        purrr::walk(~ shinyjs::toggleState(.x, condition = has_selection))
    })

    # check whether an action is possible for the selected cell, returns NULL
    # if it is, otherwise the reason why not
    # @param action one of add_class, edit_class, delete_class, toggle_lock, add_leave, delete_leave
    check_edit <- function(action) {
      edit <- values$edit
      has_cell <- length(edit) > 0
      is_future <- has_cell && is_term_after(edit$term)
      is_own <- has_cell && can_edit_instructor(edit$instructor_id)
      is_leave <- has_cell && edit$info %in% get_reasons()
      is_class <- has_cell &&
        !is_leave &&
        !edit$info %in% c("no", "?", "canceled")
      is_confirmed <- is_class && !stringr::str_detect(edit$info, "^<i>")
      verb <- c(edit_class = "edit", delete_class = "unschedule")[action]

      if (action == "add_class") {
        if (has_cell && !is_future) {
          return("Cannot schedule a course in the past.")
        }
      } else if (action == "add_leave") {
        if (has_cell && !is_future) {
          return("Cannot add a teaching absence in the past.")
        }
      } else if (action %in% c("edit_class", "delete_class")) {
        if (!is_class) {
          return(sprintf(
            "Please first select the class you want to %s in the table.",
            verb
          ))
        } else if (!is_future) {
          return(sprintf("Cannot %s a class that's in the past.", verb))
        } else if (
          is_confirmed &&
            !(action == "edit_class" && get_access_level() == "admin")
        ) {
          return(sprintf(
            "Cannot %s a class already confirmed by the UPA.",
            verb
          ))
        } else if (!is_own) {
          return(sprintf("You can only %s your own classes.", verb))
        }
      } else if (action == "toggle_lock") {
        if (!is_class || nrow(get_selected_class_records()) == 0L) {
          return(
            "Please first select the class you want to lock or unlock in the table."
          )
        }
      } else if (action == "delete_leave") {
        if (!is_leave) {
          return(
            "Please first select the teaching absence you want to delete in the table."
          )
        } else if (!is_future) {
          return("Cannot delete a teaching absence that's in the past.")
        } else if (!is_own) {
          return("You can only delete your own teaching absences.")
        }
      }
      return(NULL)
    }

    # check an action, shows a warning dialog and returns FALSE if the action
    # is not possible
    check_edit_or_warn <- function(action) {
      msg <- check_edit(action)
      if (is.null(msg)) {
        return(TRUE)
      }
      log_debug(ns = ns, action, " not possible: ", msg)
      showModal(
        modalDialog(
          title = msg,
          easyClose = TRUE,
          p(
            "Modifications are only possible for future semesters (highlighted in ",
            tags$span(style = "background-color: yellow;", "yellow"),
            ") and classes that have not yet been confirmed by the UPA (shown in ",
            HTML("<i><u>underlined italics</u></i>"),
            "). To modify anything else, please contact our wonderful UPA at ",
            tags$a(
              href = "mailto:earthsciug@colorado.edu",
              target = "_new",
              "earthsciug@colorado.edu"
            )
          ),
          footer = modalButton("OK")
        )
      )
      return(FALSE)
    }

    # add leave dialog =========
    add_leave_dialog_inputs <- reactive({
      log_debug(ns = ns, "generating leave dialog inputs")
      tagList(
        h5("Please indicate which upcoming semester you do not plan to teach."),
        if (!is.null(values$instructor_id)) {
          h4(values$instructor$full_name)
        } else {
          # super user only?
          selectizeInput(
            ns("leave_instructor_id"),
            "Instructor",
            multiple = FALSE,
            choices = c(
              "Select instructor" = "",
              get_active_ERTH_instructors()
            ),
            selected = values$edit$instructor_id
          )
        },
        selectizeInput(
          ns("leave_term"),
          "Term",
          multiple = FALSE,
          choices = c("Select term" = "", get_sorted_terms(get_future_terms())),
          selected = if (!is.null(values$edit$term)) values$edit$term else 1
        ),
        selectizeInput(
          ns("leave_reason"),
          "Type",
          multiple = FALSE,
          choices = c("Enter/select type of absence" = "", get_reasons()),
          options = list(create = TRUE)
        )
      )
    })

    # add leave ==============
    observeEvent(input$add_leave, {
      req(can_edit())
      req(check_edit_or_warn("add_leave"))
      data$not_teaching$start_add()
      # modal dialog
      dlg <- modalDialog(
        size = "s",
        title = "Adding Teaching Absence",
        add_leave_dialog_inputs(),
        footer = tagList(
          actionButton(ns("save_leave"), "Add", class = "btn-primary") |>
            shinyjs::disabled(),
          modalButton("Cancel")
        )
      )
      showModal(dlg)
    })

    observe({
      shinyjs::toggleState(
        "save_leave",
        condition = (!is.null(values$instructor_id) ||
          nchar(input$leave_instructor_id) > 0) &&
          nchar(input$leave_term) > 0 &&
          nchar(input$leave_reason) > 0
      )
    })

    # save leave =====
    observeEvent(input$save_leave, {
      req(can_edit())
      # disable inputs while saving
      c("leave_instructor_id", "leave_term", "leave_reason", "save_leave") |>
        purrr::walk(shinyjs::disable)

      # try to save
      tryCatch(
        {
          # info
          log_info(
            "adding teaching absence",
            user_msg = "Adding teaching absence..."
          )

          # values
          data_values <- list(
            term = input$leave_term,
            instructor_id = if (!is.null(values$instructor_id)) {
              values$instructor_id
            } else {
              input$leave_instructor_id
            },
            reason = input$leave_reason,
            created = get_datetime()
          )

          # update data
          data$not_teaching$update(.list = data_values)

          # commit
          if (data$not_teaching$commit()) removeModal()
        },
        error = function(e) {
          log_error(
            ns = ns,
            "failed",
            user_msg = "Data saving error",
            error = e
          )
        }
      )
    })

    # delete leave ============
    observeEvent(input$delete_leave, {
      req(can_edit())
      req(check_edit_or_warn("delete_leave"))
      showModal(
        modalDialog(
          title = "Delete teaching absence",
          h5(
            sprintf(
              "Are you sure you want to delete the %s related teaching absence for %s in %s?",
              values$edit$info,
              values$edit$instructor,
              values$edit$term
            )
          ),
          footer = tagList(
            actionButton(
              ns("delete_leave_confirm"),
              "Delete",
              class = "btn-danger"
            ),
            modalButton("Cancel")
          )
        )
      )
    })

    observeEvent(input$delete_leave_confirm, {
      req(can_edit())
      req(is.null(check_edit("delete_leave")))
      # pull out record
      record <- get_not_teaching() |>
        dplyr::filter(
          .data$instructor_id == !!values$edit$instructor_id,
          .data$term == !!values$edit$term
        )

      # try to delete
      tryCatch(
        {
          # is there a record?
          if (nrow(record) == 0L) {
            abort("could not find teaching absence to delete")
          }

          # info
          log_info(
            "deleting teaching absence",
            user_msg = "Removing teaching absence..."
          )

          # flag for delete
          data$not_teaching$start_edit(idx = record$idx)
          data$not_teaching$update(deleted = get_datetime())

          # commit
          if (data$not_teaching$commit()) removeModal()
        },
        error = function(e) {
          log_error(
            ns = ns,
            "failed",
            user_msg = "Data saving error",
            error = e
          )
        }
      )
    })

    # class dialog =========

    # prefill values for the class dialog when adding a class (based on the
    # selected cell)
    get_class_add_prefill <- function() {
      list(
        instructor_id = if (!is.null(values$instructor_id)) {
          values$instructor_id
        } else {
          values$edit$instructor_id
        },
        term = values$edit$term,
        class = values$edit$class
      )
    }

    # prefill values for the class dialog when editing a class (based on the
    # schedule record in row idx of the spreadsheet)
    get_class_edit_prefill <- function(idx) {
      record <- data$schedule$get_data()[idx, ]
      instructor_ids <- stringr::str_split(record$instructor_id, ",")[[1]] |>
        stringr::str_remove_all("[ \\r\\n]")
      instructor_ids <- instructor_ids[nchar(instructor_ids) > 0]
      main_instructor <- values$edit$instructor_id
      value_or_null <- function(x) if (is.na(x)) NULL else as.character(x)
      list(
        instructor_id = main_instructor,
        instructor_id2 = setdiff(instructor_ids, main_instructor),
        term = record$term,
        class = stringr::str_remove_all(record$class, "[ \\r\\n]"),
        subtitle = value_or_null(record$subtitle),
        section = value_or_null(record$section),
        max_students = value_or_null(record$enrollment_cap),
        room_id = if (!is.na(record$building) && !is.na(record$room)) {
          paste(
            record$building,
            stringr::str_remove(record$room, "\\.\\d+$")
          )
        },
        timeslot = if (
          !is.na(record$days) &&
            !is.na(record$start_time) &&
            !is.na(record$end_time)
        ) {
          sprintf(
            "%s: %s-%s",
            stringr::str_to_upper(record$days),
            record$start_time,
            record$end_time
          )
        },
        notes = value_or_null(record$notes)
      )
    }

    # class dialog inputs
    # @param prefill list of values to prefill (see get_class_add_prefill())
    class_dialog_inputs <- function(prefill = list()) {
      log_debug(ns = ns, "generating class dialog inputs")

      # make sure the prefilled values are available as choices
      with_value <- function(choices, value) {
        c(choices, setdiff(value, choices))
      }

      # instructor selectize
      instructor_input <-
        selectizeInput(
          ns("class_instructor_id"),
          "Instructor",
          multiple = FALSE,
          choices = c(
            "Select instructor" = "",
            get_active_ERTH_instructors()
          ) |>
            with_value(prefill$instructor_id),
          selected = if (!is.null(prefill$instructor_id)) {
            prefill$instructor_id
          } else {
            1L
          }
        )

      if (!is.null(values$instructor_id) || get_access_level() != "admin") {
        instructor_input <- instructor_input |> shinyjs::disabled()
      }

      # first block
      tagList(
        bslib::layout_columns(
          col_widths = c(6, 6),
          div(
            instructor_input,
            selectizeInput(
              ns("class_term"),
              "Term",
              multiple = FALSE,
              choices = c(
                "Select term" = "",
                get_sorted_terms(get_future_terms())
              ),
              selected = if (!is.null(prefill$term)) prefill$term else 1L
            )
          ),
          div(
            selectizeInput(
              ns("class_instructor_id2"),
              "Co-taught with",
              multiple = TRUE,
              choices = c(
                "Class is not co-taught" = "",
                get_active_ERTH_instructors()
              ) |>
                with_value(prefill$instructor_id2),
              selected = prefill$instructor_id2
            ),
            selectizeInput(
              ns("class_id"),
              "Class",
              multiple = FALSE,
              choices = c("Select class" = "", levels(get_classes()$class)),
              selected = if (!is.null(prefill$class)) prefill$class else 1L
            ),
            textInput(
              ns("subtitle"),
              "Special Topics Title",
              value = if (!is.null(prefill$subtitle)) prefill$subtitle else "",
              placeholder = "Enter a title for the special topics class"
            ) |>
              shinyjs::hidden()
          )
        ),
        # optional divider
        div(
          class = "d-flex align-items-center gap-3 my-2",
          tags$hr(class = "flex-grow-1 my-0"),
          h6(class = "mb-0 text-muted", "Preferences (optional)"),
          tags$hr(class = "flex-grow-1 my-0")
        ),
        # optional settings
        bslib::layout_columns(
          col_widths = c(6, 6),
          div(
            textInput(
              ns("section"),
              "Does this section have a specific number?",
              value = if (!is.null(prefill$section)) prefill$section else "",
              placeholder = "No specific number"
            ),
            textInput(
              ns("max_students"),
              "Do you want to limit enrollment?",
              value = if (!is.null(prefill$max_students)) {
                prefill$max_students
              } else {
                ""
              },
              placeholder = "Use classroom limit"
            )
          ),
          div(
            selectizeInput(
              ns("room_id"),
              "Do you have a preferred classroom?",
              multiple = FALSE,
              choices = c("No preference" = "", get_rooms()) |>
                with_value(prefill$room_id),
              selected = prefill$room_id
            ),
            selectizeInput(
              ns("timeslot"),
              "Do you have a preferred timeslot?",
              multiple = FALSE,
              choices = c("No preference" = "", get_teaching_times()) |>
                with_value(prefill$timeslot),
              selected = prefill$timeslot
            )
          )
        ),
        textAreaInput(
          ns("notes"),
          "Notes",
          value = if (!is.null(prefill$notes)) prefill$notes else "",
          width = "100%",
          placeholder = "Enter any notes for the UPA"
        )
      )
    }

    # the values entered in the class dialog (NA for empty fields)
    get_class_dialog_values <- function() {
      is_special_topics <- stringr::str_detect(input$class_id, "new|4700|5700")
      value_or_na <- function(x) if (nchar(x) > 0) x else NA_character_
      list(
        term = input$class_term,
        instructor_id = c(
          if (!is.null(values$instructor_id)) {
            values$instructor_id
          } else {
            input$class_instructor_id
          },
          input$class_instructor_id2
        ) |>
          paste(collapse = ", "),
        class = input$class_id,
        subtitle = if (is_special_topics) {
          value_or_na(input$subtitle)
        } else {
          NA_character_
        },
        section = value_or_na(input$section),
        enrollment_cap = stringr::str_extract(input$max_students, "\\d+") |>
          as.integer(),
        building = stringr::str_extract(input$room_id, "^[^ ]+"),
        room = stringr::str_extract(input$room_id, "(?<= ).+"),
        days = stringr::str_extract(input$timeslot, "^[^:]+"),
        start_time = stringr::str_extract(input$timeslot, "(?<=: )[^-]+"),
        end_time = stringr::str_extract(input$timeslot, "(?<=-).+"),
        notes = value_or_na(input$notes)
      )
    }

    observeEvent(input$class_id, {
      shinyjs::toggle(
        "subtitle",
        condition = stringr::str_detect(input$class_id, "new|4700|5700")
      )
    })

    # add class =========
    observeEvent(input$add_class, {
      req(can_edit())
      req(check_edit_or_warn("add_class"))
      data$schedule$start_add()
      values$class_edit_idx <- NULL
      # modal dialog
      dlg <- modalDialog(
        size = "l",
        title = "Schedule Class",
        h5(
          "Please add your planned classes. You do NOT need to add recitations or labs linked to your classes as those will be carried over by the UPA."
        ),
        class_dialog_inputs(get_class_add_prefill()),
        footer = tagList(
          # shown if required information is missing when saving
          span(
            id = ns("class_dialog_warning"),
            class = "text-danger me-auto",
            style = "display: none;"
          ),
          actionButton(ns("save_class"), "Add", class = "btn-primary"),
          modalButton("Cancel")
        )
      )
      values$class_dialog_warn <- FALSE
      showModal(dlg)
    })

    # required information that is missing in the class dialog
    get_missing_class_info <- reactive({
      is_blank <- function(x) length(x) == 0 || all(nchar(x) == 0)
      c(
        "instructor" = is_blank(input$class_instructor_id),
        "term" = is_blank(input$class_term),
        "class" = is_blank(input$class_id),
        "special topics title" = !is_blank(input$class_id) &&
          stringr::str_detect(input$class_id, "new|4700|5700") &&
          is_blank(input$subtitle)
      ) |>
        which() |>
        names()
    })

    # once the warning is shown, keep it up to date with the missing information
    observe({
      req(values$class_dialog_warn)
      missing <- get_missing_class_info()
      if (length(missing) > 0) {
        shinyjs::html(
          "class_dialog_warning",
          sprintf(
            "Please provide the %s.",
            if (length(missing) > 1) {
              paste(
                paste(utils::head(missing, -1), collapse = ", "),
                "and",
                utils::tail(missing, 1)
              )
            } else {
              missing
            }
          )
        )
        shinyjs::show("class_dialog_warning")
      } else {
        shinyjs::hide("class_dialog_warning")
      }
    })

    # save class =====
    observeEvent(input$save_class, {
      req(can_edit())
      # required information missing?
      if (length(get_missing_class_info()) > 0) {
        values$class_dialog_warn <- TRUE
        return()
      }
      # disable inputs while saving
      c(
        "class_instructor_id",
        "class_term",
        "class_id",
        "save_class",
        "subtitle",
        "class_instructor_id2",
        "section",
        "max_students",
        "room_id",
        "timeslot",
        "notes",
        "edit_record"
      ) |>
        purrr::walk(shinyjs::disable)

      # try to save
      tryCatch(
        {
          class_values <- get_class_dialog_values()
          if (is.null(values$class_edit_idx)) {
            # add (only the fields that have values)
            log_info(
              "adding class to schedule",
              user_msg = "Adding class to schedule..."
            )
            data$schedule$start_add()
            data$schedule$update(
              .list = c(
                class_values[!purrr::map_lgl(class_values, is.na)],
                list(created = get_datetime(), confirmed = FALSE)
              )
            )
          } else {
            # edit (all fields to also clear removed values)
            log_info(
              "updating class in schedule",
              user_msg = "Updating class in schedule..."
            )
            data$schedule$start_edit(idx = values$class_edit_idx)
            data$schedule$update(
              .list = c(class_values, list(updated = get_datetime()))
            )
          }

          # commit
          if (data$schedule$commit()) removeModal()
        },
        error = function(e) {
          log_error(
            ns = ns,
            "failed",
            user_msg = "Data saving error",
            error = e
          )
        }
      )
    })

    # edit class ====

    # show the edit dialog for the schedule record in row idx of the spreadsheet
    show_edit_class_dialog <- function(idx) {
      values$class_edit_idx <- idx
      records <- get_selected_class_records() |>
        dplyr::distinct(.data$idx, .keep_all = TRUE)

      # if there are multiple sections, pick which one to edit
      record_input <- if (nrow(records) > 1L) {
        selectizeInput(
          ns("edit_record"),
          "Section to edit",
          width = "100%",
          choices = stats::setNames(
            records$idx,
            sprintf(
              "#%s: %s %s-%s in %s %s",
              ifelse(is.na(records$section), "?", records$section),
              ifelse(is.na(records$days), "?", records$days),
              ifelse(is.na(records$start_time), "?", records$start_time),
              ifelse(is.na(records$end_time), "?", records$end_time),
              ifelse(is.na(records$building), "?", records$building),
              ifelse(is.na(records$room), "?", records$room)
            )
          ),
          selected = idx
        )
      }

      showModal(
        modalDialog(
          size = "l",
          title = "Edit Class",
          record_input,
          class_dialog_inputs(get_class_edit_prefill(idx)),
          footer = tagList(
            # shown if required information is missing when saving
            span(
              id = ns("class_dialog_warning"),
              class = "text-danger me-auto",
              style = "display: none;"
            ),
            actionButton(ns("save_class"), "Save", class = "btn-primary"),
            modalButton("Cancel")
          )
        )
      )
      values$class_dialog_warn <- FALSE
    }

    observeEvent(input$edit_class, {
      req(can_edit())
      req(check_edit_or_warn("edit_class"))
      show_edit_class_dialog(min(get_selected_class_records()$idx))
    })

    # switch to a different section
    observeEvent(input$edit_record, {
      idx <- as.integer(input$edit_record)
      req(!is.na(idx), !identical(idx, values$class_edit_idx))
      req(idx %in% get_selected_class_records()$idx)
      show_edit_class_dialog(idx)
    })

    # delete class ====
    observeEvent(input$delete_class, {
      req(can_edit())
      req(check_edit_or_warn("delete_class"))
      showModal(
        modalDialog(
          title = "Delete Class",
          h5(
            sprintf(
              "Are you sure you want to delete (unschedule) %s from the teaching schedule of %s for %s?",
              if (length(unique(get_selected_class_records()$idx)) > 1L) {
                sprintf(
                  "all %d sections of %s",
                  length(unique(get_selected_class_records()$idx)),
                  values$edit$class
                )
              } else {
                values$edit$class
              },
              values$edit$instructor,
              values$edit$term
            )
          ),
          footer = tagList(
            actionButton(
              ns("delete_class_confirm"),
              "Delete Class",
              class = "btn-danger"
            ),
            modalButton("Cancel")
          )
        )
      )
    })

    observeEvent(input$delete_class_confirm, {
      req(can_edit())
      req(is.null(check_edit("delete_class")))
      # pull out record
      record <- get_schedule() |>
        dplyr::filter(
          .data$class == !!values$edit$class,
          .data$instructor_id == !!values$edit$instructor_id,
          .data$term == !!values$edit$term
        )

      # try to delete
      tryCatch(
        {
          # is there a record?
          if (nrow(record) == 0L) {
            abort("could not find schedule record to delete")
          }

          # info
          log_info(
            "deleting schedule record(s)",
            user_msg = sprintf(
              "Unscheduling %d section%s...",
              nrow(record),
              if (nrow(record) > 1) "s" else ""
            )
          )

          # update
          data$schedule$start_edit(idx = record$idx)
          data$schedule$update(deleted = get_datetime())

          # commit
          if (data$schedule$commit()) removeModal()
        },
        error = function(e) {
          log_error(
            ns = ns,
            "failed",
            user_msg = "Data saving error",
            error = e
          )
        }
      )
    })

    # lock/unlock class ====

    # schedule records (all sections) of the selected class
    get_selected_class_records <- reactive({
      if (length(values$edit) == 0L) {
        return(get_schedule()[0, ])
      }
      get_schedule() |>
        dplyr::filter(
          .data$class == !!values$edit$class,
          .data$instructor_id == !!values$edit$instructor_id,
          .data$term == !!values$edit$term
        )
    })

    # whether the selected class is locked (confirmed by the UPA)
    is_selected_class_locked <- reactive({
      records <- get_selected_class_records()
      nrow(records) > 0L && all(records$confirmed)
    })

    # show lock or unlock depending on the selected class
    observeEvent(is_selected_class_locked(), {
      if (is_selected_class_locked()) {
        updateActionButton(
          session,
          "toggle_lock",
          label = "Unlock Class",
          icon = icon("lock-open")
        )
      } else {
        updateActionButton(
          session,
          "toggle_lock",
          label = "Lock Class",
          icon = icon("lock")
        )
      }
    })

    observeEvent(input$toggle_lock, {
      req(get_access_level() == "admin")
      req(check_edit_or_warn("toggle_lock"))
      records <- get_selected_class_records()
      lock <- !is_selected_class_locked()

      tryCatch(
        {
          log_info(
            if (lock) "locking" else "unlocking",
            " schedule record(s)",
            user_msg = sprintf(
              "%s %d section%s...",
              if (lock) "Locking" else "Unlocking",
              length(unique(records$idx)),
              if (length(unique(records$idx)) > 1) "s" else ""
            )
          )
          data$schedule$start_edit(idx = unique(records$idx))
          data$schedule$update(confirmed = lock, updated = get_datetime())
          data$schedule$commit()
        },
        error = function(e) {
          log_error(
            ns = ns,
            "failed",
            user_msg = "Data saving error",
            error = e
          )
        }
      )
    })

    # download table =====
    output$download_table <- downloadHandler(
      filename = "earthschedule.csv",
      content = function(filename) {
        log_debug(ns = ns, "downloading data table to csv")
        get_schedule_for_table() |>
          prepare_schedule_table_export() |>
          readr::write_csv(file = filename, na = "")
      }
    )
  })
}

# sidebar UI (generated dynamically since it depends on the data)
module_schedule_sidebar <- function(id) {
  ns <- NS(id)
  uiOutput(ns("sidebar")) |>
    shinycssloaders::withSpinner(proxy.height = "100px")
}

# main UI
# @param access_level the access level the app starts with (see access_levels())
module_schedule_ui <- function(id, access_level = "faculty") {
  ns <- NS(id)
  btn_class <- paste(header_button_class(), "w-100 text-start text-nowrap")
  bslib::card(
    id = ns("schedule_box"),
    class = if (access_level == "student") "no-actions",
    full_screen = TRUE,
    bslib::card_header(
      h2(
        "Schedule",
        textOutput(ns("instructor_name"), inline = TRUE)
      )
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        id = ns("actions_sidebar"),
        position = "right",
        width = 200,
        gap = "0.5rem",
        open = if (access_level == "student") "closed" else "desktop",
        # instructor selection (faculty only)
        div(
          id = ns("instructor_select"),
          style = if (access_level != "faculty") "display: none;",
          uiOutput(ns("instructor_select_ui"))
        ),
        # download table
        downloadButton(
          ns("download_table"),
          "Download Table",
          icon = icon("download"),
          class = btn_class
        ) |>
          add_tooltip("Download table as CSV file"),
        # add class
        actionButton(
          ns("add_class"),
          "Schedule Class",
          icon = icon("person-chalkboard"),
          class = btn_class
        ) |>
          shinyjs::hidden() |>
          add_tooltip("Add a class to the teaching plan."),
        # edit class
        actionButton(
          ns("edit_class"),
          "Edit Class",
          icon = icon("pen-to-square"),
          class = btn_class
        ) |>
          shinyjs::disabled() |>
          shinyjs::hidden() |>
          add_tooltip("Edit a section of this class."),
        # delete class
        actionButton(
          ns("delete_class"),
          "Delete Class",
          icon = icon("xmark"),
          class = btn_class
        ) |>
          shinyjs::disabled() |>
          shinyjs::hidden() |>
          add_tooltip("Delete ALL sections of this class."),
        # add absence
        actionButton(
          ns("add_leave"),
          "Add Absence",
          icon = icon("plane"),
          class = btn_class
        ) |>
          shinyjs::hidden() |>
          add_tooltip(
            "Add information about a teaching absence (sabbatical, family leave, chair, directorship, etc.)."
          ),
        # delete absence
        actionButton(
          ns("delete_leave"),
          "Delete Absence",
          icon = icon("plane-slash"),
          class = btn_class
        ) |>
          shinyjs::hidden() |>
          add_tooltip("Delete a teaching absence."),
        # lock/unlock class (admin only)
        actionButton(
          ns("toggle_lock"),
          "Lock Class",
          icon = icon("lock"),
          class = btn_class
        ) |>
          shinyjs::disabled() |>
          shinyjs::hidden() |>
          add_tooltip(
            "Lock (confirm) or unlock ALL sections of this class. Locked classes can no longer be edited or deleted by faculty."
          )
      ),
      module_selector_table_ui(ns("schedule"))
    ),
    bslib::card_footer(
      "Use the search bar in the upper right to filter the schedule (e.g. by course number or course name). ",
      "Use the scrollbar to scroll through all results."
    )
  )
}
