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
      first_term = NULL,
      last_term = NULL,
      instructor_id = NULL,
      instructor = NULL,
      edit = list()
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
      combine_schedule(
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
        ) |>
        prepare_schedule_table_columns()
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
          selected = c(
            "Undergraduate Classes",
            "Graduate Classes",
            "Day/Time",
            "Location",
            "Enrollment"
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

    # check whether an action is possible for the selected cell, returns NULL
    # if it is, otherwise the reason why not
    # @param action one of add_class, edit_class, delete_class, add_leave, delete_leave
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
        } else if (is_confirmed) {
          return(sprintf(
            "Cannot %s a class already confirmed by the UPA.",
            verb
          ))
        } else if (!is_own) {
          return(sprintf("You can only %s your own classes.", verb))
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

    # add class dialog =========
    add_class_dialog_inputs <- reactive({
      log_debug(ns = ns, "generating class dialog inputs")

      # instructor selectize
      instructor_input <-
        selectizeInput(
          ns("class_instructor_id"),
          "Instructor",
          multiple = FALSE,
          choices = c("Select instructor" = "", get_active_ERTH_instructors()),
          selected = if (!is.null(values$instructor_id)) {
            values$instructor_id
          } else if (!is.null(values$edit$instructor_id)) {
            values$edit$instructor_id
          } else {
            1L
          }
        )

      if (!is.null(values$instructor_id) || get_access_level() != "admin") {
        instructor_input <- instructor_input |> shinyjs::disabled()
      }

      # first block
      tagList(
        h5(
          "Please add your planned classes. You do NOT need to add recitations or labs linked to your classes as those will be carried over by the UPA."
        ),
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
              selected = if (!is.null(values$edit$term)) {
                values$edit$term
              } else {
                1L
              }
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
              )
            ),
            selectizeInput(
              ns("class_id"),
              "Class",
              multiple = FALSE,
              choices = c("Select class" = "", levels(get_classes()$class)),
              selected = if (!is.null(values$edit$class)) {
                values$edit$class
              } else {
                1L
              }
            ),
            textInput(
              ns("subtitle"),
              "Special Topics Title",
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
              placeholder = "No specific number"
            ),
            textInput(
              ns("max_students"),
              "Do you want to limit enrollment?",
              placeholder = "Use classroom limit"
            )
          ),
          div(
            selectizeInput(
              ns("room_id"),
              "Do you have a preferred classroom?",
              multiple = FALSE,
              choices = c("No preference" = "", get_rooms())
            ),
            selectizeInput(
              ns("timeslot"),
              "Do you have a preferred timeslot?",
              multiple = FALSE,
              choices = c("No preference" = "", get_teaching_times())
            )
          )
        ),
        textAreaInput(
          ns("notes"),
          "Notes",
          width = "100%",
          placeholder = "Enter any notes for the UPA"
        )
      )
    })
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
      # modal dialog
      dlg <- modalDialog(
        size = "l",
        title = "Schedule Class",
        add_class_dialog_inputs(),
        footer = tagList(
          actionButton(ns("save_class"), "Add", class = "btn-primary"),
          modalButton("Cancel")
        )
      )
      showModal(dlg)
      shinyjs::toggleState("save_class", condition = toggle_save_class_add())
    })

    toggle_save_class_add <- reactive({
      return(
        nchar(input$class_instructor_id) > 0 &&
          nchar(input$class_term) > 0 &&
          nchar(input$class_id) > 0 &&
          (!stringr::str_detect(input$class_id, "new|4700|5700") ||
            nchar(input$subtitle) > 0)
      )
    })

    observeEvent(toggle_save_class_add(), {
      req(isolate(input$add_class))
      shinyjs::toggleState("save_class", condition = toggle_save_class_add())
    })

    # save class =====
    observeEvent(input$save_class, {
      req(can_edit())
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
        "notes"
      ) |>
        purrr::walk(shinyjs::disable)

      # try to save
      tryCatch(
        {
          # info
          log_info(
            "adding class to schedule",
            user_msg = "Adding class to schedule..."
          )

          # values
          data_values <- list(
            term = input$class_term,
            instructor_id = if (!is.null(values$instructor_id)) {
              values$instructor_id
            } else {
              input$class_instructor_id
            },
            class = input$class_id,
            created = get_datetime(),
            confirmed = FALSE,
            notes = input$notes
          )

          # optional details
          if (!is.null(input$class_instructor_id2)) {
            data_values$instructor_id <- c(
              data_values$instructor_id,
              input$class_instructor_id2
            ) |>
              paste(collapse = ", ")
          }

          if (
            stringr::str_detect(input$class_id, "new|4700|5700") &&
              nchar(input$subtitle) > 0
          ) {
            data_values$subtitle <- input$subtitle
          }

          if (nchar(input$section) > 0) {
            data_values$section <- input$section
          }

          if (nchar(input$max_students) > 0) {
            data_values$enrollment_cap <- stringr::str_extract(
              input$max_students,
              "\\d+"
            ) |>
              as.integer()
          }

          if (nchar(input$room_id) > 0) {
            data_values$building <- stringr::str_extract(
              input$room_id,
              "^[^ ]+"
            )
            data_values$room <- stringr::str_extract(input$room_id, "(?<= ).+")
          }

          if (nchar(input$timeslot) > 0) {
            data_values$days <- stringr::str_extract(input$timeslot, "^[^:]+")
            data_values$start_time <- stringr::str_extract(
              input$timeslot,
              "(?<=: )[^-]+"
            )
            data_values$end_time <- stringr::str_extract(
              input$timeslot,
              "(?<=-).+"
            )
          }

          # update data
          data$schedule$update(.list = data_values)

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
    observeEvent(input$edit_class, {
      req(can_edit())
      req(check_edit_or_warn("edit_class"))
      showModal(
        modalDialog(
          title = "Edit scheduled class",
          h5(
            "Sorry, this functionality is not yet implemented. If you really need to change this class, please delete the existing record and add it anew."
          ),
          footer = tagList(modalButton("Cancel"))
        )
      )
    })

    # delete class ====
    observeEvent(input$delete_class, {
      req(can_edit())
      req(check_edit_or_warn("delete_class"))
      showModal(
        modalDialog(
          title = "Delete class",
          h5(
            sprintf(
              "Are you sure you want to remove all sections of %s from the teaching schedule of %s for %s?",
              values$edit$class,
              values$edit$instructor,
              values$edit$term
            )
          ),
          footer = tagList(
            actionButton(
              ns("delete_class_confirm"),
              "Delete",
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

    # download table =====
    output$download_table <- downloadHandler(
      filename = "earthschedule.csv",
      content = function(filename) {
        log_debug(ns = ns, "downloading data table to csv")
        get_schedule_for_table() |>
          readr::write_csv(file = filename)
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
          shinyjs::hidden() |>
          add_tooltip("Editing a class is not yet implemented."),
        # delete class
        actionButton(
          ns("delete_class"),
          "Delete Class",
          icon = icon("xmark"),
          class = btn_class
        ) |>
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
          add_tooltip("Delete a teaching absence.")
      ),
      module_selector_table_ui(ns("schedule"))
    ),
    bslib::card_footer(
      "Use the search bar in the upper right to filter the schedule (e.g. by course number or course name). ",
      "Use the scrollbar to scroll through all results."
    )
  )
}
