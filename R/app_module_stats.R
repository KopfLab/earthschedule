# stats module server: enrollment, student credit hour (SCH) and TA equivalent
# plots by class, class type or total (see logic_enrollment.R)
# @param get_courses function returning the course summary (see
#   sch_read_courses()), NULL if no enrollment data is available
# @param is_visible reactive returning whether the stats tab is visible (the
#   enrollment data is only read once the tab is first shown)
module_stats_server <- function(
  id,
  get_courses = NULL,
  is_visible = reactive(TRUE)
) {
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # whether the tab was shown (stays TRUE once it was)
    was_shown <- reactiveVal(FALSE)
    observeEvent(is_visible(), {
      if (isTRUE(is_visible())) was_shown(TRUE)
    })

    # course summary (only read once the stats tab is first shown)
    get_data <- reactive({
      req(was_shown())
      validate(need(
        !is.null(get_courses),
        "No enrollment data file was provided to the app."
      ))
      tryCatch(
        get_courses(),
        error = function(e) {
          log_error(
            ns = ns,
            "failed to read the enrollment data",
            user_msg = "Could not read the enrollment data",
            error = e
          )
          NULL
        }
      )
    })

    # classes and semesters available in the data
    observeEvent(get_data(), {
      courses <- get_data()
      classes <- sch_label_classes(courses)
      updateSelectizeInput(
        session,
        "classes",
        choices = stats::setNames(classes$code, classes$label),
        server = TRUE
      )
      semesters <- sch_get_semesters(courses)
      default_from <- if ("Fall 2018" %in% semesters) "Fall 2018" else semesters[1]
      updateSelectInput(
        session,
        "from",
        choices = semesters,
        selected = default_from
      )
      updateSelectInput(
        session,
        "to",
        choices = rev(semesters),
        selected = utils::tail(semesters, 1)
      )
    })

    # the top classes can only be based on one of the shown values (by
    # default the first one)
    observeEvent(input$values, ignoreNULL = FALSE, {
      values <- input$values
      if (length(values) == 0) {
        values <- sch_plot_values$value
      }
      selected <- if (isTRUE(input$top_based_on %in% values)) {
        input$top_based_on
      } else {
        values[1]
      }
      updateRadioButtons(
        session,
        "top_based_on",
        choices = stats_value_choices()[stats_value_choices() %in% values],
        selected = selected
      )
    })

    # plot parameters (debounced so the plot does not re-render on every
    # click while changing several inputs)
    get_params <- reactive({
      validate(
        need(length(input$values) > 0, "Select at least one value to plot."),
        need(input$from, "Loading the enrollment data...")
      )
      to <- if (isTRUE(nzchar(input$to))) input$to else NULL
      validate(need(
        is.null(to) ||
          sch_term_to_yearterm(input$from) <= sch_term_to_yearterm(to),
        "The 'from' semester must be before the 'to' semester."
      ))
      list(
        plot_type = input$plot_type,
        values = input$values,
        top_based_on = if (isTRUE(input$top_based_on %in% input$values)) {
          input$top_based_on
        } else {
          input$values[1]
        },
        n_top = input$n_top,
        show_other = input$show_other,
        show_class_numbers = input$show_class_numbers,
        # specific classes can only be selected for the plots by class
        classes = if (identical(input$plot_type, "classes")) input$classes,
        period = input$period,
        n_years = input$n_years,
        from = input$from,
        to = to,
        include_grad = input$include_grad,
        include_grad_field = input$include_grad && input$include_grad_field,
        n_breaks = input$n_breaks
      )
    }) |>
      debounce(500)

    get_plot <- reactive({
      courses <- get_data()
      req(courses)
      params <- get_params()
      validate(
        need(
          isTRUE(params$n_top >= 1 && params$n_top <= sch_max_top),
          sprintf(
            "The number of top classes must be between 1 and %d.",
            sch_max_top
          )
        ),
        need(
          isTRUE(params$n_years >= 2),
          "The running average must include at least 2 years."
        ),
        need(isTRUE(params$n_breaks >= 2), "Use at least 2 y-axis breaks.")
      )
      log_debug(ns = ns, "rendering ", params$plot_type, " plot")
      shared <- list(
        df = courses,
        values = params$values,
        classes = params$classes,
        period = params$period,
        n_years = params$n_years,
        from = params$from,
        to = params$to,
        include_grad = params$include_grad,
        include_grad_field = params$include_grad_field,
        n_breaks = params$n_breaks,
        base_size = 13
      )
      tryCatch(
        switch(
          params$plot_type,
          classes = exec(
            sch_plot_top_classes,
            !!!shared,
            top_based_on = params$top_based_on,
            n_top = params$n_top,
            show_other = params$show_other,
            show_class_numbers = params$show_class_numbers
          ),
          class_types = exec(sch_plot_class_types, !!!shared),
          total = exec(sch_plot_total, !!!shared)
        ),
        error = function(e) {
          validate(need(
            FALSE,
            paste("Could not create the plot:", conditionMessage(e))
          ))
        }
      )
    })

    # (scales to the size of the card)
    output$plot <- renderPlot(get_plot(), res = 96)

    # details on click =====

    # details of the clicked bar or segment
    get_details <- reactiveVal(NULL)
    observeEvent(get_plot(), get_details(NULL))
    observeEvent(input$plot_click, {
      click <- input$plot_click
      p <- get_plot()
      params <- get_params()
      bar <- sch_find_bar(p, click$x, click$y, value = click$panelvar1)
      if (is.null(bar)) {
        return()
      }
      log_debug(
        ns = ns,
        "showing details for ",
        bar$group,
        " in ",
        bar$period,
        if (!is.null(bar$segment)) paste0(" (", bar$segment, ")")
      )
      details <- tryCatch(
        sch_get_bar_details(
          get_data(),
          bar,
          plot_type = params$plot_type,
          groups = levels(p$data$group),
          classes = params$classes,
          period = params$period,
          n_years = params$n_years,
          from = params$from,
          to = params$to,
          include_grad = params$include_grad,
          include_grad_field = params$include_grad_field
        ),
        error = function(e) {
          log_error(
            ns = ns,
            "failed to get the details",
            user_msg = "Could not get the details of the selected bar",
            error = e
          )
          NULL
        }
      )
      req(details)
      details$period_type <- params$period
      get_details(details)
      bslib::toggle_sidebar("details_sidebar", open = TRUE)
    })

    output$details <- renderUI({
      details <- get_details()
      if (is.null(details)) {
        return(p(
          class = "text-muted",
          "Click on a bar or one of its sections/classes to see its details."
        ))
      }
      stats_details_ui(details)
    })

    # downloads =====

    # file name base (e.g. "2026-10-10_12-00-00_enrollment_by_class_AY")
    get_download_basename <- reactive({
      params <- get_params()
      paste0(
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        "_enrollment_",
        c(classes = "by_class", class_types = "by_class_level", total = "total")[[
          params$plot_type
        ]],
        "_",
        params$period
      )
    })

    # PDF download of the current plot
    plot_download_server(
      "plot_download",
      plot_func = get_plot,
      filename_func = reactive(paste0(get_download_basename(), ".pdf"))
    )

    # xlsx download of the data underlying the current plot type
    output$download_data <- downloadHandler(
      filename = function() paste0(get_download_basename(), ".xlsx"),
      content = function(filename) {
        params <- get_params()
        log_debug(ns = ns, "downloading ", params$plot_type, " data to xlsx")
        tables <- sch_get_export_data(
          get_data(),
          plot_type = params$plot_type,
          classes = params$classes,
          period = params$period,
          n_years = params$n_years,
          from = params$from,
          to = params$to,
          include_grad = params$include_grad,
          include_grad_field = params$include_grad_field
        )
        writexl::write_xlsx(tables, path = filename)
      }
    )
  })
}

# the values that can be plotted (names are the labels)
stats_value_choices <- function() {
  c(
    "Enrollment" = "enrollment",
    "Student credit hours (SCH)" = "SCH",
    "TA equivalents" = "TA"
  )
}

# stats sidebar (plot settings), defaults mirror the defaults of the
# sch_plot_...() functions
module_stats_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      ns("plot_type"),
      "Plot",
      choices = c(
        "By class" = "classes",
        "By class level" = "class_types",
        "Total" = "total"
      )
    ),
    radioButtons(
      ns("period"),
      "Time axis",
      choices = c(
        "Semesters" = "semester",
        "Academic years" = "AY",
        "Running average (academic years)" = "running"
      )
    ),
    conditionalPanel(
      "input.period == 'running'",
      ns = ns,
      numericInput(
        ns("n_years"),
        "Years in running average",
        value = 3,
        min = 2,
        max = 6,
        step = 1
      )
    ),
    checkboxGroupInput(
      ns("values"),
      "Show",
      choices = stats_value_choices(),
      selected = stats_value_choices()
    ),
    conditionalPanel(
      "input.plot_type == 'classes'",
      ns = ns,
      radioButtons(
        ns("top_based_on"),
        "Top classes based on",
        choices = stats_value_choices(),
        selected = "TA"
      ),
      numericInput(
        ns("n_top"),
        "Number of top classes",
        value = 6,
        min = 1,
        max = sch_max_top,
        step = 1
      ),
      checkboxInput(
        ns("show_other"),
        "Show other",
        value = TRUE
      ),
      checkboxInput(
        ns("show_class_numbers"),
        "Show class numbers on plot",
        value = TRUE
      ),
      selectizeInput(
        ns("classes"),
        "Only include specific classes",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "all classes")
      )
    ),
    selectInput(ns("from"), "From semester", choices = NULL),
    selectInput(ns("to"), "To semester", choices = NULL),
    checkboxInput(
      ns("include_grad"),
      "Include graduate classes",
      value = TRUE
    ),
    conditionalPanel(
      "input.include_grad",
      ns = ns,
      checkboxInput(
        ns("include_grad_field"),
        "Include graduate field classes",
        value = FALSE
      )
    ),
    numericInput(
      ns("n_breaks"),
      "Number of y-axis breaks",
      value = 5,
      min = 2,
      max = 15,
      step = 1
    )
  )
}

# stats main UI
module_stats_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex align-items-center",
      h2("Enrollment, Student Credit Hours & TA Equivalents", class = "me-auto"),
      plot_download_link(
        ns("plot_download"),
        label = "Save Plot",
        class = header_button_class()
      ),
      downloadButton(
        ns("download_data"),
        "Download Data",
        icon = icon("file-excel"),
        class = paste(header_button_class(), "ms-2")
      ) |>
        add_tooltip(
          "Download the data underlying the plot (for all classes) as an Excel file: the values per period (summary), the courses per semester (courses) and their sections (sections)."
        )
    ),
    bslib::layout_sidebar(
      # details of the clicked bar / segment
      sidebar = bslib::sidebar(
        id = ns("details_sidebar"),
        position = "right",
        width = 500,
        open = FALSE,
        uiOutput(ns("details"))
      ),
      plotOutput(
        ns("plot"),
        height = "100%",
        click = ns("plot_click")
      ) |>
        shinycssloaders::withSpinner() |>
        bslib::as_fill_carrier()
    ),
    bslib::card_footer(
      "Lecture values are solid and lab values are transparent and stacked on top, both outlined by section for individual classes and by class otherwise (not outlined for running averages). ",
      "Click on a bar or one of its sections/classes to see its details. ",
      "A # above a bar means the class has a teaching assistant (in at least one semester for academic years and running averages), this information is likely incomplete and cannot distinguish between 25% and 50% TAs. ",
      sprintf(
        "SCH of LEC, REC and SEM sections count as lecture SCH, of MLS, LAB and FLD sections as lab SCH, lecture sections of classes with lab sections (LEC+LAB) count 1 lab credit hour per student. Other SCH (DIS, THE, IND, OTH) is not included. 1 TA equivalent = %s lecture SCH or %s lab SCH. ",
        sch_per_ta_lecture,
        sch_per_ta_lab
      ),
      "Summers are not included, academic years are fall + spring (e.g. AY24 = Fall 2023 + Spring 2024) and only complete academic years are shown. ",
      "Classes are matched by course number across the GEOL to ERTH rename."
    )
  )
}

# details UI =====

# a small html table
# @param df data frame
# @param highlight logical vector of rows to highlight
stats_html_table <- function(df, highlight = rep(FALSE, nrow(df))) {
  format_cell <- function(x) {
    if (is.logical(x)) {
      return(ifelse(is.na(x), "", ifelse(x, "yes", "no")))
    }
    if (is.numeric(x)) {
      return(format(x, big.mark = ",", trim = TRUE, drop0trailing = TRUE))
    }
    dplyr::coalesce(as.character(x), "")
  }
  cells <- purrr::map(df, format_cell)
  div(
    style = "overflow-x: auto;",
    tags$table(
    class = "table table-sm table-striped small",
    tags$thead(tags$tr(purrr::map(names(df), tags$th))),
    tags$tbody(
      purrr::map(seq_len(nrow(df)), \(i) {
        tags$tr(
          class = if (highlight[i]) "table-warning",
          purrr::map(cells, \(col) tags$td(col[i]))
        )
      })
    )
    )
  )
}

# the details of a clicked bar or segment
# @param details list from sch_get_bar_details() plus period_type
stats_details_ui <- function(details) {
  value_labels <- stats::setNames(sch_plot_values$label, sch_plot_values$value)
  bar_values <- details$values |>
    dplyr::transmute(
      value = value_labels[as.character(.data$value)],
      lecture = round(.data$lecture, 2),
      lab = round(.data$lab, 2),
      total = .data$lecture + .data$lab
    )
  single_class <- dplyr::n_distinct(details$courses$course) == 1
  multi_term <- dplyr::n_distinct(details$courses$term) > 1
  tagList(
    h4(details$title),
    h6(class = "text-muted", details$subtitle),
    if (!is.null(details$section)) h6(paste("Section", details$section)),
    div(class = "mt-3", stats_html_table(bar_values)),
    if (details$period_type == "running") {
      p(class = "small text-muted", "Values are averages per academic year, the tables below list the totals of all semesters.")
    },
    if (!single_class) {
      tagList(
        h5(class = "mt-3", "Classes"),
        stats_html_table(
          details$classes |>
            dplyr::select(
              "course",
              "title",
              if (multi_term) "semesters",
              "enrl",
              "sch",
              "ta",
              "has_ta"
            ) |>
            dplyr::rename(
              Course = "course",
              Title = "title",
              Enrl = "enrl",
              SCH = "sch",
              "TA eq." = "ta",
              TA = "has_ta"
            )
        )
      )
    } else {
      tagList(
        h5(class = "mt-3", "Courses"),
        stats_html_table(
          details$courses |>
            dplyr::select(
              Term = "term",
              Courses = "cross_listed",
              Activity = "activity",
              Enrl = "enrl",
              SCH = "sch",
              "TA eq." = "ta",
              TA = "has_ta"
            )
        ),
        h5(class = "mt-3", "Sections"),
        stats_html_table(
          details$sections |>
            dplyr::select(
              Term = "term",
              Section = "section_number",
              Activity = "activity",
              Enrl = "enrl",
              Cap = "max_enrl",
              SCH = "sch",
              Instructors = "instructors",
              TAs = "tas"
            ),
          highlight = !is.null(details$section) &
            details$sections$section %in% details$section
        )
      )
    }
  )
}
