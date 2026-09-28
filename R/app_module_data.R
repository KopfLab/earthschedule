# data server ----
# downloads the google spreadsheet and reads the requested sheets (tabs), each
# managed by its own module_data_table_server()
# @param data_sheet_id google sheets id with the app data
# @param gs_key_file google service account key file
# @param sheets named list of sheet column definitions (see read_excel_sheet() for the column format), the names are the sheet (tab) names in the spreadsheet
# @param dev_cache_file local xlsx file to cache the spreadsheet in dev mode (to avoid downloading on every app reload)
# @return list with reload_data(), is_authenticated() and one data table module per sheet (named by sheet)
module_data_server <- function(
  id,
  data_sheet_id,
  gs_key_file,
  sheets,
  dev_cache_file = NULL
) {
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # reactive values =========
    values <- reactiveValues(
      load_data = 1L,
      file_path = NULL,
      locked = TRUE,
      error = FALSE
    )

    # dev cache =========
    use_dev_cache <- function() {
      shiny::in_devmode() && !is.null(dev_cache_file)
    }
    clear_dev_cache <- function() {
      if (use_dev_cache() && file.exists(dev_cache_file)) {
        file.remove(dev_cache_file)
      }
    }

    # (re-) load data event =====
    reload_data <- function() {
      # enforce reload even for dev mode
      clear_dev_cache()
      values$load_data <- values$load_data + 1L
    }
    observeEvent(input$reload, reload_data())

    # data tables =========
    get_local_file <- reactive({
      values$file_path
    })
    report_error <- function() {
      values$error <- TRUE
    }

    tables <- purrr::imap(
      sheets,
      function(cols, sheet) {
        module_data_table_server(
          id = sheet,
          data_sheet_id = data_sheet_id,
          gs_key_file = gs_key_file,
          local_file = get_local_file,
          report_error = report_error,
          reload_data = reload_data,
          clear_dev_cache = clear_dev_cache,
          sheet = sheet,
          cols = cols
        )
      }
    )

    # download data event =========
    observeEvent(
      values$load_data,
      {
        # lock when this cascade starts
        lock_app()
        log_info(
          ns = ns,
          "requesting google spreadsheet data",
          user_msg = "Fetching data"
        )
        values$file_path <-
          tryCatch(
            {
              # don't download from scratch every time if in development mode
              if (use_dev_cache() && file.exists(dev_cache_file)) {
                file_path <- dev_cache_file
                log_debug(ns = ns, "in DEV mode, using local data file")
              } else {
                file_path <- download_gs(
                  data_sheet_id,
                  gs_key_file = gs_key_file
                )
              }

              # save locally if in dev mode
              if (use_dev_cache() && !file.exists(dev_cache_file)) {
                file.copy(file_path, dev_cache_file)
                log_debug(
                  ns = ns,
                  "in DEV mode, saving downloaded data to local file"
                )
              }
              file_path
            },
            error = function(e) {
              log_error(
                ns = ns,
                "download failed",
                user_msg = "Data loading error",
                error = e
              )
              values$error <- TRUE
              NULL
            }
          )
      },
      priority = 10L
    )

    # read data event =========
    observeEvent(
      values$load_data,
      {
        req(!values$error)
        log_debug(ns = ns, "file path: ", values$file_path)
        log_info(
          ns = ns,
          "loading data from xlsx file",
          user_msg = "Loading data"
        )

        # reading data sheets
        tryCatch(
          purrr::walk(tables, ~ .x$read_data(ignore_other_cols = TRUE)),
          error = function(e) {
            log_error(
              ns = ns,
              "data read failed",
              user_msg = "Data reading error",
              error = e
            )
            values$error <- TRUE
          }
        )
      },
      priority = 9L
    )

    # always authenticate
    is_authenticated <- reactive({
      TRUE
    })

    # lock/unlock events ====
    observe(
      {
        # triggers
        values$load_data
        values$locked
        values$error

        # unlock if authenticated
        isolate({
          if (is_authenticated() && values$locked && !values$error) {
            values$locked <- FALSE
          } else if (!values$locked) {
            unlock_app()
          } else {
            # reset
            purrr::walk(tables, ~ .x$reset())
            log_info(ns = ns, "app stays locked")
          }
        })
      },
      priority = 1L
    )

    lock_app <- function() {
      log_info(ns = ns, "locking app")
      values$locked <- TRUE
      values$error <- FALSE
    }

    unlock_app <- function() {
      log_info(ns = ns, "unlocking app")
      log_success(ns = ns, "loading all done", user_msg = "Complete")
    }

    #  return functions ====
    c(
      list(
        reload_data = reload_data,
        is_authenticated = is_authenticated
      ),
      tables
    )
  })
}

# data ui components - reload button ------
module_data_reload_button <- function(id) {
  ns <- NS(id)
  actionButton(
    ns("reload"),
    "Reload",
    icon = icon("rotate"),
    class = "btn-sm btn-outline-secondary w-100"
  ) |>
    add_tooltip("Reload all data")
}
