# app utility functions =====

# inject CSS/JS required by the app
use_app_utils <- function() {
  tagList(
    shinyjs::useShinyjs(),
    # adopt error color of the theme and make error validation larger
    tags$style(HTML(
      ".shiny-output-error-validation {
        color: var(--bs-danger) !important;
        font-size: 1.25rem;
      }"
    )) |>
      singleton(),
    # support ansi color codes from cli
    tags$style(HTML(paste(format(cli::ansi_html_style()), collapse = "\n"))) |>
      singleton(),
    # keep navbar inputs/links compact
    tags$style(HTML(".navbar .form-group {margin-bottom: 0;}")) |> singleton()
  )
}

# logging =====
# note: fatal and trace are overkill for this app

# call the other log functions instead for clarity in the code
# @param ... toast parameters
log_any <- function(
  msg,
  log_fun,
  ns = NULL,
  toaster = NULL,
  position = "bottom-right",
  ...
) {
  ns_prefix <- if (!is.null(ns)) paste0("[", ns(NULL), "] ") else ""
  if (!is.null(toaster)) {
    log_fun(paste0(
      ns_prefix,
      msg,
      " [GUI msg: '",
      toaster,
      "']",
      collapse = ""
    ))
    bslib::toast(
      HTML(cli::ansi_html(toaster)),
      position = position,
      ...
    ) |>
      bslib::show_toast()
  } else {
    log_fun(paste0(ns_prefix, msg, collapse = ""))
  }
}

log_error <- function(..., ns = NULL, user_msg = NULL, error = NULL) {
  error_msg <-
    if (!is.null(error)) {
      if (inherits(error, "condition")) {
        error <- conditionMessage(error)
      }
      gsub("\\n", "<br>", cli::ansi_html(error)) |> paste(collapse = "<br>")
    } else {
      ""
    }

  pkg <- utils::packageName()
  issue_title <- sprintf(
    "Version %s: %s",
    if (!is.null(pkg)) as.character(utils::packageVersion(pkg)) else "app",
    user_msg
  )

  issue_body <- sprintf(
    "Please describe here what you were attempting to do in the app when this issue occurred.\n\n## Trace (do NOT delete)\n\n<pre>%s</pre>",
    error_msg
  )

  issue_url <- sprintf(
    "https://github.com/KopfLab/%s/issues/new?title=%s&body=%s",
    if (!is.null(pkg)) pkg else "geoapps",
    utils::URLencode(issue_title, reserved = TRUE),
    utils::URLencode(HTML(issue_body), reserved = TRUE)
  )

  error_screen <- modalDialog(
    title = span(
      style = "color: red;",
      h2(user_msg, style = "color: red;"),
      h4(
        "Please try again. If the issue persists, please",
        tags$a("report this error", href = issue_url, target = "_blank")
      )
    ),
    easyClose = TRUE,
    if (nchar(error_msg) > 0) pre(HTML(error_msg))
  )

  log_any(
    msg = paste0(..., if (!is.null(error)) paste0(": ", error), collapse = ""),
    ns = ns,
    log_fun = rlog::log_error,
    toaster = user_msg,
    header = "Error",
    type = "danger",
    duration_s = 10
  )

  showModal(error_screen)
}

log_warning <- function(..., ns = NULL, user_msg = NULL, warning = NULL) {
  log_any(
    msg = paste0(..., collapse = ""),
    ns = ns,
    log_fun = rlog::log_warn,
    toaster = if (!is.null(warning)) cli::ansi_strip(warning) else user_msg,
    header = if (!is.null(warning)) user_msg else NULL,
    type = "warning",
    duration_s = 5
  )
}

log_info <- function(..., ns = NULL, user_msg = NULL) {
  log_any(
    msg = paste0(..., collapse = ""),
    ns = ns,
    log_fun = rlog::log_info,
    toaster = user_msg,
    type = "info",
    duration_s = 2
  )
}

log_success <- function(..., ns = NULL, user_msg = NULL) {
  log_any(
    msg = paste0(..., collapse = ""),
    ns = ns,
    log_fun = rlog::log_info,
    toaster = user_msg,
    type = "success",
    duration_s = 2
  )
}

log_debug <- function(..., ns = NULL) {
  log_any(msg = paste0(..., collapse = ""), ns = ns, log_fun = rlog::log_debug)
}

# ui helpers =====

# convenience function for adding non-breaking spaces
spaces <- function(n = 1) {
  htmltools::HTML(rep("&nbsp;", n))
}

# add a bslib tooltip to a widget
# note: apply shinyjs::hidden()/disabled() to the widget BEFORE adding the
# tooltip so the widget itself (not the tooltip wrapper) carries those states
add_tooltip <- function(widget, ...) {
  bslib::tooltip(widget, ...)
}

# consistent styling for the small action buttons in card headers
header_button_class <- function() {
  "btn-sm btn-outline-secondary"
}

# dev mode toggle button (only shown when the app is started in dev mode)
dev_mode_toggle_button <- function(id = "dev_mode_toggle") {
  if (shiny::in_devmode()) {
    actionButton(id, "Toggle Dev Mode", class = "btn-sm btn-outline-danger")
  }
}

# access levels =====

# the access levels of the app (names are the labels)
access_levels <- function() {
  c("Admin" = "admin", "Faculty" = "faculty", "Student" = "student")
}

# the title suffix for an access level (e.g. " | Faculty View")
access_level_title <- function(access_level) {
  label <- names(access_levels())[access_levels() == access_level]
  sprintf(" | %s View", label)
}

# access level dropdown (only shown when the app is started in dev mode)
dev_mode_access_level_select <- function(
  id = "dev_access_level",
  selected = "faculty"
) {
  if (shiny::in_devmode()) {
    selectInput(
      id,
      "Access level (dev mode):",
      choices = access_levels(),
      selected = selected
    )
  }
}
