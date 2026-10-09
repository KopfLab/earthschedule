# app UI shell: a navbar with the top level tabs (Schedule, Rooms, Stats,
# Admin) and a link to the paths app; the Schedule tab has a sidebar with the
# data reload button and the schedule settings, and the schedule card as the
# main content
# @param access_level the access level the app starts with (see access_levels())
# @param start_tab value of the navbar tab to show when the app starts
# @param paths_app_url url of the ERTH paths app (NULL to omit the link)
app_ui <- function(
  access_level = "faculty",
  start_tab = "schedule",
  paths_app_url = NULL
) {
  app_title <- "Department of Earth Science: Teaching Preferences & Planning"
  # don't start on a tab that is hidden at this access level
  if (start_tab %in% hidden_tabs(access_level)) {
    start_tab <- "schedule"
  }

  # return ui function (request param required by shiny for bookmarking)
  function(request) {
    bslib::page_navbar(
      # the access level part of the title is updated by the server (the
      # access level can change in dev mode)
      title = tagList(
        app_title,
        textOutput("access_level_title", inline = TRUE)
      ),
      window_title = paste0(app_title, access_level_title(access_level)),
      theme = bslib::bs_theme(version = 5),
      navbar_options = bslib::navbar_options(bg = "#f39c12", theme = "dark"),
      fillable = TRUE,
      header = use_app_utils(),
      id = "main_nav",
      selected = start_tab,
      # right-align the tabs
      bslib::nav_spacer(),
      bslib::nav_panel(
        title = "Schedule",
        value = "schedule",
        icon = icon("calendar"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 280,
            module_data_reload_button("data"),
            # database backup (admin only)
            downloadButton(
              "download_backup",
              "Download Database Backup",
              icon = icon("database"),
              class = "btn-sm btn-outline-secondary w-100"
            ) |>
              shinyjs::hidden() |>
              add_tooltip(
                "Download the entire Google spreadsheet (all sheets) as an Excel file."
              ),
            module_schedule_sidebar("schedule"),
            dev_mode_toggle_button(),
            dev_mode_access_level_select(selected = access_level)
          ),
          module_schedule_ui("schedule", access_level = access_level)
        )
      ),
      # restricted tabs (shown depending on the access level, see app_server)
      coming_soon_panel(
        "Rooms",
        value = "rooms",
        icon = icon("person-chalkboard")
      ),
      coming_soon_panel("Stats", value = "stats", icon = icon("chart-bar")),
      bslib::nav_panel(
        title = "Admin",
        value = "admin",
        icon = icon("user-shield"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 280,
            module_admin_sidebar("admin")
          ),
          module_admin_ui("admin")
        )
      ),
      if (!is.null(paths_app_url)) {
        bslib::nav_item(
          tags$a(
            href = paths_app_url,
            target = "_top",
            icon("link"),
            "Switch to ERTH Paths App"
          )
        )
      }
    )
  }
}

# placeholder tab for features that are not implemented yet
coming_soon_panel <- function(title, value = title, icon = NULL) {
  bslib::nav_panel(
    title = title,
    value = value,
    icon = icon,
    div(
      class = "text-center text-muted p-5",
      h4("coming soon...")
    )
  )
}
