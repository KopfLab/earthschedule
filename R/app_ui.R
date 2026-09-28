# app UI shell: a navbar (with a link to the paths app) above a sidebar with
# the data reload button and the schedule settings, and the schedule card as
# the main content
# @param paths_app_url url of the ERTH paths app (NULL to omit the link)
app_ui <- function(paths_app_url = NULL) {
  app_title <- "Department of Earth Science: Teaching Preferences & Planning"

  # return ui function (request param required by shiny for bookmarking)
  function(request) {
    bslib::page_navbar(
      title = app_title,
      window_title = app_title,
      theme = bslib::bs_theme(version = 5),
      navbar_options = bslib::navbar_options(bg = "#f39c12", theme = "dark"),
      fillable = TRUE,
      header = use_app_utils(),
      sidebar = bslib::sidebar(
        width = 280,
        module_data_reload_button("data"),
        module_schedule_sidebar("schedule"),
        dev_mode_toggle_button()
      ),
      bslib::nav_panel(title = NULL, module_schedule_ui("schedule")),
      bslib::nav_spacer(),
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
