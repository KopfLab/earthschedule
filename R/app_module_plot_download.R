# plot download module =====
# reusable: a button that opens a save dialog and downloads the current plot as
# a PDF (filename / width / height configurable in the dialog).
# adapted from isoexplorer's app_module_plot_download.R

# server
# @param plot_func reactive returning the ggplot to save
# @param filename_func reactive returning the default filename
plot_download_server <- function(id, plot_func, filename_func) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    save_dialog <- reactive({
      modalDialog(
        title = "Save plot",
        fade = FALSE,
        easyClose = TRUE,
        size = "s",
        textInput(ns("save_name"), "Filename:", filename_func()),
        numericInput(ns("save_width"), "Width [inches]:", 14),
        numericInput(ns("save_height"), "Height [inches]:", 10),
        footer = tagList(
          downloadButton(
            ns("download"),
            label = "Download",
            icon = icon("download")
          ),
          modalButton("Close")
        )
      )
    })
    observeEvent(input$download_dialog, showModal(save_dialog()))

    output$download <- downloadHandler(
      filename = function() isolate(input$save_name),
      content = function(filename) {
        log_debug(
          ns = ns,
          "saving plot ",
          input$save_name,
          " (",
          input$save_width,
          " by ",
          input$save_height,
          ")"
        )
        ggplot2::ggsave(
          file = filename,
          plot = plot_func(),
          width = isolate(input$save_width),
          height = isolate(input$save_height),
          device = "pdf"
        )
      }
    )
  })
}

# UI: a button that opens the save dialog
plot_download_link <- function(
  id,
  label = "Save",
  tooltip = "Save the plot as a PDF",
  class = NULL
) {
  ns <- NS(id)
  actionButton(
    ns("download_dialog"),
    label,
    icon = icon("file-pdf"),
    class = class
  ) |>
    add_tooltip(tooltip)
}
