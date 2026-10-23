# class files module server: lists the files (advertisements & syllabi) of the
# classes in the google drive folder and provides the dialog to download,
# delete and upload the files of a class
# @param folder_id the id of the google drive folder with the class file
#   subfolders (see class_file_folders())
# @param get_access_level reactive returning the access level (see access_levels())
# @return list with get_icons(classes), which returns the html of the class
#   file icons that open the dialog (to add to the class names)
module_class_files_server <- function(
  id,
  folder_id,
  gs_key_file,
  get_access_level
) {
  moduleServer(id, function(input, output, session) {
    # namespace
    ns <- session$ns

    # reactive values
    values <- reactiveValues(
      folder_ids = NULL,
      files = NULL,
      class = NULL,
      folder = NULL,
      delete_id = NULL,
      uploads = NULL,
      qrcode_class = NULL
    )

    # files can be uploaded and deleted by faculty and admins
    can_modify <- reactive({
      get_access_level() %in% c("admin", "faculty")
    })

    # load files ======

    # (re)load the list of files (of one or all subfolders)
    load_files <- function(folder = NULL) {
      tryCatch(
        {
          if (is.null(values$folder_ids)) {
            log_info(ns = ns, "looking for the class file folders")
            values$folder_ids <- gdrive_get_class_file_folders(
              folder_id,
              gs_key_file = gs_key_file
            )
          }
          folder_ids <- values$folder_ids
          if (!is.null(folder)) {
            folder_ids <- folder_ids[folder]
          }
          log_info(
            ns = ns,
            "loading the class files list for ",
            paste(names(folder_ids), collapse = " & ")
          )
          files <- gdrive_list_class_files(folder_ids, gs_key_file = gs_key_file)
          # keep the files of the other subfolders
          if (!is.null(values$files)) {
            files <- values$files |>
              dplyr::filter(!.data$folder %in% names(folder_ids)) |>
              dplyr::bind_rows(files) |>
              prepare_class_files()
          }
          # (only update if something changed to avoid re-rendering the table)
          if (!identical(files, values$files)) {
            values$files <- files
          }
        },
        error = function(e) {
          log_error(
            ns = ns,
            "failed to load the class files",
            user_msg = "Could not load the class files from Google Drive",
            error = e
          )
          if (is.null(values$files)) {
            values$files <- prepare_class_files(dplyr::tibble(
              folder = character(0),
              id = character(0),
              name = character(0),
              size = numeric(0),
              modified = as.POSIXct(character(0))
            ))
          }
        }
      )
    }

    # load all files once at the start
    observeEvent(TRUE, once = TRUE, load_files())

    # icons =====

    # the html of the class file icons for each of the classes (empty for
    # classes without a course number), icons are dimmed if there are no
    # files and not shown at all if there are no files and they can't be
    # uploaded (student mode), ERTH classes also get a QR code icon (between
    # the advertisement and syllabus icons)
    get_icons <- function(classes) {
      req(values$files)
      classes <- as.character(classes)
      counts <- values$files |> dplyr::count(.data$folder, .data$prefix)
      folders <- class_file_folders()
      folder_icons <- purrr::map_chr(class_file_icons(), \(x) as.character(icon(x)))
      modify <- can_modify()
      # link that sets an input (to the class and folder)
      make_link <- function(input_id, value, title, icon_html, class = "") {
        sprintf(
          paste0(
            "<a href=\"#\" class=\"class-file-icon%s\" title=\"%s\" ",
            "onclick=\"event.preventDefault(); event.stopPropagation(); ",
            "Shiny.setInputValue('%s', %s, {priority: 'event'});\" ",
            "ondblclick=\"event.stopPropagation();\">%s</a>"
          ),
          class,
          title,
          ns(input_id),
          value,
          icon_html
        )
      }
      # (only the unique classes)
      icons <- purrr::map_chr(
        unique(classes),
        function(class) {
          prefix <- get_class_prefix(class)
          if (is.na(prefix)) {
            return("")
          }
          # (only safe characters for the javascript)
          safe_class <- stringr::str_remove_all(class, "[^A-Za-z0-9/ _-]")
          links <- purrr::map_chr(folders, function(folder) {
            n <- counts$n[counts$folder == folder & counts$prefix == prefix]
            n <- if (length(n) > 0) n[1] else 0L
            if (n == 0 && !modify) {
              return("")
            }
            make_link(
              "open",
              sprintf("{class: '%s', folder: '%s'}", safe_class, folder),
              sprintf(
                "%s: %s",
                names(folders)[folders == folder],
                if (n == 1) "1 file" else sprintf("%d files", n)
              ),
              folder_icons[folder],
              class = if (n == 0) " no-files" else ""
            )
          })
          qrcode <- if (has_class_qrcode(class)) {
            make_link(
              "qrcode",
              sprintf("'%s'", safe_class),
              "QR code",
              as.character(icon("qrcode"))
            )
          } else {
            ""
          }
          c(links[1], qrcode, links[-1]) |>
            paste(collapse = "") |>
            (\(x) if (nchar(x) > 0) {
              sprintf("<span class=\"class-file-icons\">%s</span>", x)
            } else {
              ""
            })()
        }
      )
      icons[match(classes, unique(classes))]
    }

    # QR code =====

    # show the QR code of a class (with download buttons)
    observeEvent(input$qrcode, {
      req(has_class_qrcode(input$qrcode))
      log_info(ns = ns, "showing the QR code for ", input$qrcode)
      svg <- tryCatch(create_class_qrcode(input$qrcode), error = function(e) {
        log_error(
          ns = ns,
          "failed to create the QR code",
          user_msg = sprintf("Could not create the QR code for %s", input$qrcode),
          error = e
        )
        NULL
      })
      req(svg)
      values$qrcode_class <- input$qrcode
      url <- get_class_search_url(input$qrcode)
      showModal(modalDialog(
        title = sprintf("%s: QR code", input$qrcode),
        easyClose = TRUE,
        tags$img(
          src = paste0(
            "data:image/svg+xml;charset=utf-8,",
            utils::URLencode(svg, reserved = TRUE)
          ),
          alt = sprintf("QR code for %s", input$qrcode),
          class = "d-block mx-auto",
          style = "width: 100%; max-width: 400px;"
        ),
        p(
          class = "text-muted small mt-2 mb-0 text-center",
          "Links to ",
          tags$a(href = url, target = "_blank", url)
        ),
        footer = tagList(
          downloadButton(ns("qrcode_png"), "PNG"),
          downloadButton(ns("qrcode_svg"), "SVG"),
          modalButton("Close")
        )
      ))
    })

    # download the QR code
    download_qrcode <- function(ext) {
      downloadHandler(
        filename = function() {
          paste0(get_class_qrcode_name(values$qrcode_class), ".", ext)
        },
        content = function(file) {
          log_info(ns = ns, "downloading the QR code for ", values$qrcode_class)
          create_class_qrcode(values$qrcode_class, file)
        }
      )
    }
    output$qrcode_png <- download_qrcode("png")
    output$qrcode_svg <- download_qrcode("svg")

    # dialog =====

    # the files of the selected class and folder
    get_class_files <- reactive({
      req(values$files, values$class, values$folder)
      values$files |>
        dplyr::filter(
          .data$folder == values$folder,
          .data$prefix == get_class_prefix(values$class)
        )
    })

    # the label of the selected folder
    get_folder_label <- function() {
      names(class_file_folders())[class_file_folders() == values$folder]
    }

    # show the class files dialog
    show_dialog <- function() {
      files <- get_class_files()

      # download handlers for each file
      purrr::walk2(files$id, files$name, function(file_id, file_name) {
        output[[paste0("download_", file_id)]] <- downloadHandler(
          filename = file_name,
          content = function(file) {
            log_info(ns = ns, "downloading class file '", file_name, "'")
            tryCatch(
              gdrive_download_file(file_id, file, gs_key_file = gs_key_file),
              error = function(e) {
                log_error(
                  ns = ns,
                  "failed to download the class file",
                  user_msg = sprintf("Could not download '%s'", file_name),
                  error = e
                )
                abort("download failed", parent = e)
              }
            )
          }
        )
      })

      # files table
      files_table <- if (nrow(files) == 0L) {
        p(
          class = "text-muted",
          sprintf(
            "There are no %s files for %s yet.",
            tolower(get_folder_label()),
            values$class
          )
        )
      } else {
        tags$table(
          class = "table table-sm table-hover align-middle mb-0",
          tags$thead(tags$tr(
            tags$th("File"),
            tags$th("Uploaded"),
            tags$th(class = "text-end", "Size"),
            if (can_modify()) tags$th()
          )),
          tags$tbody(
            purrr::pmap(
              list(files$id, files$name, files$modified, files$size),
              function(file_id, file_name, modified, size) {
                tags$tr(
                  tags$td(
                    downloadLink(
                      ns(paste0("download_", file_id)),
                      label = tagList(icon("download"), file_name)
                    )
                  ),
                  tags$td(
                    class = "text-nowrap",
                    format(modified, "%Y-%m-%d %H:%M")
                  ),
                  tags$td(class = "text-end text-nowrap", format_file_size(size)),
                  if (can_modify()) {
                    tags$td(
                      class = "text-end",
                      tags$a(
                        href = "#",
                        class = "text-danger",
                        title = "Delete file",
                        onclick = sprintf(
                          "event.preventDefault(); Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                          ns("delete"),
                          file_id
                        ),
                        icon("trash")
                      )
                    )
                  }
                )
              }
            )
          )
        )
      }

      showModal(modalDialog(
        title = sprintf("%s: %s", values$class, get_folder_label()),
        size = "l",
        easyClose = TRUE,
        files_table,
        if (can_modify()) {
          div(
            class = "mt-3",
            fileInput(
              ns("upload"),
              sprintf("Upload %s files:", tolower(get_folder_label())),
              multiple = TRUE,
              accept = paste0(".", class_file_extensions()),
              width = "100%"
            ),
            p(
              class = "text-muted small mb-0",
              sprintf(
                "Allowed file types: %s. Files are named with the class prefix %s (added automatically if missing) to associate them with the class.",
                paste(class_file_extensions(), collapse = ", "),
                get_class_prefix(values$class)
              )
            )
          )
        },
        footer = modalButton("Close")
      ))
    }

    # open the dialog (from the icons)
    observeEvent(input$open, {
      req(
        input$open$folder %in% class_file_folders(),
        !is.na(get_class_prefix(input$open$class))
      )
      log_info(
        ns = ns,
        "showing ",
        input$open$folder,
        " files for ",
        input$open$class
      )
      values$class <- input$open$class
      values$folder <- input$open$folder
      # make sure the list is up to date
      load_files(values$folder)
      show_dialog()
    })

    # delete =====
    observeEvent(input$delete, {
      req(can_modify())
      file <- get_class_files() |> dplyr::filter(.data$id == input$delete)
      req(nrow(file) == 1L)
      values$delete_id <- file$id
      showModal(modalDialog(
        title = "Delete file",
        h5(sprintf(
          "Are you sure you want to delete '%s'?",
          file$name
        )),
        footer = tagList(
          actionButton(ns("delete_confirm"), "Delete", class = "btn-danger"),
          actionButton(ns("back"), "Cancel")
        )
      ))
    })

    observeEvent(input$delete_confirm, {
      req(can_modify())
      file <- get_class_files() |> dplyr::filter(.data$id == values$delete_id)
      req(nrow(file) == 1L)
      tryCatch(
        {
          log_info(
            ns = ns,
            "deleting class file '",
            file$name,
            "'",
            user_msg = sprintf("Deleting '%s'...", file$name)
          )
          gdrive_trash_file(file$id, gs_key_file = gs_key_file)
          load_files(values$folder)
          show_dialog()
        },
        error = function(e) {
          log_error(
            ns = ns,
            "failed to delete the class file",
            user_msg = sprintf("Could not delete '%s'", file$name),
            error = e
          )
        }
      )
    })

    # back to the files dialog
    observeEvent(input$back, show_dialog())

    # upload =====
    observeEvent(input$upload, {
      req(can_modify(), input$upload)
      uploads <- input$upload |>
        dplyr::as_tibble() |>
        dplyr::select("name", "datapath")

      # only allowed file types
      wrong <- uploads |> dplyr::filter(!is_class_file_extension(.data$name))
      if (nrow(wrong) > 0) {
        log_warning(
          ns = ns,
          "not uploading files with wrong type: ",
          paste(wrong$name, collapse = ", "),
          user_msg = sprintf(
            "Only %s files can be uploaded, not uploading %s",
            paste(class_file_extensions(), collapse = ", "),
            paste(wrong$name, collapse = ", ")
          )
        )
        uploads <- uploads |> dplyr::filter(is_class_file_extension(.data$name))
      }
      req(nrow(uploads) > 0)

      # file names with class prefix and existing files to replace
      uploads <- uploads |>
        dplyr::mutate(
          target = get_class_file_name(.data$name, values$class),
          overwrite_id = get_class_files()$id[
            match(.data$target, get_class_files()$name)
          ]
        )
      # (last one wins if the same name is uploaded multiple times)
      uploads <- uploads[!duplicated(uploads$target, fromLast = TRUE), ]
      values$uploads <- uploads

      # confirm overwrites
      existing <- uploads |> dplyr::filter(!is.na(.data$overwrite_id))
      if (nrow(existing) > 0) {
        showModal(modalDialog(
          title = "Replace existing files?",
          h5(
            if (nrow(existing) == 1) {
              "This file already exists and will be replaced:"
            } else {
              "These files already exist and will be replaced:"
            }
          ),
          tags$ul(purrr::map(existing$target, tags$li)),
          footer = tagList(
            actionButton(ns("upload_confirm"), "Replace", class = "btn-danger"),
            actionButton(ns("back"), "Cancel")
          )
        ))
      } else {
        upload_files()
      }
    })

    observeEvent(input$upload_confirm, upload_files())

    # upload the files in values$uploads
    upload_files <- function() {
      req(can_modify(), values$uploads)
      uploads <- values$uploads
      values$uploads <- NULL
      tryCatch(
        {
          for (i in seq_len(nrow(uploads))) {
            log_info(
              ns = ns,
              "uploading class file '",
              uploads$target[i],
              "'",
              user_msg = sprintf("Uploading '%s'...", uploads$target[i])
            )
            gdrive_upload_file(
              path = uploads$datapath[i],
              name = uploads$target[i],
              folder_id = values$folder_ids[[values$folder]],
              gs_key_file = gs_key_file,
              overwrite_id = if (!is.na(uploads$overwrite_id[i])) {
                uploads$overwrite_id[i]
              }
            )
          }
          log_success(
            ns = ns,
            "upload complete",
            user_msg = sprintf(
              "Uploaded %d file%s",
              nrow(uploads),
              if (nrow(uploads) > 1) "s" else ""
            )
          )
          load_files(values$folder)
          show_dialog()
        },
        error = function(e) {
          log_error(
            ns = ns,
            "failed to upload the class files",
            user_msg = "Could not upload the files",
            error = e
          )
          # (some files may have been uploaded)
          load_files(values$folder)
        }
      )
    }

    # return
    list(
      get_icons = get_icons
    )
  })
}
