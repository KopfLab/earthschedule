# class files (advertisements & syllabi) =====
# the files are stored in subfolders of a shared google drive folder, each file
# name starts with the class prefix (e.g. ERTH3181 or ERTH4185_5185 for the
# cross-listed ERTH4185/5185) to associate it with its class

# the class file subfolders (names are the labels), each subfolder is found by
# the start of its name in the google drive folder (case insensitive)
class_file_folders <- function() {
  c("Advertisement" = "advertisement", "Syllabus" = "syllabus")
}

# the icons for the class file subfolders
class_file_icons <- function() {
  c(advertisement = "bullhorn", syllabus = "file-lines")
}

# the file extensions that can be uploaded
class_file_extensions <- function() {
  c("pdf", "docx", "pptx")
}

# regexp for a class prefix at the start of a file name, e.g. "ERTH3181",
# "ERTH-3181", "erth 3181" or "ERTH4185_5185"
# (department, first number, additional numbers of cross-listed classes)
class_prefix_regexp <- function() {
  "^\\s*([A-Za-z]{4})[\\s_-]*(\\d{4})((?:\\s*[-_/]\\s*\\d{4})*)"
}

# the class prefix for class names, e.g. "ERTH3181" for "ERTH 3181" and
# "ERTH4185_5185" for "ERTH4185/5185" (NA for classes without a number)
# @param class the class name(s)
get_class_prefix <- function(class) {
  class <- stringr::str_to_upper(as.character(class))
  dept <- stringr::str_extract(class, "^\\s*[A-Z]{4}") |> stringr::str_trim()
  numbers <- stringr::str_extract_all(class, "\\d{4}") |>
    purrr::map_chr(\(x) if (length(x) > 0) paste(x, collapse = "_") else NA)
  ifelse(!is.na(dept) & !is.na(numbers), paste0(dept, numbers), NA_character_)
}

# parse the class prefix at the start of file names
# additional numbers only count as cross-listings if they end in the same 3
# digits as the first number (so e.g. the year in "ERTH3181_2025.pdf" is not
# part of the prefix)
# @param filename the file name(s)
# @return tibble with the normalized class `prefix` (NA if there is none) and
#   the `rest` of the file name after the prefix
parse_file_class_prefix <- function(filename) {
  m <- stringr::str_match(filename, class_prefix_regexp())
  purrr::pmap(
    list(filename, m[, 1], m[, 2], m[, 3], m[, 4]),
    function(filename, match, dept, number, more) {
      if (is.na(match)) {
        return(list(prefix = NA_character_, rest = filename))
      }
      numbers <- number
      matched <- stringr::str_match(filename, "^\\s*[A-Za-z]{4}[\\s_-]*\\d{4}")[
        1,
        1
      ]
      # add the cross-listed numbers one at a time
      while (nchar(more) > 0) {
        nxt <- stringr::str_match(more, "^\\s*[-_/]\\s*(\\d{4})")
        if (
          stringr::str_sub(nxt[1, 2], 2) != stringr::str_sub(number, 2) ||
            nxt[1, 2] %in% numbers
        ) {
          break
        }
        numbers <- c(numbers, nxt[1, 2])
        matched <- paste0(matched, nxt[1, 1])
        more <- stringr::str_sub(more, nchar(nxt[1, 1]) + 1L)
      }
      list(
        prefix = paste0(
          stringr::str_to_upper(dept),
          paste(numbers, collapse = "_")
        ),
        rest = stringr::str_sub(filename, nchar(matched) + 1L)
      )
    }
  ) |>
    dplyr::bind_rows() |>
    # make sure there's always the right columns (even for no filenames)
    dplyr::bind_rows(dplyr::tibble(
      prefix = character(0),
      rest = character(0)
    ))
}

# the name a file gets when it is uploaded for a class: a class prefix at the
# start of the file name is normalized (e.g. "ERTH-3181 syllabus.pdf" ->
# "ERTH3181 syllabus.pdf") and replaced with the class prefix if it is (part
# of) the class (e.g. "ERTH4185_syllabus.pdf" -> "ERTH4185_5185_syllabus.pdf"
# for ERTH4185/5185), otherwise the class prefix is added to the file name
# @param filename the original file name(s)
# @param class the class name
get_class_file_name <- function(filename, class) {
  prefix <- get_class_prefix(class)
  if (length(class) != 1L || is.na(prefix)) {
    abort("`class` must be a single class with a course number")
  }
  class_dept <- stringr::str_sub(prefix, 1, 4)
  class_numbers <- stringr::str_split(stringr::str_sub(prefix, 5), "_")[[1]]
  filename <- stringr::str_trim(filename)
  parsed <- parse_file_class_prefix(filename)
  is_part <- purrr::map_lgl(parsed$prefix, function(x) {
    !is.na(x) &&
      stringr::str_sub(x, 1, 4) == class_dept &&
      all(stringr::str_split(stringr::str_sub(x, 5), "_")[[1]] %in% class_numbers)
  })
  rest <- ifelse(is_part, parsed$rest, filename)
  # separate the prefix from the rest unless there's a separator already
  sep <- ifelse(stringr::str_detect(rest, "^[A-Za-z0-9]"), "_", "")
  paste0(prefix, sep, rest)
}

# whether a file name has an allowed extension for class files
is_class_file_extension <- function(filename) {
  tolower(tools::file_ext(filename)) %in% class_file_extensions()
}

# format file sizes (in bytes) for display
format_file_size <- function(bytes) {
  purrr::map_chr(as.numeric(bytes), function(x) {
    if (is.na(x)) {
      return("")
    }
    format(structure(x, class = "object_size"), units = "auto", standard = "SI")
  })
}

# google drive =====

# find the class file subfolders in the class files folder (creates the ones
# that do not exist yet)
# @param folder_id the id of the google drive folder with the subfolders
# @return named vector of the subfolder ids (names are class_file_folders())
gdrive_get_class_file_folders <- function(folder_id, gs_key_file) {
  authenticate_gdrive(gs_key_file = gs_key_file)
  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  existing <- googledrive::drive_ls(googledrive::as_id(folder_id), type = "folder")
  purrr::map_chr(class_file_folders(), function(folder) {
    idx <- which(stringr::str_starts(
      stringr::str_to_lower(existing$name),
      stringr::fixed(folder)
    ))
    if (length(idx) > 0) {
      return(as.character(existing$id[idx[1]]))
    }
    log_info("creating class files subfolder '", folder, "'")
    googledrive::drive_mkdir(
      folder,
      path = googledrive::as_id(folder_id)
    )$id |>
      as.character()
  }) |>
    stats::setNames(class_file_folders())
}

# list the files in class file subfolders
# @param folder_ids named vector of subfolder ids (see gdrive_get_class_file_folders())
# @return tibble with the subfolder (`folder`), file `id`, `name`, `size` (in
#   bytes), `modified` time, and class `prefix` of each file
gdrive_list_class_files <- function(folder_ids, gs_key_file) {
  authenticate_gdrive(gs_key_file = gs_key_file)
  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  purrr::map2(folder_ids, names(folder_ids), function(folder_id, folder) {
    files <- googledrive::drive_ls(googledrive::as_id(folder_id))
    # (not the folders)
    is_folder <- purrr::map_lgl(
      files$drive_resource,
      \(x) identical(x$mimeType, "application/vnd.google-apps.folder")
    )
    files <- files[!is_folder, ]
    dplyr::tibble(
      folder = rep(folder, nrow(files)),
      id = as.character(files$id),
      name = files$name,
      size = purrr::map_dbl(
        files$drive_resource,
        \(x) if (!is.null(x$size)) as.numeric(x$size) else NA_real_
      ),
      modified = purrr::map_chr(
        files$drive_resource,
        \(x) if (!is.null(x$modifiedTime)) x$modifiedTime else NA_character_
      ) |>
        lubridate::ymd_hms(quiet = TRUE) |>
        lubridate::with_tz(Sys.timezone())
    )
  }) |>
    dplyr::bind_rows() |>
    prepare_class_files()
}

# add the class prefix to the class files and sort them from newest to oldest
prepare_class_files <- function(files) {
  files |>
    dplyr::mutate(prefix = parse_file_class_prefix(.data$name)$prefix) |>
    dplyr::arrange(dplyr::desc(.data$modified), .data$name)
}

# download a google drive file
# @param path where to save the file
gdrive_download_file <- function(file_id, path, gs_key_file) {
  authenticate_gdrive(gs_key_file = gs_key_file)
  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  googledrive::drive_download(
    googledrive::as_id(file_id),
    path = path,
    overwrite = TRUE
  )
  if (!file.exists(path)) {
    abort("file download failed")
  }
  invisible(path)
}

# upload a file to a google drive folder
# @param path the local file to upload
# @param name the name of the file in google drive
# @param overwrite_id the id of an existing file to replace with the upload
#   (keeps its name), NULL to upload as a new file
gdrive_upload_file <- function(
  path,
  name,
  folder_id,
  gs_key_file,
  overwrite_id = NULL
) {
  authenticate_gdrive(gs_key_file = gs_key_file)
  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  if (!is.null(overwrite_id)) {
    googledrive::drive_update(googledrive::as_id(overwrite_id), media = path)
  } else {
    googledrive::drive_upload(
      media = path,
      path = googledrive::as_id(folder_id),
      name = name
    )
  }
}

# move a google drive file to the trash
gdrive_trash_file <- function(file_id, gs_key_file) {
  authenticate_gdrive(gs_key_file = gs_key_file)
  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  googledrive::drive_trash(googledrive::as_id(file_id))
}
