# google sheet ======

authenticate_gdrive <- function(gs_key_file) {
  # google drive authentication
  googlesheets4::gs4_deauth()
  googledrive::drive_deauth()

  # authenticate with key file
  tryCatch(
    token_obj <- gargle::credentials_service_account(
      path = gs_key_file,
      scopes = c(
        "https://www.googleapis.com/auth/spreadsheets", # full access to sheets
        "https://www.googleapis.com/auth/drive" # full access to drive files
      )
    ),
    error = function(e) {
      abort(paste0("google authentication failed"), parent = e)
    }
  )

  # check token
  if (is.null(token_obj)) {
    abort("google authentication failed with the provided key file")
  }

  # run authentication
  googlesheets4::gs4_auth(token = token_obj)
  googledrive::drive_auth(token = token_obj)

  # check result
  if (
    !(googledrive::drive_has_token() &&
      identical(googledrive::drive_token()$auth_token$hash(), token_obj$hash()))
  ) {
    abort("google authentication failed with the provided key file")
  }
}

# download a google sheet
download_gs <- function(gs_id, gs_key_file = "gdrive_access_key.json") {
  authenticate_gdrive(gs_key_file = gs_key_file)
  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  download_to <- paste0(tempfile(), ".xlsx")
  result <- googledrive::drive_download(
    googledrive::as_id(gs_id),
    path = download_to
  )
  if (!file.exists(download_to)) {
    abort("google sheet download failed")
  }
  return(download_to)
}

# make actual changes in google spreadsheet
commit_changes_to_gs <- function(
  df,
  gs_id,
  gs_sheet,
  gs_key_file = "gdrive_access_key.json"
) {
  # safety checks
  stopifnot(
    "`df` required" = !missing(df) || !is.data.frame(df)
  )

  # update
  if (any(df$.update & !df$.add)) {
    df |>
      dplyr::filter(.data$.update & !.data$.add) |>
      update_in_gs(
        gs_id = gs_id,
        gs_sheet = gs_sheet,
        gs_key_file = gs_key_file
      )
  }

  # add
  if (any(df$.add)) {
    df |>
      dplyr::filter(.data$.add) |>
      add_to_gs(gs_id = gs_id, gs_sheet = gs_sheet, gs_key_file = gs_key_file)
  }

  # delete
  if (any(df$.delete)) {
    abort("deleting data in the google sheet is not yet implemented")
  }

  return(invisible(df))
}

# add rows to the google spreadsheet
add_to_gs <- function(
  df_add,
  gs_id,
  gs_sheet,
  gs_key_file = "gdrive_access_key.json"
) {
  # set formula for ID
  if (all(is.na(df_add[[1]]))) {
    df_add[[1]] <- googlesheets4::gs4_formula(
      '=INDIRECT("R[-1]C[0]", FALSE) + 1'
    )
  } else if (any(is.na(df_add[[1]]))) {
    abort("ids for new records must either all be set explicitly or all NA")
  }

  # remove add/edit/update flag columns
  df_add <- df_add |>
    dplyr::select(-".rowid", -".add", -".update", -".delete")

  # authenticate
  authenticate_gdrive(gs_key_file = gs_key_file)

  # write
  tryCatch(
    googlesheets4::sheet_append(gs_id, df_add, sheet = gs_sheet),
    error = function(e) {
      sprintf("adding new %s data failed", gs_sheet) |>
        abort(parent = e)
    }
  )
}

# update record in google spreadsheet
update_in_gs <- function(
  df_update,
  gs_id,
  gs_sheet,
  gs_key_file = "gdrive_access_key.json"
) {
  # remove add/edit/update flag columns
  rows <- df_update$.rowid
  df_update <- df_update |>
    dplyr::select(-".rowid", -".add", -".update", -".delete")

  # authenticate
  authenticate_gdrive(gs_key_file = gs_key_file)

  # write
  for (i in seq_along(rows)) {
    # one row at a time because of the range issue
    tryCatch(
      googlesheets4::range_write(
        gs_id,
        df_update[i, ],
        sheet = gs_sheet,
        range = paste0("A", rows[i] + 1L),
        col_names = FALSE,
        reformat = FALSE
      ),
      error = function(e) {
        sprintf("updating %s data row %d failed", gs_sheet, rows[i]) |>
          abort(parent = e)
      }
    )
  }
}

# excel file ======

# read excel sheet with column type checks
# @param list of columns and their types, for undefined column types assumes "character" by default - note that ALL columns in the spreadsheet must be included here and the first column must be a unique ID (both will be checked and throw errors if not true)
read_excel_sheet <- function(
  file_path,
  sheet,
  cols,
  timezone = Sys.timezone(),
  ignore_other_cols = FALSE
) {
  # parse cols param
  col_types <- unname(cols)
  col_names <- names(cols)
  no_types = if (is.null(col_names)) {
    seq_along(col_types)
  } else {
    nchar(col_names) == 0
  }
  no_types_vals <- col_types[no_types]
  col_types[no_types] <- "character"
  col_names[no_types] <- no_types_vals
  cols <- col_types |> stats::setNames(col_names)

  # type map
  excel_col_types <- c(
    "character" = "text",
    "datetime" = "date",
    "double" = "numeric",
    "factor" = "text",
    "integer" = "numeric",
    "logical" = "logical",
    "number" = "numeric"
  )

  # safety check
  stopifnot(
    "unsupported column type" = all(col_types %in% names(excel_col_types))
  )

  # warn about factors
  if (any(col_types == "factor")) {
    warn(
      "reading data in as 'factor' is not recommended if the data needs in this table needs to be editable - convert to factor when creating the selection table instead"
    )
  }

  # read excel sheet
  sheet_cols <- names(readxl::read_excel(file_path, sheet = sheet, n_max = 0))
  sheet_col_types <- excel_col_types[cols[sheet_cols]] |> unname()
  if (!ignore_other_cols && any(is.na(sheet_col_types))) {
    sprintf(
      "unexpected column(s) in spreadsheet: %s",
      paste(sheet_cols[is.na(sheet_col_types)], collapse = ", ")
    ) |>
      abort()
  }
  sheet_col_types[is.na(sheet_col_types)] <- "skip"

  data <- readxl::read_excel(
    file_path,
    sheet = sheet,
    col_types = sheet_col_types
  )

  # parse map
  tz_adjust <- function(x) lubridate::with_tz(x, timezone)
  col_type_parsers <- c(
    "integer" = expr(as.integer),
    "factor" = expr(as.factor),
    "datetime" = expr(tz_adjust)
  )

  # parsers
  parsers <-
    purrr::map2(
      names(cols),
      col_type_parsers[cols],
      ~ if (!is.null(.y)) {
        call2(unname(.y), sym(.x))
      } else {
        call2(expr(identity), sym(.x))
      }
    ) |>
    stats::setNames(names(cols))

  # parse data
  data <- data |>
    dplyr::mutate(!!!parsers) |>
    dplyr::mutate(
      .rowid = dplyr::row_number(),
      .add = FALSE,
      .update = FALSE,
      .delete = FALSE,
      .after = 1L
    )

  # return
  return(data)
}

# get index by id
get_index_by_id <- function(df, id) {
  # safety check
  stopifnot(
    "`df` required" = !missing(df) || !is.data.frame(df),
    "`id` required" = !missing(id)
  )

  # any ids provided?
  if (is_empty(id)) {
    return(c())
  }

  # find idx by id
  idx <- purrr::map_int(
    id,
    ~ {
      idx <- which(df[[1]] == .x)
      if (length(idx) < 1L) {
        abort(sprintf("`id` not in the dataset: %s", .x))
      } else if (length(idx) > 1L) {
        abort(sprintf("`id` not unique: %s", .x))
      }
      idx
    }
  )
  return(idx)
}

# check if value is quasi identical (including NA checks)
is_value_identical <- function(old_value, new_value) {
  old_value_is_na <- is.na(old_value) || nchar(old_value) == 0
  new_value_is_na <- is.na(new_value) || nchar(new_value) == 0
  return(
    identical(new_value, old_value) ||
      (all(old_value_is_na) && all(new_value_is_na))
  )
}

# update data by id or index
update_data <- function(
  df,
  .id,
  .idx = get_index_by_id(df, .id),
  ...,
  .list = NULL
) {
  # safety check
  stopifnot(
    "`df` required" = !missing(df) || !is.data.frame(df),
    "`.idx` needs to be in the dataset" = all(.idx %in% df$.rowid)
  )

  if (length(.idx) > 0) {
    if (!is.null(.list)) {
      updates <- quos(!!!.list)
    } else {
      updates <- enquos(...)
    }
    for (i in seq_along(updates)) {
      for (idx in .idx) {
        old_value <- df[[names(updates)[i]]][idx]
        new_value <- with(df[.idx, ], eval_tidy(updates[[i]]))
        # check if this is a change
        # message(names(updates)[i], ": ", old_value, " / ", new_value, " --> ", is_value_identical(old_value, new_value))
        if (!is_value_identical(old_value, new_value)) {
          df[[names(updates)[i]]][idx] <- new_value
          df$.update[idx] <- TRUE
        }
      }
    }
  }
  return(df)
}

# delete data by id or index
delete_data <- function(df, .id, .idx = get_index_by_id(df, .id)) {
  # safety check
  stopifnot(
    "`df` required" = !missing(df) || !is.data.frame(df),
    "`.idx` needs to be in the dataset" = all(.idx %in% df$.rowid)
  )

  if (length(.idx) > 0) {
    df$.delete[.idx] <- TRUE
  }
  return(df)
}

# data data
add_data <- function(df, ..., .list = NULL) {
  # safety check
  stopifnot(
    "`df` required" = !missing(df) || !is.data.frame(df)
  )

  # add new line
  .idx <- max(df$.rowid) + 1L
  df <- df |>
    dplyr::bind_rows(
      dplyr::tibble(
        .rowid = .idx,
        .add = TRUE,
        .update = FALSE,
        .delete = FALSE
      )
    )

  if (!is.null(.list)) {
    adds <- quos(!!!.list)
  } else {
    adds <- enquos(...)
  }

  for (i in seq_along(adds)) {
    df[.idx, ] <- within(
      df[.idx, ],
      assign(names(adds)[i], eval_tidy(adds[[i]]))
    )
  }

  return(df)
}
