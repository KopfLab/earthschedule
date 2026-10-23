# class QR codes =====
# QR codes that link to the class search page of a class with the department
# logo and the class number in the center (the logo covers part of the code,
# which is readable thanks to the highest error correction level)

# the department and (lowest) course number of a class, e.g. "ERTH" and "1010"
# for "ERTH1010" and "ERTH" and "4185" for the cross-listed "ERTH4185/5185"
# @param class the class (a single class)
parse_class_code <- function(class) {
  class <- stringr::str_to_upper(as.character(class))
  dept <- stringr::str_extract(class, "^\\s*[A-Z]{4}") |> stringr::str_trim()
  numbers <- stringr::str_extract_all(class, "\\d{4}")[[1]]
  if (!rlang::is_string(class) || is.na(dept) || length(numbers) == 0) {
    abort(sprintf("'%s' is not a valid class code (e.g. ERTH1010)", class))
  }
  list(dept = dept, number = min(numbers))
}

# whether a QR code (with the department logo) is available for the classes,
# i.e. whether they are ERTH classes with a course number
# @param classes the classes
has_class_qrcode <- function(classes) {
  purrr::map_lgl(as.character(classes), function(class) {
    code <- tryCatch(parse_class_code(class), error = function(e) NULL)
    !is.null(code) && code$dept == "ERTH"
  })
}

# the name of the QR code of a class (without extension), e.g.
# "ERTH4185_qrcode" for "ERTH4185/5185"
# @param class the class (e.g. "ERTH1010")
get_class_qrcode_name <- function(class) {
  code <- parse_class_code(class)
  paste0(code$dept, code$number, "_qrcode")
}

# the url of the class search page of a class, e.g.
# https://classes.colorado.edu/?keyword=ERTH%201010&subject=ERTH for ERTH1010
# @param class the class (e.g. "ERTH1010")
get_class_search_url <- function(class) {
  code <- parse_class_code(class)
  sprintf(
    "https://classes.colorado.edu/?keyword=%s&subject=%s",
    utils::URLencode(paste(code$dept, code$number), reserved = TRUE),
    utils::URLencode(code$dept, reserved = TRUE)
  )
}

# the department logo with the class number, e.g. "ERTH 4185" for
# "ERTH4185/5185" (all text is outlined so it does not depend on the fonts,
# the template includes "ERTH" and the outlines of the digits)
# @param class the class (e.g. "ERTH1010")
get_class_logo_svg <- function(class) {
  code <- parse_class_code(class)
  if (code$dept != "ERTH") {
    abort(sprintf("the class logo is only available for ERTH classes, not '%s'", class))
  }
  svg <- system.file("qrcode", "class_logo.svg", package = "earthschedule") |>
    readr::read_file()
  digits <- stringr::str_match_all(svg, '<path id="digit-(\\d)" d="([^"]+)"/>')[[1]]
  digits <- rlang::set_names(digits[, 3], digits[, 2])
  for (i in 1:4) {
    svg <- stringr::str_replace(
      svg,
      stringr::fixed(sprintf("{{digit%d}}", i)),
      digits[[stringr::str_sub(code$number, i, i)]]
    )
  }
  return(svg)
}

# create the QR code of a class that links to its class search page with the
# department logo and class number in the center
# @param class the class (e.g. "ERTH1010")
# @param file the file to save the QR code in (.png or .svg), NULL to only
#   return the svg
# @param size the width and height of the png file in pixels
# @param logo_width the width of the logo relative to the QR code
# @param quiet_zone the number of empty modules around the QR code
# @return the svg of the QR code (invisibly)
create_class_qrcode <- function(
  class,
  file = NULL,
  size = 1200,
  logo_width = 0.65,
  quiet_zone = 2
) {
  if (!is.null(file)) {
    ext <- tolower(tools::file_ext(file))
    if (!ext %in% c("png", "svg")) {
      abort("`file` must be a .png or .svg file")
    }
  }

  # QR code modules (without the quiet zone added by qrcode)
  url <- get_class_search_url(class)
  modules <- unclass(qrcode::qr_code(url, ecl = "H"))
  attributes(modules) <- list(dim = dim(modules))
  keep <- which(rowSums(modules) > 0)
  modules <- modules[min(keep):max(keep), min(keep):max(keep)]
  n <- nrow(modules)

  # logo area (in modules, centered), the modules behind it are left out
  logo_w <- n * logo_width
  logo_h <- logo_w * 54 / 260
  logo_x <- (n - logo_w) / 2
  logo_y <- (n - logo_h) / 2
  # (module i covers i-1 to i, with half a module of padding)
  covered <- function(from, to) (floor(from - 0.5) + 1):ceiling(to + 0.5)
  modules[covered(logo_y, logo_y + logo_h), covered(logo_x, logo_x + logo_w)] <-
    FALSE

  # dark modules as one svg path
  dark <- which(modules, arr.ind = TRUE)
  path <- sprintf(
    "M%d %dh1v1h-1z",
    dark[, "col"] - 1L + quiet_zone,
    dark[, "row"] - 1L + quiet_zone
  ) |>
    paste(collapse = "")

  # the logo (nested svg without the xml declaration)
  logo <- get_class_logo_svg(class) |>
    stringr::str_remove("^<\\?xml[^>]*>\\s*") |>
    stringr::str_replace(
      "<svg [^>]*?viewBox",
      sprintf(
        '<svg x="%s" y="%s" width="%s" height="%s" viewBox',
        logo_x + quiet_zone,
        logo_y + quiet_zone,
        logo_w,
        logo_h
      )
    )

  total <- n + 2 * quiet_zone
  svg <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 %d %d" width="%d" height="%d">\n',
      total,
      total,
      size,
      size
    ),
    sprintf('<rect width="%d" height="%d" fill="#fff"/>\n', total, total),
    sprintf('<path fill="#000" shape-rendering="crispEdges" d="%s"/>\n', path),
    logo,
    "\n</svg>\n"
  )

  # save
  if (!is.null(file)) {
    if (ext == "svg") {
      readr::write_file(svg, file)
    } else {
      rsvg::rsvg_png(charToRaw(svg), file, width = size, height = size)
    }
  }
  return(invisible(svg))
}
