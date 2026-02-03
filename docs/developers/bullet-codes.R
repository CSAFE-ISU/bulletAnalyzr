# Main Functions ----------------------------------------------------------

#' Parse bullet scan filepath
#'
#' Detects which study a filepath belongs to and parses the directory structure
#' to determine the bullet code. The filepath should point to a bullet directory
#' (not a file), e.g., ".../Houston Set Final/Group 1/KA/Bullet 1".
#'
#' @param filepath Character string. The path to a bullet directory.
#' @param show_format Logical. If TRUE, displays a message showing the format for the study.
#' @return A character string with the bullet code, or NA if study cannot be detected.
#'
#' @examples
#' parse_filepath(".../Barsto Broached/Barrel 1/Bullet 1")
#' parse_filepath(".../Barsto Broached/Barrel 1/Bullet 1", show_format = TRUE)
#' parse_filepath(".../Houston Set Final/Group 1/KA/Bullet 1", show_format = TRUE)
parse_filepath <- function(filepath, show_format = FALSE) {
  filepath <- gsub("\\\\", "/", filepath)
  filepath <- sub("/$", "", filepath)

  studies <- .get_study_config()

  for (s in studies) {
    if (grepl(s$path_pattern, filepath, fixed = TRUE)) {
      if (show_format) message("Format: ", s$format)
      return(s$path_parser(filepath))
    }
  }

  warning("Could not detect study from filepath: ", filepath)
  return(NA)
}

#' Parse bullet scan filename
#'
#' Detects which study a filename belongs to and calls the appropriate parse function.
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @param show_format Logical. If TRUE, displays a message showing the format for the study.
#' @return A character string with the bullet code, or NA if study cannot be detected.
#'
#' @examples
#' parse_filename("Barsto Broached - Barrel 1 - Bullet 2 - Land 1 - ...")
#' parse_filename("HS224 Clone - Test Set 1 - Barrel 1 - Bullet 1 - Land 1 - ...", show_format = TRUE)
parse_filename <- function(filename, show_format = FALSE) {
  studies <- .get_study_config()

  for (s in studies) {
    if (grepl(s$filename_pattern, filename)) {
      if (show_format) message("Format: ", s$format)
      return(s$filename_parser(filename))
    }
  }

  warning("Could not detect study from filename: ", filename)
  return(NA)
}

#' Parse bullet codes into a data frame
#'
#' Takes a vector of bullet codes and creates a data frame with columns for each
#' component. Column names are derived from the study format (e.g., study, barrel,
#' bullet). All bullet codes must be from the same study.
#'
#' @param codes Character vector. Bullet codes in format "study.part1.part2...".
#' @return A data frame with a column for each part of the bullet code.
#'
#' @examples
#' parse_bullet_codes("hmb224c.1.1.1")
#' parse_bullet_codes(c("hmb224c.1.1.1", "hmb224c.1.1.2", "hmb224c.2.1.1"))
#' parse_bullet_codes(c("bar.1.1", "bar.1.2", "bar.2.1"))
parse_bullet_codes <- function(codes) {
  if (length(codes) == 0) {
    return(data.frame())
  }

  # Get study code from first bullet code
  first_study <- strsplit(codes[1], "\\.")[[1]][1]

  # Look up format for this study
  format_str <- .get_format_by_code(first_study)
  if (is.na(format_str)) {
    stop("Unknown study code: ", first_study)
  }

  # Extract column names from format string
  col_names <- .parse_format_columns(format_str)

  # Parse each bullet code
  rows <- lapply(codes, function(code) {
    parts <- strsplit(code, "\\.")[[1]]
    if (length(parts) != length(col_names)) {
      warning("Bullet code '", code, "' has ", length(parts),
              " parts but expected ", length(col_names))
      parts <- c(parts, rep(NA, length(col_names) - length(parts)))
      parts <- parts[1:length(col_names)]
    }
    setNames(as.list(parts), col_names)
  })

  do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE))
}


# Study Configuration -----------------------------------------------------

.study_cache <- new.env(parent = emptyenv())

#' Get unified study configuration
#'
#' Returns a list of study configurations with patterns and parsers for both
#' filepath and filename parsing. Results are cached for performance.
#' Order matters - more specific patterns must come before general ones.
#'
#' @return A list of study configuration lists, each containing path_pattern,
#'   filename_pattern, path_parser, filename_parser, and format.
#' @keywords internal
.get_study_config <- function() {
  if (!exists("studies", envir = .study_cache)) {
    .study_cache$studies <- list(
      list(path_pattern = "Barsto Broached",              filename_pattern = "^Barsto Broached",       path_parser = parse_barsto_path,    filename_parser = parse_barsto_filename,    format = "bar.<barrel>.<bullet>"),
      list(path_pattern = "Boxes 1-6 2024",               filename_pattern = "^Boxes 1-6 2024",        path_parser = parse_boxes1624_path, filename_parser = parse_boxes1624_filename, format = "box1624.<box>.<bullet>"),
      list(path_pattern = "Boxes 1-6",                    filename_pattern = "^Boxes 1-6",             path_parser = parse_boxes16_path,   filename_parser = parse_boxes16_filename,   format = "box16.<box>.<bullet>"),
      list(path_pattern = "Carney Study",                 filename_pattern = "^Carney Study",          path_parser = parse_carney_path,    filename_parser = parse_carney_filename,    format = "car.<bullet>"),
      list(path_pattern = "Clones 224 2",                 filename_pattern = "^Clones 224 2",          path_parser = parse_clones2242_path,filename_parser = parse_clones2242_filename,format = "cln2242.<set>.<barrel>.<bullet>"),
      list(path_pattern = "Hamby 224 Clone",              filename_pattern = "^HS224 Clone.*Test Set", path_parser = parse_hmb224c_path,   filename_parser = parse_hmb224c_filename,   format = "hmb224c.<testset>.<barrel>.<bullet>"),
      list(path_pattern = "Clones 224",                   filename_pattern = "^HS224 Clone",           path_parser = parse_clones224_path, filename_parser = parse_clones224_filename, format = "cln224.<set>.<barrel>.<bullet>"),
      list(path_pattern = "CSAFE Persistence",            filename_pattern = "CSAFE Persistence.*SW",  path_parser = parse_cspsw_path,     filename_parser = parse_cspsw_filename,     format = "cspsw.<barrel>.<bullet>"),
      list(path_pattern = "CTS Forensic Testing Program", filename_pattern = "^CTS",                   path_parser = parse_cts_path,       filename_parser = parse_cts_filename,       format = "cts.<year>.<item>.<bullet>"),
      list(path_pattern = "DFSC",                         filename_pattern = "^DFSC",                  path_parser = parse_dfsc_path,      filename_parser = parse_dfsc_filename,      format = "dfsc.<brand>.<bullet>"),
      list(path_pattern = "Glock GMB BBL",                filename_pattern = "^Glock GMB BBL",         path_parser = parse_glck_path,      filename_parser = parse_glck_filename,      format = "glck.<material>.<bullet>"),
      list(path_pattern = "Hamby Set 224",                filename_pattern = "^HS224 |^HS224-",        path_parser = parse_hmb224_path,    filename_parser = parse_hmb224_filename,    format = "hmb224.<barrel>.<bullet>"),
      list(path_pattern = "Hamby 259 Clone Set",          filename_pattern = "^HS259",                 path_parser = parse_hmb259_path,    filename_parser = parse_hmb259_filename,    format = "hmb259.<barrel>.<bullet>"),
      list(path_pattern = "Hamby Set 10",                 filename_pattern = "^HS10",                  path_parser = parse_hmb10_path,     filename_parser = parse_hmb10_filename,     format = "hmb10.<barrel>.<bullet>"),
      list(path_pattern = "Hamby Set 36",                 filename_pattern = "^HS36",                  path_parser = parse_hmb36_path,     filename_parser = parse_hmb36_filename,     format = "hmb36.<barrel>.<bullet>"),
      list(path_pattern = "Hamby Set 44 Final",           filename_pattern = "^HS44",                  path_parser = parse_hmb44_path,     filename_parser = parse_hmb44_filename,     format = "hmb44.<barrel>.<bullet>"),
      list(path_pattern = "Hamby Set 5",                  filename_pattern = "^Hamby Set 5",           path_parser = parse_hmb5_path,      filename_parser = parse_hmb5_filename,      format = "hmb5.<testset>.<type>.<bullet>"),
      list(path_pattern = "Hamby Set X",                  filename_pattern = "^Hamby Set X",           path_parser = parse_hmbx_path,      filename_parser = parse_hmbx_filename,      format = "hmbx.<testset>.<type>.<bullet>"),
      list(path_pattern = "Houston Set 3 Redo",           filename_pattern = "^Houston Set 3 Redo",    path_parser = parse_hst3r_path,     filename_parser = parse_hst3r_filename,     format = "hst3r.<test>.<barrel>.<bullet>"),
      list(path_pattern = "Houston Set 3",                filename_pattern = "^Houston Set 3",         path_parser = parse_hst3_path,      filename_parser = parse_hst3_filename,      format = "hst3.<test>.<barrel>.<bullet>"),
      list(path_pattern = "Houston Set Final",            filename_pattern = "^HTX",                   path_parser = parse_hstfin_path,    filename_parser = parse_hstfin_filename,    format = "hstfin.<group>.<barrel>.<bullet>"),
      list(path_pattern = "Houston 1",                    filename_pattern = "^Houston 1",             path_parser = parse_hst1_path,      filename_parser = parse_hst1_filename,      format = "hst1.<barrel>.<bullet>"),
      list(path_pattern = "Houston NIJ",                  filename_pattern = "^Houston NIJ",           path_parser = parse_hstnij_path,    filename_parser = parse_hstnij_filename,    format = "hstnij.<barrel>.<bullet>"),
      list(path_pattern = "Houston 2023",                 filename_pattern = "^Houston -",             path_parser = parse_hst23_path,     filename_parser = parse_hst23_filename,     format = "hst23.<barrel>.<bullet>"),
      list(path_pattern = "Phoenix Test",                 filename_pattern = "^Phoenix",               path_parser = parse_phx_path,       filename_parser = parse_phx_filename,       format = "phx.<gun>.<bullet>"),
      list(path_pattern = "SR Scans",                     filename_pattern = "^SR -",                  path_parser = parse_srs_path,       filename_parser = parse_srs_filename,       format = "srs.<box>.<bullet>"),
      list(path_pattern = "St Louis",                     filename_pattern = "^St\\.? Louis",          path_parser = parse_stl_path,       filename_parser = parse_stl_filename,       format = "stl.<firearm>.<bullet>"),
      list(path_pattern = "Virginia",                     filename_pattern = "^VS -",                  path_parser = parse_vrg_path,       filename_parser = parse_vrg_filename,       format = "vrg.<ammo>.<bullet>")
    )
  }
  .study_cache$studies
}

#' Get bullet code format for a study
#'
#' @param study_name Character string. The name of the study.
#' @return A character string with the bullet code format, or NA if not found.
#'
#' @examples
#' get_format("Barsto Broached")
#' get_format("Houston Set Final")
get_format <- function(study_name) {
  studies <- .get_study_config()
  for (s in studies) {
    if (s$path_pattern == study_name) return(s$format)
  }
  NA
}


# Internal Helpers --------------------------------------------------------

#' Extract relative path components after a study directory
#'
#' @param filepath Character string. Full filepath.
#' @param study_name Character string. The study directory name to find in the path.
#' @return Character vector of path components after the study directory.
#' @keywords internal
.get_path_parts <- function(filepath, study_name) {
  idx <- regexpr(study_name, filepath, fixed = TRUE)
  after <- substring(filepath, idx + nchar(study_name))
  after <- sub("^/", "", after)
  parts <- strsplit(after, "/")[[1]]
  parts[nchar(parts) > 0]
}

#' Parse barrel/bullet directory pattern shared by multiple studies
#'
#' Handles paths with structure: Barrel #/Bullet # or Unknown(s)/Bullet #
#'
#' @param filepath Character string. Full filepath.
#' @param study_name Character string. The study directory name.
#' @param study_code Character string. The short study code prefix.
#' @return A character string with the bullet code.
#' @keywords internal
.parse_barrel_bullet_path <- function(filepath, study_name, study_code) {
  parts <- .get_path_parts(filepath, study_name)
  if (grepl("^Unknown", parts[1])) {
    barrel <- "U"
  } else {
    barrel <- sub("Barrel ", "", parts[1])
  }
  bullet <- gsub("'", "", sub("Bullet ", "", parts[2]))
  paste(study_code, barrel, bullet, sep = ".")
}

#' Get format string by study code
#'
#' Looks up the format string for a study given its short code (e.g., "hmb224c").
#'
#' @param study_code Character string. The study code prefix (first part of bullet code).
#' @return A character string with the format, or NA if not found.
#' @keywords internal
.get_format_by_code <- function(study_code) {
  studies <- .get_study_config()
  for (s in studies) {
    # Extract the study code from the format string (part before first .)
    format_code <- strsplit(s$format, "\\.")[[1]][1]
    if (format_code == study_code) {
      return(s$format)
    }
  }
  NA
}

#' Parse format string into column names
#'
#' Extracts column names from a format string. The first part becomes "study",
#' and parts in angle brackets become their contained names.
#'
#' @param format_str Character string. Format like "hmb224c.<testset>.<barrel>.<bullet>".
#' @return Character vector of column names.
#' @keywords internal
.parse_format_columns <- function(format_str) {
  parts <- strsplit(format_str, "\\.")[[1]]
  col_names <- character(length(parts))
  col_names[1] <- "study"
  for (i in 2:length(parts)) {
    # Extract name from angle brackets
    col_names[i] <- gsub("[<>]", "", parts[i])
  }
  col_names
}


# Filename Parsers --------------------------------------------------------

#' Parse Barsto Broached filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format bar.<barrel>.<bullet>
parse_barsto_filename <- function(filename) {
  parts <- strsplit(filename, " - ")[[1]]
  study <- "bar"
  barrel <- as.integer(gsub("Barrel ", "", parts[2]))
  bullet <- as.integer(gsub("Bullet ", "", parts[3]))
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Boxes 1-6 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format box16.<box>.<bullet>
parse_boxes16_filename <- function(filename) {
  study <- "box16"
  box <- as.integer(sub(".*Box (\\d+).*", "\\1", filename))
  bullet <- as.integer(sub(".*Bullet (\\d+).*", "\\1", filename))
  return(paste(study, box, bullet, sep = "."))
}

#' Parse Boxes 1-6 2024 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format box1624.<box>.<bullet>
parse_boxes1624_filename <- function(filename) {
  study <- "box1624"
  box <- as.integer(sub(".*Box (\\d+).*", "\\1", filename))
  bullet <- as.integer(sub(".*Bullet (\\d+).*", "\\1", filename))
  return(paste(study, box, bullet, sep = "."))
}

#' Parse Carney Study filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format car.<bullet>
parse_carney_filename <- function(filename) {
  study <- "car"
  bullet <- sub(".*Bullet (\\d+[A-Z]).*", "\\1", filename)
  return(paste(study, bullet, sep = "."))
}

#' Parse Clones 224 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format cln224.<set>.<barrel>.<bullet>
parse_clones224_filename <- function(filename) {
  study <- "cln224"
  set_num <- sub(".*Set (\\d+).*", "\\1", filename)
  if (grepl("Unknown - Bullet", filename)) {
    barrel <- "U"
  } else {
    barrel <- sub(".*Barrel (\\d+).*", "\\1", filename)
  }
  bullet <- sub(".*Bullet ([A-Z0-9]+).*", "\\1", filename)
  return(paste(study, set_num, barrel, bullet, sep = "."))
}

#' Parse Clones 224 2 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format cln2242.<set>.<barrel>.<bullet>
parse_clones2242_filename <- function(filename) {
  study <- "cln2242"
  set_num <- sub(".*Set (\\d+).*", "\\1", filename)
  if (grepl("Barrel Unknown|Unknown - Bullet", filename)) {
    barrel <- "U"
  } else {
    barrel <- sub(".*Barrel (\\d+).*", "\\1", filename)
  }
  bullet <- sub(".*Bullet ([A-Z0-9]+).*", "\\1", filename)
  return(paste(study, set_num, barrel, bullet, sep = "."))
}

#' Parse CSAFE Persistence SW filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format cspsw.<barrel>.<bullet>
parse_cspsw_filename <- function(filename) {
  study <- "cspsw"
  barrel <- as.integer(sub(".*- SW (\\d+) -.*", "\\1", filename))
  bullet <- as.integer(sub(".*Bullet (\\d+).*", "\\1", filename))
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse CTS Forensic Testing Program filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format cts.<year>.<item>.<bullet>
parse_cts_filename <- function(filename) {
  study <- "cts"
  year <- sub(".*- (\\d{2})-(526|527|5261|5262) -.*", "\\1", filename)
  item <- as.integer(sub(".*Item (\\d+).*", "\\1", filename))
  bullet <- sub(".*Bullet ([A-Z]).*", "\\1", filename)
  return(paste(study, year, item, bullet, sep = "."))
}

#' Parse DFSC filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format dfsc.<brand>.<bullet>
parse_dfsc_filename <- function(filename) {
  study <- "dfsc"
  if (grepl("Ruger", filename)) {
    brand <- "ru"
  } else if (grepl("Hi-Point", filename)) {
    brand <- "hi"
  } else if (grepl("Springfield", filename)) {
    brand <- "sp"
  } else if (grepl("Glock", filename)) {
    brand <- "gl"
  } else if (grepl("Comparison Set 1", filename)) {
    brand <- "c1"
  } else if (grepl("Comparison Set 2", filename)) {
    brand <- "c2"
  } else if (grepl("Comparison Set 3", filename)) {
    brand <- "c3"
  } else {
    brand <- "unknown"
  }
  if (grepl("- IQ", filename, ignore.case = TRUE)) {
    bullet <- "IQ"
  } else if (grepl("3[Kk] Bullet", filename)) {
    bullet <- sub(".*3[Kk] Bullet (\\d+).*", "\\1", filename)
  } else {
    bullet <- sub(".*Bullet (\\d+).*", "\\1", filename)
  }
  return(paste(study, brand, bullet, sep = "."))
}

#' Parse Glock GMB BBL filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format glck.<material>.<bullet>
parse_glck_filename <- function(filename) {
  study <- "glck"
  if (grepl("Brass", filename)) {
    material <- "b"
  } else if (grepl("Copper", filename)) {
    material <- "c"
  } else {
    material <- "unknown"
  }
  bullet <- as.integer(sub(".*Bullet (\\d+).*", "\\1", filename))
  return(paste(study, material, bullet, sep = "."))
}

#' Parse Hamby Set 10 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hmb10.<barrel>.<bullet>
parse_hmb10_filename <- function(filename) {
  study <- "hmb10"
  if (grepl("Unknown - Bullet", filename)) {
    barrel <- "U"
  } else {
    barrel <- sub(".*Barrel (\\d+).*", "\\1", filename)
  }
  bullet <- sub(".*Bullet ([A-Z0-9]+).*", "\\1", filename)
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Hamby Set 224 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hmb224.<barrel>.<bullet>
parse_hmb224_filename <- function(filename) {
  study <- "hmb224"
  if (grepl("Unknown - Bullet", filename)) {
    barrel <- "U"
  } else {
    barrel <- sub(".*Barrel (\\d+).*", "\\1", filename)
  }
  bullet <- sub(".*Bullet ([A-Z0-9]+).*", "\\1", filename)
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Hamby 224 Clone filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hmb224c.<testset>.<barrel>.<bullet>
parse_hmb224c_filename <- function(filename) {
  study <- "hmb224c"
  test_set <- sub(".*Test Set (\\d+).*", "\\1", filename)
  if (grepl("Barrel \\d+", filename)) {
    barrel <- sub(".*Barrel (\\d+).*", "\\1", filename)
  } else {
    barrel <- "U"
  }
  bullet <- sub(".*Bullet ([A-Z0-9]+).*", "\\1", filename)
  return(paste(study, test_set, barrel, bullet, sep = "."))
}

#' Parse Hamby 259 Clone Set filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hmb259.<barrel>.<bullet>
parse_hmb259_filename <- function(filename) {
  study <- "hmb259"
  barrel <- sub(".*Barrel (\\d+).*", "\\1", filename)
  bullet <- sub(".*Bullet (\\d+).*", "\\1", filename)
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Hamby Set 36 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hmb36.<barrel>.<bullet>
parse_hmb36_filename <- function(filename) {
  study <- "hmb36"
  if (grepl("Unknown - Bullet", filename)) {
    barrel <- "U"
  } else {
    barrel <- sub(".*Barrel (\\d+).*", "\\1", filename)
  }
  bullet <- sub(".*Bullet ([A-Z0-9]+).*", "\\1", filename)
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Hamby Set 44 Final filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hmb44.<barrel>.<bullet>
parse_hmb44_filename <- function(filename) {
  study <- "hmb44"
  if (grepl("Unknown - Bullet", filename)) {
    barrel <- "U"
  } else {
    barrel <- sub(".*Barrel (\\d+).*", "\\1", filename)
  }
  bullet <- sub(".*Bullet ?'?([A-Z0-9]+)'?.*", "\\1", filename)
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Hamby Set 5 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hmb5.<testset>.<type>.<bullet>
parse_hmb5_filename <- function(filename) {
  study <- "hmb5"
  test_set <- sub(".*Test Set (\\d+).*", "\\1", filename)
  if (grepl("Known", filename)) {
    type <- "K"
  } else if (grepl("Questioned", filename)) {
    type <- "Q"
  } else {
    type <- "U"
  }
  bullet <- sub(".*Bullet ([A-Z0-9]+).*", "\\1", filename)
  return(paste(study, test_set, type, bullet, sep = "."))
}

#' Parse Hamby Set X filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hmbx.<testset>.<type>.<bullet>
parse_hmbx_filename <- function(filename) {
  study <- "hmbx"
  test_set <- sub(".*Test Set (\\d+).*", "\\1", filename)
  if (grepl("Known", filename)) {
    type <- "K"
  } else if (grepl("Questioned", filename)) {
    type <- "Q"
  } else {
    type <- "U"
  }
  bullet <- sub(".*Bullet ([A-Z0-9]+).*", "\\1", filename)
  return(paste(study, test_set, type, bullet, sep = "."))
}

#' Parse Houston 1 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hst1.<barrel>.<bullet>
parse_hst1_filename <- function(filename) {
  study <- "hst1"
  if (grepl("Random Barrel", filename)) {
    barrel_match <- regmatches(filename, regexpr("Random Barrel [0-9]+", filename))
    barrel_num <- gsub("Random Barrel ", "", barrel_match)
    barrel <- paste0("R", barrel_num)
  } else {
    barrel_match <- regmatches(filename, regexpr("Barrel [A-Z]", filename))
    barrel <- gsub("Barrel ", "", barrel_match)
  }
  bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
  bullet <- gsub("Bullet ", "", bullet_match)
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Houston 2023 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hst23.<barrel>.<bullet>
parse_hst23_filename <- function(filename) {
  study <- "hst23"
  barrel_match <- regmatches(filename, regexpr("Barrel [A-Z]", filename))
  barrel <- gsub("Barrel ", "", barrel_match)
  bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
  bullet <- gsub("Bullet ", "", bullet_match)
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Houston Set 3 filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hst3.<test>.<barrel>.<bullet>
parse_hst3_filename <- function(filename) {
  study <- "hst3"
  test_match <- regmatches(filename, regexpr("Test [A-Z]", filename))
  test <- gsub("Test ", "", test_match)
  if (grepl("Barrel Unknown", filename)) {
    barrel <- "U"
  } else {
    barrel_match <- regmatches(filename, regexpr("Barrel [A-Z]", filename))
    barrel <- gsub("Barrel ", "", barrel_match)
  }
  bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
  bullet <- gsub("Bullet ", "", bullet_match)
  return(paste(study, test, barrel, bullet, sep = "."))
}

#' Parse Houston Set 3 Redo filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hst3r.<test>.<barrel>.<bullet>
parse_hst3r_filename <- function(filename) {
  study <- "hst3r"
  test_match <- regmatches(filename, regexpr("Test [A-Z]", filename))
  test <- gsub("Test ", "", test_match)
  if (grepl("Barrel Unknown", filename)) {
    barrel <- "U"
  } else {
    barrel_match <- regmatches(filename, regexpr("Barrel [A-Z]", filename))
    barrel <- gsub("Barrel ", "", barrel_match)
  }
  bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
  bullet <- gsub("Bullet ", "", bullet_match)
  return(paste(study, test, barrel, bullet, sep = "."))
}

#' Parse Houston Set Final filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hstfin.<group>.<barrel>.<bullet>
parse_hstfin_filename <- function(filename) {
  study <- "hstfin"
  group_match <- regmatches(filename, regexpr("Group [0-9]+", filename))
  group <- gsub("Group ", "", group_match)
  if (grepl(" - U[0-9]+ - ", filename)) {
    barrel <- "U"
    bullet_match <- regmatches(filename, regexpr(" - U[0-9]+ - ", filename))
    bullet <- gsub(" - ", "", bullet_match)
  } else if (grepl("K[A-Z] Bullet [0-9]+", filename)) {
    combo_match <- regmatches(filename, regexpr("K[A-Z] Bullet [0-9]+", filename))
    barrel <- sub(" Bullet.*", "", combo_match)
    bullet <- sub(".*Bullet ", "", combo_match)
  } else if (grepl("Kit K[A-Z]", filename)) {
    kit_match <- regmatches(filename, regexpr("Kit K[A-Z]", filename))
    barrel <- gsub("Kit ", "", kit_match)
    bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
    bullet <- gsub("Bullet ", "", bullet_match)
  } else {
    barrel <- "UNK"
    bullet <- "UNK"
  }
  return(paste(study, group, barrel, bullet, sep = "."))
}

#' Parse Houston NIJ filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format hstnij.<barrel>.<bullet>
parse_hstnij_filename <- function(filename) {
  study <- "hstnij"
  barrel_match <- regmatches(filename, regexpr("Houston NIJ- [0-9]+", filename))
  barrel <- gsub("Houston NIJ- ", "", barrel_match)
  bullet_match <- regmatches(filename, regexpr("Bullet [A-Z]", filename))
  bullet <- gsub("Bullet ", "", bullet_match)
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse Phoenix Test filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format phx.<gun>.<bullet>
parse_phx_filename <- function(filename) {
  study <- "phx"
  if (grepl("Unknown", filename)) {
    unknown_match <- regmatches(filename, regexpr("Unknown 1-[A-Z]", filename))
    bullet_id <- gsub("Unknown 1-", "", unknown_match)
    barrel <- "U"
    bullet <- bullet_id
  } else {
    gun_match <- regmatches(filename, regexpr("Gun 1-[A-Z][0-9]+", filename))
    barrel <- gsub("Gun 1-", "", gun_match)
    bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
    bullet <- gsub("Bullet ", "", bullet_match)
  }
  return(paste(study, barrel, bullet, sep = "."))
}

#' Parse SR Scans filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format srs.<box>.<bullet>
parse_srs_filename <- function(filename) {
  study <- "srs"
  box_match <- regmatches(filename, regexpr("Box [0-9]+", filename))
  box <- gsub("Box ", "", box_match)
  bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
  bullet <- gsub("Bullet ", "", bullet_match)
  return(paste(study, box, bullet, sep = "."))
}

#' Parse St Louis filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format stl.<firearm>.<bullet>
parse_stl_filename <- function(filename) {
  study <- "stl"
  firearm_match <- regmatches(filename, regexpr("Firearm [A-Z]", filename))
  firearm <- gsub("Firearm ", "", firearm_match)
  bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
  bullet <- gsub("Bullet ", "", bullet_match)
  return(paste(study, firearm, bullet, sep = "."))
}

#' Parse Virginia filename
#'
#' @param filename Character string. The basename of the file (not the full path).
#' @return A character string with the bullet code in format vrg.<ammo>.<bullet>
parse_vrg_filename <- function(filename) {
  study <- "vrg"
  ammo_match <- regmatches(filename, regexpr("VS - [0-9]+", filename))
  ammo_num <- gsub("VS - ", "", ammo_match)
  ammo_map <- c("1" = "b", "2" = "f", "3" = "p", "5" = "t", "7" = "s")
  ammo <- ammo_map[ammo_num]
  bullet_match <- regmatches(filename, regexpr("Bullet [0-9]+", filename))
  bullet <- gsub("Bullet ", "", bullet_match)
  return(paste(study, ammo, bullet, sep = "."))
}


# Filepath Parsers --------------------------------------------------------

#' @rdname parse_filepath
parse_barsto_path <- function(filepath) {
  .parse_barrel_bullet_path(filepath, "Barsto Broached", "bar")
}

#' @rdname parse_filepath
parse_boxes16_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Boxes 1-6")
  box <- as.integer(sub("Box ", "", parts[1]))
  bullet <- as.integer(sub("Bullet ", "", parts[2]))
  paste("box16", box, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_boxes1624_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Boxes 1-6 2024")
  box <- as.integer(sub("Box ", "", parts[1]))
  bullet <- as.integer(sub("Bullet ", "", parts[2]))
  paste("box1624", box, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_carney_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Carney Study")
  bullet <- sub("Bullet ", "", parts[length(parts)])
  paste("car", bullet, sep = ".")
}

#' @rdname parse_filepath
parse_clones224_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Clones 224")
  set_num <- sub("Set ", "", parts[1])
  if (grepl("^Unknown", parts[2])) {
    barrel <- "U"
  } else {
    barrel <- sub("Barrel ", "", parts[2])
  }
  bullet <- sub("Bullet ", "", parts[3])
  paste("cln224", set_num, barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_clones2242_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Clones 224 2")
  set_num <- sub("Set ", "", parts[1])
  if (grepl("^Unknown", parts[2])) {
    barrel <- "U"
  } else {
    barrel <- sub("Barrel ", "", parts[2])
  }
  bullet <- sub("Bullet ", "", parts[3])
  paste("cln2242", set_num, barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_cspsw_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "CSAFE Persistence")
  # parts[1] = "SW", parts[2] = "SW #", parts[3] = "Bullet #"
  barrel <- as.integer(sub("SW ", "", parts[2]))
  bullet <- as.integer(sub("Bullet ", "", parts[3]))
  paste("cspsw", barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_cts_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "CTS Forensic Testing Program")
  # parts[1] = "test_18-526_sample_F1", parts[2] = "Item #", parts[3] = "Bullet #"
  year <- sub("test_(\\d+)-.*", "\\1", parts[1])
  item <- as.integer(sub("Item ", "", parts[2]))
  if (length(parts) >= 3) {
    bullet <- sub("Bullet ", "", parts[3])
  } else {
    bullet <- NA
  }
  paste("cts", year, item, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_dfsc_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "DFSC")
  if (grepl("^Comparison Set", parts[1])) {
    set_num <- sub("Comparison Set ", "", parts[1])
    brand <- paste0("c", set_num)
    if (parts[2] == "1Q") {
      bullet <- "IQ"
    } else {
      bullet <- sub("Bullet ", "", parts[3])
    }
  } else {
    dir_name <- parts[1]
    if (grepl("Ruger", dir_name)) brand <- "ru"
    else if (grepl("Hi-Point", dir_name)) brand <- "hi"
    else if (grepl("Springfield", dir_name)) brand <- "sp"
    else if (grepl("Glock", dir_name)) brand <- "gl"
    else brand <- "unknown"
    bullet <- sub("Bullet ", "", parts[2])
  }
  paste("dfsc", brand, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_glck_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Glock GMB BBL")
  if (grepl("Brass", parts[1])) material <- "b"
  else if (grepl("Copper", parts[1])) material <- "c"
  else material <- "unknown"
  bullet <- as.integer(sub("Bullet ", "", parts[2]))
  paste("glck", material, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hmb224c_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Hamby 224 Clone")
  test_set <- sub("Test Set ", "", parts[1])
  if (length(parts) >= 3 && grepl("^Barrel", parts[2])) {
    barrel <- sub("Barrel ", "", parts[2])
    bullet <- sub("Bullet ", "", parts[3])
  } else {
    barrel <- "U"
    bullet <- sub("Bullet ", "", parts[2])
  }
  paste("hmb224c", test_set, barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hmb259_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Hamby 259 Clone Set")
  barrel_idx <- grep("^Barrel", parts)
  bullet_idx <- grep("^Bullet", parts)
  barrel <- sub("Barrel ", "", parts[barrel_idx[1]])
  bullet <- sub("Bullet ", "", parts[bullet_idx[1]])
  paste("hmb259", barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hmb10_path <- function(filepath) {
  .parse_barrel_bullet_path(filepath, "Hamby Set 10", "hmb10")
}

#' @rdname parse_filepath
parse_hmb224_path <- function(filepath) {
  .parse_barrel_bullet_path(filepath, "Hamby Set 224", "hmb224")
}

#' @rdname parse_filepath
parse_hmb36_path <- function(filepath) {
  .parse_barrel_bullet_path(filepath, "Hamby Set 36", "hmb36")
}

#' @rdname parse_filepath
parse_hmb44_path <- function(filepath) {
  .parse_barrel_bullet_path(filepath, "Hamby Set 44 Final", "hmb44")
}

#' @rdname parse_filepath
parse_hmb5_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Hamby Set 5")
  test_set <- sub("Test Set ", "", parts[1])
  if (grepl("Known", parts[2])) type <- "K"
  else if (grepl("Questioned", parts[2])) type <- "Q"
  else type <- "U"
  bullet <- sub("Bullet ", "", parts[3])
  paste("hmb5", test_set, type, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hmbx_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Hamby Set X")
  test_set <- sub("Test Set ", "", parts[1])
  if (grepl("Known", parts[2])) type <- "K"
  else if (grepl("Questioned", parts[2])) type <- "Q"
  else type <- "U"
  bullet <- sub("Bullet ", "", parts[3])
  paste("hmbx", test_set, type, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hst1_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Houston 1")
  if (grepl("^Random Barrel", parts[1])) {
    barrel <- paste0("R", sub("Random Barrel ", "", parts[1]))
  } else {
    barrel <- sub("Barrel ", "", parts[1])
  }
  bullet <- sub("Bullet ", "", parts[2])
  paste("hst1", barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hst23_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Houston 2023")
  barrel <- sub("Barrel ", "", parts[1])
  bullet <- sub("Bullet ", "", parts[2])
  paste("hst23", barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hstnij_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Houston NIJ")
  barrel <- parts[1]
  bullet <- parts[2]
  paste("hstnij", barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hst3_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Houston Set 3")
  test <- sub("Test ", "", parts[1])
  if (grepl("^Unknown", parts[2])) {
    barrel <- "U"
  } else {
    barrel <- sub("Barrel ", "", parts[2])
  }
  bullet <- sub("Bullet ", "", parts[3])
  paste("hst3", test, barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hst3r_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Houston Set 3 Redo")
  test <- sub("Test ", "", parts[1])
  if (grepl("^Unknown", parts[2])) {
    barrel <- "U"
  } else {
    barrel <- sub("Barrel ", "", parts[2])
  }
  bullet <- sub("Bullet ", "", parts[3])
  paste("hst3r", test, barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_hstfin_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Houston Set Final")
  group <- sub("Group ?", "", parts[1])
  if (grepl("^Unknown", parts[2])) {
    barrel <- "U"
    bullet <- parts[3]
  } else {
    barrel <- parts[2]
    bullet <- sub("Bullet ", "", parts[3])
  }
  paste("hstfin", group, barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_phx_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Phoenix Test")
  if (grepl("^Unknown", parts[1])) {
    barrel <- "U"
    bullet <- sub("Unknown 1-", "", parts[1])
  } else {
    barrel <- sub("Gun 1-", "", parts[1])
    bullet <- sub("Bullet ", "", parts[2])
  }
  paste("phx", barrel, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_srs_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "SR Scans")
  box <- sub("Box ", "", parts[1])
  bullet <- sub("Bullet ", "", parts[2])
  paste("srs", box, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_stl_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "St Louis")
  firearm <- sub("Firearm ", "", parts[1])
  bullet <- sub("Bullet ", "", parts[2])
  paste("stl", firearm, bullet, sep = ".")
}

#' @rdname parse_filepath
parse_vrg_path <- function(filepath) {
  parts <- .get_path_parts(filepath, "Virginia")
  ammo_num <- sub("^(\\d+)-.*", "\\1", parts[1])
  ammo_map <- c("1" = "b", "2" = "f", "3" = "p", "5" = "t", "7" = "s")
  ammo <- ammo_map[ammo_num]
  bullet <- sub("Bullet ", "", parts[2])
  paste("vrg", ammo, bullet, sep = ".")
}