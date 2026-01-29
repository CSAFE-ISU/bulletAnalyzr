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
#' parse_filepath(".../Houston Set Final/Group 1/KA/Bullet 1", show_format = TRUE)
parse_filepath <- function(filepath, show_format = FALSE) {
  filepath <- gsub("\\\\", "/", filepath)
  filepath <- sub("/$", "", filepath)

  # Order matters - check more specific patterns before general ones

  if (grepl("Barsto Broached", filepath, fixed = TRUE)) {
    if (show_format) message("Format: bar.<barrel>.<bullet>")
    return(parse_barsto_path(filepath))
  }
  if (grepl("Boxes 1-6 2024", filepath, fixed = TRUE)) {
    if (show_format) message("Format: box1624.<box>.<bullet>")
    return(parse_boxes1624_path(filepath))
  }
  if (grepl("Boxes 1-6", filepath, fixed = TRUE)) {
    if (show_format) message("Format: box16.<box>.<bullet>")
    return(parse_boxes16_path(filepath))
  }
  if (grepl("Carney Study", filepath, fixed = TRUE)) {
    if (show_format) message("Format: car.<bullet>")
    return(parse_carney_path(filepath))
  }
  if (grepl("Clones 224 2", filepath, fixed = TRUE)) {
    if (show_format) message("Format: cln2242.<set>.<barrel>.<bullet>")
    return(parse_clones2242_path(filepath))
  }
  if (grepl("Clones 224", filepath, fixed = TRUE)) {
    if (show_format) message("Format: cln224.<set>.<barrel>.<bullet>")
    return(parse_clones224_path(filepath))
  }
  if (grepl("CSAFE Persistence", filepath, fixed = TRUE)) {
    if (show_format) message("Format: cspsw.<barrel>.<bullet>")
    return(parse_cspsw_path(filepath))
  }
  if (grepl("CTS Forensic Testing Program", filepath, fixed = TRUE)) {
    if (show_format) message("Format: cts.<year>.<item>.<bullet>")
    return(parse_cts_path(filepath))
  }
  if (grepl("DFSC", filepath, fixed = TRUE)) {
    if (show_format) message("Format: dfsc.<brand>.<bullet>")
    return(parse_dfsc_path(filepath))
  }
  if (grepl("Glock GMB BBL", filepath, fixed = TRUE)) {
    if (show_format) message("Format: glck.<material>.<bullet>")
    return(parse_glck_path(filepath))
  }
  if (grepl("Hamby 224 Clone", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hmb224c.<testset>.<barrel>.<bullet>")
    return(parse_hmb224c_path(filepath))
  }
  if (grepl("Hamby 259 Clone Set", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hmb259.<barrel>.<bullet>")
    return(parse_hmb259_path(filepath))
  }
  if (grepl("Hamby Set 44 Final", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hmb44.<barrel>.<bullet>")
    return(parse_hmb44_path(filepath))
  }
  if (grepl("Hamby Set 10", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hmb10.<barrel>.<bullet>")
    return(parse_hmb10_path(filepath))
  }
  if (grepl("Hamby Set 224", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hmb224.<barrel>.<bullet>")
    return(parse_hmb224_path(filepath))
  }
  if (grepl("Hamby Set 36", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hmb36.<barrel>.<bullet>")
    return(parse_hmb36_path(filepath))
  }
  if (grepl("Hamby Set 5", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hmb5.<testset>.<type>.<bullet>")
    return(parse_hmb5_path(filepath))
  }
  if (grepl("Hamby Set X", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hmbx.<testset>.<type>.<bullet>")
    return(parse_hmbx_path(filepath))
  }
  if (grepl("Houston Set 3 Redo", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hst3r.<test>.<barrel>.<bullet>")
    return(parse_hst3r_path(filepath))
  }
  if (grepl("Houston Set 3", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hst3.<test>.<barrel>.<bullet>")
    return(parse_hst3_path(filepath))
  }
  if (grepl("Houston Set Final", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hstfin.<group>.<barrel>.<bullet>")
    return(parse_hstfin_path(filepath))
  }
  if (grepl("Houston 1", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hst1.<barrel>.<bullet>")
    return(parse_hst1_path(filepath))
  }
  if (grepl("Houston NIJ", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hstnij.<barrel>.<bullet>")
    return(parse_hstnij_path(filepath))
  }
  if (grepl("Houston 2023", filepath, fixed = TRUE)) {
    if (show_format) message("Format: hst23.<barrel>.<bullet>")
    return(parse_hst23_path(filepath))
  }
  if (grepl("Phoenix Test", filepath, fixed = TRUE)) {
    if (show_format) message("Format: phx.<gun>.<bullet>")
    return(parse_phx_path(filepath))
  }
  if (grepl("SR Scans", filepath, fixed = TRUE)) {
    if (show_format) message("Format: srs.<box>.<bullet>")
    return(parse_srs_path(filepath))
  }
  if (grepl("St Louis", filepath, fixed = TRUE)) {
    if (show_format) message("Format: stl.<firearm>.<bullet>")
    return(parse_stl_path(filepath))
  }
  if (grepl("Virginia", filepath, fixed = TRUE)) {
    if (show_format) message("Format: vrg.<ammo>.<bullet>")
    return(parse_vrg_path(filepath))
  }

  # Unknown study
  warning(paste("Could not detect study from filepath:", filepath))
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
  # Order matters - check more specific patterns before general ones

  if (grepl("^Barsto Broached", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_barsto_filename(filename))
  }
  if (grepl("^Boxes 1-6 2024", filename)) {
    if (show_format) message("Format: <study>.<box>.<bullet>")
    return(parse_boxes1624_filename(filename))
  }
  if (grepl("^Boxes 1-6", filename)) {
    if (show_format) message("Format: <study>.<box>.<bullet>")
    return(parse_boxes16_filename(filename))
  }
  if (grepl("^Carney Study", filename)) {
    if (show_format) message("Format: <study>.<bullet>")
    return(parse_carney_filename(filename))
  }
  if (grepl("^Clones 224 2", filename)) {
    if (show_format) message("Format: <study>.<set>.<barrel>.<bullet>")
    return(parse_clones2242_filename(filename))
  }
  # HS224 Clone with "Test Set" is Hamby 224 Clone, with just "Set" is Clones 224
  if (grepl("^HS224 Clone.*Test Set", filename)) {
    if (show_format) message("Format: <study>.<testset>.<barrel>.<bullet>")
    return(parse_hmb224c_filename(filename))
  }
  if (grepl("^HS224 Clone", filename)) {
    if (show_format) message("Format: <study>.<set>.<barrel>.<bullet>")
    return(parse_clones224_filename(filename))
  }
  if (grepl("CSAFE Persistence.*SW", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_cspsw_filename(filename))
  }
  if (grepl("^CTS", filename)) {
    if (show_format) message("Format: <study>.<year>.<item>.<bullet>")
    return(parse_cts_filename(filename))
  }
  if (grepl("^DFSC", filename)) {
    if (show_format) message("Format: <study>.<brand>.<bullet>")
    return(parse_dfsc_filename(filename))
  }
  if (grepl("^Glock GMB BBL", filename)) {
    if (show_format) message("Format: <study>.<material>.<bullet>")
    return(parse_glck_filename(filename))
  }
  if (grepl("^HS224 ", filename) || grepl("^HS224-", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_hmb224_filename(filename))
  }
  if (grepl("^HS259", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_hmb259_filename(filename))
  }
  if (grepl("^HS10", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_hmb10_filename(filename))
  }
  if (grepl("^HS36", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_hmb36_filename(filename))
  }
  if (grepl("^HS44", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_hmb44_filename(filename))
  }
  if (grepl("^Hamby Set 5", filename)) {
    if (show_format) message("Format: <study>.<testset>.<type>.<bullet>")
    return(parse_hmb5_filename(filename))
  }
  if (grepl("^Hamby Set X", filename)) {
    if (show_format) message("Format: <study>.<testset>.<type>.<bullet>")
    return(parse_hmbx_filename(filename))
  }
  if (grepl("^Houston Set 3 Redo", filename)) {
    if (show_format) message("Format: <study>.<test>.<barrel>.<bullet>")
    return(parse_hst3r_filename(filename))
  }
  if (grepl("^Houston Set 3", filename)) {
    if (show_format) message("Format: <study>.<test>.<barrel>.<bullet>")
    return(parse_hst3_filename(filename))
  }
  if (grepl("^Houston 1", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_hst1_filename(filename))
  }
  if (grepl("^Houston NIJ", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_hstnij_filename(filename))
  }
  if (grepl("^Houston -", filename)) {
    if (show_format) message("Format: <study>.<barrel>.<bullet>")
    return(parse_hst23_filename(filename))
  }
  if (grepl("^HTX", filename)) {
    if (show_format) message("Format: <study>.<group>.<barrel>.<bullet>")
    return(parse_hstfin_filename(filename))
  }
  if (grepl("^Phoenix", filename)) {
    if (show_format) message("Format: <study>.<gun>.<bullet>")
    return(parse_phx_filename(filename))
  }
  if (grepl("^SR -", filename)) {
    if (show_format) message("Format: <study>.<box>.<bullet>")
    return(parse_srs_filename(filename))
  }
  if (grepl("^St\\. Louis", filename) || grepl("^St Louis", filename)) {
    if (show_format) message("Format: <study>.<firearm>.<bullet>")
    return(parse_stl_filename(filename))
  }
  if (grepl("^VS -", filename)) {
    if (show_format) message("Format: <study>.<ammo>.<bullet>")
    return(parse_vrg_filename(filename))
  }

  # Unknown study
  warning(paste("Could not detect study from filename:", filename))
  return(NA)
}

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

# ============================================================================
# Filepath parsers (for parse_filepath)
# ============================================================================

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
