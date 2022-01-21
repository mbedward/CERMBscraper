#' Read a YAML formatted intel log file as a data frame
#'
#' Rural Fire Service intel log files contain one or more situation reports
#' ("sit reps") describing the status of a given fire and actions being
#' undertaken to control it at a given time. The sequence of report information
#' is presented in YAML format. This function reads a log file and converts the
#' information to tabular format (a data frame) with a column for each of seven
#' attributes: Subject (usually 'Sit Rep'), Priority, Reliability, DateTime,
#' Information (narrative text), Author and Role. In addition, the function
#' extracts grid references from the Information attribute and returns them as
#' an extra column, GridRef. Grid reference values are 6-digit integers, following
#' the RFS convention of reducing a full grid reference (6-digit easting and
#' 7-digit northing) by taking digits 2-4 of the easting and digits 3-5 of the
#' northing, to give a location with 100m precision.
#'
#' @param log_path (character) Path and name of the intel log file.
#'
#' @return A data frame with a column for each of seven attributes plus an extra
#'   column with integer values for abbreviated grid references.
#'
#' @examples
#' \dontrun{
#' dat <- read_intel_log("1234567890_IntelLog.yaml")
#' }
#'
#' @export
#'
read_intel_log <- function(log_path) {
  if (!file.exists(log_path)) stop("Can't find the file ", log_path)

  x <- yaml::read_yaml(log_path)
  x <- lapply(x, as.data.frame)
  dat <- do.call(rbind, x)

  # Extract grid reference digits
  info_col <- match("information", tolower(colnames(dat)))
  if (is.na(info_col)) {
    warning("No information column - can't extract grid references")

  } else {
    # Extract refs plus additional bits
    ptn <- stringr::regex("GR[\\s\\:]*[\\d\\s[:punct:]]+", ignore_case = TRUE)

    x <- stringr::str_extract(dat[[info_col]], ptn) %>%

      # Separate out and concatenate the digits
      stringr::str_replace_all("[^\\d]", "") %>%

      # Just take first 6 digits (in case the grid ref is followed
      # by subsequent digits, e.g. '231456 22 degrees')
      stringr::str_sub(1, 6)

    # Add as new column of integer values
    dat$GridRef <- as.integer(x)
  }

  dat
}

