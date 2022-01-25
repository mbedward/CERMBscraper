#' Parse the text of an Incident Action Plan document into sections
#'
#' @param iap_path (character) Path and name of the IAP file. This must be a
#'   PDF-format document.
#'
#' @param sections (character) Either \code{NULL} for all sections (default), or
#'   one or more names of sections to return. Options are: 'situation',
#'   'mission', 'execution', 'command', 'safety'. Section names may be
#'   abbreviated and case is ignored.
#'
#' @return A data frame with columns: section, start_pos, end_pos, text;
#'   or \code{NULL} if the document does not contain any recognized sections.
#'
#' @export
#'
read_iap_sections <- function(iap_path, sections = NULL) {
  if (is.null(sections)) {
    sections <- names(CERMBscraper::IAP_SECTION_NAMES)
  } else {
    sections <- match.arg(tolower(sections),
                          names(CERMBscraper::IAP_SECTION_NAMES),
                          several.ok = TRUE)
  }

  if (!file.exists(iap_path)) stop("Can't find the file ", log_path)

  # First, try to recover text from the document assuming that it is a
  # standard PDF with text. Use the `tabulizer` package rather than
  # `pdftools` because it can cope with double page formatting that is
  # used in some documents.
  #
  iap_text <- suppressWarnings( tabulizer::extract_text(iap_path) )

  # If we didn't get any text, the document is probably a scanned image rather
  # than a standard PDF.
  if (!any(stringr::str_length(iap_text) > 0)) {
    iap_text <- .do_extract_text_from_image(iap_path)
  }

  # Remove extraneous unicode characters (bullet symbols etc)
  iap_text <- gsub("[^\\x00-\\x7F]+", "", iap_text, perl = TRUE)

  # Partition text into recognizable sections and return the requested ones
  dat <- .do_split_sections(iap_text)

  if (is.null(dat)) {
    # No recognized sections were found
    NULL
  } else {
    dplyr::filter(dat, section %in% sections)
  }
}


# Helper function to split IAP text into recognizable sections.
#
# Returns a data frame with columns: section, start_pos, end_pos, text.
#
.do_split_sections <- function(iap_text) {
  # Combine text from the one or more images and split into lines
  iap_text <- paste(iap_text, collapse = "\n")
  iap_text <- stringr::str_split(iap_text, "[\\n\\r]+")[[1]]

  # Locate section headers
  iheader <- lapply(CERMBscraper::IAP_SECTION_NAMES, function(ptn) {
    ptn <- paste0("^\\s*", ptn)
    which( stringr::str_detect(iap_text, ptn) )
  })

  # Check for no sections
  if (all(lengths(iheader) == 0)) {
    warning("No recognized sections in document")
    return(NULL)
  }

  # Transform the iheader list into a sorted look-up table, allowing for
  # any repeated sections
  #
  # 1. Convert to vector. Any repeated names will have a rep number appended.
  iheader <- unlist(iheader)

  # 2. Remove any rep number suffixes from names.
  names(iheader) <- stringr::str_replace(names(iheader), "\\d+$", "")

  # 3. Add end marker
  iheader <- c(iheader, 'END_OF_FILE' = length(iap_text) + 1)

  # 4. Create sorted look-up table
  section_lookup <- data.frame(section = names(iheader), start_pos = iheader) %>%
    dplyr::arrange(start_pos)

  # Split text into sections and return as a data frame
  dat <- section_lookup %>%
    dplyr::mutate(end_pos = dplyr::lead(start_pos - 1)) %>%
    dplyr::filter(section != "END_OF_FILE")

  dat$text <- NA_character_

  for (i in seq_len(nrow(dat))) {
    dat$text[i] = paste(iap_text[dat$start_pos[i]:dat$end_pos[i]], collapse = " ")
  }

  # Return the result
  dat
}


# Helper function to retrieve text from an IAP document that contains a scanned
# image rather than text.
#
.do_extract_text_from_image <- function(iap_path) {
  num_pages <- pdftools::pdf_info(iap_path)$pages

  image_paths <- tempfile("iap_scan", fileext = rep(".png", num_pages))

  pdftools::pdf_convert(iap_path,
                        format = "png",
                        pages = 1:num_pages,
                        filenames = img_paths,
                        dpi = 600)

  # Scrape text from each image
  ocr_txt <- lapply(1:num_pages, function(ipage) {
    img <- magick::image_read(img_paths[ipage])

    info <- magick::image_info(img)

    # If the image is wider than tall, assume it needs rotating
    if (info$width > info$height) {
      img <- magick::image_rotate(img, degrees = 90)
      magick::image_write(img, path = img_paths[ipage], format = "png")
    }

    tesseract::ocr(img)
  })

  ocr_txt
}

