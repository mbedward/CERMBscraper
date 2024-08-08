#' Parse the text of an Incident Action Plan document into sections
#'
#' This function has been specifically written to deal with IAP documents but
#' might work with other report formats as well if one or more relevant section
#' names can be provided via the \code{sections} argument.
#'
#' @param doc_path (character) Path and name of the IAP document (or a similarly
#'   structured report). This must be a PDF-format document.
#'
#' @param sections (character) Either \code{NULL} for a pre-defined set of IAP
#'   section headers (default), or one or more names of sections to return. The
#'   default section names are defined in the character vector
#'   \code{'CERMBscraper::IAP_SECTION_NAMES'} which is included with the package.
#'
#' @param doctype (character) Type of documents to process. Options are:
#'   \code{'simple'} (default) to only read standard documents with PDF text
#'   elements; \code{'scanned'} to only read documents containing images of
#'   scanned hard copies; or \code{'all'} to attempt to read all documents.
#'
#' Set to \code{FALSE} (default) to skip
#'   documents containing scanned images; or \code{TRUE} to attempt to retrieve
#'   text using OCR. Scanned image processing often fails, and relies on each
#'   page of the document corresponding to a single page of the hard copy.
#'
#' @param dpi (integer) Resolution to use when processing scanned image
#'   documents (default 300).
#'
#' @return A data frame with columns: section, start_pos, end_pos, text;
#'   or \code{NULL} if the document does not contain any recognized sections.
#'
#' @export
#'
read_iap_sections <- function(doc_path,
                              sections = NULL,
                              doctype = c("simple", "scanned", "all"),
                              dpi = 300) {

  doctype <- match.arg(doctype)

  if (is.null(sections)) {
    sections <- names(CERMBscraper::IAP_SECTION_NAMES)
  } else {
    sections <- match.arg(tolower(sections),
                          names(CERMBscraper::IAP_SECTION_NAMES),
                          several.ok = TRUE)
  }

  if (!file.exists(doc_path)) stop("Can't find the file ", doc_path)

  doc_text <- NULL
  is_text <- is_text_doc(doc_path)

  if (is_text) {
    if (doctype %in% c("simple", "all")) {
      doc_text <- suppressWarnings( tabulapdf::extract_text(doc_path) )
    } else {
      message("...skipping simple PDF document")
    }
  } else {  # scanned image doc
    if (doctype %in% c("scanned", "all")) {
      doc_text <- .do_extract_text_from_image(doc_path)
    } else {
      message("...skipping scanned image document")
    }
  }

  if (is.null(doc_text)) {
    NULL
  } else {
    dat <- .do_split_sections(doc_text)

    if (is.null(dat)) {
      # No recognized sections were found
      NULL
    } else {
      dplyr::filter(dat, section %in% sections)
    }
  }
}


# Helper function to split document text into recognizable sections.
#
# Returns a data frame with columns: section, start_pos, end_pos, text.
#
.do_split_sections <- function(doc_text) {
  # Combine text from the one or more pages
  doc_text <- paste(doc_text, collapse = "\n")

  # Split text into lines
  doc_text <- stringr::str_split(doc_text, "[\\n\\r]+")[[1]]

  # Locate section headers
  iheader <- lapply(CERMBscraper::IAP_SECTION_NAMES, function(ptn) {
    ptn <- paste0("^\\s*", ptn)
    which( stringr::str_detect(doc_text, ptn) )
  })

  # Check for no sections
  if (all(lengths(iheader) == 0)) {
    warning("No recognized sections in document", immediate. = TRUE)
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
  iheader <- c(iheader, 'END_OF_FILE' = length(doc_text) + 1)

  # 4. Create sorted look-up table
  section_lookup <- data.frame(section = names(iheader), start_pos = iheader) %>%
    dplyr::arrange(start_pos)

  # Split text into sections and return as a data frame
  dat <- section_lookup %>%
    dplyr::mutate(end_pos = dplyr::lead(start_pos - 1)) %>%
    dplyr::filter(section != "END_OF_FILE")

  dat$text <- NA_character_

  for (i in seq_len(nrow(dat))) {
    dat$text[i] = paste(doc_text[dat$start_pos[i]:dat$end_pos[i]], collapse = " ")
  }

  # Return the result
  dat
}


#' Detect whether a document contains text elements
#'
#' This function can be used to classify documents into those with standard PDF
#' text elements versus those with no such elements (typically documents
#' containing scanned images).
#'
#' @param doc_path (character) Path and name of the input file. This must be a
#'   PDF-format document.
#'
#' @return \code{TRUE} is the document is made up of scanned images rather than
#'   standard text elements; \code{FALSE} otherwise.
#'
#' @export
#'
is_text_doc <- function(doc_path) {
  txt <- pdftools::pdf_text(doc_path)

  # Return TRUE if any alphanumeric characters were retrieved
  any(stringr::str_detect(txt, "[:alnum:]"))
}


# Helper function to retrieve text from an IAP document that contains a scanned
# image rather than text.
#
.do_extract_text_from_image <- function(doc_path, dpi = 300) {
  num_pages <- pdftools::pdf_info(doc_path)$pages

  image_paths <- tempfile("iap_scan", fileext = rep(".png", num_pages))

  # Convert to high-res image
  pdftools::pdf_convert(doc_path,
                        format = "png",
                        pages = 1:num_pages,
                        filenames = image_paths,
                        dpi = dpi)

  # Scrape text from each image
  ocr_txt <- lapply(1:num_pages, function(ipage) {
    img <- magick::image_read(image_paths[ipage])
    info <- magick::image_info(img)

    # If the image is wider than tall, assume it needs rotating
    if (info$width > info$height) {
      img <- magick::image_rotate(img, degrees = 90)
    }

    # Convert image to grey scale and do OCR
    img %>%
      magick::image_convert(type = "Grayscale") %>%
      tesseract::ocr()
  })

  ocr_txt
}

