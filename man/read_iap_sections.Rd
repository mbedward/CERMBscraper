% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_doc_functions.R
\name{read_iap_sections}
\alias{read_iap_sections}
\title{Parse the text of an Incident Action Plan document into sections}
\usage{
read_iap_sections(iap_path, sections = NULL, scanned_images = FALSE, dpi = 300)
}
\arguments{
\item{iap_path}{(character) Path and name of the IAP file. This must be a
PDF-format document.}

\item{sections}{(character) Either \code{NULL} for all sections (default), or
one or more names of sections to return. Options are: 'situation',
'mission', 'execution', 'command', 'safety'. Section names may be
abbreviated and case is ignored.}

\item{scanned_images}{(logical) Set to \code{FALSE} (default) to skip
documents containing scanned images; or \code{TRUE} to attempt to retrieve
text using OCR. Scanned image processing often fails, and relies on each
page of the document corresponding to a single page of the hard copy.}

\item{dpi}{(integer) Resolution to use when processing scanned image
documents (default 300).}
}
\value{
A data frame with columns: section, start_pos, end_pos, text;
  or \code{NULL} if the document does not contain any recognized sections.
}
\description{
Parse the text of an Incident Action Plan document into sections
}
