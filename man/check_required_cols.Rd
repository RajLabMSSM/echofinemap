% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_required_cols.R
\name{check_required_cols}
\alias{check_required_cols}
\title{Check for necessary columns}
\usage{
check_required_cols(
  dat,
  finemap_methods = NULL,
  dataset_type = "GWAS",
  verbose = TRUE
)
}
\arguments{
\item{dat}{\link[data.table]{data.table} containing SNP-level 
data to fine-map.}

\item{finemap_methods}{Fine-mapping methods to check.}

\item{dataset_type}{Dataset type ("GWAS" or "QTL").}

\item{verbose}{Print messages.}
}
\description{
Check whether the input data (\code{dat}) 
has the minimum columns required
to run each fine-mapping method, as well as suggested columns.
}
\examples{
dat <- echodata::BST1
finemap_methods <- echofinemap::check_required_cols(dat=dat)
}
