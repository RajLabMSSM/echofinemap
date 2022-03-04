% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/required_cols.R
\name{required_cols}
\alias{required_cols}
\title{Table of required columns}
\usage{
required_cols(
  dataset_type = "GWAS",
  for_all = c("SNP", "CHR", "POS", "Effect", "StdErr")
)
}
\arguments{
\item{dataset_type}{Dataset type ("GWAS" or "QTL").}

\item{for_all}{Columns required for all methods.}
}
\description{
Return a table with the minimum columns required
to run each fine-mapping method, as well as suggested columns.
}
\examples{
d <- echofinemap::required_cols()
}