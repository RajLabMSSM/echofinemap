% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PAINTOR_process_results.R
\name{PAINTOR_process_results}
\alias{PAINTOR_process_results}
\title{PAINTOR: process results}
\usage{
PAINTOR_process_results(dat_merged, res_paths, credset_thresh, verbose = TRUE)
}
\arguments{
\item{dat_merged}{Merged PAINTOR results.}

\item{res_paths}{Paths to PAINTOR results.}

\item{credset_thresh}{The minimum mean Posterior Probability
(across all fine-mapping methods used) of SNPs to be included in 
the "mean.CS" column.}

\item{verbose}{Print messages.}
}
\description{
Process PAINTOR fine-mapping results.
}
\keyword{internal}
