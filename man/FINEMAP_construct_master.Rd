% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FINEMAP_construct_master.R
\name{FINEMAP_construct_master}
\alias{FINEMAP_construct_master}
\title{Construct the \code{FINAMAP} master file}
\source{
\url{http://www.christianbenner.com}

\code{
locus_dir <- file.path(tempdir(),echodata::locus_dir)
master_path <- echofinemap:::FINEMAP_construct_master(locus_dir=locus_dir,
                                                      n_samples=25000)
}
}
\usage{
FINEMAP_construct_master(
  locus_dir,
  n_samples,
  dir = "FINEMAP",
  dataset_number = 1,
  data.k_path = NULL,
  verbose = TRUE
)
}
\description{
Creates and saves the master file
which tells \code{FINEMAP} where to find each input file.
}
\seealso{
Other FINEMAP: 
\code{\link{FINEMAP_construct_data}()},
\code{\link{FINEMAP_find_executable}()},
\code{\link{FINEMAP_process_results}()},
\code{\link{FINEMAP}()}
}
\concept{FINEMAP}
\keyword{internal}
