% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_method_path.R
\name{create_method_path}
\alias{create_method_path}
\title{Create method path}
\usage{
create_method_path(
  locus_dir,
  finemap_method,
  include_astrices = FALSE,
  LD_reference = NULL,
  compress = FALSE
)
}
\arguments{
\item{locus_dir}{Locus-specific directory to store results in.}

\item{finemap_method}{Fine-mapping method to run. 
See \link[echofinemap]{lfm} for a list of all fine-mapping methods currently
available.}

\item{include_astrices}{Whether to keep any astrices in file path.}

\item{LD_reference}{Name of the LD reference panel.}

\item{compress}{Whether to add ".gz" at the end of the file path.}
}
\description{
Create a fine-mapping method-specific path.
}
\examples{
locus_dir <- echodata::locus_dir
path <- echofinemap::create_method_path(locus_dir = locus_dir, 
                                        finemap_method = "SUSIE")
}
\seealso{
Other finemapping functions: 
\code{\link{multifinemap_handler_method}()},
\code{\link{multifinemap_handler}()},
\code{\link{multifinemap}()}
}
\concept{finemapping functions}
