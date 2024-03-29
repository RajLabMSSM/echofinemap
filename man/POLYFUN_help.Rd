% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/POLYFUN_help.R
\name{POLYFUN_help}
\alias{POLYFUN_help}
\title{POLYFUN help}
\usage{
POLYFUN_help(polyfun = NULL, conda_env = "echoR_mini", verbose = TRUE)
}
\arguments{
\item{polyfun}{[Optional] Path to PolyFun executable.}

\item{conda_env}{Conda environment name.}

\item{verbose}{Print messages.}
}
\value{
PolyFun help message as character vector.
}
\description{
Display PolyFun help. 
Will automatically install PolyFun if has not been installed already.
}
\examples{
h <- echofinemap::POLYFUN_help()
}
\seealso{
Other polyfun: 
\code{\link{POLYFUN_compute_priors}()},
\code{\link{POLYFUN_download_ref_files}()},
\code{\link{POLYFUN_find_folder}()},
\code{\link{POLYFUN_finemapper}()},
\code{\link{POLYFUN_gather_annotations}()},
\code{\link{POLYFUN_gather_ldscores}()},
\code{\link{POLYFUN_import_priors}()},
\code{\link{POLYFUN_initialize}()},
\code{\link{POLYFUN_munge_summ_stats}()},
\code{\link{POLYFUN_prepare_snp_input}()},
\code{\link{POLYFUN_run_ldsc}()},
\code{\link{POLYFUN}()}
}
\concept{polyfun}
