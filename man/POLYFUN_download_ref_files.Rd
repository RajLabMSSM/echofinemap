% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/POLYFUN_download_ref_files.R
\name{POLYFUN_download_ref_files}
\alias{POLYFUN_download_ref_files}
\title{Download reference files}
\source{
\code{
ref_prefix <- POLYFUN_download_ref_files()
}
}
\usage{
POLYFUN_download_ref_files(
  alkes_url = paste("https://data.broadinstitute.org/alkesgroup",
    "LDSCORE/1000G_Phase1_plinkfiles.tgz", sep = "/"),
  output_dir = tools::R_user_dir(package = "echofinemap", which = "cache"),
  force_overwrite = FALSE,
  return_prefix = TRUE,
  download_method = "axel",
  conda_env = "echoR_mini",
  verbose = TRUE
)
}
\value{
File prefix.
}
\description{
Download 1000 Genomes reference files.
}
\seealso{
Other polyfun: 
\code{\link{POLYFUN_compute_priors}()},
\code{\link{POLYFUN_find_folder}()},
\code{\link{POLYFUN_finemapper}()},
\code{\link{POLYFUN_gather_annotations}()},
\code{\link{POLYFUN_gather_ldscores}()},
\code{\link{POLYFUN_help}()},
\code{\link{POLYFUN_import_priors}()},
\code{\link{POLYFUN_initialize}()},
\code{\link{POLYFUN_munge_summ_stats}()},
\code{\link{POLYFUN_prepare_snp_input}()},
\code{\link{POLYFUN_run_ldsc}()},
\code{\link{POLYFUN}()}
}
\concept{polyfun}
\keyword{internal}
