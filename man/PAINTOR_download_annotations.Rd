% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PAINTOR_download_annotations.R
\name{PAINTOR_download_annotations}
\alias{PAINTOR_download_annotations}
\title{Download annotations for PAINTOR}
\usage{
PAINTOR_download_annotations(
  dat_merged,
  locus_dir,
  PT_results_path,
  annot_sample = FALSE,
  annot_xgr = NULL,
  annot_roadmap = NULL,
  chrom_states = NULL,
  use_annotations = TRUE,
  conda_env = "echoR_mini",
  nThread = 1,
  verbose = TRUE
)
}
\arguments{
\item{locus_dir}{Locus-specific directory to store results in.}

\item{annot_xgr}{Use annotations from \pkg{XGR}
via \link[echoannot]{XGR_query}.}

\item{annot_roadmap}{Use annotations from \pkg{Roadmap} 
via \link[echoannot]{ROADMAP_query}.}

\item{chrom_states}{Filter results by chromatin states.}

\item{use_annotations}{Whether to perform functional fine-mapping with 
specified annotations (\code{TRUE}) 
or simply perform statistical fine-mapping without any annotations.}

\item{conda_env}{Conda environment to use.}

\item{nThread}{Number of threads to parallelise across (when applicable).}

\item{verbose}{Print messages.}
}
\description{
Download annotations to perform functional fine-mapping with PAINTOR.
}
\keyword{internal}
