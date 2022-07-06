% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multifinemap_handler_method.R
\name{multifinemap_handler_method}
\alias{multifinemap_handler_method}
\title{Multi-finemap handler: select fine-mapping method}
\usage{
multifinemap_handler_method(
  dat,
  locus_dir,
  fullSS_path = NULL,
  finemap_method,
  finemap_args = NULL,
  dataset_type = "GWAS",
  force_new_finemap = FALSE,
  LD_matrix = NULL,
  n_causal = 5,
  conditioned_snps,
  compute_n = "ldsc",
  PAINTOR_QTL_datasets = NULL,
  PP_threshold = 0.95,
  case_control = TRUE,
  priors_col = NULL,
  verbose = TRUE,
  nThread = 1,
  conda_env = "echoR_mini"
)
}
\arguments{
\item{dat}{Fine-mapping results data.}

\item{compute_n}{How to compute per-SNP sample size (new column "N").
\itemize{
\item{\code{0}: }{N will not be computed.}
\item{\code{>0}: }{If any number >0 is provided,
that value will be set as N for every row.
\strong{Note}: Computing N this way is incorrect and should be avoided
if at all possible.}
\item{\code{"sum"}: }{N will be computed as:
cases (N_CAS) + controls (N_CON), so long as both columns are present}.
\item{\code{"effective"}: }{N will be computed as effective sample size:
cases (N_CAS) + controls (N_CON), so long as both columns are present}.
}}

\item{priors_col}{[Optional] Name of the a column in 
\code{dat} to extract SNP-wise prior probabilities from.}

\item{verbose}{Print messages.}
}
\description{
Multi-finemap handler: select fine-mapping method
}
\seealso{
Other finemapping functions: 
\code{\link{create_method_path}()},
\code{\link{multifinemap_handler}()},
\code{\link{multifinemap}()}
}
\concept{finemapping functions}
\keyword{internal}