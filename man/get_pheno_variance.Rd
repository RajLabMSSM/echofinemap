% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pheno_variance.R
\name{get_pheno_variance}
\alias{get_pheno_variance}
\title{Compute phenotype variance}
\usage{
get_pheno_variance(dat, case_control, var_y, verbose = TRUE)
}
\arguments{
\item{dat}{Fine-mapping results data.}

\item{case_control}{Whether the summary statistics come from a case-control
study (e.g. a GWAS of having Alzheimer's Disease or not) (\code{TRUE})
or a quantitative study (e.g. a GWAS of height, or an eQTL) (\code{FALSE}).}

\item{var_y}{[Optional] User-supplied phenotypic variance value(s). 
Can be one of the following:
\itemize{
\item{\code{NULL}: }{Variance will be inferred automatically by SUSIE.}
\item{Numeric vector: }{Variance will be computed directly from vector.}
\item{Character string: }{The name of a column in \code{dat}
 to extract a numeric vector from to compute variance.}
\item{"case_control"}{Variance will be inferred from the proportion 
of cases/controls in the study. 
Only works when both "N_cases" and "N_controls" are columns in \code{dat}.}
}}

\item{verbose}{Print messages.}
}
\description{
Compute phenotype variance (\code{var_y} in \pkg{susieR}).
}
\keyword{internal}
