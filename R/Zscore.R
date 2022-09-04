Zscore <- function(x, z.info){ 
    # Need to use the mean and standard deviation of the FULL dataset 
    ## (i.e. all beta fomr the full summary stats file)
    sample.stdv <- z.info$sample.stdv
    sample.mean <- z.info$sample.mean 
    z <- (x - sample.mean) / sample.stdv 
    return(z)
}