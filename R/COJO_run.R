#' Run \emph{GCTA-COJO}
#'
#' @family COJO 
#' @inheritParams COJO_args
#' @inheritDotParams COJO_args
COJO_run <- function(gcta_path,
                     bfile,
                     cojo_slct = TRUE,
                     cojo_cond = TRUE,
                     cojo_joint = FALSE,
                     conditioned_snps = NULL,
                     prefix = "cojo",
                     cojo_dir,
                     cojo_file = file.path(cojo_dir,
                                           paste0(prefix,".file.ma")),
                     exclude = file.path(cojo_dir,
                                         "excluded_snps.txt"),
                     verbose = TRUE,
                     ...){   
    # Stepwise selection procedure to identify independent SNPs
    cmd <- paste(gcta_path, 
                 COJO_args(bfile = bfile, 
                           cojo_file = cojo_file, 
                           out = file.path(cojo_dir,prefix), 
                           cojo_slct = cojo_slct,
                           cojo_cond = cojo_cond,
                           cojo_joint = cojo_joint,
                           exclude = exclude, 
                           ...
                 ) 
    )
    messager("+ Running COJO",v=verbose)
    echoconda::cmd_print(cmd, verbose = verbose)
    system(cmd) 
    #### Return out files ####
    paths <- COJO_list_files(cojo_dir = cojo_dir,
                             prefix = prefix,
                             nm_step = cojo_slct,
                             nm_joint = cojo_joint,
                             nm_cond = cojo_cond)
    return(paths)
}
