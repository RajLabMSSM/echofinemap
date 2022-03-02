test_that("FINEMAP works", {
  
    locus_dir <- file.path(tempdir(),echodata::locus_dir)
    dat <- echodata::BST1;
    LD_matrix <- echodata::BST1_LD_matrix
    dir.create(file.path(locus_dir,"FINEMAP"),
     showWarnings = FALSE, recursive = TRUE)
    out <- echoLD::subset_common_snps(LD_matrix, dat)
    LD_matrix <- out$LD
    dat <- out$DT
    
    #### FINEMAP with different params ####  
    dat2 <- echofinemap::FINEMAP(dat=dat,
                                locus_dir=locus_dir,
                                LD_matrix=LD_matrix,
                                finemap_version="1.4")
    
    dat3 <- echofinemap::FINEMAP(dat=dat,
                                 locus_dir=locus_dir,
                                 LD_matrix=LD_matrix,
                                 finemap_version="1.3.1")
    
    dat4 <- echofinemap::FINEMAP(dat=dat,
                                 locus_dir=locus_dir,
                                 LD_matrix=LD_matrix,
                                 finemap_version="1.3.1",
                                 force_new = TRUE,
                                 n_causal = 10)
    
    #### unit tests ####
    run_tests <- function(dat,
                          cred_set=5){
        testthat::expect_true(all(c("CS","PP") %in% colnames(dat)))
        testthat::expect_equal(sum(dat$CS>0, na.rm = TRUE), cred_set)
        testthat::expect_equal(sum(dat$PP>0.95, na.rm = TRUE), cred_set)
    }
    
    testthat::expect_true(!all(c("CS","PP") %in% colnames(dat)))
    run_tests(dat = dat2, cred_set = 5)
    run_tests(dat = dat3, cred_set = 5)
    run_tests(dat = dat4, cred_set = 10) 
})
