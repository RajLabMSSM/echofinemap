test_that("FINEMAP works", {

    ## Find the FINEMAP binary (downloads if needed)
    FINEMAP_path <- echofinemap:::FINEMAP_find_executable(verbose = FALSE)
    ## Verify it can actually run on this system
    testthat::skip_if_not(
        isTRUE(echofinemap:::FINEMAP_check_runnable(FINEMAP_path,
                                                     verbose = FALSE)),
        "FINEMAP binary cannot run on this system"
    )

    set.seed(1234)
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    out <- echoLD::subset_common_snps(LD_matrix, dat)
    LD_matrix <- out$LD
    dat <- out$DT

    #### FINEMAP v1.4.1 (default) ####
    dat2 <- echofinemap::FINEMAP(dat = dat,
                                  locus_dir = locus_dir,
                                  LD_matrix = LD_matrix,
                                  FINEMAP_path = FINEMAP_path,
                                  force_new = TRUE)

    testthat::expect_true(all(c("CS", "PP") %in% colnames(dat2)))
    testthat::expect_gt(sum(dat2$CS > 0, na.rm = TRUE), 0)
    testthat::expect_gt(sum(dat2$PP > 0.95, na.rm = TRUE), 0)
})
