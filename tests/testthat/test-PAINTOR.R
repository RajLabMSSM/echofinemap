test_that("PAINTOR works", {
    
    dat <- echodata::BST1
    ## For example only;
    ## normally you need to compute ZSCORE using the
    ## full genome-wide summary stats.
    dat[,ZSCORE:=(-log10(P))]
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(),echodata::locus_dir)
    #### Single GWAS ####
    dat2 <- PAINTOR(dat = dat,
                    locus_dir = locus_dir,
                    LD_matrix = LD_matrix,
                    max_causal = 2,
                    method = "enumerate",
                    set_seed = 2019)
    testthat::expect_false(all(c("PP","CS") %in% names(dat)))
    testthat::expect_true(all(c("PP","CS") %in% names(dat2)))
    testthat::expect_equal(sum(dat2$CS),2)
    testthat::expect_equal(max(dat2$PP),1)
    testthat::expect_equal(round(mean(dat2$PP),3),0.021)
    
    #### Multi-GWAS ####
    dat_gwas2 <- data.table::copy(dat)
    dat_gwas2$ZSCORE <- abs(jitter(dat_gwas2$ZSCORE, amount = 1e-16))
    dat3 <- PAINTOR(dat = list("gwas1"=dat, "gwas2"=dat_gwas2),
                    locus_dir = locus_dir,
                    LD_matrix = LD_matrix,
                    max_causal = 2,
                    method = "enumerate",
                    set_seed = 2022)
    testthat::expect_false(all(c("PP","CS") %in% names(dat)))
    testthat::expect_true(all(c("PP","CS") %in% names(dat3)))
    testthat::expect_equal(sum(dat3$CS),2)
    testthat::expect_equal(max(dat3$PP),1)
    testthat::expect_equal(round(mean(dat3$PP),3),0.021)
})
