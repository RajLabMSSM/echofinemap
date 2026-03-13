test_that("POLYFUN_SUSIE works", {

    testthat::skip_if_not_installed("echoconda")
    testthat::skip_if_not_installed("Ckmeans.1d.dp")
    testthat::skip_if_not(
        echoconda::env_exists(conda_env = "echoR_mini"),
        message = "echoR_mini conda env not available"
    )
    ## PolyFun submodule must be installed
    testthat::skip_if_not(
        dir.exists(system.file("tools", "polyfun", package = "echofinemap")),
        message = "PolyFun submodule not installed"
    )

    set.seed(1234)
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix

    run_tests <- function(datx, daty, n_causal, cs_snps){
        testthat::expect_true(
            !all(c("POLYFUN.h2", "CS", "PP") %in% colnames(datx))
        )
        testthat::expect_true(
            all(c("POLYFUN.h2", "CS", "PP") %in% colnames(daty))
        )
        cs <- subset(daty, PP > .95)
        testthat::expect_equal(nrow(cs), n_causal)
        testthat::expect_equal(cs[cs$CS == min(cs$CS), ]$SNP, cs_snps)
    }

    #### POLYFUN: SUSIE #####
    dat2 <- echofinemap::POLYFUN(locus_dir = locus_dir,
                                  dat = dat,
                                  LD_matrix = LD_matrix,
                                  method = "SUSIE")
    run_tests(datx = dat,
              daty = dat2,
              n_causal = 5,
              cs_snps = "rs10003136")
})

test_that("POLYFUN_FINEMAP works", {

    testthat::skip_if_not_installed("echoconda")
    testthat::skip_if_not_installed("Ckmeans.1d.dp")
    testthat::skip_if_not(
        echoconda::env_exists(conda_env = "echoR_mini"),
        message = "echoR_mini conda env not available"
    )
    testthat::skip_if_not(
        dir.exists(system.file("tools", "polyfun", package = "echofinemap")),
        message = "PolyFun submodule not installed"
    )
    ## FINEMAP binary must be runnable
    FINEMAP_path <- tryCatch(
        echofinemap:::FINEMAP_find_executable(verbose = FALSE),
        error = function(e) NULL
    )
    testthat::skip_if(is.null(FINEMAP_path),
                      "FINEMAP executable not available")
    testthat::skip_if_not(
        isTRUE(tryCatch(
            echofinemap:::FINEMAP_check_runnable(FINEMAP_path, verbose = FALSE),
            error = function(e) FALSE
        )),
        "FINEMAP binary cannot run on this system"
    )

    set.seed(1234)
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix

    dat3 <- echofinemap::POLYFUN(locus_dir = locus_dir,
                                  dat = dat,
                                  LD_matrix = LD_matrix,
                                  method = "FINEMAP",
                                  force_new = TRUE)
    testthat::expect_true(all(c("POLYFUN.h2", "CS", "PP") %in% colnames(dat3)))
    testthat::expect_gt(sum(dat3$CS > 0, na.rm = TRUE), 0)
    testthat::expect_gt(sum(dat3$PP > 0.95, na.rm = TRUE), 0)
})
