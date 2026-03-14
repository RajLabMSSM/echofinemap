test_that("POLYFUN_compute_priors works", {

    testthat::skip_if_not_installed("echoconda")
    conda_available <- tryCatch(
        echoconda::env_exists(conda_env = "echoR_mini"),
        error = function(e) FALSE
    )
    testthat::skip_if_not(conda_available,
                          message = "echoR_mini conda env not available")
    testthat::skip_if_not(
        dir.exists(system.file("tools", "polyfun", package = "echofinemap")),
        message = "PolyFun submodule not installed"
    )
    ## This test is computationally intensive
    testthat::skip_on_cran()

    fullSS_path <- echodata::example_fullSS()
    ldsc_files <- echofinemap::POLYFUN_compute_priors(
        fullSS_path = fullSS_path
    )
    testthat::expect_gt(length(ldsc_files), 0)
})
