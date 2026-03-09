test_that("POLYFUN_compute_priors works", {

    testthat::skip_if_not_installed("echoconda")
    testthat::skip_if_not(
        echoconda::env_exists(conda_env = "echoR_mini"),
        message = "echoR_mini conda env not available"
    )

    fullSS_path <- echodata::example_fullSS()
    ldsc_files <- echofinemap::POLYFUN_compute_priors(fullSS_path=fullSS_path)
})
