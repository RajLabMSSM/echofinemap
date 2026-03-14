test_that("POLYFUN_help works", {

    testthat::skip_if_not_installed("echoconda")
    conda_available <- tryCatch(
        echoconda::env_exists(conda_env = "echoR_mini"),
        error = function(e) FALSE
    )
    testthat::skip_if_not(conda_available,
                          message = "echoR_mini conda env not available")

    out <- POLYFUN_help()
    testthat::expect_true(grepl("PolyFun",out[[2]]))
    testthat::expect_gte(length(out), 50)
})
