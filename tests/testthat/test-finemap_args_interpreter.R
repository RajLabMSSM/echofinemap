test_that("finemap_args_interpreter returns args for matching method", {

    finemap_args <- list(
        FINEMAP = list("--n-iterations" = 5000, "--sss" = NULL),
        SUSIE = list(rescale_priors = TRUE)
    )
    res <- echofinemap:::finemap_args_interpreter(
        finemap_args = finemap_args,
        method = "FINEMAP"
    )
    testthat::expect_true(is.list(res))
    testthat::expect_equal(res[["--n-iterations"]], 5000)
})

test_that("finemap_args_interpreter returns NULL for non-matching method", {

    finemap_args <- list(
        FINEMAP = list("--n-iterations" = 5000)
    )
    res <- echofinemap:::finemap_args_interpreter(
        finemap_args = finemap_args,
        method = "ABF"
    )
    testthat::expect_null(res)
})

test_that("finemap_args_interpreter returns NULL for empty args", {

    finemap_args <- list()
    res <- echofinemap:::finemap_args_interpreter(
        finemap_args = finemap_args,
        method = "FINEMAP"
    )
    testthat::expect_null(res)
})
