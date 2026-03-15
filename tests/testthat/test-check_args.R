test_that("check_args returns defaults when no user args given", {

    finemap_args <- list(SUSIE = list())
    res <- echofinemap:::check_args(
        finemap_args = finemap_args,
        finemap_method = "SUSIE",
        verbose = FALSE
    )
    testthat::expect_true(is.list(res))
    ## Should contain the default args for SUSIE
    testthat::expect_true(length(res) > 0)
})

test_that("check_args respects user-supplied valid arguments", {

    finemap_args <- list(SUSIE = list(rescale_priors = FALSE))
    res <- echofinemap:::check_args(
        finemap_args = finemap_args,
        finemap_method = "SUSIE",
        verbose = FALSE
    )
    testthat::expect_false(res$rescale_priors)
})

test_that("check_args ignores invalid argument names", {

    finemap_args <- list(SUSIE = list(fake_arg_xyz = 42))
    res <- echofinemap:::check_args(
        finemap_args = finemap_args,
        finemap_method = "SUSIE",
        verbose = FALSE
    )
    testthat::expect_null(res$fake_arg_xyz)
})

test_that("check_args works with FINEMAP method", {

    finemap_args <- list(FINEMAP = list())
    res <- echofinemap:::check_args(
        finemap_args = finemap_args,
        finemap_method = "FINEMAP",
        verbose = FALSE
    )
    testthat::expect_true(is.list(res))
    testthat::expect_true(length(res) > 0)
})
