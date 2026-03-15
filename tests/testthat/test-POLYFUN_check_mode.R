test_that("POLYFUN_check_mode accepts valid modes", {

    res <- echofinemap:::POLYFUN_check_mode(
        mode = "precomputed", verbose = FALSE
    )
    testthat::expect_equal(res, "precomputed")

    res2 <- echofinemap:::POLYFUN_check_mode(
        mode = "parametric", verbose = FALSE
    )
    testthat::expect_equal(res2, "parametric")

    res3 <- echofinemap:::POLYFUN_check_mode(
        mode = "non-parametric", verbose = FALSE
    )
    testthat::expect_equal(res3, "non-parametric")
})

test_that("POLYFUN_check_mode errors on invalid mode", {

    testthat::expect_error(
        echofinemap:::POLYFUN_check_mode(
            mode = "invalid_mode", verbose = FALSE
        ),
        regexp = "must be one of"
    )
})

test_that("POLYFUN_check_mode handles uppercase input", {

    res <- echofinemap:::POLYFUN_check_mode(
        mode = "PRECOMPUTED", verbose = FALSE
    )
    testthat::expect_equal(res, "precomputed")
})
