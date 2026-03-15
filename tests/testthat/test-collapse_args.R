test_that("collapse_args returns NULL for empty list", {

    res <- echofinemap:::collapse_args(list())
    testthat::expect_null(res)
})

test_that("collapse_args passes through character strings", {

    input <- "--n-iterations 5000 --sss"
    res <- echofinemap:::collapse_args(input)
    testthat::expect_equal(res, input)
})

test_that("collapse_args collapses named list into string", {

    args_list <- list("--n-iterations" = 5000, "--sss" = NULL)
    res <- echofinemap:::collapse_args(args_list)
    testthat::expect_true(is.character(res))
    testthat::expect_true(grepl("--n-iterations 5000", res))
    testthat::expect_true(grepl("--sss", res))
})

test_that("collapse_args handles single-element list", {

    args_list <- list("--model" = "sss")
    res <- echofinemap:::collapse_args(args_list)
    testthat::expect_equal(res, "--model sss")
})
