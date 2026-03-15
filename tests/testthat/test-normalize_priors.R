test_that("normalize_priors sums to 1", {

    x <- c(2, 3, 5)
    res <- echofinemap:::normalize_priors(x, verbose = FALSE)
    testthat::expect_equal(sum(res), 1)
    testthat::expect_equal(res, x / sum(x))
})

test_that("normalize_priors handles custom function", {

    x <- c(10, 20, 30)
    fn <- function(x) x / max(x)
    res <- echofinemap:::normalize_priors(x, fn = fn, verbose = FALSE)
    testthat::expect_equal(res, c(1/3, 2/3, 1))
})

test_that("normalize_priors handles NA values", {

    x <- c(1, 2, NA, 4)
    res <- echofinemap:::normalize_priors(x, verbose = FALSE)
    ## Default fn divides by sum(x, na.rm=TRUE), i.e. 7
    testthat::expect_equal(res[1], 1/7)
    testthat::expect_true(is.na(res[3]))
})
