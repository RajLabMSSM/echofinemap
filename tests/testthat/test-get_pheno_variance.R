test_that("get_pheno_variance from numeric vector", {

    dat <- echodata::BST1
    var_y <- c(1, 2, 3, 4, 5)
    res <- echofinemap:::get_pheno_variance(
        dat = dat,
        case_control = FALSE,
        var_y = var_y,
        verbose = FALSE
    )
    testthat::expect_equal(res, stats::var(var_y))
})

test_that("get_pheno_variance from column name", {

    dat <- data.table::data.table(
        Effect = c(0.1, 0.2, 0.3, 0.4),
        StdErr = c(0.01, 0.02, 0.03, 0.04)
    )
    res <- echofinemap:::get_pheno_variance(
        dat = dat,
        case_control = FALSE,
        var_y = "Effect",
        verbose = FALSE
    )
    testthat::expect_equal(res, stats::var(dat$Effect))
})

test_that("get_pheno_variance from case-control columns", {

    dat <- data.table::data.table(
        N_cases = rep(1000, 5),
        N_controls = rep(2000, 5)
    )
    res <- echofinemap:::get_pheno_variance(
        dat = dat,
        case_control = TRUE,
        var_y = "N",
        verbose = FALSE
    )
    testthat::expect_true(is.numeric(res))
    testthat::expect_true(res > 0 && res < 1)
})

test_that("get_pheno_variance falls back to 1 when data unavailable", {

    dat <- data.table::data.table(
        Effect = c(0.1, 0.2)
    )
    res <- echofinemap:::get_pheno_variance(
        dat = dat,
        case_control = FALSE,
        var_y = "nonexistent_column",
        verbose = FALSE
    )
    testthat::expect_equal(res, 1)
})
